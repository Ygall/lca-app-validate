`%||%` <- function(x, y) if (is.null(x)) y else x

esc_xml <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

get_lettre <- function(propositions, L) {
  propositions[[which(vapply(propositions, function(x) x$lettre, character(1)) == L)]]
}

reference_article <- function(sujet) {
  src <- sujet$source
  morceaux <- c(
    paste0(src$premier_auteur %||% "", " et al."),
    paste0(src$revue %||% "", ".", if (!is.null(src$annee)) paste0(" ", src$annee, ".") else "")
  )
  if (!is.null(src$pmid) && nzchar(src$pmid)) morceaux <- c(morceaux, paste0("PMID ", src$pmid, "."))
  if (!is.null(src$doi) && nzchar(src$doi)) morceaux <- c(morceaux, paste0("DOI : ", src$doi))
  paste(morceaux, collapse = " ")
}

# ---- DOCX (raw OOXML paragraphs, cf. word/document.xml) ----

docx_raw <- function(xml) cat("\n\n```{=openxml}\n", xml, "\n```\n\n", sep = "")

docx_pbdr <- paste0(
  "<w:pBdr>",
  "<w:top w:val=\"single\" w:sz=\"4\" w:space=\"1\" w:color=\"auto\"/>",
  "<w:left w:val=\"single\" w:sz=\"4\" w:space=\"4\" w:color=\"auto\"/>",
  "<w:bottom w:val=\"single\" w:sz=\"4\" w:space=\"1\" w:color=\"auto\"/>",
  "<w:right w:val=\"single\" w:sz=\"4\" w:space=\"4\" w:color=\"auto\"/>",
  "</w:pBdr>"
)

docx_encart <- function(sujet) {
  titre <- esc_xml(sujet$source$titre %||% sujet$identifiant %||% "")
  ref <- esc_xml(reference_article(sujet))
  xml <- paste0(
    "<w:p><w:pPr>", docx_pbdr, "<w:jc w:val=\"both\"/></w:pPr>",
    "<w:r><w:rPr><w:b/><w:sz w:val=\"28\"/></w:rPr><w:t xml:space=\"preserve\">", titre, "</w:t></w:r></w:p>",
    "<w:p><w:pPr>", docx_pbdr, "<w:jc w:val=\"both\"/></w:pPr>",
    "<w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">À partir de l'article </w:t></w:r>",
    "<w:r><w:t xml:space=\"preserve\">« ", titre, " », ", ref, ", répondez aux questions suivantes.</w:t></w:r></w:p>"
  )
  docx_raw(xml)
}

docx_question_block <- function(enonce, propositions, vision) {
  complet <- identical(vision, "complet")
  lettres <- c("A", "B", "C", "D", "E")

  paras <- paste0(
    "<w:p><w:pPr><w:rPr><w:b/></w:rPr></w:pPr>",
    "<w:r><w:rPr><w:b/></w:rPr><w:t xml:space=\"preserve\">", esc_xml(enonce), "</w:t></w:r></w:p>"
  )

  for (L in lettres) {
    p <- get_lettre(propositions, L)
    correcte <- complet && isTRUE(p$correcte)
    lettre_run <- if (correcte) {
      paste0("<w:r><w:rPr><w:b/><w:color w:val=\"00B050\"/></w:rPr><w:t xml:space=\"preserve\">", L, ") </w:t></w:r>")
    } else {
      paste0("<w:r><w:t xml:space=\"preserve\">", L, ") </w:t></w:r>")
    }
    texte_run <- paste0("<w:r><w:t xml:space=\"preserve\">", esc_xml(p$texte), "</w:t></w:r>")
    paras <- paste0(
      paras,
      "<w:p><w:pPr><w:spacing w:line=\"240\" w:lineRule=\"auto\"/></w:pPr><w:r><w:tab/></w:r>",
      lettre_run, texte_run, "</w:p>"
    )
  }

  if (complet) {
    justifs <- vapply(lettres, function(L) {
      p <- get_lettre(propositions, L)
      paste0(L, " : ", esc_xml(p$justification %||% ""))
    }, character(1))
    paras <- paste0(
      paras,
      "<w:p><w:pPr><w:spacing w:line=\"240\" w:lineRule=\"auto\"/></w:pPr>",
      "<w:r><w:rPr><w:i/><w:sz w:val=\"20\"/></w:rPr><w:t xml:space=\"preserve\">",
      paste(justifs, collapse = " ; "), "</w:t></w:r></w:p>"
    )
  }

  paras <- paste0(paras, "<w:p/>")
  docx_raw(paras)
}

# ---- PPTX (raw OOXML paragraphs, cf. ppt/slides/slideN.xml) ----
#
# Un bloc `{=openxml}` inséré tel quel finit comme enfant direct de <p:spTree>,
# hors de toute forme (<p:sp>) — invalide en PresentationML (contrairement à
# WordprocessingML, où <w:p> est un enfant direct valide de <w:body>). Chaque
# diapo doit donc reconstruire elle-même l'unique forme "Content Placeholder"
# (même schéma que celle générée par pandoc à partir du markdown), avec tous
# ses paragraphes à l'intérieur de son <p:txBody> — jamais de <a:p> nu.

pptx_raw <- function(xml) cat("\n\n```{=openxml}\n", xml, "\n```\n\n", sep = "")

pptx_content_shape <- function(paragraphs_xml) {
  # Décalé un peu plus bas que la position par défaut du gabarit (y=1200151)
  # pour laisser respirer sous le titre.
  paste0(
    "<p:sp><p:nvSpPr><p:cNvPr id=\"3\" name=\"Content Placeholder 2\"/>",
    "<p:cNvSpPr><a:spLocks noGrp=\"1\"/></p:cNvSpPr>",
    "<p:nvPr><p:ph idx=\"1\"/></p:nvPr></p:nvSpPr>",
    "<p:spPr><a:xfrm><a:off x=\"457200\" y=\"1500000\"/><a:ext cx=\"8229600\" cy=\"3094623\"/></a:xfrm></p:spPr>",
    "<p:txBody><a:bodyPr><a:normAutofit/></a:bodyPr><a:lstStyle/>",
    paragraphs_xml,
    "</p:txBody></p:sp>"
  )
}

# marL="0" indent="0" + <a:buNone/> : sans ça, chaque paragraphe hérite de la
# puce par défaut du placeholder de contenu du thème.
pptx_para <- function(texte, correcte = FALSE, size = 1800, italic = FALSE) {
  rpr <- paste0(
    "<a:rPr lang=\"fr-FR\"",
    if (!is.null(size)) paste0(" sz=\"", size, "\"") else "",
    if (isTRUE(correcte)) " b=\"1\"" else "",
    if (isTRUE(italic)) " i=\"1\"" else "",
    " dirty=\"0\">",
    if (isTRUE(correcte)) "<a:solidFill><a:srgbClr val=\"00B050\"/></a:solidFill>" else "",
    "</a:rPr>"
  )
  paste0(
    "<a:p><a:pPr marL=\"0\" indent=\"0\"><a:buNone/></a:pPr><a:r>", rpr,
    "<a:t xml:space=\"preserve\">", esc_xml(texte), "</a:t></a:r></a:p>"
  )
}

pptx_first_slide <- function(sujet) {
  titre <- sujet$source$titre %||% sujet$identifiant %||% "Sujet LCA"
  cat("\n\n## ", titre, "\n\n")
  pptx_raw(pptx_content_shape(pptx_para(reference_article(sujet))))
}

pptx_question_slide <- function(propositions, corrige = FALSE) {
  lettres <- c("A", "B", "C", "D", "E")
  xml <- paste(vapply(lettres, function(L) {
    p <- get_lettre(propositions, L)
    pptx_para(paste0(L, ") ", p$texte), correcte = corrige && isTRUE(p$correcte))
  }, character(1)), collapse = "")

  if (corrige) {
    justifs <- vapply(lettres, function(L) {
      p <- get_lettre(propositions, L)
      paste0(L, " : ", p$justification %||% "")
    }, character(1))
    xml <- paste0(xml, pptx_para(paste(justifs, collapse = " ; "), size = 1400, italic = TRUE))
  }

  pptx_raw(pptx_content_shape(xml))
}
