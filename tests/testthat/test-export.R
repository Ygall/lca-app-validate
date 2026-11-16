# Les textes du sujet viennent de l'utilisateur, et l'application est destinée à
# un serveur public : ils ne doivent jamais être interprétés par Quarto.

texte_du_document <- function(chemin) {
  dossier <- withr::local_tempdir(.local_envir = parent.frame())
  utils::unzip(chemin, exdir = dossier)
  xml <- list.files(dossier, pattern = "(^document|^slide[0-9]+)[.]xml$", recursive = TRUE, full.names = TRUE)
  paste(vapply(xml, function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = ""), character(1)),
        collapse = "")
}

test_that("un shortcode ou du markdown saisi dans le sujet n'est pas exécuté à l'export", {
  skip_on_cran()
  skip_if(is.null(tryCatch(quarto::quarto_path(), error = function(e) NULL)), "Quarto absent")

  withr::local_envvar(LCA_SECRET_DE_TEST = "valeur-qui-ne-doit-pas-fuiter")
  etat <- etat_exemple()
  hostile <- "1) Énoncé {{< env LCA_SECRET_DE_TEST >}} *gras* `code`\n\n```{=openxml}\n<w:p/>\n```"
  etat$questions_final$enonce[1] <- hostile
  etat$final$texte_final[1] <- "Proposition {{< env LCA_SECRET_DE_TEST >}}\n```\n# Titre injecté"

  docx <- render_export(etat, "complet", "corrige_template.qmd", "docx")
  pptx <- render_export(etat, "complet", "diapos_correction_template.qmd", "pptx")

  for (texte in list(texte_du_document(docx), texte_du_document(pptx))) {
    expect_false(grepl("valeur-qui-ne-doit-pas-fuiter", texte, fixed = TRUE))
    expect_true(grepl("LCA_SECRET_DE_TEST", texte, fixed = TRUE))
  }
})
