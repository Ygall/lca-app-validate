#' Diff complet (énoncé + propositions) d'une seule question, entre l'original
#' (version initiale) et l'état final (après arbitrage / édition en Partie 2)
compute_question_diff <- function(rv, numero) {
  props <- dplyr::left_join(
    rv$final[rv$final$numero == numero, ],
    rv$propositions_json[rv$propositions_json$numero == numero, ],
    by = c("numero", "lettre")
  )
  props$texte_modifie <- props$texte_final != props$texte
  props$justification_modifie <- props$justification_final != props$justification
  props$statut_modifie <- props$correcte_final != props$correcte
  props <- props[order(props$lettre), ]

  # Notions : comparaison (id + rôle) entre le JSON d'origine et l'état arbitré.
  lignes_notions <- function(df, L) {
    if (is.null(df)) return(notions_vide())
    r <- df[df$question == numero & df$lettre == L, ]
    r[order(r$numero, r$id), ]
  }
  cle_notions <- function(r) paste(r$id, r$role, collapse = "|")
  props$notions_initiales <- vapply(props$lettre, function(L) format_notions(lignes_notions(rv$notions_json, L), rv$referentiel), character(1))
  props$notions_finales <- vapply(props$lettre, function(L) format_notions(lignes_notions(rv$notions_final, L), rv$referentiel), character(1))
  props$notions_modifie <- vapply(props$lettre, function(L) {
    !identical(cle_notions(lignes_notions(rv$notions_json, L)), cle_notions(lignes_notions(rv$notions_final, L)))
  }, logical(1))
  props$any_modifie <- props$texte_modifie | props$justification_modifie | props$statut_modifie | props$notions_modifie

  flags_q <- rv$flags[rv$flags$numero == numero, c("lettre", "commentaire")]
  props <- dplyr::left_join(props, flags_q, by = "lettre")
  props$flagged <- !is.na(props$commentaire) & nzchar(trimws(props$commentaire))

  enonce_original <- rv$questions_meta$enonce[rv$questions_meta$numero == numero]
  enonce_final <- rv$questions_final$enonce[rv$questions_final$numero == numero]
  enonce_modifie <- !identical(enonce_original, enonce_final)

  any_modifie <- enonce_modifie || any(props$any_modifie)
  # Un flag "non résolu" signale une proposition marquée en Partie 1 dont la
  # question n'a encore reçu aucun arbitrage en Partie 2 (énoncé ou
  # proposition) : une fois la question modifiée, on considère le
  # signalement traité, quelle que soit la proposition effectivement touchée.
  props$flag_non_resolu <- props$flagged & !any_modifie

  list(
    numero = numero,
    enonce_original = enonce_original,
    enonce_final = enonce_final,
    enonce_modifie = enonce_modifie,
    propositions = props,
    any_modifie = any_modifie,
    n_flags_non_resolus = sum(props$flag_non_resolu)
  )
}

#' TRUE si la question `numero` diffère de la version initiale (énoncé ou propositions)
question_modifiee <- function(rv, numero) {
  compute_question_diff(rv, numero)$any_modifie
}

#' Compare les réponses cochées par l'humain (Partie 1) au corrigé initial du JSON
#' — indépendamment des arbitrages faits ensuite en Partie 2. Table de
#' contingence 2x2 (initial correct × humain coché) -> accord, désaccords, taux
#' d'accord et kappa de Cohen.
compute_agreement_stats <- function(rv) {
  df <- dplyr::left_join(
    rv$propositions_json[, c("numero", "lettre", "correcte")],
    rv$human_answers,
    by = c("numero", "lettre")
  )
  n <- nrow(df)
  a <- sum(df$correcte & df$coche_humain)
  b <- sum(df$correcte & !df$coche_humain)
  c <- sum(!df$correcte & df$coche_humain)
  d <- sum(!df$correcte & !df$coche_humain)

  p_o <- (a + d) / n
  p_e <- ((a + b) / n) * ((a + c) / n) + ((c + d) / n) * ((b + d) / n)
  kappa <- if (isTRUE(all.equal(p_e, 1))) NA_real_ else (p_o - p_e) / (1 - p_e)

  list(
    total = n,
    ia_oui_humain_oui = a,
    ia_non_humain_non = d,
    accord = a + d,
    ia_oui_humain_non = b,
    ia_non_humain_oui = c,
    pourcentage_accord = p_o,
    kappa = kappa
  )
}

lettre_coloree <- function(letter, correcte) {
  couleur <- if (isTRUE(correcte)) "#1E8A3B" else "#A6402E"
  htmltools::tags$strong(style = paste0("color:", couleur, ";"), paste0(letter, ") "))
}

render_diff_proposition <- function(row) {
  letter <- row$lettre[1]
  detail_notions <- if (isTRUE(row$notions_modifie[1])) {
    htmltools::tags$div(
      class = "small text-muted ms-3 mt-1",
      htmltools::tags$em("Notions initiales : "), row$notions_initiales[1], htmltools::tags$br(),
      htmltools::tags$em("Notions modifiées : "), row$notions_finales[1]
    )
  }
  # Seules les notions ont changé : le texte reste affiché une fois (pas de
  # paire barré/nouveau identique), suivi du détail des notions.
  seules_notions <- isTRUE(row$notions_modifie[1]) &&
    !(row$texte_modifie[1] || row$justification_modifie[1] || row$statut_modifie[1])
  if (seules_notions) {
    return(htmltools::tagList(
      htmltools::tags$div(
        class = "diff-line diff-unchanged",
        lettre_coloree(letter, row$correcte_final[1]), row$texte_final[1]
      ),
      detail_notions
    ))
  }
  if (!row$any_modifie[1]) {
    flag_badge <- if (isTRUE(row$flag_non_resolu[1])) {
      htmltools::tags$span(
        class = "flag-badge ms-2", title = row$commentaire[1],
        htmltools::tags$span(class = "flag-dot"), "Flag"
      )
    }
    return(htmltools::tags$div(
      class = "diff-line diff-unchanged d-flex align-items-center justify-content-between",
      htmltools::tags$span(lettre_coloree(letter, row$correcte_final[1]), row$texte_final[1]),
      flag_badge
    ))
  }
  htmltools::tagList(
    htmltools::tags$div(
      class = "diff-line diff-old",
      lettre_coloree(letter, row$correcte[1]), row$texte[1]
    ),
    htmltools::tags$div(
      class = "diff-line diff-new",
      lettre_coloree(letter, row$correcte_final[1]), row$texte_final[1]
    ),
    if (isTRUE(row$justification_modifie[1])) {
      htmltools::tags$div(
        class = "small text-muted ms-3 mt-1",
        htmltools::tags$em("Justification initiale : "), row$justification[1], htmltools::tags$br(),
        htmltools::tags$em("Justification modifiée : "), row$justification_final[1]
      )
    },
    detail_notions
  )
}

#' Rendu complet d'une question (tel que renvoyé par compute_question_diff) :
#' énoncé (avec diff initial/modifié si édité) puis chaque proposition
render_question_diff <- function(qd) {
  entete <- if (qd$enonce_modifie) {
    htmltools::tagList(
      htmltools::tags$div(class = "diff-line diff-old", qd$enonce_original),
      htmltools::tags$div(class = "diff-line diff-new mb-2", qd$enonce_final)
    )
  } else {
    htmltools::tags$p(class = "fw-bold mb-2", qd$enonce_final)
  }

  htmltools::tagList(
    entete,
    lapply(seq_len(nrow(qd$propositions)), function(i) render_diff_proposition(qd$propositions[i, ]))
  )
}
