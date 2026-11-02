init_rv <- function() {
  shiny::reactiveValues(
    imported = FALSE,
    raw_json = NULL,
    sujet_meta = NULL,
    controle_qualite = NULL,
    questions_meta = NULL,
    questions_final = NULL,
    propositions_json = NULL,
    human_answers = NULL,
    final = NULL,
    flags = NULL,
    notions_json = NULL,
    notions_final = NULL,
    referentiel = NULL
  )
}

#' Peuple rv avec un sujet fraîchement parsé, réinitialise l'état de session
load_sujet_into_rv <- function(rv, parsed) {
  rv$raw_json <- parsed$raw
  rv$sujet_meta <- parsed$sujet_meta
  rv$controle_qualite <- parsed$controle_qualite
  rv$questions_meta <- parsed$questions_meta
  rv$questions_final <- data.frame(
    numero = parsed$questions_meta$numero,
    enonce = parsed$questions_meta$enonce,
    stringsAsFactors = FALSE
  )
  rv$propositions_json <- parsed$propositions_json
  # Notions : référence initiale immuable (JSON) vs état arbitré (Partie 2).
  rv$notions_json <- parsed$notions_json
  rv$notions_final <- parsed$notions_json

  rv$human_answers <- data.frame(
    numero = parsed$propositions_json$numero,
    lettre = parsed$propositions_json$lettre,
    coche_humain = FALSE,
    stringsAsFactors = FALSE
  )

  rv$flags <- data.frame(
    numero = parsed$propositions_json$numero,
    lettre = parsed$propositions_json$lettre,
    commentaire = "",
    stringsAsFactors = FALSE
  )

  rv$final <- data.frame(
    numero = parsed$propositions_json$numero,
    lettre = parsed$propositions_json$lettre,
    correcte_final = parsed$propositions_json$correcte,
    texte_final = parsed$propositions_json$texte,
    justification_final = parsed$propositions_json$justification,
    statut_source = "ia",
    stringsAsFactors = FALSE
  )

  rv$imported <- TRUE
}
