chemin_exemple <- function() {
  system.file("extdata", "exemple_sujet.json", package = "lcavalidate", mustWork = TRUE)
}

chemin_schema <- function() {
  system.file("schema", "schema-sujet-lca.json", package = "lcavalidate", mustWork = TRUE)
}

# État de session du sujet d'exemple (14 questions), sous forme de liste :
# les fonctions testées lisent `rv$champ`, liste ou reactiveValues.
etat_exemple <- function() {
  etat <- etat_initial(parse_sujet_json(chemin_exemple()))
  etat$referentiel <- load_referentiel_notions(
    system.file("schema", "boussole_lca_notions.json", package = "lcavalidate", mustWork = TRUE)
  )
  etat
}

# Aller-retour par le texte JSON d'une session, comme un export suivi d'un import.
aller_retour <- function(etat) {
  lire_session(session_vers_json(session_vers_liste(etat)))
}
