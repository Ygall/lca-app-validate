sanitize_filename <- function(x) {
  x <- x %||% ""
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  if (!nzchar(x)) x <- "sujet"
  x
}

session_save_path <- function(rv, saves_dir) {
  id <- sanitize_filename(rv$sujet_meta$identifiant %||% rv$sujet_meta$source$titre %||% "sujet")
  file.path(saves_dir, paste0(id, ".rds"))
}

#' Sauvegarde l'état courant de la session (sujet + réponses + arbitrages)
#'
#' Appelée à chaque point de sauvegarde naturel déjà existant (import, sortie
#' d'une question en Partie 1/2) : pas de débounce nécessaire, l'écriture d'un
#' .rds de cette taille est quasi instantanée.
persist_session_rds <- function(rv, saves_dir) {
  if (!isTRUE(rv$imported)) return(invisible(NULL))
  if (!dir.exists(saves_dir)) dir.create(saves_dir, recursive = TRUE)

  etat <- list(
    raw_json = rv$raw_json,
    sujet_meta = rv$sujet_meta,
    controle_qualite = rv$controle_qualite,
    questions_meta = rv$questions_meta,
    questions_final = rv$questions_final,
    propositions_json = rv$propositions_json,
    human_answers = rv$human_answers,
    final = rv$final,
    flags = rv$flags,
    notions_json = rv$notions_json,
    notions_final = rv$notions_final,
    sauvegarde_le = Sys.time()
  )
  saveRDS(etat, session_save_path(rv, saves_dir))
  invisible(NULL)
}

#' Liste les sessions sauvegardées disponibles, avec titre et progression humaine
list_saved_sessions <- function(saves_dir) {
  if (!dir.exists(saves_dir)) return(list())
  fichiers <- list.files(saves_dir, pattern = "\\.rds$", full.names = TRUE)

  resultats <- lapply(fichiers, function(f) {
    etat <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(etat)) return(NULL)
    list(
      chemin = f,
      titre = etat$sujet_meta$source$titre %||% etat$sujet_meta$identifiant %||% basename(f),
      repondu = length(unique(etat$human_answers$numero[etat$human_answers$coche_humain])),
      total = nrow(etat$questions_meta),
      sauvegarde_le = etat$sauvegarde_le
    )
  })
  resultats[!vapply(resultats, is.null, logical(1))]
}

#' Supprime définitivement un .rds de session sauvegardée
delete_session_rds <- function(path) {
  if (file.exists(path)) file.remove(path)
  invisible(NULL)
}

#' Recharge une session sauvegardée dans rv (sujet + réponses + arbitrages déjà faits)
load_session_rds <- function(rv, path) {
  etat <- readRDS(path)
  rv$raw_json <- etat$raw_json
  rv$sujet_meta <- etat$sujet_meta
  rv$controle_qualite <- etat$controle_qualite
  rv$questions_meta <- etat$questions_meta
  rv$questions_final <- etat$questions_final %||% data.frame(
    numero = etat$questions_meta$numero, enonce = etat$questions_meta$enonce, stringsAsFactors = FALSE
  )
  rv$propositions_json <- etat$propositions_json
  rv$human_answers <- etat$human_answers
  rv$final <- etat$final
  # Compatibilité avec les sessions .rds sauvegardées avant l'introduction
  # des flags (2026-08-14) : reconstruit une table vide plutôt que NULL.
  rv$flags <- etat$flags %||% data.frame(
    numero = etat$propositions_json$numero,
    lettre = etat$propositions_json$lettre,
    commentaire = "",
    stringsAsFactors = FALSE
  )
  # Compatibilité avec les sessions sauvegardées avant l'édition des notions
  # (2026-11-02) : on les reconstruit depuis le JSON d'origine.
  rv$notions_json <- etat$notions_json %||% notions_depuis_raw(etat$raw_json)
  rv$notions_final <- etat$notions_final %||% rv$notions_json
  rv$imported <- TRUE
  invisible(NULL)
}
