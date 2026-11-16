# Session de travail = un seul fichier JSON, conservé par l'utilisateur.
#
# Rien n'est écrit sur le serveur : l'application peut être déployée
# publiquement, sans contrôle d'accès, sans que les sessions des uns soient
# visibles des autres. Le même contenu est aussi rangé dans le stockage local
# du navigateur (cf. inst/app/www/session.js) pour reprendre après une
# fermeture d'onglet.
#
# Le fichier ne contient que le sujet d'origine et ce que l'utilisateur a
# produit ; les tables dérivées du sujet sont recalculées à la lecture. Le
# format est du JSON et non du .rds : désérialiser un .rds déposé par un
# inconnu sur un serveur public n'est pas sûr.

SESSION_FORMAT <- "lca-validate-session"
SESSION_VERSION <- 1L
# Garde-fou à la lecture d'un fichier de session (le schéma en admet 15).
SESSION_MAX_QUESTIONS <- 60L

sanitize_filename <- function(x) {
  x <- x %||% ""
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  if (!nzchar(x)) x <- "sujet"
  x
}

#' Fuseau horaire annoncé par le navigateur, s'il est connu de R (sinon celui
#' du serveur) : un serveur public tourne en général en UTC.
#' @noRd
fuseau_valide <- function(tz) {
  if (is.character(tz) && length(tz) == 1 && !is.na(tz) && tz %in% OlsonNames()) tz else ""
}

nom_fichier_session <- function(rv, tz = "") {
  id <- sanitize_filename(rv$sujet_meta$identifiant %||% rv$sujet_meta$source$titre %||% "sujet")
  paste0("session-lca_", id, "_", format(Sys.time(), "%Y%m%d-%H%M", tz = fuseau_valide(tz)), ".json")
}

#' Table -> liste d'enregistrements (une liste nommée par ligne), pour un JSON
#' explicite et indépendant des conventions de jsonlite sur les data frames.
#' @noRd
df_vers_enregistrements <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) lapply(df, function(colonne) colonne[[i]]))
}

#' Contenu d'un fichier de session à partir de l'état courant
#'
#' `rv` peut être l'objet reactiveValues de l'application ou une simple liste
#' portant les mêmes champs.
#' @noRd
session_vers_liste <- function(rv) {
  list(
    format = SESSION_FORMAT,
    version = SESSION_VERSION,
    sauvegarde_le = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    sujet = rv$raw_json,
    etat = list(
      questions = df_vers_enregistrements(rv$questions_final),
      propositions = df_vers_enregistrements(rv$final),
      reponses = df_vers_enregistrements(rv$human_answers),
      flags = df_vers_enregistrements(rv$flags),
      notions = if (!is.null(rv$notions_final)) df_vers_enregistrements(rv$notions_final)
    )
  )
}

session_vers_json <- function(liste, pretty = FALSE) {
  as.character(jsonlite::toJSON(liste, auto_unbox = TRUE, null = "null", na = "null",
                                digits = NA, pretty = pretty))
}

#' Résumé affiché à l'accueil pour la session rangée dans le navigateur
#' @noRd
resume_session <- function(rv) {
  conserves <- numeros_conserves(rv$questions_final)
  reponses <- rv$human_answers[rv$human_answers$numero %in% conserves, , drop = FALSE]
  list(
    titre = rv$sujet_meta$source$titre %||% rv$sujet_meta$identifiant %||% "Sujet LCA",
    repondu = length(unique(reponses$numero[reponses$coche_humain])),
    total = length(conserves)
  )
}

est_session <- function(brut) {
  is.list(brut) && identical(brut$format, SESSION_FORMAT)
}

# ---- Lecture : aucune valeur du fichier n'est reprise sans contrôle de type ----

chr1 <- function(x, defaut = "") {
  if (is.atomic(x) && length(x) == 1 && !is.na(x)) as.character(x) else defaut
}

lgl1 <- function(x, defaut = FALSE) {
  if (is.logical(x) && length(x) == 1 && !is.na(x)) x else defaut
}

int1 <- function(x, defaut = NA_integer_) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x) && abs(x) < .Machine$integer.max) as.integer(x) else defaut
}

enregistrements <- function(x) {
  if (!is.list(x)) return(list())
  Filter(is.list, unname(x))
}

#' Reconstruit l'état de session à partir du contenu d'un fichier de session
#'
#' Le sujet est reparsé comme à l'import, ce qui redonne l'état initial ; les
#' valeurs du fichier y sont ensuite reportées une à une, après contrôle de
#' type et uniquement pour des questions et propositions connues. Un fichier
#' incomplet ou retouché à la main donne donc toujours un état cohérent.
#' Lève une erreur (message destiné à l'utilisateur) si le fichier n'est pas
#' exploitable.
#' @noRd
etat_depuis_session <- function(brut) {
  if (!est_session(brut)) {
    stop("Ce fichier n'est pas une session de l'application Validation LCA.", call. = FALSE)
  }
  version <- int1(brut$version)
  if (is.na(version) || version > SESSION_VERSION) {
    stop("Cette session a été enregistrée avec une version plus récente de l'application.", call. = FALSE)
  }
  parsed <- tryCatch(
    {
      stopifnot(is.list(brut$sujet), is.list(brut$sujet$questions), length(brut$sujet$questions) > 0)
      parse_sujet_raw(brut$sujet)
    },
    error = function(e) NULL
  )
  if (is.null(parsed) || nrow(parsed$propositions_json) == 0) {
    stop("Le sujet contenu dans ce fichier de session est illisible.", call. = FALSE)
  }

  etat <- etat_initial(parsed)
  s <- if (is.list(brut$etat)) brut$etat else list()

  # Questions : énoncés, suppressions, questions ajoutées, ordre d'affichage.
  qf <- etat$questions_final
  originales <- qf$numero
  ordre <- integer(0)
  for (r in enregistrements(s$questions)) {
    n <- int1(r$numero)
    if (is.na(n) || n < 1 || n %in% ordre) next
    if (n %in% originales) {
      i <- match(n, qf$numero)
      qf$enonce[i] <- chr1(r$enonce, qf$enonce[i])
      qf$supprimee[i] <- lgl1(r$supprimee)
    } else if (lgl1(r$ajoutee) && nrow(qf) < SESSION_MAX_QUESTIONS) {
      section <- chr1(r$section_article, "transversal")
      qf <- rbind(qf, lignes_questions(
        n, enonce = chr1(r$enonce), supprimee = lgl1(r$supprimee), ajoutee = TRUE,
        notion = chr1(r$notion),
        difficulte = min(max(int1(r$difficulte, 5L), 1L), 10L),
        section_article = if (section %in% names(SECTIONS_ARTICLE)) section else "transversal"
      ))
    } else {
      next
    }
    ordre <- c(ordre, n)
  }
  qf <- qf[order(match(qf$numero, c(ordre, setdiff(qf$numero, ordre)))), , drop = FALSE]
  rownames(qf) <- NULL
  etat$questions_final <- qf
  ajoutees <- qf$numero[qf$ajoutee]

  # Propositions arbitrées.
  final <- etat$final
  for (n in ajoutees) final <- rbind(final, lignes_final_vides(n))
  for (r in enregistrements(s$propositions)) {
    i <- which(final$numero == int1(r$numero, -1L) & final$lettre == chr1(r$lettre))
    if (length(i) != 1) next
    final$texte_final[i] <- chr1(r$texte_final, final$texte_final[i])
    final$justification_final[i] <- chr1(r$justification_final, final$justification_final[i])
    final$correcte_final[i] <- lgl1(r$correcte_final, final$correcte_final[i])
    statut <- chr1(r$statut_source)
    if (statut %in% c("ia", "humain", "manuel")) final$statut_source[i] <- statut
  }
  final$statut_source[final$numero %in% ajoutees] <- "manuel"
  rownames(final) <- NULL
  etat$final <- final

  # Réponses du jeu en aveugle et signalements (questions d'origine seulement).
  for (r in enregistrements(s$reponses)) {
    i <- which(etat$human_answers$numero == int1(r$numero, -1L) & etat$human_answers$lettre == chr1(r$lettre))
    if (length(i) == 1) etat$human_answers$coche_humain[i] <- lgl1(r$coche_humain)
  }
  for (r in enregistrements(s$flags)) {
    i <- which(etat$flags$numero == int1(r$numero, -1L) & etat$flags$lettre == chr1(r$lettre))
    if (length(i) == 1) etat$flags$commentaire[i] <- substr(chr1(r$commentaire), 1, 500)
  }

  # Notions arbitrées : absentes du fichier (session antérieure à leur
  # édition) = celles du sujet ; présentes, elles les remplacent.
  if (is.list(s$notions)) {
    notions <- notions_vide()
    for (r in enregistrements(s$notions)) {
      ligne <- data.frame(
        question = int1(r$question, -1L), lettre = chr1(r$lettre), id = chr1(r$id),
        numero = chr1(r$numero), role = chr1(r$role), stringsAsFactors = FALSE
      )
      valide <- ligne$question %in% qf$numero && ligne$lettre %in% LETTRES && nzchar(ligne$id) &&
        grepl("^[0-9]{3}$", ligne$numero) && ligne$role %in% c("evaluee", "associee") &&
        !any(notions$question == ligne$question & notions$lettre == ligne$lettre & notions$id == ligne$id)
      if (valide) notions <- rbind(notions, ligne)
    }
    etat$notions_final <- notions
  }

  etat
}

#' Lit un fichier déposé à l'accueil : sujet neuf (JSON lca-create, validé
#' contre le schéma) ou session exportée — le type est reconnu au contenu,
#' quelle que soit la zone de dépôt utilisée.
#' @return list(ok, type = "sujet" | "session", etat) ou list(ok = FALSE, erreurs)
#' @noRd
lire_fichier_import <- function(path, schema_path) {
  brut <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(brut)) {
    return(list(ok = FALSE, erreurs = "Ce fichier n'est pas un JSON lisible."))
  }
  if (est_session(brut)) return(lire_session(brut))

  validation <- validate_sujet_json(path, schema_path)
  if (!validation$valide) return(list(ok = FALSE, erreurs = validation$erreurs))
  parsed <- tryCatch(parse_sujet_raw(brut), error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, erreurs = paste("Échec de l'import :", conditionMessage(parsed))))
  }
  list(ok = TRUE, type = "sujet", etat = etat_initial(parsed))
}

#' Lit une session depuis son contenu déjà désérialisé, ou depuis son texte
#' JSON (stockage local du navigateur).
#' @noRd
lire_session <- function(brut) {
  if (is.character(brut)) {
    # parse_json() et non fromJSON() : ce dernier irait lire un fichier ou une
    # URL si la chaîne y ressemblait, or elle vient du navigateur.
    brut <- tryCatch(jsonlite::parse_json(brut, simplifyVector = FALSE), error = function(e) NULL)
  }
  etat <- tryCatch(etat_depuis_session(brut), error = function(e) e)
  if (inherits(etat, "error")) return(list(ok = FALSE, erreurs = conditionMessage(etat)))
  list(ok = TRUE, type = "session", etat = etat)
}

#' Convertir une ancienne sauvegarde `.rds` en fichier de session
#'
#' Jusqu'à la version 0.1 de l'application, les sessions étaient enregistrées
#' en `.rds` dans le dossier `saves/` de l'application. Cette fonction en fait
#' un fichier de session `.json`, à déposer à l'accueil de l'application
#' (« Reprendre une session »). Elle est à utiliser sur votre poste, avec vos
#' propres sauvegardes.
#'
#' @param chemin_rds Chemin de l'ancienne sauvegarde `.rds`.
#' @param sortie Chemin du fichier de session à écrire. Par défaut, à côté du
#'   `.rds`, avec l'extension `.json`.
#' @return Le chemin du fichier écrit, de façon invisible.
#' @examples
#' \dontrun{
#' convertir_session_rds("saves/mon-sujet.rds")
#' }
#' @export
convertir_session_rds <- function(chemin_rds, sortie = sub("\\.rds$", ".json", chemin_rds, ignore.case = TRUE)) {
  if (identical(normalizePath(sortie, mustWork = FALSE), normalizePath(chemin_rds, mustWork = FALSE))) {
    stop("`sortie` doit être différent de `chemin_rds`.", call. = FALSE)
  }
  ancien <- readRDS(chemin_rds)
  if (!is.list(ancien) || is.null(ancien$raw_json) || is.null(ancien$final)) {
    stop("Ce fichier n'est pas une sauvegarde de l'application Validation LCA.", call. = FALSE)
  }
  liste <- session_vers_liste(list(
    raw_json = ancien$raw_json,
    questions_final = ancien$questions_final,
    final = ancien$final,
    human_answers = ancien$human_answers,
    flags = ancien$flags,
    notions_final = ancien$notions_final
  ))
  if (inherits(ancien$sauvegarde_le, "POSIXct")) {
    liste$sauvegarde_le <- format(ancien$sauvegarde_le, "%Y-%m-%dT%H:%M:%S%z")
  }
  # Relecture de contrôle : mieux vaut échouer ici qu'à l'import dans l'application.
  etat_depuis_session(jsonlite::parse_json(session_vers_json(liste), simplifyVector = FALSE))
  writeLines(session_vers_json(liste, pretty = TRUE), sortie, useBytes = TRUE)
  invisible(sortie)
}
