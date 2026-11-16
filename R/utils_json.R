`%||%` <- function(x, y) if (is.null(x)) y else x

# Le validateur (contexte V8 + schéma compilé) est coûteux à créer : un seul
# par schéma et par processus R.
.cache <- new.env(parent = emptyenv())

validateur_sujet <- function(schema_path) {
  if (is.null(.cache[[schema_path]])) {
    .cache[[schema_path]] <- jsonvalidate::json_validator(schema_path, engine = "ajv")
  }
  .cache[[schema_path]]
}

#' Valide un fichier JSON contre le schéma officiel du sujet LCA
#'
#' Rejet explicite : retourne la liste des non-conformités (chemin + message)
#' plutôt qu'un simple TRUE/FALSE, pour un message d'erreur exploitable côté UI.
#' @noRd
validate_sujet_json <- function(path, schema_path) {
  ok <- tryCatch(validateur_sujet(schema_path)(path, verbose = TRUE), error = function(e) e)

  if (inherits(ok, "error")) {
    return(list(valide = FALSE, erreurs = paste("JSON illisible :", conditionMessage(ok))))
  }
  if (isTRUE(ok)) {
    return(list(valide = TRUE, erreurs = character(0)))
  }

  err <- attr(ok, "errors")
  erreurs <- if (!is.null(err) && nrow(err) > 0) {
    sprintf("%s : %s", ifelse(nzchar(err$instancePath), err$instancePath, "(racine)"), err$message)
  } else {
    "Le fichier ne respecte pas le schéma attendu."
  }
  list(valide = FALSE, erreurs = erreurs)
}

#' Parse un sujet LCA (JSON produit par le skill lca-create) en structures tabulaires
#' @noRd
parse_sujet_json <- function(path) {
  parse_sujet_raw(jsonlite::read_json(path, simplifyVector = FALSE))
}

#' Idem, à partir du JSON déjà désérialisé (listes imbriquées, sans simplification)
#' @noRd
parse_sujet_raw <- function(raw) {
  questions_meta <- purrr::map_dfr(raw$questions, function(q) {
    tibble::tibble(
      numero = q$numero,
      type = q$type,
      enonce = q$enonce,
      notion = q$notion %||% NA_character_,
      objectif_pedagogique = q$objectif_pedagogique %||% NA_character_,
      difficulte = q$difficulte,
      section_article = q$section_article,
      nb_propositions_correctes = q$nb_propositions_correctes,
      commentaire_pedagogique = q$commentaire_pedagogique %||% NA_character_
    )
  })

  propositions_json <- purrr::map_dfr(raw$questions, function(q) {
    purrr::map_dfr(q$propositions, function(p) {
      tibble::tibble(
        numero = q$numero,
        lettre = p$lettre,
        texte = p$texte,
        correcte = isTRUE(p$correcte),
        justification = p$justification %||% "",
        erreur_visee = p$erreur_visee %||% NA_character_
      )
    })
  })

  list(
    raw = raw,
    sujet_meta = raw$sujet,
    controle_qualite = raw$controle_qualite,
    questions_meta = questions_meta,
    propositions_json = propositions_json,
    notions_json = notions_depuis_raw(raw)
  )
}

#' Reconstruit un JSON consolidé à partir de l'état final arbitré
#'
#' Seules les questions conservées sont exportées, dans leur ordre final et
#' renumérotées de 1 à k (champ `numero` et préfixe de l'énoncé) ; une question
#' ajoutée reçoit les métadonnées saisies à sa création.
#'
#' vision = "complet"  : JSON au format d'origine, réponses et justifications
#'                        finales injectées.
#' vision = "etudiant"  : structure réduite (pas de corrigé), non conforme au
#'                        schéma source, destinée à la distribution aux étudiants.
#' @noRd
build_consolidated_json <- function(rv, vision = c("complet", "etudiant")) {
  vision <- match.arg(vision)

  raw <- rv$raw_json
  qf <- rv$questions_final[!rv$questions_final$supprimee, , drop = FALSE]
  enonces <- renumeroter_enonces(qf$enonce)
  originales <- raw$questions
  names(originales) <- vapply(originales, function(q) as.character(q$numero), character(1))

  questions <- lapply(seq_len(nrow(qf)), function(k) {
    n <- qf$numero[k]
    q <- originales[[as.character(n)]]
    if (is.null(q)) {
      # Question ajoutée en Comparaison : pas d'équivalent dans le JSON d'origine.
      q <- list(
        numero = k, type = NULL, enonce = "",
        notion = if (is.na(qf$notion[k])) "" else qf$notion[k],
        difficulte = if (is.na(qf$difficulte[k])) 5L else qf$difficulte[k],
        section_article = if (is.na(qf$section_article[k])) "transversal" else qf$section_article[k],
        propositions = lapply(LETTRES, function(L) list(lettre = L))
      )
    }
    q$numero <- k
    q$enonce <- enonces[k]
    q$propositions <- lapply(q$propositions, function(p) {
      mask <- rv$final$numero == n & rv$final$lettre == p$lettre
      p$texte <- rv$final$texte_final[mask][1]
      p$correcte <- isTRUE(rv$final$correcte_final[mask][1])
      p$justification <- rv$final$justification_final[mask][1]
      if (isTRUE(p$correcte)) p$erreur_visee <- NULL
      # Notions arbitrées en Partie 2. Une proposition qui n'en a jamais eu
      # n'en reçoit pas de champ vide ; une proposition vidée en garde un (tableau vide).
      lignes <- rv$notions_final[rv$notions_final$question == n & rv$notions_final$lettre == p$lettre, ]
      if (nrow(lignes) > 0) {
        p$notions <- lapply(seq_len(nrow(lignes)), function(i) {
          list(id = lignes$id[i], numero = lignes$numero[i], role = lignes$role[i])
        })
      } else if (!is.null(p$notions)) {
        p$notions <- list()
      }
      p
    })
    # L'arbitrage peut avoir changé le nombre de propositions exactes : le
    # compteur suit, et une QRU qui n'en a plus exactement une devient QRM.
    nb_correctes <- sum(vapply(q$propositions, function(p) isTRUE(p$correcte), logical(1)))
    q$nb_propositions_correctes <- nb_correctes
    if (is.null(q$type) || (identical(q$type, "QRU") && nb_correctes != 1L)) {
      q$type <- if (nb_correctes == 1L) "QRU" else "QRM"
    }
    q
  })

  if (identical(vision, "etudiant")) {
    return(list(
      sujet = list(
        identifiant = rv$sujet_meta$identifiant %||% NA_character_,
        titre = rv$sujet_meta$source$titre %||% NA_character_
      ),
      questions = lapply(questions, function(q) {
        list(
          numero = q$numero, type = q$type, enonce = q$enonce,
          propositions = lapply(q$propositions, function(p) list(lettre = p$lettre, texte = p$texte))
        )
      })
    ))
  }

  raw$questions <- questions
  # Champs du sujet dérivés des questions : tenus à jour quand ils existent.
  if (!is.null(raw$sujet$parametres$nombre_questions)) {
    raw$sujet$parametres$nombre_questions <- length(questions)
  }
  if (!is.null(raw$controle_qualite$distribution_propositions_correctes)) {
    nb <- vapply(questions, function(q) as.integer(q$nb_propositions_correctes), integer(1))
    raw$controle_qualite$distribution_propositions_correctes <-
      as.list(stats::setNames(tabulate(nb, nbins = 5), as.character(1:5)))
  }
  if (!is.null(raw$controle_qualite$difficulte_moyenne) && length(questions)) {
    difficultes <- vapply(questions, function(q) as.numeric(q$difficulte %||% NA), numeric(1))
    raw$controle_qualite$difficulte_moyenne <- round(mean(difficultes, na.rm = TRUE), 1)
  }
  # Un sujet à l'ancien format auquel des notions ont été ajoutées doit déclarer
  # la version du référentiel utilisée pour rester conforme au schéma.
  if (nrow(rv$notions_final) > 0 && is.null(raw$sujet$referentiel_notions) && !is.null(rv$referentiel)) {
    raw$sujet$referentiel_notions <- Filter(Negate(is.null), list(
      projet = rv$referentiel$projet, version = rv$referentiel$version,
      commit = rv$referentiel$commit, date_version = rv$referentiel$date_version
    ))
  }
  raw$validation <- list(
    date_validation = as.character(Sys.Date()),
    outil = "lca-app-validate"
  )
  raw
}
