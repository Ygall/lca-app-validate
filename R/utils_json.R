`%||%` <- function(x, y) if (is.null(x)) y else x

#' Valide un fichier JSON contre le schéma officiel du sujet LCA
#'
#' Rejet explicite : retourne la liste des non-conformités (chemin + message)
#' plutôt qu'un simple TRUE/FALSE, pour un message d'erreur exploitable côté UI.
validate_sujet_json <- function(path, schema_path) {
  validateur <- jsonvalidate::json_validator(schema_path, engine = "ajv")
  ok <- tryCatch(validateur(path, verbose = TRUE), error = function(e) e)

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
parse_sujet_json <- function(path) {
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)

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
#' vision = "complet"  : JSON conforme au schéma d'origine, réponses et
#'                        justifications finales injectées, réimportable.
#' vision = "etudiant"  : structure réduite (pas de corrigé), non conforme au
#'                        schéma source, destinée à la distribution aux étudiants.
build_consolidated_json <- function(rv, vision = c("complet", "etudiant")) {
  vision <- match.arg(vision)

  if (identical(vision, "etudiant")) {
    return(list(
      sujet = list(
        identifiant = rv$sujet_meta$identifiant %||% NA_character_,
        titre = rv$sujet_meta$source$titre %||% NA_character_
      ),
      questions = lapply(rv$questions_meta$numero, function(n) {
        q <- rv$questions_meta[rv$questions_meta$numero == n, ]
        enonce <- rv$questions_final$enonce[rv$questions_final$numero == n]
        props <- rv$final[rv$final$numero == n, ]
        props <- props[order(props$lettre), ]
        list(
          numero = n,
          type = q$type[1],
          enonce = enonce,
          propositions = lapply(seq_len(nrow(props)), function(i) {
            list(lettre = props$lettre[i], texte = props$texte_final[i])
          })
        )
      })
    ))
  }

  raw <- rv$raw_json
  raw$questions <- lapply(raw$questions, function(q) {
    q$enonce <- rv$questions_final$enonce[rv$questions_final$numero == q$numero]
    q$propositions <- lapply(q$propositions, function(p) {
      mask <- rv$final$numero == q$numero & rv$final$lettre == p$lettre
      p$texte <- rv$final$texte_final[mask][1]
      p$correcte <- isTRUE(rv$final$correcte_final[mask][1])
      p$justification <- rv$final$justification_final[mask][1]
      if (isTRUE(p$correcte)) p$erreur_visee <- NULL
      # Notions arbitrées en Partie 2. Une proposition qui n'en a jamais eu
      # n'en reçoit pas de champ vide ; une proposition vidée en garde un (tableau vide).
      lignes <- rv$notions_final[rv$notions_final$question == q$numero & rv$notions_final$lettre == p$lettre, ]
      if (nrow(lignes) > 0) {
        p$notions <- lapply(seq_len(nrow(lignes)), function(i) {
          list(id = lignes$id[i], numero = lignes$numero[i], role = lignes$role[i])
        })
      } else if (!is.null(p$notions)) {
        p$notions <- list()
      }
      p
    })
    q
  })
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
