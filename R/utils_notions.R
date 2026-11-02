#' Charge le référentiel de notions (Boussole LCA) figé dans schema/
#'
#' Sert à retrouver le libellé d'une notion à partir de son `id` (stable d'une
#' version à l'autre) et à proposer la liste des notions ajoutables en Partie 2.
#' Le `numero` affiché pour une notion déjà présente reste celui du sujet, car
#' c'est lui qui figure dans les justifications.
load_referentiel_notions <- function(path) {
  if (!file.exists(path)) return(NULL)
  ref <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(ref)) return(NULL)

  domaines <- purrr::map_dfr(ref$domaines, function(d) {
    tibble::tibble(domaine = d$numero, domaine_code = d$code, domaine_libelle = d$libelle)
  })
  notions <- purrr::map_dfr(ref$notions, function(n) {
    tibble::tibble(
      id = n$id,
      numero = n$numero,
      libelle = n$libelle,
      description = n$description %||% NA_character_,
      domaine = n$domaine
    )
  })
  notions <- dplyr::left_join(notions, domaines, by = "domaine")
  list(
    projet = ref$projet, version = ref$version, commit = ref$commit,
    date_version = ref$date_version, notions = notions
  )
}

#' Choix pour le selectize d'ajout de notion : un groupe par domaine du
#' référentiel, valeur = `id` de la notion, libellé = « 049 · Facteur de confusion ».
choix_notions_selectize <- function(referentiel) {
  if (is.null(referentiel)) return(list(""))
  n <- referentiel$notions
  n <- n[order(n$domaine, n$numero), ]
  groupes <- lapply(split(n, factor(n$domaine, levels = unique(n$domaine))), function(d) {
    stats::setNames(d$id, paste(d$numero, d$libelle, sep = " · "))
  })
  noms <- vapply(split(n, factor(n$domaine, levels = unique(n$domaine))), function(d) {
    paste(d$domaine_code[1], d$domaine_libelle[1])
  }, character(1))
  names(groupes) <- noms
  c(list(""), groupes)
}

notions_vide <- function() {
  data.frame(question = integer(), lettre = character(), id = character(),
             numero = character(), role = character(), stringsAsFactors = FALSE)
}

#' Rattachements de notions du JSON d'origine, au format long :
#' une ligne par (question, proposition, notion). Un sujet sans notions
#' (format antérieur) donne une table vide.
notions_depuis_raw <- function(raw) {
  lignes <- purrr::map_dfr(raw$questions, function(q) {
    purrr::map_dfr(q$propositions, function(p) {
      purrr::map_dfr(p$notions %||% list(), function(n) {
        tibble::tibble(question = as.integer(q$numero), lettre = p$lettre,
                       id = n$id, numero = n$numero, role = n$role)
      })
    })
  })
  if (nrow(lignes) == 0) return(notions_vide())
  as.data.frame(lignes, stringsAsFactors = FALSE)
}

#' Applique une modification de notion à la table `notions` (état arbitré).
#'
#' action : "ajouter" (rôle « évaluée » si la proposition n'en a pas encore,
#' sinon « associée »), "retirer", ou "basculer" (évaluée <-> associée).
#' Fonction pure : renvoie la nouvelle table, inchangée si l'action est sans objet.
modifier_notions <- function(notions, question, lettre, action, id, referentiel) {
  est_ici <- notions$question == question & notions$lettre == lettre
  ligne <- which(est_ici & notions$id == id)

  if (identical(action, "retirer")) {
    if (length(ligne)) notions <- notions[-ligne, , drop = FALSE]
  } else if (identical(action, "basculer")) {
    if (length(ligne)) {
      notions$role[ligne] <- if (identical(notions$role[ligne[1]], "evaluee")) "associee" else "evaluee"
    }
  } else if (identical(action, "ajouter")) {
    ref <- if (!is.null(referentiel)) referentiel$notions[referentiel$notions$id == id, ] else NULL
    if (!length(ligne) && !is.null(ref) && nrow(ref) == 1) {
      role <- if (any(est_ici & notions$role == "evaluee")) "associee" else "evaluee"
      notions <- rbind(notions, data.frame(question = as.integer(question), lettre = lettre, id = id,
                                           numero = ref$numero, role = role, stringsAsFactors = FALSE))
    }
  }
  rownames(notions) <- NULL
  notions
}

#' Numéros à trois chiffres cités entre parenthèses dans une justification
#' (convention du schéma : « … (036) » ou « … (036, 037) »).
numeros_cites_justification <- function(justification) {
  blocs <- regmatches(justification, gregexpr("\\([0-9]{3}(?:\\s*,\\s*[0-9]{3})*\\)", justification, perl = TRUE))[[1]]
  sort(unique(unlist(regmatches(blocs, gregexpr("[0-9]{3}", blocs)))))
}

#' Libellé d'une notion à partir de son id (repli : l'id lui-même)
libelle_notion <- function(id, referentiel) {
  if (is.null(referentiel)) return(id)
  lib <- referentiel$notions$libelle[match(id, referentiel$notions$id)]
  ifelse(is.na(lib), id, lib)
}

#' Texte lisible des notions d'une proposition, pour le diff de la Partie 3
format_notions <- function(lignes, referentiel) {
  if (nrow(lignes) == 0) return("aucune")
  paste0(lignes$numero, " ", libelle_notion(lignes$id, referentiel),
         " (", ifelse(lignes$role == "evaluee", "évaluée", "associée"), ")", collapse = " ; ")
}

#' Répartition des notions théoriques mobilisées par les propositions d'un sujet
#'
#' @param lignes table longue des notions (état arbitré `rv$notions_final`)
#' @param raw JSON d'origine, pour les effectifs de propositions et les notions ciblées
#' @return list(table, n_propositions, n_propositions_avec_notion, version_sujet,
#'   version_referentiel, referentiel_different)
compute_notions_repartition <- function(lignes, raw, referentiel = NULL) {
  n_props <- sum(vapply(raw$questions, function(q) length(q$propositions), integer(1)))
  n_props_notion <- nrow(unique(lignes[, c("question", "lettre")]))
  ciblees <- vapply(raw$sujet$parametres$notions_ciblees %||% list(), function(n) n$id, character(1))

  tableau <- if (nrow(lignes) == 0) {
    tibble::tibble(id = character(), numero = character(), libelle = character(),
                   description = character(), n_evaluee = integer(), n_associee = integer(),
                   n_total = integer(), ciblee = logical())
  } else {
    agg <- dplyr::summarise(
      dplyr::group_by(lignes, id),
      numero = dplyr::first(numero),
      n_evaluee = sum(role == "evaluee"),
      n_associee = sum(role != "evaluee"),
      .groups = "drop"
    )
    agg$n_total <- agg$n_evaluee + agg$n_associee
    lib <- if (!is.null(referentiel)) referentiel$notions[, c("id", "libelle", "description")] else tibble::tibble(id = character(), libelle = character(), description = character())
    agg <- dplyr::left_join(agg, lib, by = "id")
    agg$libelle <- ifelse(is.na(agg$libelle), agg$id, agg$libelle)
    agg$ciblee <- agg$id %in% ciblees
    agg[order(-agg$n_total, -agg$n_evaluee, agg$numero), ]
  }

  ref_sujet <- raw$sujet$referentiel_notions
  list(
    table = tableau,
    n_propositions = n_props,
    n_propositions_avec_notion = n_props_notion,
    version_sujet = ref_sujet$version %||% NA_character_,
    version_referentiel = referentiel$version %||% NA_character_,
    referentiel_different = referentiel_different(raw, referentiel)
  )
}

#' TRUE si le sujet déclare un référentiel dont le commit diffère de la copie locale
referentiel_different <- function(raw, referentiel) {
  ref_sujet <- raw$sujet$referentiel_notions
  !is.null(referentiel) && !is.null(ref_sujet$commit) && !identical(ref_sujet$commit, referentiel$commit)
}

#' Histogramme horizontal (HTML/CSS) : une ligne par notion, barre empilée
#' notion évaluée / notion associée, triée par fréquence décroissante.
render_notions_repartition <- function(rep) {
  tab <- rep$table
  if (nrow(tab) == 0) {
    return(htmltools::tags$p(
      class = "text-muted mb-0",
      "Aucune notion du référentiel rattachée aux propositions de ce sujet."
    ))
  }

  max_total <- max(tab$n_total)
  lignes <- lapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    htmltools::tags$div(
      class = "notion-row",
      htmltools::tags$div(
        class = "notion-label", title = if (!is.na(r$description)) r$description else NULL,
        htmltools::tags$span(class = "notion-num", r$numero), r$libelle,
        if (isTRUE(r$ciblee)) htmltools::tags$span(class = "notion-cible", title = "Notion ciblée par l'enseignant", "★")
      ),
      htmltools::tags$div(
        class = "notion-track",
        title = sprintf("%d évaluée(s), %d associée(s)", r$n_evaluee, r$n_associee),
        htmltools::tags$div(class = "notion-seg notion-seg-evaluee",
                            style = sprintf("width:%.1f%%;", 100 * r$n_evaluee / max_total)),
        htmltools::tags$div(class = "notion-seg notion-seg-associee",
                            style = sprintf("width:%.1f%%;", 100 * r$n_associee / max_total))
      ),
      htmltools::tags$div(class = "notion-count", r$n_total)
    )
  })

  htmltools::tagList(
    htmltools::tags$div(
      class = "notion-legend small text-muted mb-2",
      htmltools::tags$span(class = "notion-swatch notion-seg-evaluee"), "notion évaluée",
      htmltools::tags$span(class = "notion-swatch notion-seg-associee ms-3"), "notion associée",
      if (any(tab$ciblee)) htmltools::tagList(htmltools::tags$span(class = "notion-cible ms-3", "★"), "notion ciblée")
    ),
    htmltools::tags$div(class = "notion-chart", lignes)
  )
}

#' Pastilles des notions d'une proposition (Partie 2) : clic sur le libellé =
#' basculer évaluée/associée, clic sur × = retirer. Les clics remontent via
#' `input_id` (valeur « lettre|action|id »).
render_notions_pastilles <- function(lignes, lettre, referentiel, input_id) {
  if (nrow(lignes) == 0) {
    return(htmltools::tags$span(class = "small text-muted", "Aucune notion rattachée."))
  }
  envoi <- function(action, id) {
    sprintf("Shiny.setInputValue('%s', '%s|%s|%s', {priority: 'event'}); return false;", input_id, lettre, action, id)
  }
  htmltools::tags$div(
    class = "d-flex flex-wrap gap-1",
    lapply(seq_len(nrow(lignes)), function(i) {
      r <- lignes[i, ]
      evaluee <- identical(r$role, "evaluee")
      htmltools::tags$span(
        class = paste("notion-chip", if (evaluee) "notion-chip-evaluee" else "notion-chip-associee"),
        htmltools::tags$a(
          href = "#", class = "notion-chip-label",
          title = sprintf("Notion %s : cliquer pour la passer en %s",
                          if (evaluee) "évaluée" else "associée", if (evaluee) "associée" else "évaluée"),
          onclick = envoi("basculer", r$id),
          paste(r$numero, libelle_notion(r$id, referentiel))
        ),
        htmltools::tags$a(href = "#", class = "notion-chip-x", title = "Retirer cette notion",
                          onclick = envoi("retirer", r$id), "×")
      )
    })
  )
}
