LETTRES <- LETTERS[1:5]

# Valeurs admises par le schéma pour `section_article`, avec leur libellé.
SECTIONS_ARTICLE <- c(
  titre_resume = "Titre / résumé",
  introduction = "Introduction",
  methodes = "Méthodes",
  resultats = "Résultats",
  discussion = "Discussion",
  declarations = "Déclarations",
  transversal = "Transversal"
)

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

# Champs de rv qui constituent une session (tout sauf `imported` et `referentiel`).
CHAMPS_ETAT <- c(
  "raw_json", "sujet_meta", "controle_qualite", "questions_meta", "questions_final",
  "propositions_json", "human_answers", "final", "flags", "notions_json", "notions_final"
)

#' Lignes de `questions_final` : une par question, dans l'ordre d'affichage.
#'
#' `numero` est l'identifiant interne stable d'une question (celui du JSON
#' d'origine, ou max + 1 pour une question ajoutée) : il ne change jamais, même
#' quand des questions sont ajoutées ou supprimées. Le numéro affiché et exporté
#' est recalculé par [table_questions()]. `notion`, `difficulte` et
#' `section_article` ne servent qu'aux questions ajoutées (celles d'origine
#' gardent les valeurs du JSON).
#' @noRd
lignes_questions <- function(numero, enonce = "", supprimee = FALSE, ajoutee = FALSE,
                             notion = NA_character_, difficulte = NA_integer_,
                             section_article = NA_character_) {
  data.frame(
    numero = as.integer(numero), enonce = as.character(enonce),
    supprimee = as.logical(supprimee), ajoutee = as.logical(ajoutee),
    notion = as.character(notion), difficulte = as.integer(difficulte),
    section_article = as.character(section_article),
    stringsAsFactors = FALSE
  )
}

#' Lignes de `final` (une par proposition) d'une question ajoutée : tout est à
#' rédiger, et le statut est d'emblée « manuel » (ni version initiale ni
#' réponse en aveugle à laquelle se référer).
#' @noRd
lignes_final_vides <- function(numero) {
  data.frame(
    numero = as.integer(numero), lettre = LETTRES, correcte_final = FALSE,
    texte_final = "", justification_final = "", statut_source = "manuel",
    stringsAsFactors = FALSE
  )
}

#' État de session initial d'un sujet fraîchement parsé (aucune réponse,
#' aucun arbitrage) : liste dont les noms sont ceux de `CHAMPS_ETAT`.
#' @noRd
etat_initial <- function(parsed) {
  props <- parsed$propositions_json
  list(
    raw_json = parsed$raw,
    sujet_meta = parsed$sujet_meta,
    controle_qualite = parsed$controle_qualite,
    questions_meta = parsed$questions_meta,
    questions_final = lignes_questions(parsed$questions_meta$numero, parsed$questions_meta$enonce),
    propositions_json = props,
    human_answers = data.frame(
      numero = props$numero, lettre = props$lettre, coche_humain = FALSE,
      stringsAsFactors = FALSE
    ),
    final = data.frame(
      numero = props$numero, lettre = props$lettre, correcte_final = props$correcte,
      texte_final = props$texte, justification_final = props$justification,
      statut_source = "ia", stringsAsFactors = FALSE
    ),
    flags = data.frame(
      numero = props$numero, lettre = props$lettre, commentaire = "",
      stringsAsFactors = FALSE
    ),
    # Notions : référence initiale immuable (JSON) vs état arbitré (Partie 2).
    notions_json = parsed$notions_json,
    notions_final = parsed$notions_json
  )
}

#' Peuple rv avec un état de session (sujet neuf ou session reprise)
#' @noRd
charger_etat_dans_rv <- function(rv, etat) {
  for (champ in CHAMPS_ETAT) rv[[champ]] <- etat[[champ]]
  rv$imported <- TRUE
  invisible(NULL)
}

#' Table des questions dans l'ordre d'affichage, avec leur numéro final
#'
#' Les questions conservées sont renumérotées de 1 à k (`numero_final`, celui
#' des exports) ; une question supprimée n'a pas de numéro final.
#' @noRd
table_questions <- function(questions_final) {
  conservee <- !questions_final$supprimee
  numero_final <- rep(NA_integer_, nrow(questions_final))
  numero_final[conservee] <- seq_len(sum(conservee))
  data.frame(
    numero = questions_final$numero,
    supprimee = questions_final$supprimee,
    ajoutee = questions_final$ajoutee,
    numero_final = numero_final,
    etiquette = ifelse(conservee, paste0("Q", numero_final), "✕"),
    stringsAsFactors = FALSE
  )
}

#' Numéros internes des questions conservées (non supprimées)
#' @noRd
numeros_conserves <- function(questions_final) {
  questions_final$numero[!questions_final$supprimee]
}

#' Ajoute une question vierge, après la question `apres` (ou en fin de sujet)
#'
#' Fonction pure : renvoie les deux tables modifiées et le numéro interne créé.
#' @noRd
ajouter_question <- function(questions_final, final, apres = NULL, notion = "",
                             difficulte = 5L, section_article = "transversal") {
  numero <- max(c(0L, questions_final$numero)) + 1L
  difficulte <- suppressWarnings(as.integer(difficulte))
  if (length(difficulte) != 1 || is.na(difficulte)) difficulte <- 5L
  if (!isTRUE(section_article %in% names(SECTIONS_ARTICLE))) section_article <- "transversal"
  nouvelle <- lignes_questions(
    numero, ajoutee = TRUE, notion = trimws(notion %||% ""),
    difficulte = min(max(difficulte, 1L), 10L), section_article = section_article
  )
  position <- if (is.null(apres)) NA_integer_ else match(apres, questions_final$numero)
  if (is.na(position)) position <- nrow(questions_final)
  questions_final <- rbind(
    questions_final[seq_len(position), , drop = FALSE],
    nouvelle,
    questions_final[seq_len(nrow(questions_final) - position) + position, , drop = FALSE]
  )
  rownames(questions_final) <- NULL
  final <- rbind(final, lignes_final_vides(numero))
  rownames(final) <- NULL
  list(questions_final = questions_final, final = final, numero = numero)
}

#' Supprime (`TRUE`) ou restaure (`FALSE`) une question
#'
#' La suppression est réversible : la question reste dans la session, mais elle
#' est exclue des exports, des compteurs et du score d'accord.
#' @noRd
marquer_suppression <- function(questions_final, numero, supprimee = TRUE) {
  questions_final$supprimee[questions_final$numero == numero] <- isTRUE(supprimee)
  questions_final
}

#' Renumérote le préfixe « 3) » des énoncés, dans leur ordre final
#'
#' Les énoncés produits par lca-create commencent par leur numéro. Quand c'est
#' la convention du sujet (au moins un énoncé sur deux), le préfixe est réécrit
#' d'après la position finale — et ajouté aux énoncés qui n'en ont pas (question
#' ajoutée). Sinon les énoncés sont renvoyés tels quels.
#' @noRd
renumeroter_enonces <- function(enonces) {
  motif <- "^\\s*[0-9]+\\s*([).])\\s+"
  numerote <- grepl(motif, enonces)
  if (!length(enonces) || mean(numerote) < 0.5) return(enonces)
  premier <- enonces[numerote][1]
  separateur <- gsub("[^).]", "", regmatches(premier, regexpr(motif, premier)))
  paste0(seq_along(enonces), separateur, " ", trimws(sub(motif, "", enonces)))
}

#' Points à vérifier avant diffusion : écarts au format attendu que l'ajout ou
#' la suppression de questions (ou l'arbitrage) peut introduire.
#' @noRd
controles_sujet <- function(rv) {
  tq <- table_questions(rv$questions_final)
  tq <- tq[!tq$supprimee, , drop = FALSE]
  alertes <- character(0)
  if (nrow(tq) < 13 || nrow(tq) > 15) {
    alertes <- c(alertes, sprintf(
      "Le sujet compte %d question(s) : le format des EDN (schéma lca-create) en attend 13 à 15.",
      nrow(tq)
    ))
  }
  for (k in seq_len(nrow(tq))) {
    n <- tq$numero[k]
    props <- rv$final[rv$final$numero == n, , drop = FALSE]
    props <- props[order(props$lettre), , drop = FALSE]
    enonce <- rv$questions_final$enonce[rv$questions_final$numero == n]
    problemes <- c(
      if (!nzchar(trimws(enonce))) "énoncé vide",
      if (any(!nzchar(trimws(props$texte_final)))) {
        sprintf("proposition(s) %s sans texte",
                paste(props$lettre[!nzchar(trimws(props$texte_final))], collapse = ", "))
      },
      if (!any(props$correcte_final)) "aucune proposition correcte"
    )
    if (length(problemes)) {
      alertes <- c(alertes, sprintf("%s : %s.", tq$etiquette[k], paste(problemes, collapse = " ; ")))
    }
  }
  alertes
}
