# Accueil : démarrer une session (import d'un sujet), en reprendre une (fichier
# de session exporté, ou session rangée dans le stockage local du navigateur),
# exporter ou fermer la session en cours.
#
# Rien n'est conservé côté serveur. La session vit dans la mémoire de la
# session Shiny, et elle est recopiée à chaque modification dans le stockage
# local du navigateur (inst/app/www/session.js) ; le fichier exporté est la
# seule sauvegarde durable, que l'utilisateur garde chez lui.

mod_accueil_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("statut")),
    shiny::uiOutput(ns("session_navigateur")),
    shiny::uiOutput(ns("demarrage")),
    shiny::uiOutput(ns("session_en_cours"))
  )
}

#' Éléments de la barre de navigation : état de la sauvegarde dans le
#' navigateur et export de la session, accessibles depuis tous les onglets.
#' @noRd
mod_accueil_barre_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("barre"), class = "d-flex align-items-center gap-2")
}

#' @param avant_export function() appelée juste avant de sérialiser la session
#'   pour un export : enregistre la saisie en cours de l'onglet affiché.
#' @noRd
mod_accueil_server <- function(id, rv, schema_path, avant_export = function() invisible(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    statut <- shiny::reactiveVal(NULL)
    # Ce que le navigateur annonce à la connexion : stockage local utilisable
    # ou non, résumé de la session qui s'y trouve (ou NULL), fuseau horaire.
    navigateur <- shiny::reactiveVal(list(disponible = NA, meta = NULL, fuseau = ""))
    # Dernier compte rendu d'écriture dans le stockage local.
    derniere_sauvegarde <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$navigateur, {
      navigateur(list(
        disponible = isTRUE(input$navigateur$disponible),
        meta = if (is.list(input$navigateur$meta)) input$navigateur$meta,
        fuseau = fuseau_valide(input$navigateur$fuseau)
      ))
    })
    shiny::observeEvent(input$sauvegarde, derniere_sauvegarde(input$sauvegarde))

    # ---- Démarrer ou reprendre une session ----

    charger <- function(resultat) {
      if (isTRUE(rv$imported)) return(invisible(NULL))
      if (!isTRUE(resultat$ok)) {
        statut(list(ok = FALSE, erreurs = resultat$erreurs))
        return(invisible(NULL))
      }
      charger_etat_dans_rv(rv, resultat$etat)
      statut(list(ok = TRUE, type = resultat$type))
    }

    # Les deux zones de dépôt acceptent indifféremment un sujet ou une
    # session : le type est reconnu au contenu (cf. lire_fichier_import()).
    shiny::observeEvent(input$fichier_sujet, {
      charger(lire_fichier_import(input$fichier_sujet$datapath, schema_path))
    })
    shiny::observeEvent(input$fichier_session, {
      charger(lire_fichier_import(input$fichier_session$datapath, schema_path))
    })

    shiny::observeEvent(input$reprendre_navigateur, {
      session$sendCustomMessage("lca-session-lire", list())
    })
    shiny::observeEvent(input$contenu_navigateur, {
      charger(lire_session(input$contenu_navigateur))
    })

    shiny::observeEvent(input$oublier_navigateur, {
      shiny::showModal(shiny::modalDialog(
        title = "Supprimer la session enregistrée dans ce navigateur ?",
        "Elle sera définitivement effacée de ce navigateur. Un fichier de session déjà exporté n'est pas concerné.",
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("confirmer_oubli"), "Supprimer", class = "btn-danger")
        )
      ))
    })
    shiny::observeEvent(input$confirmer_oubli, {
      session$sendCustomMessage("lca-session-effacer", list(recharger = FALSE))
      etat <- navigateur()
      etat$meta <- NULL
      navigateur(etat)
      shiny::removeModal()
    })

    # ---- Sauvegarde automatique dans le navigateur ----

    # Lit tout l'état : se recalcule à chaque modification de la session. La
    # temporisation regroupe les écritures rapprochées (une navigation modifie
    # plusieurs tables d'affilée).
    session_json <- shiny::reactive({
      shiny::req(rv$imported)
      session_vers_json(session_vers_liste(rv))
    })
    session_json_stable <- shiny::debounce(session_json, 800)
    shiny::observeEvent(session_json_stable(), {
      session$sendCustomMessage("lca-session-enregistrer", list(
        json = session_json_stable(),
        meta = resume_session(rv)
      ))
    })

    # ---- Export / fermeture de la session ----

    telechargement <- function() {
      shiny::downloadHandler(
        filename = function() nom_fichier_session(rv, navigateur()$fuseau),
        content = function(file) {
          shiny::req(rv$imported)
          avant_export()
          writeLines(session_vers_json(session_vers_liste(rv), pretty = TRUE), file, useBytes = TRUE)
        },
        contentType = "application/json"
      )
    }
    output$exporter <- telechargement()
    output$exporter_barre <- telechargement()

    shiny::observeEvent(input$fermer, {
      shiny::showModal(shiny::modalDialog(
        title = "Fermer la session ?",
        shiny::p("La session en cours sera effacée de ce navigateur, et l'application reviendra à l'accueil."),
        shiny::p(shiny::strong("Exportez-la d'abord"), " si vous souhaitez la reprendre plus tard : sans fichier de session, le travail effectué est perdu."),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("confirmer_fermeture"), "Fermer la session", class = "btn-danger")
        )
      ))
    })
    shiny::observeEvent(input$confirmer_fermeture, {
      # Repartir d'une page neuve plutôt que de vider rv : les modules gardent
      # un état propre à la session (question affichée, garde-fous de saisie).
      session$sendCustomMessage("lca-session-effacer", list(recharger = TRUE))
    })

    # ---- Affichage ----

    output$statut <- shiny::renderUI({
      s <- statut()
      if (is.null(s)) return(NULL)
      if (!s$ok) {
        return(shiny::div(
          class = "alert alert-danger",
          shiny::strong("Fichier rejeté : il ne respecte pas le format attendu."),
          shiny::tags$ul(class = "mb-0", lapply(s$erreurs, shiny::tags$li))
        ))
      }
      shiny::div(
        class = "alert alert-success",
        if (identical(s$type, "session")) {
          "Session reprise là où vous l'aviez laissée. Les autres onglets sont disponibles."
        } else {
          "Sujet importé : nouvelle session démarrée. Les autres onglets sont maintenant disponibles."
        }
      )
    })

    output$session_navigateur <- shiny::renderUI({
      meta <- navigateur()$meta
      if (isTRUE(rv$imported) || is.null(meta)) return(NULL)
      bslib::card(
        class = "border-primary",
        bslib::card_header("Une session est enregistrée dans ce navigateur"),
        bslib::card_body(
          shiny::div(
            class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
            shiny::div(
              shiny::strong(chr1(meta$titre, "Sujet LCA")), shiny::tags$br(),
              shiny::tags$small(
                class = "text-muted",
                sprintf("%s/%s questions répondues en aveugle — enregistrée le %s",
                        chr1(meta$repondu, "?"), chr1(meta$total, "?"), chr1(meta$date, "?"))
              )
            ),
            shiny::div(
              class = "d-flex gap-2",
              shiny::actionButton(ns("reprendre_navigateur"), "Reprendre", class = "btn-primary"),
              shiny::actionButton(ns("oublier_navigateur"), "Supprimer", class = "btn-outline-danger")
            )
          ),
          shiny::p(class = "small text-muted mb-0 mt-2",
                   "Démarrer une nouvelle session ou en reprendre une autre la remplacera.")
        )
      )
    })

    output$demarrage <- shiny::renderUI({
      if (isTRUE(rv$imported)) return(NULL)
      shiny::tagList(bslib::layout_columns(
        col_widths = c(6, 6), fill = FALSE, fillable = FALSE,
        bslib::card(
          bslib::card_header("Nouvelle session"),
          bslib::card_body(
            shiny::p("Importez un sujet LCA au format JSON (produit par le skill lca-create) pour démarrer une session vierge."),
            shiny::fileInput(ns("fichier_sujet"), "Sujet LCA (.json)", accept = ".json", width = "100%",
                             buttonLabel = "Parcourir…", placeholder = "Aucun fichier choisi")
          )
        ),
        bslib::card(
          bslib::card_header("Reprendre une session"),
          bslib::card_body(
            shiny::p("Déposez un fichier de session exporté depuis l'application pour retrouver vos réponses et vos arbitrages."),
            shiny::fileInput(ns("fichier_session"), "Fichier de session (.json)", accept = ".json", width = "100%",
                             buttonLabel = "Parcourir…", placeholder = "Aucun fichier choisi")
          )
        )),
        if (isFALSE(navigateur()$disponible)) {
          shiny::p(
            class = "small text-danger",
            "Aucune donnée n'est conservée sur le serveur, et le stockage local de ce navigateur est indisponible (navigation privée ?) : la session ne sera pas enregistrée automatiquement. Exportez-la avant de quitter la page (bouton « Exporter la session »)."
          )
        } else {
          shiny::p(
            class = "small text-muted",
            "Aucune donnée n'est conservée sur le serveur. La session est enregistrée automatiquement dans ce navigateur, sur cet appareil ; pour la garder durablement ou la reprendre ailleurs, exportez-la (bouton « Exporter la session »)."
          )
        }
      )
    })

    output$session_en_cours <- shiny::renderUI({
      shiny::req(rv$imported)
      tq <- table_questions(rv$questions_final)
      mouvements <- c(
        if (any(tq$ajoutee & !tq$supprimee)) sprintf("%d ajoutée(s)", sum(tq$ajoutee & !tq$supprimee)),
        if (any(tq$supprimee)) sprintf("%d supprimée(s)", sum(tq$supprimee))
      )
      bslib::card(
        bslib::card_header("Session en cours"),
        bslib::card_body(
          shiny::p(shiny::strong("Titre : "), rv$sujet_meta$source$titre %||% "—"),
          shiny::p(shiny::strong("Type d'étude : "), rv$sujet_meta$type_etude %||% "—"),
          shiny::p(shiny::strong("Nombre de questions : "), sum(!tq$supprimee),
                   if (length(mouvements)) sprintf(" (%s)", paste(mouvements, collapse = ", "))),
          shiny::p(shiny::strong("Difficulté cible : "), rv$sujet_meta$parametres$difficulte_cible %||% "—"),
          shiny::div(
            class = "d-flex flex-wrap gap-2",
            shiny::downloadButton(ns("exporter"), "Exporter la session", class = "btn-primary"),
            shiny::actionButton(ns("fermer"), "Fermer la session…", class = "btn-outline-danger")
          ),
          shiny::p(
            class = "small text-muted mb-0 mt-3",
            "Le fichier de session contient le sujet, vos réponses et vos arbitrages. Conservez-le : c'est lui qu'il faut déposer à l'accueil pour reprendre ce travail plus tard ou sur un autre appareil."
          )
        )
      )
    })

    output$barre <- shiny::renderUI({
      shiny::req(rv$imported)
      sauvegarde <- derniere_sauvegarde()
      etat <- if (is.null(sauvegarde)) {
        NULL
      } else if (isTRUE(sauvegarde$ok)) {
        shiny::tags$small(
          class = "text-white-50", title = "La session est aussi enregistrée dans ce navigateur",
          paste("Enregistrée à", chr1(sauvegarde$heure, "?"))
        )
      } else {
        shiny::tags$small(
          class = "text-warning",
          title = "Le stockage local de ce navigateur est indisponible ou plein : seul l'export conserve la session.",
          "Non enregistrée dans le navigateur"
        )
      }
      shiny::tagList(
        etat,
        shiny::downloadButton(ns("exporter_barre"), "Exporter la session", class = "btn-sm btn-light")
      )
    })

    invisible(NULL)
  })
}
