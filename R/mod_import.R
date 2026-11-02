mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("panneau_reprise")),
    bslib::card(
      bslib::card_header("Importer un sujet LCA format JSON"),
      bslib::card_body(
        shiny::fileInput(ns("fichier"), "Fichier JSON", accept = ".json"),
        shiny::uiOutput(ns("statut")),
        shiny::uiOutput(ns("resume"))
      )
    )
  )
}

mod_import_server <- function(id, rv, schema_path, saves_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$statut <- shiny::renderUI(NULL)

    # Liste figée au démarrage de la session : une suppression ne fait que
    # masquer l'entrée (supprimes) plutôt que de recalculer la liste depuis
    # le disque, pour garder des index stables sur les boutons dynamiques.
    saves_snapshot <- list_saved_sessions(saves_dir)
    supprimes <- shiny::reactiveVal(character(0))

    output$panneau_reprise <- shiny::renderUI({
      if (isTRUE(rv$imported) || length(saves_snapshot) == 0) return(NULL)
      visibles <- Filter(function(i) !(saves_snapshot[[i]]$chemin %in% supprimes()), seq_along(saves_snapshot))
      if (length(visibles) == 0) return(NULL)
      bslib::card(
        bslib::card_header("Reprendre une session en cours"),
        bslib::card_body(
          lapply(visibles, function(i) {
            s <- saves_snapshot[[i]]
            shiny::div(
              class = "d-flex justify-content-between align-items-center border-bottom py-2",
              shiny::div(
                shiny::strong(s$titre), shiny::tags$br(),
                shiny::tags$small(
                  class = "text-muted",
                  sprintf("%d/%d questions répondues — sauvegardé le %s",
                          s$repondu, s$total, format(s$sauvegarde_le, "%d/%m/%Y %H:%M"))
                )
              ),
              shiny::div(
                class = "d-flex gap-2",
                shiny::actionButton(ns(paste0("reprendre_", i)), "Reprendre", class = "btn-sm btn-primary"),
                shiny::actionButton(ns(paste0("supprimer_", i)), "Supprimer", class = "btn-sm btn-outline-danger")
              )
            )
          })
        )
      )
    })

    lapply(seq_along(saves_snapshot), function(i) {
      shiny::observeEvent(input[[paste0("reprendre_", i)]], {
        load_session_rds(rv, saves_snapshot[[i]]$chemin)
      }, ignoreInit = TRUE)

      shiny::observeEvent(input[[paste0("supprimer_", i)]], {
        shiny::showModal(shiny::modalDialog(
          title = "Supprimer cette session ?",
          sprintf("« %s » (%d/%d questions répondues) sera définitivement supprimée.",
                  saves_snapshot[[i]]$titre, saves_snapshot[[i]]$repondu, saves_snapshot[[i]]$total),
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns(paste0("confirmer_suppression_", i)), "Supprimer", class = "btn-danger")
          )
        ))
      }, ignoreInit = TRUE)

      shiny::observeEvent(input[[paste0("confirmer_suppression_", i)]], {
        delete_session_rds(saves_snapshot[[i]]$chemin)
        supprimes(c(supprimes(), saves_snapshot[[i]]$chemin))
        shiny::removeModal()
      }, ignoreInit = TRUE)
    })

    shiny::observeEvent(input$fichier, {
      validation <- validate_sujet_json(input$fichier$datapath, schema_path)

      if (!validation$valide) {
        output$statut <- shiny::renderUI({
          shiny::div(
            class = "alert alert-danger",
            shiny::strong("Fichier rejeté : il ne respecte pas le format attendu."),
            shiny::tags$ul(lapply(validation$erreurs, shiny::tags$li))
          )
        })
        return(invisible(NULL))
      }

      resultat <- tryCatch(parse_sujet_json(input$fichier$datapath), error = function(e) e)

      if (inherits(resultat, "error")) {
        output$statut <- shiny::renderUI({
          shiny::div(class = "alert alert-danger", paste("Échec de l'import :", conditionMessage(resultat)))
        })
        return(invisible(NULL))
      }

      load_sujet_into_rv(rv, resultat)
      persist_session_rds(rv, saves_dir)

      output$statut <- shiny::renderUI({
        shiny::div(class = "alert alert-success",
                   "Sujet importé avec succès. Les autres onglets sont maintenant disponibles.")
      })
    })

    output$resume <- shiny::renderUI({
      shiny::req(rv$imported)
      shiny::tagList(
        shiny::p(shiny::strong("Titre : "), rv$sujet_meta$source$titre %||% "—"),
        shiny::p(shiny::strong("Type d'étude : "), rv$sujet_meta$type_etude %||% "—"),
        shiny::p(shiny::strong("Nombre de questions : "), nrow(rv$questions_meta)),
        shiny::p(shiny::strong("Difficulté cible : "), rv$sujet_meta$parametres$difficulte_cible %||% "—")
      )
    })
  })
}
