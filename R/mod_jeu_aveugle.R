mod_jeu_aveugle_ui <- function(id) {
  ns <- shiny::NS(id)
  mod_question_nav_ui(ns("nav"), main_content = shiny::tagList(
    shiny::uiOutput(ns("panneau_validation")),
    bslib::card(
      bslib::card_header(shiny::uiOutput(ns("entete"))),
      bslib::card_body(shiny::uiOutput(ns("propositions_ui")))
    ),
    shiny::div(class = "mt-3 d-flex gap-2",
               shiny::actionButton(ns("verifier"), "Vérifier mes réponses", class = "btn-outline-secondary"))
  ))
}

mod_jeu_aveugle_server <- function(id, rv, saves_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    total <- shiny::reactive({ shiny::req(rv$questions_meta); nrow(rv$questions_meta) })

    save_current <- function(idx) {
      shiny::req(rv$human_answers)
      lettres_idx <- rv$propositions_json$lettre[rv$propositions_json$numero == idx]
      for (L in lettres_idx) {
        mask_h <- rv$human_answers$numero == idx & rv$human_answers$lettre == L
        val_rep <- input[[paste0("reponse_", L)]]
        if (!is.null(val_rep)) rv$human_answers$coche_humain[mask_h] <- isTRUE(val_rep)

        mask_f <- rv$flags$numero == idx & rv$flags$lettre == L
        val_flag <- input[[paste0("flag_", L)]]
        if (!is.null(val_flag)) {
          rv$flags$commentaire[mask_f] <- if (isTRUE(val_flag)) "Signalé lors du jeu en aveugle" else ""
        }
      }
      persist_session_rds(rv, saves_dir)
    }

    etat_repondu <- function(i) {
      shiny::req(rv$human_answers)
      any(rv$human_answers$coche_humain[rv$human_answers$numero == i])
    }

    nav <- mod_question_nav_server("nav", total, on_navigate = save_current, state_fn = etat_repondu)

    # Même protection que dans mod_comparaison.R : le groupe de cases à
    # cocher et les champs de flag sont reconstruits (renderUI) à chaque
    # changement de question, ce qui est asynchrone côté navigateur. Un
    # save_now() externe (déclenché par un changement d'onglet de la barre
    # de navigation principale) survenant juste après une navigation interne
    # pourrait sinon lire input$reponses encore associé à la question
    # quittée et l'enregistrer sous le numéro de la nouvelle — le save de la
    # question quittée ayant de toute façon déjà été fait de façon fiable
    # par on_navigate juste avant.
    chargement_recent <- FALSE
    derniere_idx_affichee <- NULL
    shiny::observe({
      shiny::req(rv$imported)
      idx <- nav$current()
      if (!identical(derniere_idx_affichee, idx)) {
        derniere_idx_affichee <<- idx
        chargement_recent <<- TRUE
      }
    })
    lapply(c(paste0("reponse_", LETTERS[1:5]), paste0("flag_", LETTERS[1:5])), function(fid) {
      shiny::observeEvent(input[[fid]], { chargement_recent <<- FALSE }, ignoreInit = TRUE)
    })

    output$entete <- shiny::renderUI({
      shiny::req(rv$imported)
      idx <- nav$current()
      enonce <- rv$questions_final$enonce[rv$questions_final$numero == idx]
      shiny::strong(enonce)
    })

    # Toujours des cases à cocher, quel que soit le type QRU/QRM réel de la
    # question : l'étudiant ne doit jamais pouvoir déduire du type de widget
    # (radio = une seule réponse possible) le nombre de propositions exactes.
    # Chaque proposition est une case à cocher individuelle (plutôt qu'un
    # checkboxGroupInput unique) pour pouvoir lui accoler à gauche une
    # deuxième case à cocher de signalement ("flag"), simple indicateur
    # booléen sans texte à saisir — la logique de détection (côté
    # Comparaison/Vue d'ensemble) reste inchangée : elle regarde juste si
    # rv$flags$commentaire est non vide.
    output$propositions_ui <- shiny::renderUI({
      shiny::req(rv$imported, rv$flags)
      idx <- nav$current()
      props <- rv$propositions_json[rv$propositions_json$numero == idx, ]
      props <- props[order(props$lettre), ]
      checked_prev <- rv$human_answers$lettre[rv$human_answers$numero == idx & rv$human_answers$coche_humain]
      flagged_prev <- rv$flags$lettre[rv$flags$numero == idx & nzchar(trimws(rv$flags$commentaire))]

      shiny::tagList(lapply(seq_len(nrow(props)), function(i) {
        L <- props$lettre[i]
        shiny::div(
          class = "d-flex align-items-center gap-2 mb-1",
          shiny::div(
            class = "flag-checkbox-wrap",
            title = "Signaler cette proposition (à revoir en Comparaison)",
            shiny::checkboxInput(session$ns(paste0("flag_", L)), label = "\U0001F6A9",
                                  value = L %in% flagged_prev)
          ),
          shiny::div(
            class = "flex-grow-1",
            shiny::checkboxInput(session$ns(paste0("reponse_", L)),
                                  label = paste0(L, ") ", props$texte[i]),
                                  value = L %in% checked_prev, width = "100%")
          )
        )
      }))
    })

    output$panneau_validation <- shiny::renderUI(NULL)

    shiny::observeEvent(input$verifier, {
      save_current(nav$current())
      incomplet <- rv$human_answers |>
        dplyr::group_by(numero) |>
        dplyr::summarise(repondu = any(coche_humain), .groups = "drop") |>
        dplyr::filter(!repondu) |>
        dplyr::pull(numero)

      output$panneau_validation <- shiny::renderUI({
        if (length(incomplet) == 0) {
          shiny::div(class = "alert alert-success",
                     "Toutes les questions comportent au moins une réponse cochée.")
        } else {
          shiny::div(class = "alert alert-warning",
                     shiny::strong(sprintf("%d question(s) sans aucune réponse cochée : ", length(incomplet))),
                     paste(sort(incomplet), collapse = ", "),
                     shiny::tags$br(),
                     shiny::tags$small("Cliquez sur les numéros de questions dans la barre latérale pour vous y rendre."))
        }
      })
    })

    list(save_now = function() {
      if (chargement_recent) return(invisible(NULL))
      save_current(shiny::isolate(nav$current()))
    })
  })
}
