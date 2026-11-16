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

mod_jeu_aveugle_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    # Questions jouables en aveugle : celles du sujet d'origine encore
    # conservées. Une question ajoutée en Comparaison est rédigée par
    # l'utilisateur lui-même (rien à deviner), une question supprimée ne
    # compte plus.
    questions <- shiny::reactive({
      shiny::req(rv$questions_final)
      tq <- table_questions(rv$questions_final)
      tq[!tq$supprimee & !tq$ajoutee, , drop = FALSE]
    })

    # Les cases à cocher portent le numéro de leur question dans leur
    # identifiant (`reponse_3_A`) : elles sont reconstruites (renderUI) à chaque
    # changement de question, ce qui est asynchrone côté navigateur. Avec un
    # identifiant commun à toutes les questions, un enregistrement survenant
    # avant que le navigateur ait réaffiché les cases (deux clics rapprochés sur
    # « Suivant », changement d'onglet immédiat — d'autant plus probable que le
    # serveur est distant) lisait les réponses de la question quittée et les
    # rangeait sous le numéro de la nouvelle. Ici, les cases d'une question ne
    # peuvent être lues que pour elle ; pas encore affichées, elles valent NULL
    # et rien n'est écrit.
    id_case <- function(prefixe, idx, L) paste(prefixe, idx, L, sep = "_")

    save_current <- function(idx) {
      shiny::req(rv$human_answers)
      lettres_idx <- rv$propositions_json$lettre[rv$propositions_json$numero == idx]
      for (L in lettres_idx) {
        mask_h <- rv$human_answers$numero == idx & rv$human_answers$lettre == L
        val_rep <- input[[id_case("reponse", idx, L)]]
        if (!is.null(val_rep)) rv$human_answers$coche_humain[mask_h] <- isTRUE(val_rep)

        mask_f <- rv$flags$numero == idx & rv$flags$lettre == L
        val_flag <- input[[id_case("flag", idx, L)]]
        if (!is.null(val_flag)) {
          rv$flags$commentaire[mask_f] <- if (isTRUE(val_flag)) "Signalé lors du jeu en aveugle" else ""
        }
      }
    }

    etat_repondu <- function(i) {
      shiny::req(rv$human_answers)
      any(rv$human_answers$coche_humain[rv$human_answers$numero == i])
    }

    nav <- mod_question_nav_server("nav", questions, on_navigate = save_current, state_fn = etat_repondu)

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
            shiny::checkboxInput(session$ns(id_case("flag", idx, L)), label = "\U0001F6A9",
                                  value = L %in% flagged_prev)
          ),
          shiny::div(
            class = "flex-grow-1",
            shiny::checkboxInput(session$ns(id_case("reponse", idx, L)),
                                  label = paste0(L, ") ", props$texte[i]),
                                  value = L %in% checked_prev, width = "100%")
          )
        )
      }))
    })

    output$panneau_validation <- shiny::renderUI(NULL)

    shiny::observeEvent(input$verifier, {
      save_current(nav$current())
      q <- questions()
      repondues <- unique(rv$human_answers$numero[rv$human_answers$coche_humain])
      incomplet <- q$etiquette[!q$numero %in% repondues]

      output$panneau_validation <- shiny::renderUI({
        if (length(incomplet) == 0) {
          shiny::div(class = "alert alert-success",
                     "Toutes les questions comportent au moins une réponse cochée.")
        } else {
          shiny::div(class = "alert alert-warning",
                     shiny::strong(sprintf("%d question(s) sans aucune réponse cochée : ", length(incomplet))),
                     paste(incomplet, collapse = ", "),
                     shiny::tags$br(),
                     shiny::tags$small("Cliquez sur les numéros de questions dans la barre latérale pour vous y rendre."))
        }
      })
    })

    list(save_now = function() save_current(shiny::isolate(nav$current())))
  })
}
