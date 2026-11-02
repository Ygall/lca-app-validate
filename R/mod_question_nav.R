#' Sélecteur de question partagé : grille de 3 tuiles par ligne en barre
#' latérale gauche (largeur du sidebar inchangée), identique dans les 3
#' parties (jeu en aveugle, comparaison, vue d'ensemble).
#'
#' @param main_content contenu principal à afficher à droite de la grille
#'
#' Boutons Précédent/Suivant intégrés directement sous la grille, dans la
#' barre latérale (demande utilisateur du 2026-08-14 : auparavant à côté des
#' boutons d'action de chaque partie, dans le contenu principal).
mod_question_nav_ui <- function(id, main_content, sidebar_title = "Questions") {
  ns <- shiny::NS(id)
  # fillable/fill = FALSE : sinon bslib étire le bloc à toute la hauteur de
  # la page (cf. Vue d'ensemble/Export), laissant un grand espace vide.
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = sidebar_title,
      shiny::uiOutput(ns("grille")),
      shiny::div(
        class = "d-flex gap-2 mt-2",
        shiny::actionButton(ns("prev_btn"), "◀ Précédent", class = "btn-sm flex-fill"),
        shiny::actionButton(ns("next_btn"), "Suivant ▶", class = "btn-sm flex-fill")
      )
    ),
    main_content,
    fillable = FALSE,
    fill = FALSE
  )
}

#' @param total reactive() renvoyant le nombre total de questions
#' @param on_navigate function(old_index) appelée juste avant tout changement
#'   d'index, pour laisser l'appelant sauvegarder l'état de la question quittée
#' @param state_fn function(i) optionnelle renvoyant TRUE/FALSE (répondue /
#'   modifiée…) ; colore la tuile en vert plutôt que d'ajouter un badge/emoji
#' @return list(current = reactiveVal, goto = function(n))
mod_question_nav_server <- function(id, total, on_navigate, state_fn = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    current <- shiny::reactiveVal(1L)

    output$grille <- shiny::renderUI({
      n <- total()
      shiny::req(n)
      cur <- current()
      shiny::div(
        class = "question-grid",
        lapply(seq_len(n), function(i) {
          actif <- isTRUE(!is.null(state_fn) && state_fn(i))
          classe <- paste(
            "btn btn-sm",
            if (actif) "btn-success" else "btn-outline-dark",
            if (identical(i, cur)) "border border-3 border-primary" else ""
          )
          shiny::tags$a(
            href = "#", class = classe,
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;",
                               ns("goto_click"), i),
            paste0("Q", i)
          )
        })
      )
    })

    goto <- function(i) {
      old <- shiny::isolate(current())
      if (!identical(i, old)) {
        on_navigate(old)
        current(i)
      }
    }

    shiny::observeEvent(input$goto_click, goto(input$goto_click))

    shiny::observeEvent(input$prev_btn, {
      n <- current()
      if (n > 1) goto(n - 1)
    })
    shiny::observeEvent(input$next_btn, {
      n <- current()
      if (n < total()) goto(n + 1)
    })

    list(current = current, goto = goto)
  })
}
