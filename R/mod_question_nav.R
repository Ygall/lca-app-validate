#' Sélecteur de question partagé : grille de 3 tuiles par ligne en barre
#' latérale gauche (largeur du sidebar inchangée), identique dans les 3
#' parties (jeu en aveugle, comparaison, vue d'ensemble).
#'
#' @param main_content contenu principal à afficher à droite de la grille
#' @param sidebar_extra contenu optionnel ajouté sous les boutons
#'   Précédent/Suivant (ex. : ajout / suppression de question en Comparaison)
#'
#' Boutons Précédent/Suivant intégrés directement sous la grille, dans la
#' barre latérale (demande utilisateur du 2026-08-14 : auparavant à côté des
#' boutons d'action de chaque partie, dans le contenu principal).
#' @noRd
mod_question_nav_ui <- function(id, main_content, sidebar_title = "Questions", sidebar_extra = NULL) {
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
      ),
      sidebar_extra
    ),
    main_content,
    fillable = FALSE,
    fill = FALSE
  )
}

#' @param questions reactive() renvoyant la table des questions à proposer,
#'   dans l'ordre d'affichage : colonnes `numero` (identifiant interne stable),
#'   `etiquette` (texte de la tuile) et `supprimee` (cf. table_questions()).
#'   La liste peut changer en cours de session (question ajoutée, supprimée).
#' @param on_navigate function(ancien_numero) appelée juste avant tout
#'   changement de question, pour laisser l'appelant sauvegarder l'état de la
#'   question quittée
#' @param state_fn function(numero) optionnelle renvoyant TRUE/FALSE (répondue /
#'   modifiée…) ; colore la tuile en vert plutôt que d'ajouter un badge/emoji
#' @return list(current = reactive (numéro interne de la question affichée),
#'   goto = function(numero))
#' @noRd
mod_question_nav_server <- function(id, questions, on_navigate, state_fn = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    choisi <- shiny::reactiveVal(NULL)

    # Toujours un numéro de la liste : la question choisie peut en être sortie
    # depuis (supprimée dans une autre partie), auquel cas on revient à la première.
    current <- shiny::reactive({
      numeros <- questions()$numero
      shiny::req(length(numeros) > 0)
      if (isTRUE(choisi() %in% numeros)) choisi() else numeros[1]
    })

    output$grille <- shiny::renderUI({
      q <- questions()
      shiny::req(nrow(q) > 0)
      cur <- current()
      shiny::div(
        class = "question-grid",
        lapply(seq_len(nrow(q)), function(k) {
          i <- q$numero[k]
          actif <- isTRUE(!is.null(state_fn) && state_fn(i))
          classe <- paste(
            "btn btn-sm",
            if (isTRUE(q$supprimee[k])) "question-supprimee" else if (actif) "btn-success" else "btn-outline-dark",
            if (identical(i, cur)) "border border-3 border-primary" else ""
          )
          shiny::tags$a(
            href = "#", class = classe,
            title = if (isTRUE(q$supprimee[k])) "Question supprimée",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;",
                               ns("goto_click"), i),
            q$etiquette[k]
          )
        })
      )
    })

    goto <- function(i) {
      i <- as.integer(i)
      old <- shiny::isolate(current())
      if (!identical(i, old) && isTRUE(i %in% shiny::isolate(questions()$numero))) {
        on_navigate(old)
        choisi(i)
      }
    }

    voisine <- function(pas) {
      numeros <- questions()$numero
      position <- match(current(), numeros) + pas
      if (position >= 1 && position <= length(numeros)) goto(numeros[position])
    }

    shiny::observeEvent(input$goto_click, goto(input$goto_click))
    shiny::observeEvent(input$prev_btn, voisine(-1))
    shiny::observeEvent(input$next_btn, voisine(1))

    list(current = current, goto = goto)
  })
}
