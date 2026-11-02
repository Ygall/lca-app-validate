mod_diff_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("compteurs")),
    shiny::uiOutput(ns("notions")),
    mod_question_nav_ui(ns("nav"), main_content = shiny::tagList(
      shiny::uiOutput(ns("contenu"))
    ))
  )
}

#' Petit tableau de contingence 2x2 (initial correct × humain coché)
tableau_accord_2x2 <- function(accord) {
  shiny::tags$table(
    class = "table table-sm table-bordered mb-2",
    style = "font-size: 0.8rem;",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(""), shiny::tags$th("Cochée"), shiny::tags$th("Non cochée")
    )),
    shiny::tags$tbody(
      shiny::tags$tr(
        shiny::tags$th("Initial correct"),
        shiny::tags$td(accord$ia_oui_humain_oui),
        shiny::tags$td(accord$ia_oui_humain_non)
      ),
      shiny::tags$tr(
        shiny::tags$th("Initial incorrect"),
        shiny::tags$td(accord$ia_non_humain_oui),
        shiny::tags$td(accord$ia_non_humain_non)
      )
    )
  )
}

mod_diff_overview_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    total <- shiny::reactive({ shiny::req(rv$questions_meta); nrow(rv$questions_meta) })

    etat_modifiee <- function(i) {
      shiny::req(rv$final, rv$questions_final)
      question_modifiee(rv, i)
    }

    # Vue en lecture seule : rien à sauvegarder en quittant une question.
    nav <- mod_question_nav_server("nav", total, on_navigate = function(idx) invisible(NULL), state_fn = etat_modifiee)

    output$compteurs <- shiny::renderUI({
      shiny::req(rv$imported)
      qs <- rv$questions_meta$numero
      diffs <- lapply(qs, function(n) compute_question_diff(rv, n))
      q_mod <- sum(vapply(diffs, function(d) d$any_modifie, logical(1)))
      p_mod <- sum(vapply(diffs, function(d) sum(d$propositions$any_modifie), integer(1)))
      flags_non_resolus <- sum(vapply(diffs, function(d) d$n_flags_non_resolus, integer(1)))
      accord <- compute_agreement_stats(rv)

      box_theme <- bslib::value_box_theme(bg = "#E8EEFC", fg = "#16297D")
      flag_theme <- if (flags_non_resolus > 0) {
        bslib::value_box_theme(bg = "#FBE7E4", fg = "#A6402E")
      } else {
        box_theme
      }
      bslib::layout_columns(
        col_widths = c(2, 2, 2, 6),
        fill = FALSE,
        fillable = FALSE,
        bslib::value_box(title = "Questions modifiées", value = sprintf("%d / %d", q_mod, length(qs)), theme = box_theme),
        bslib::value_box(title = "Propositions modifiées", value = sprintf("%d / %d", p_mod, nrow(rv$propositions_json)), theme = box_theme),
        bslib::value_box(title = "Flags non résolus", value = flags_non_resolus, theme = flag_theme),
        bslib::card(
          bslib::card_header("Accord avec la version initiale"),
          bslib::card_body(
            tableau_accord_2x2(accord),
            shiny::p(class = "mb-0 small",
                     sprintf("Accord : %.0f%% — κ de Cohen = %s",
                             accord$pourcentage_accord * 100,
                             if (is.na(accord$kappa)) "—" else sprintf("%.2f", accord$kappa)))
          )
        )
      )
    })

    output$notions <- shiny::renderUI({
      shiny::req(rv$imported, rv$raw_json, rv$notions_final)
      # État arbitré (Partie 2), pas le JSON d'origine : suit les ajouts/retraits.
      rep <- compute_notions_repartition(rv$notions_final, rv$raw_json, rv$referentiel)
      resume <- if (nrow(rep$table) > 0) {
        sprintf("%d notions — %d propositions sur %d rattachées à une notion",
                nrow(rep$table), rep$n_propositions_avec_notion, rep$n_propositions)
      }
      bslib::card(
        class = "mb-3",
        fill = FALSE,
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          "Répartition des notions théoriques",
          shiny::tags$small(class = "text-muted fw-normal", resume)
        ),
        bslib::card_body(
          fillable = FALSE,
          class = "notion-card-body",
          render_notions_repartition(rep),
          if (isTRUE(rep$referentiel_different)) {
            shiny::p(class = "small text-muted mt-2 mb-0",
                     sprintf("Libellés lus dans le référentiel local (version %s) ; le sujet a été produit avec la version %s. Les numéros affichés sont ceux du sujet.",
                             rep$version_referentiel, rep$version_sujet))
          }
        )
      )
    })

    output$contenu <- shiny::renderUI({
      shiny::req(rv$imported)
      qd <- compute_question_diff(rv, nav$current())
      htmltools::tags$div(id = paste0("q-diff-", qd$numero), class = "diff-question", render_question_diff(qd))
    })
  })
}
