export_bloc_ui <- function(ns, key, titre, description) {
  shiny::div(
    class = "border rounded p-3 h-100 d-flex flex-column",
    shiny::h6(titre),
    shiny::p(class = "small text-muted flex-grow-1", description),
    shiny::uiOutput(ns(paste0("btn_", key)))
  )
}

mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  # fillable/fill = FALSE : par défaut bslib étire la carte et la rangée de
  # colonnes à toute la hauteur disponible de la page, au lieu de s'ajuster
  # au contenu réel — laissant un grand espace vide sous les blocs d'export.
  bslib::card(
    bslib::card_header("Export"),
    bslib::card_body(
      shiny::uiOutput(ns("quarto_statut")),
      shiny::uiOutput(ns("controles")),
      shiny::div(class = "mb-3", shiny::uiOutput(ns("btn_tout"))),
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        fill = FALSE,
        fillable = FALSE,
        export_bloc_ui(ns, "json", "JSON",
                        "Sujet consolidé complet (réponses et justifications finales), au format lca-create."),
        export_bloc_ui(ns, "sujet", "Sujet",
                        "Document Word, propositions seules, sans corrigé — pour les étudiants."),
        export_bloc_ui(ns, "corrige", "Corrigé",
                        "Document Word avec lettres colorées et justifications en ligne."),
        export_bloc_ui(ns, "diapos", "Diapos",
                        "Diapositives PowerPoint pour une correction en séance (2 diapos par question).")
      )
    ),
    fill = FALSE
  )
}

mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    quarto_ok <- tryCatch(!is.null(quarto::quarto_path()), error = function(e) FALSE)

    # Écarts au format attendu, que l'ajout ou la suppression de questions
    # peut introduire : signalés, jamais bloquants.
    output$controles <- shiny::renderUI({
      shiny::req(rv$imported)
      alertes <- controles_sujet(rv)
      if (length(alertes)) {
        shiny::div(
          class = "alert alert-warning",
          shiny::strong("À vérifier avant diffusion :"),
          shiny::tags$ul(class = "mb-0", lapply(alertes, shiny::tags$li))
        )
      }
    })

    output$quarto_statut <- shiny::renderUI({
      if (!quarto_ok) {
        shiny::div(class = "alert alert-warning",
                   "CLI Quarto non détecté : les exports Sujet, Corrigé et Diapos sont indisponibles. ",
                   "Installez Quarto (quarto.org) puis redémarrez l'application.")
      }
    })

    bouton_ou_indisponible <- function(dl_id) {
      if (quarto_ok) {
        shiny::downloadButton(session$ns(dl_id), "Télécharger", class = "w-100")
      } else {
        shiny::tags$button(class = "btn btn-secondary w-100", disabled = "disabled", "Indisponible")
      }
    }

    output$btn_tout <- shiny::renderUI({
      if (quarto_ok) {
        shiny::downloadButton(session$ns("dl_tout"), "Tout exporter (.zip)", class = "btn-primary")
      } else {
        shiny::tags$button(class = "btn btn-secondary", disabled = "disabled",
                            "Tout exporter — indisponible (Quarto requis)")
      }
    })

    output$btn_json <- shiny::renderUI(shiny::downloadButton(session$ns("dl_json"), "Télécharger", class = "w-100"))
    output$btn_sujet <- shiny::renderUI(bouton_ou_indisponible("dl_sujet"))
    output$btn_corrige <- shiny::renderUI(bouton_ou_indisponible("dl_corrige"))
    output$btn_diapos <- shiny::renderUI(bouton_ou_indisponible("dl_diapos"))

    output$dl_json <- shiny::downloadHandler(
      filename = function() {
        shiny::req(rv$imported)
        paste0("sujet_", rv$sujet_meta$identifiant %||% "lca", "_complet.json")
      },
      content = function(file) {
        shiny::req(rv$imported)
        consolidated <- build_consolidated_json(rv, "complet")
        jsonlite::write_json(consolidated, file, auto_unbox = TRUE, pretty = TRUE, na = "null")
      }
    )

    output$dl_sujet <- shiny::downloadHandler(
      filename = function() paste0("sujet_", rv$sujet_meta$identifiant %||% "lca", ".docx"),
      content = function(file) {
        shiny::req(rv$imported)
        shiny::withProgress(message = "Génération du sujet...", {
          out <- render_export(rv, "etudiant", "corrige_template.qmd", "docx")
          file.copy(out, file, overwrite = TRUE)
        })
      }
    )

    output$dl_corrige <- shiny::downloadHandler(
      filename = function() paste0("corrige_", rv$sujet_meta$identifiant %||% "lca", ".docx"),
      content = function(file) {
        shiny::req(rv$imported)
        shiny::withProgress(message = "Génération du corrigé...", {
          out <- render_export(rv, "complet", "corrige_template.qmd", "docx")
          file.copy(out, file, overwrite = TRUE)
        })
      }
    )

    output$dl_diapos <- shiny::downloadHandler(
      filename = function() paste0("diapos_correction_", rv$sujet_meta$identifiant %||% "lca", ".pptx"),
      content = function(file) {
        shiny::req(rv$imported)
        shiny::withProgress(message = "Génération des diapositives...", {
          out <- render_export(rv, "complet", "diapos_correction_template.qmd", "pptx")
          file.copy(out, file, overwrite = TRUE)
        })
      }
    )

    output$dl_tout <- shiny::downloadHandler(
      filename = function() {
        shiny::req(rv$imported)
        paste0("export_", rv$sujet_meta$identifiant %||% "lca", ".zip")
      },
      content = function(file) {
        shiny::req(rv$imported)
        id <- rv$sujet_meta$identifiant %||% "lca"

        shiny::withProgress(message = "Génération des 4 exports...", value = 0, {
          tmp_dir <- tempfile("lca_export_zip_")
          dir.create(tmp_dir)
          on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

          shiny::incProgress(0.1, detail = "JSON")
          consolidated <- build_consolidated_json(rv, "complet")
          jsonlite::write_json(consolidated, file.path(tmp_dir, paste0("sujet_", id, "_complet.json")),
                                auto_unbox = TRUE, pretty = TRUE, na = "null")

          shiny::incProgress(0.25, detail = "Sujet (docx)")
          out_sujet <- render_export(rv, "etudiant", "corrige_template.qmd", "docx")
          file.copy(out_sujet, file.path(tmp_dir, paste0("sujet_", id, ".docx")), overwrite = TRUE)

          shiny::incProgress(0.25, detail = "Corrigé (docx)")
          out_corrige <- render_export(rv, "complet", "corrige_template.qmd", "docx")
          file.copy(out_corrige, file.path(tmp_dir, paste0("corrige_", id, ".docx")), overwrite = TRUE)

          shiny::incProgress(0.25, detail = "Diapos (pptx)")
          out_diapos <- render_export(rv, "complet", "diapos_correction_template.qmd", "pptx")
          file.copy(out_diapos, file.path(tmp_dir, paste0("diapos_correction_", id, ".pptx")), overwrite = TRUE)

          shiny::incProgress(0.15, detail = "Compression (.zip)")
          # `root` plutôt qu'un setwd() : le répertoire de travail est commun à
          # toutes les sessions d'un même processus R sur un serveur.
          zip::zip(normalizePath(file, mustWork = FALSE), list.files(tmp_dir), root = tmp_dir)
        })
      }
    )
  })
}
