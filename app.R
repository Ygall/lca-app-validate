library(shiny)
library(bslib)

APP_DIR <- getwd()

for (f in list.files(file.path(APP_DIR, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

shiny::addResourcePath("www", file.path(APP_DIR, "www"))

# Paramètre de version basé sur la date de modification du fichier : évite
# de servir une version de diff.css mise en cache par le navigateur après
# une modification du fichier pendant le développement.
DIFF_CSS_VERSION <- as.integer(file.info(file.path(APP_DIR, "www", "diff.css"))$mtime)

SAVES_DIR <- file.path(APP_DIR, "saves")
SCHEMA_PATH <- file.path(APP_DIR, "schema", "schema-sujet-lca.json")
REFERENTIEL_NOTIONS <- load_referentiel_notions(file.path(APP_DIR, "schema", "boussole_lca_notions.json"))

ONGLETS_A_MASQUER <- c("jeu", "comparaison", "diff", "export")

ui <- page_navbar(
  title = "Validation LCA",
  id = "main_nav",
  theme = bs_theme(version = 5, brand = TRUE),
  navbar_options = navbar_options(bg = "#2039B9", theme = "dark"),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                href = paste0("www/diff.css?v=", DIFF_CSS_VERSION))),
  nav_panel("Import", mod_import_ui("import"), value = "import"),
  nav_panel("1. Jeu en aveugle", mod_jeu_aveugle_ui("jeu"), value = "jeu"),
  nav_panel("2. Comparaison", mod_comparaison_ui("comparaison", referentiel = REFERENTIEL_NOTIONS), value = "comparaison"),
  nav_panel("3. Vue d'ensemble", mod_diff_overview_ui("diff"), value = "diff"),
  nav_panel("4. Export", mod_export_ui("export"), value = "export")
)

server <- function(input, output, session) {
  rv <- init_rv()
  rv$referentiel <- REFERENTIEL_NOTIONS

  mod_import_server("import", rv, schema_path = SCHEMA_PATH, saves_dir = SAVES_DIR)
  jeu_api <- mod_jeu_aveugle_server("jeu", rv, saves_dir = SAVES_DIR)
  comparaison_api <- mod_comparaison_server("comparaison", rv, saves_dir = SAVES_DIR)
  mod_diff_overview_server("diff", rv)
  mod_export_server("export", rv, app_dir = APP_DIR)

  observe({
    if (isTRUE(rv$imported)) {
      for (t in ONGLETS_A_MASQUER) nav_show("main_nav", target = t, session = session)
    } else {
      for (t in ONGLETS_A_MASQUER) nav_hide("main_nav", target = t, session = session)
    }
  })

  # Changer d'onglet via la barre de navigation (plutôt que les boutons
  # Précédent/Suivant internes) ne doit pas faire perdre la réponse en cours
  # de saisie sur la question actuellement affichée.
  onglet_precedent <- reactiveVal(NULL)
  observeEvent(input$main_nav, {
    ancien <- onglet_precedent()
    if (identical(ancien, "jeu")) jeu_api$save_now()
    if (identical(ancien, "comparaison")) comparaison_api$save_now()
    onglet_precedent(input$main_nav)
  })
}

shinyApp(ui, server)
