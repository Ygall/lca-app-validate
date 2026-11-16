#' Chemin d'un fichier installé avec le package (schéma, gabarits Quarto, CSS…)
#' @noRd
chemin_app <- function(...) {
  system.file(..., package = "lcavalidate", mustWork = TRUE)
}

# Colonnes citées sans guillemets dans les appels dplyr (utils_notions.R).
utils::globalVariables(c("id", "numero", "role"))

# Dépendances utilisées hors du code R du package : knitr et rmarkdown par les
# gabarits Quarto des exports, yaml par bslib pour lire _brand.yml. Elles sont
# en Imports pour être installées avec le package ; cette fonction, jamais
# appelée, les référence pour que R CMD check ne les signale pas inutilisées.
dependances_indirectes <- function() {
  list(knitr::knit, rmarkdown::render, yaml::read_yaml)
}

ONGLETS_A_MASQUER <- c("jeu", "comparaison", "diff", "export")

app_ui <- function(referentiel) {
  # Paramètre de version basé sur la date de modification du fichier : évite
  # de servir une version de diff.css mise en cache par le navigateur après
  # une modification du fichier (développement, mise à jour du package).
  version <- as.integer(max(file.info(list.files(chemin_app("app", "www"), full.names = TRUE))$mtime))

  bslib::page_navbar(
    title = "Validation LCA",
    id = "main_nav",
    theme = bslib::bs_theme(version = 5, brand = chemin_app("app", "_brand.yml")),
    navbar_options = bslib::navbar_options(bg = "#2039B9", theme = "dark"),
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = paste0("lcavalidate/diff.css?v=", version)),
      # Préfixe des inputs du module d'accueil, lu par session.js.
      shiny::tags$script(shiny::HTML(sprintf(
        "window.lcaSessionNs = %s;", jsonlite::toJSON(shiny::NS("accueil")(""), auto_unbox = TRUE)
      ))),
      shiny::tags$script(src = paste0("lcavalidate/session.js?v=", version))
    ),
    bslib::nav_panel("Accueil", mod_accueil_ui("accueil"), value = "accueil"),
    bslib::nav_panel("1. Jeu en aveugle", mod_jeu_aveugle_ui("jeu"), value = "jeu"),
    bslib::nav_panel("2. Comparaison", mod_comparaison_ui("comparaison", referentiel = referentiel), value = "comparaison"),
    bslib::nav_panel("3. Vue d'ensemble", mod_diff_overview_ui("diff"), value = "diff"),
    bslib::nav_panel("4. Export", mod_export_ui("export"), value = "export"),
    bslib::nav_spacer(),
    bslib::nav_item(mod_accueil_barre_ui("accueil"))
  )
}

app_server <- function(referentiel, schema_path) {
  function(input, output, session) {
    rv <- init_rv()
    rv$referentiel <- referentiel

    # Enregistre dans rv la saisie en cours de l'onglet affiché (ou quitté) :
    # elle n'y est sinon reportée qu'à la navigation entre questions.
    enregistrer_saisie <- function(onglet) {
      if (identical(onglet, "jeu")) jeu_api$save_now()
      if (identical(onglet, "comparaison")) comparaison_api$save_now()
    }

    mod_accueil_server(
      "accueil", rv, schema_path = schema_path,
      avant_export = function() enregistrer_saisie(shiny::isolate(input$main_nav))
    )
    jeu_api <- mod_jeu_aveugle_server("jeu", rv)
    comparaison_api <- mod_comparaison_server("comparaison", rv)
    mod_diff_overview_server("diff", rv)
    mod_export_server("export", rv)

    shiny::observe({
      if (isTRUE(rv$imported)) {
        for (t in ONGLETS_A_MASQUER) bslib::nav_show("main_nav", target = t, session = session)
      } else {
        for (t in ONGLETS_A_MASQUER) bslib::nav_hide("main_nav", target = t, session = session)
      }
    })

    # Changer d'onglet via la barre de navigation (plutôt que les boutons
    # Précédent/Suivant internes) ne doit pas faire perdre la réponse en cours
    # de saisie sur la question actuellement affichée.
    onglet_precedent <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$main_nav, {
      enregistrer_saisie(onglet_precedent())
      onglet_precedent(input$main_nav)
    })
  }
}

#' Lancer l'application de validation des sujets LCA
#'
#' Ouvre l'application Shiny qui permet de valider un sujet de lecture critique
#' d'article (LCA) produit par le skill `lca-create` : jeu en aveugle,
#' comparaison au corrigé initial, vue d'ensemble des modifications, exports.
#'
#' L'application ne conserve rien sur la machine qui l'exécute : la session de
#' travail est enregistrée dans le navigateur et s'exporte dans un fichier, à
#' déposer à l'accueil pour la reprendre.
#'
#' Les exports Word et PowerPoint nécessitent
#' [Quarto](https://quarto.org) ; sans lui, seuls l'export JSON et l'export de
#' la session sont proposés.
#'
#' @param ... Options transmises à [shiny::shinyApp()] (`port`, `host`,
#'   `launch.browser`…).
#' @return Un objet application Shiny. Affiché dans la console (cas d'un
#'   appel direct), il lance l'application.
#' @examples
#' if (interactive()) {
#'   lancer_app()
#' }
#' @export
lancer_app <- function(...) {
  shiny::addResourcePath("lcavalidate", chemin_app("app", "www"))
  referentiel <- load_referentiel_notions(chemin_app("schema", "boussole_lca_notions.json"))
  shiny::shinyApp(
    ui = app_ui(referentiel),
    server = app_server(referentiel, chemin_app("schema", "schema-sujet-lca.json")),
    options = list(...)
  )
}
