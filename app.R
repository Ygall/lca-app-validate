# Point d'entrée pour un déploiement (Posit Connect, Connect Cloud,
# shinyapps.io, Shiny Server) à partir des sources du dépôt.
#
# En usage local, installer le package puis appeler lcavalidate::lancer_app().

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE, quiet = TRUE)
lcavalidate::lancer_app()
