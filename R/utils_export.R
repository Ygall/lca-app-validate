#' Rend un template Quarto (docx ou pptx) à partir de l'état consolidé de rv
#'
#' Écrit le JSON consolidé et une copie du template dans un dossier temporaire
#' dédié, appelle quarto::quarto_render() avec des chemins absolus, puis
#' retourne le chemin du fichier produit (copié hors du dossier temporaire
#' avant que celui-ci ne soit nettoyé).
render_export <- function(rv, vision, template_relpath, output_format, app_dir) {
  tmp_dir <- tempfile("lca_export_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  consolidated <- build_consolidated_json(rv, "complet")
  tmp_json <- file.path(tmp_dir, "export.json")
  jsonlite::write_json(consolidated, tmp_json, auto_unbox = TRUE, pretty = TRUE, na = "null")

  src_qmd <- file.path(app_dir, template_relpath)
  if (!file.exists(src_qmd)) stop("Template Quarto introuvable : ", src_qmd)
  tmp_qmd <- file.path(tmp_dir, basename(template_relpath))
  file.copy(src_qmd, tmp_qmd, overwrite = TRUE)

  src_helpers <- file.path(app_dir, "quarto", "_export_helpers.R")
  file.copy(src_helpers, file.path(tmp_dir, "_export_helpers.R"), overwrite = TRUE)

  out_file <- paste0(tools::file_path_sans_ext(basename(template_relpath)), ".", output_format)

  quarto::quarto_render(
    input = normalizePath(tmp_qmd),
    output_format = output_format,
    execute_params = list(export_json = normalizePath(tmp_json), vision = vision),
    output_file = out_file
  )

  rendered_path <- file.path(tmp_dir, out_file)
  if (!file.exists(rendered_path)) {
    stop("Le rendu Quarto n'a pas produit le fichier attendu : ", rendered_path)
  }

  if (identical(output_format, "pptx")) {
    adjust_pptx_title_placeholder(rendered_path)
  }

  final_path <- tempfile(fileext = paste0(".", output_format))
  file.copy(rendered_path, final_path, overwrite = TRUE)
  final_path
}

#' Repositionne et réduit la police du placeholder de titre généré par Quarto
#'
#' Le titre de chaque diapo vient d'un en-tête markdown (`##`) : Quarto/pandoc
#' génère lui-même la forme correspondante, sans possibilité de contrôler sa
#' taille de police ou sa position depuis le markdown. On corrige donc après
#' coup, en éditant directement le XML de chaque diapo (le contenu, lui, est
#' entièrement maîtrisé en amont via pptx_content_shape()).
adjust_pptx_title_placeholder <- function(pptx_path, title_y = 350000, title_h = 650000, title_sz = 2000) {
  ns <- c(
    p = "http://schemas.openxmlformats.org/presentationml/2006/main",
    a = "http://schemas.openxmlformats.org/drawingml/2006/main"
  )

  edit_dir <- tempfile("pptxedit_")
  dir.create(edit_dir)
  on.exit(unlink(edit_dir, recursive = TRUE), add = TRUE)
  utils::unzip(pptx_path, exdir = edit_dir)

  slide_files <- list.files(file.path(edit_dir, "ppt", "slides"), pattern = "^slide[0-9]+\\.xml$", full.names = TRUE)
  for (f in slide_files) {
    doc <- xml2::read_xml(f)
    title_sp <- xml2::xml_find_first(doc, ".//p:sp[.//p:ph[@type='title']]", ns)
    if (inherits(title_sp, "xml_missing")) next

    spPr <- xml2::xml_find_first(title_sp, "./p:spPr", ns)
    xfrm <- xml2::xml_add_child(spPr, "a:xfrm", .where = 0)
    xml2::xml_add_child(xfrm, "a:off", x = "457200", y = as.character(title_y))
    xml2::xml_add_child(xfrm, "a:ext", cx = "8229600", cy = as.character(title_h))

    runs <- xml2::xml_find_all(title_sp, ".//a:r/a:rPr", ns)
    for (r in runs) xml2::xml_set_attr(r, "sz", as.character(title_sz))

    xml2::write_xml(doc, f)
  }

  # all.files = TRUE : sans ça, list.files() ignore silencieusement
  # _rels/.rels (fichier commençant par un point) — or ce fichier est la
  # racine de découverte du paquet OPC ; l'omettre du zip produit un pptx
  # que PowerPoint juge corrompu ("réparer" puis rien à afficher).
  files <- list.files(edit_dir, recursive = TRUE, full.names = FALSE, all.files = TRUE)
  files <- files[!files %in% c(".", "..")]
  old_wd <- setwd(edit_dir)
  zip::zip(pptx_path, files)
  setwd(old_wd)

  invisible(pptx_path)
}
