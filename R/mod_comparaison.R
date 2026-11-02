#' Éditeur de notions d'une proposition : pastilles (réactives, rendues côté
#' serveur) + liste déroulante des notions du référentiel, groupées par domaine.
#' Un choix dans la liste n'est pas lu comme valeur d'input : il est transmis
#' en événement (`notion_action` = « lettre|ajouter|id »), puis la liste est vidée.
notions_editeur_ui <- function(ns, L, choix) {
  envoi_ajout <- sprintf(
    "function(value) { var s = this; if (!value) return; Shiny.setInputValue('%s', '%s|ajouter|' + value, {priority: 'event'}); setTimeout(function() { s.clear(true); }, 0); }",
    ns("notion_action"), L
  )
  shiny::div(
    class = "notions-editeur mt-2",
    shiny::tags$label(class = "form-label small fw-bold mb-1", "Notions théoriques"),
    shiny::uiOutput(ns(paste0("notions_", L))),
    shiny::selectizeInput(
      ns(paste0("notion_add_", L)), NULL, choices = choix, width = "100%",
      options = list(placeholder = "Ajouter une notion…", onItemAdd = I(envoi_ajout),
                     onInitialize = I("function() { this.setValue(''); }"))
    )
  )
}

comparaison_ligne_ui <- function(ns, L, choix_notions = list("")) {
  shiny::tagList(
    shiny::uiOutput(ns(paste0("style_", L))),
    shiny::div(
      id = ns(paste0("bloc_", L)),
      class = "border rounded p-2 mb-2",
      shiny::div(class = "d-flex align-items-center justify-content-between mb-1",
                 shiny::div(class = "d-flex align-items-center gap-2",
                            shiny::strong(paste0(L, ")")),
                            shiny::uiOutput(ns(paste0("badges_", L)), inline = TRUE)),
                 shiny::uiOutput(ns(paste0("flagbadge_", L)), inline = TRUE)),
      shiny::uiOutput(ns(paste0("statut_tiles_", L))),
      shiny::uiOutput(ns(paste0("manuel_check_", L))),
      shiny::textAreaInput(ns(paste0("texte_", L)), NULL, width = "100%", rows = 2),
      shiny::textAreaInput(ns(paste0("justif_", L)), "Justification", width = "100%", rows = 2),
      notions_editeur_ui(ns, L, choix_notions)
    )
  )
}

mod_comparaison_ui <- function(id, referentiel = NULL) {
  ns <- shiny::NS(id)
  lettres <- LETTERS[1:5]
  choix_notions <- choix_notions_selectize(referentiel)
  mod_question_nav_ui(ns("nav"), main_content = shiny::tagList(
    shiny::uiOutput(ns("avert_referentiel")),
    bslib::card(
      bslib::card_body(
        shiny::tags$label("Énoncé de la question", class = "fw-bold", "for" = ns("enonce")),
        shiny::textAreaInput(ns("enonce"), NULL, width = "100%", rows = 2),
        bslib::layout_column_wrap(
          width = 1 / 3, fill = FALSE, fillable = FALSE, gap = "0.5rem",
          !!!lapply(lettres, function(L) comparaison_ligne_ui(ns, L, choix_notions))
        )
      )
    )
  ))
}

mod_comparaison_server <- function(id, rv, saves_dir) {
  shiny::moduleServer(id, function(input, output, session) {    ns <- session$ns
    lettres <- LETTERS[1:5]
    total <- shiny::reactive({ shiny::req(rv$questions_meta); nrow(rv$questions_meta) })

    # Le statut (Initial/Vous/Modifier) se commit immédiatement au clic sur une
    # tuile (cf. observeEvent(input$choix_statut) plus bas) : rv$final en est
    # la seule source de vérité, plus besoin de la resynchroniser ici.
    save_current <- function(idx) {
      shiny::req(rv$final)
      for (L in lettres) {
        texte <- input[[paste0("texte_", L)]]
        justif <- input[[paste0("justif_", L)]]
        manuel_val <- isTRUE(input[[paste0("manuel_", L)]])
        mask <- rv$final$numero == idx & rv$final$lettre == L
        if (identical(rv$final$statut_source[mask][1], "manuel")) {
          rv$final$correcte_final[mask] <- manuel_val
        }
        rv$final$texte_final[mask] <- texte %||% rv$final$texte_final[mask]
        rv$final$justification_final[mask] <- justif %||% rv$final$justification_final[mask]
      }
      if (!is.null(input$enonce)) {
        rv$questions_final$enonce[rv$questions_final$numero == idx] <- input$enonce
      }
      persist_session_rds(rv, saves_dir)
    }

    etat_modifiee <- function(i) {
      shiny::req(rv$final, rv$questions_final)
      question_modifiee(rv, i)
    }

    nav <- mod_question_nav_server("nav", total, on_navigate = save_current, state_fn = etat_modifiee)

    load_current <- function(idx) {
      shiny::req(rv$final)
      shiny::updateTextAreaInput(session, "enonce", value = rv$questions_final$enonce[rv$questions_final$numero == idx])
      for (L in lettres) {
        f <- rv$final[rv$final$numero == idx & rv$final$lettre == L, ]
        shiny::updateTextAreaInput(session, paste0("texte_", L), value = f$texte_final[1])
        shiny::updateTextAreaInput(session, paste0("justif_", L), value = f$justification_final[1])
      }
    }

    # Ne recharger les textAreaInput qu'au changement RÉEL de question, pas à
    # chaque écriture dans rv$final (ex. clic sur une tuile de statut, qui ne
    # modifie qu'une lettre) : sinon updateTextAreaInput réécrit les 5x2 zones
    # de texte avec la dernière valeur *persistée* de rv$final, effaçant toute
    # frappe non encore enregistrée sur les *autres* propositions de la même
    # question (bug rapporté : "la question se reset en cliquant sur
    # Vous/Modifier"). `derniere_idx_chargee` est une variable de closure
    # ordinaire (pas un reactiveVal) : sa lecture ne crée donc pas de
    # dépendance réactive, seule la lecture de nav$current() en crée une.
    derniere_idx_chargee <- NULL
    # `chargement_recent` protège contre une seconde course, bien plus grave,
    # confirmée en testant l'app : updateTextAreaInput() est asynchrone
    # (message envoyé au navigateur, pas de garantie de prise en compte
    # immédiate côté serveur). Juste après une navigation interne
    # (Suivant/Précédent), input$texte_A etc. peuvent donc, l'instant
    # suivant, contenir encore côté serveur le texte de la question
    # QUITTÉE. Si à ce moment précis l'utilisateur change d'onglet dans la
    # barre de navigation principale, app.R déclenche un save_now() qui,
    # sans ce garde-fou, sauvegarderait ce texte périmé sous le numéro de la
    # NOUVELLE question — reproduisant exactement le bug observé
    # (« la question 2 a été remplacée par la question 3 »). Le save de la
    # question quittée a de toute façon déjà été fait, de façon fiable, par
    # goto()/save_current() juste avant ce chargement : il n'y a donc rien à
    # perdre à ignorer un save_now() tant que le navigateur n'a pas confirmé
    # avoir rattrapé son retard (passage à FALSE dès le premier écho reçu
    # sur l'un des champs).
    chargement_recent <- FALSE
    lapply(c(paste0("texte_", lettres), paste0("justif_", lettres), "enonce"), function(fid) {
      shiny::observeEvent(input[[fid]], { chargement_recent <<- FALSE }, ignoreInit = TRUE)
    })
    shiny::observe({
      shiny::req(rv$imported)
      idx <- nav$current()
      if (!identical(derniere_idx_chargee, idx)) {
        load_current(idx)
        derniere_idx_chargee <<- idx
        chargement_recent <<- TRUE
      }
    })

    # Tuiles "Initial / Vous / Modifier" façon sélecteur de question : commit
    # immédiat dans rv$final, pas d'attente de la navigation. On sauvegarde
    # d'abord tous les champs actuellement affichés (texte/justification/case
    # "Correcte" manuelle des 5 propositions) : sans ça, seul le statut de la
    # lettre cliquée était committé, et le rechargement déclenché par cette
    # écriture dans rv$final écrasait les modifications non enregistrées des
    # *autres* propositions avec leur dernière valeur persistée.
    shiny::observeEvent(input$choix_statut, {
      parts <- strsplit(input$choix_statut, "\\|")[[1]]
      L <- parts[1]
      valeur <- parts[2]
      idx <- nav$current()
      save_current(idx)
      correcte_final <- switch(valeur,
        ia = isTRUE(rv$propositions_json$correcte[rv$propositions_json$numero == idx & rv$propositions_json$lettre == L]),
        humain = isTRUE(rv$human_answers$coche_humain[rv$human_answers$numero == idx & rv$human_answers$lettre == L]),
        manuel = isTRUE(input[[paste0("manuel_", L)]])
      )
      mask <- rv$final$numero == idx & rv$final$lettre == L
      rv$final$statut_source[mask] <- valeur
      rv$final$correcte_final[mask] <- correcte_final
      persist_session_rds(rv, saves_dir)
    })

    # Notions théoriques : ajout / retrait / bascule évaluée↔associée, appliqués
    # immédiatement à rv$notions_final (comme les tuiles de statut). Ne touche
    # ni aux zones de texte ni à rv$final : rien à resynchroniser côté champs.
    shiny::observeEvent(input$notion_action, {
      parts <- strsplit(input$notion_action, "|", fixed = TRUE)[[1]]
      shiny::req(length(parts) == 3, parts[1] %in% lettres)
      rv$notions_final <- modifier_notions(
        rv$notions_final, nav$current(), parts[1], parts[2], parts[3], rv$referentiel
      )
      persist_session_rds(rv, saves_dir)
    })

    output$avert_referentiel <- shiny::renderUI({
      shiny::req(rv$imported, rv$raw_json)
      if (isTRUE(referentiel_different(rv$raw_json, rv$referentiel))) {
        shiny::div(
          class = "alert alert-warning py-2 small",
          sprintf("Ce sujet utilise la version %s du référentiel de notions, l'application la version %s : les numéros des notions ajoutées suivent la version de l'application.",
                  rv$raw_json$sujet$referentiel_notions$version, rv$referentiel$version)
        )
      }
    })

    lapply(lettres, function(L) {
      # Pastilles + contrôle de cohérence avec les numéros cités dans la
      # justification (dépend de la saisie en cours dans la zone de justification).
      output[[paste0("notions_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported, rv$notions_final)
        idx <- nav$current()
        lignes <- rv$notions_final[rv$notions_final$question == idx & rv$notions_final$lettre == L, ]
        cites <- numeros_cites_justification(input[[paste0("justif_", L)]] %||% "")
        presents <- sort(unique(lignes$numero))
        incoherences <- c(
          if (length(setdiff(presents, cites))) sprintf("non cité(s) dans la justification : %s", paste(setdiff(presents, cites), collapse = ", ")),
          if (length(setdiff(cites, presents))) sprintf("cité(s) sans notion rattachée : %s", paste(setdiff(cites, presents), collapse = ", "))
        )
        shiny::tagList(
          render_notions_pastilles(lignes, L, rv$referentiel, ns("notion_action")),
          if (length(incoherences)) {
            shiny::div(class = "small text-warning-emphasis mt-1",
                       paste0("⚠ Numéros ", paste(incoherences, collapse = " — ")))
          }
        )
      })

      output[[paste0("badges_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported)
        idx <- nav$current()
        j <- rv$propositions_json[rv$propositions_json$numero == idx & rv$propositions_json$lettre == L, ]
        h <- rv$human_answers[rv$human_answers$numero == idx & rv$human_answers$lettre == L, ]
        shiny::tagList(
          shiny::tags$span(class = "source-badge source-badge-initial", "Initial"),
          shiny::tags$span(class = paste("mini-case", if (isTRUE(j$correcte[1])) "bg-success" else "bg-danger")),
          shiny::tags$span(class = "source-badge source-badge-vous ms-2", "Vous"),
          shiny::tags$span(class = paste("mini-case", if (isTRUE(h$coche_humain[1])) "bg-success" else "bg-danger"))
        )
      })

      output[[paste0("statut_tiles_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported, rv$final)
        idx <- nav$current()
        actuel <- rv$final$statut_source[rv$final$numero == idx & rv$final$lettre == L]
        choix <- c(ia = "Initial", humain = "Vous", manuel = "Modifier")
        shiny::div(
          class = "d-flex gap-1 mb-1",
          lapply(names(choix), function(v) {
            classe <- paste("btn btn-sm", if (identical(actuel, v)) "btn-primary" else "btn-outline-dark")
            shiny::tags$a(
              href = "#", class = classe,
              onclick = sprintf("Shiny.setInputValue('%s', '%s|%s', {priority: 'event'}); return false;",
                                 ns("choix_statut"), L, v),
              choix[[v]]
            )
          })
        )
      })

      output[[paste0("manuel_check_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported, rv$final)
        idx <- nav$current()
        mask <- rv$final$numero == idx & rv$final$lettre == L
        if (identical(rv$final$statut_source[mask][1], "manuel")) {
          shiny::checkboxInput(session$ns(paste0("manuel_", L)), "Correcte",
                                value = isTRUE(rv$final$correcte_final[mask][1]))
        }
      })

      output[[paste0("flagbadge_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported, rv$flags)
        idx <- nav$current()
        commentaire <- rv$flags$commentaire[rv$flags$numero == idx & rv$flags$lettre == L]
        if (length(commentaire) && nzchar(trimws(commentaire[1]))) {
          shiny::tags$span(
            class = "flag-badge", title = commentaire[1],
            shiny::tags$span(class = "flag-dot"), "Flag"
          )
        }
      })

      output[[paste0("style_", L)]] <- shiny::renderUI({
        shiny::req(rv$imported)
        idx <- nav$current()
        j <- rv$propositions_json[rv$propositions_json$numero == idx & rv$propositions_json$lettre == L, ]
        h <- rv$human_answers[rv$human_answers$numero == idx & rv$human_answers$lettre == L, ]
        desaccord <- !identical(isTRUE(j$correcte[1]), isTRUE(h$coche_humain[1]))
        if (desaccord) {
          shiny::tags$style(shiny::HTML(sprintf(
            "#%s { background-color: #fdeee6; border-color: #f4a98c; }", session$ns(paste0("bloc_", L))
          )))
        }
      })
    })

    list(save_now = function() {
      if (chargement_recent) return(invisible(NULL))
      save_current(shiny::isolate(nav$current()))
    })
  })
}
