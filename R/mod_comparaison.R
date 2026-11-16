#' Éditeur de notions d'une proposition : pastilles (réactives, rendues côté
#' serveur) + liste déroulante des notions du référentiel, groupées par domaine.
#' Un choix dans la liste n'est pas lu comme valeur d'input : il est transmis
#' en événement (`notion_action` = « lettre|ajouter|id »), puis la liste est vidée.
#' @noRd
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
  # Ajout / suppression de question : dans la barre latérale, sous la grille.
  actions_questions <- shiny::tagList(
    shiny::tags$hr(class = "my-2"),
    shiny::tags$button(
      type = "button", class = "btn btn-sm btn-outline-primary w-100",
      onclick = sprintf("Shiny.setInputValue('%s', 'ajouter', {priority: 'event'}); return false;",
                        ns("action_question")),
      "+ Ajouter une question"
    ),
    shiny::uiOutput(ns("bouton_suppression")),
    shiny::tags$small(
      class = "text-muted",
      "Une question supprimée est exclue des exports et du score d'accord. Les questions conservées sont renumérotées, y compris dans les documents exportés."
    )
  )
  mod_question_nav_ui(ns("nav"), sidebar_extra = actions_questions, main_content = shiny::tagList(
    shiny::uiOutput(ns("avert_referentiel")),
    shiny::uiOutput(ns("bandeau_question")),
    bslib::card(
      id = ns("carte_question"),
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

mod_comparaison_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lettres <- LETTERS[1:5]
    # Toutes les questions, y compris supprimées (pour pouvoir les restaurer)
    # et ajoutées.
    questions <- shiny::reactive({
      shiny::req(rv$questions_final)
      table_questions(rv$questions_final)
    })
    question_ajoutee <- function(idx) isTRUE(rv$questions_final$ajoutee[rv$questions_final$numero == idx])
    question_supprimee <- function(idx) isTRUE(rv$questions_final$supprimee[rv$questions_final$numero == idx])

    # Le statut (Initial/Vous/Modifier) se commit immédiatement au clic sur une
    # tuile (cf. observeEvent(input$choix_statut) plus bas) : rv$final en est
    # la seule source de vérité, plus besoin de la resynchroniser ici.
    save_current <- function(idx) {
      shiny::req(rv$final)
      # Champs pas encore à jour pour cette question (cf. saisie_a_jour() plus
      # bas) : ils portent encore le contenu de la question quittée.
      if (!saisie_a_jour()) return(invisible(NULL))
      for (L in lettres) {
        texte <- input[[paste0("texte_", L)]]
        justif <- input[[paste0("justif_", L)]]
        manuel_val <- input[[id_manuel(idx, L)]]
        mask <- rv$final$numero == idx & rv$final$lettre == L
        if (identical(rv$final$statut_source[mask][1], "manuel") && !is.null(manuel_val)) {
          rv$final$correcte_final[mask] <- isTRUE(manuel_val)
        }
        rv$final$texte_final[mask] <- texte %||% rv$final$texte_final[mask]
        rv$final$justification_final[mask] <- justif %||% rv$final$justification_final[mask]
      }
      if (!is.null(input$enonce)) {
        rv$questions_final$enonce[rv$questions_final$numero == idx] <- input$enonce
      }
    }

    etat_modifiee <- function(i) {
      shiny::req(rv$final, rv$questions_final)
      question_modifiee(rv, i)
    }

    nav <- mod_question_nav_server("nav", questions, on_navigate = save_current, state_fn = etat_modifiee)

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
    #
    # Seconde course, bien plus grave, confirmée en testant l'app :
    # updateTextAreaInput() est asynchrone (message envoyé au navigateur).
    # Juste après un changement de question, input$texte_A etc. contiennent
    # encore, côté serveur, le texte de la question QUITTÉE. Un enregistrement
    # qui survient à ce moment (deux clics rapprochés sur « Suivant »,
    # changement d'onglet immédiat) rangerait ce texte sous le numéro de la
    # NOUVELLE question (« la question 2 a été remplacée par la question 3 »).
    # La fenêtre est de quelques millisecondes en local, mais d'un aller-retour
    # réseau complet sur un serveur distant.
    #
    # D'où un accusé de réception : chaque chargement porte un numéro, que le
    # navigateur renvoie (input$charge_ack) une fois les champs mis à jour —
    # il traite le message « lca-accuse » après les mises à jour d'inputs de
    # la même réponse du serveur, et renvoie leurs valeurs avec l'accusé.
    # Tant que l'accusé du DERNIER chargement n'est pas revenu, les champs ne
    # sont pas ceux de la question affichée et save_current() ne fait rien.
    # Rien n'est perdu : la question quittée a été enregistrée par la
    # navigation elle-même, et l'utilisateur n'a encore rien pu saisir dans
    # la nouvelle. (Un premier garde-fou, levé au premier écho d'un champ,
    # ne suffisait pas : l'écho d'un chargement antérieur le levait à tort.)
    charge_id <- 0L
    saisie_a_jour <- function() isTRUE(shiny::isolate(input$charge_ack) == charge_id)
    shiny::observe({
      shiny::req(rv$imported)
      idx <- nav$current()
      if (!identical(derniere_idx_chargee, idx)) {
        load_current(idx)
        derniere_idx_chargee <<- idx
        charge_id <<- charge_id + 1L
        session$sendCustomMessage("lca-accuse", list(id = ns("charge_ack"), valeur = charge_id))
      }
    })

    # Les cases « Correcte » sont reconstruites (renderUI) à chaque question :
    # leur identifiant porte le numéro de la question (`manuel_3_A`), pour
    # qu'elles ne puissent être lues que pour elle (même principe que les
    # cases du jeu en aveugle).
    id_manuel <- function(idx, L) paste("manuel", idx, L, sep = "_")

    # Tuiles "Initial / Vous / Modifier" façon sélecteur de question : commit
    # immédiat dans rv$final, pas d'attente de la navigation. On sauvegarde
    # d'abord tous les champs actuellement affichés (texte/justification/case
    # "Correcte" manuelle des 5 propositions) : sans ça, seul le statut de la
    # lettre cliquée était committé, et le rechargement déclenché par cette
    # écriture dans rv$final écrasait les modifications non enregistrées des
    # *autres* propositions avec leur dernière valeur persistée.
    shiny::observeEvent(input$choix_statut, {
      parts <- strsplit(input$choix_statut, "|", fixed = TRUE)[[1]]
      L <- parts[1]
      valeur <- parts[2]
      idx <- nav$current()
      # Clic sur une tuile affichée pour une autre question (réaffichage en
      # cours) : ignoré plutôt qu'appliqué à la mauvaise question.
      shiny::req(length(parts) == 3, L %in% lettres, valeur %in% c("ia", "humain", "manuel"),
                 identical(parts[3], as.character(idx)))
      save_current(idx)
      mask <- rv$final$numero == idx & rv$final$lettre == L
      correcte_final <- switch(valeur,
        ia = isTRUE(rv$propositions_json$correcte[rv$propositions_json$numero == idx & rv$propositions_json$lettre == L]),
        humain = isTRUE(rv$human_answers$coche_humain[rv$human_answers$numero == idx & rv$human_answers$lettre == L]),
        # « Modifier » part de la réponse actuellement retenue ; la case
        # « Correcte » qui apparaît alors permet de la changer.
        manuel = isTRUE(rv$final$correcte_final[mask][1])
      )
      rv$final$statut_source[mask] <- valeur
      rv$final$correcte_final[mask] <- correcte_final
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
    })

    # ---- Ajout / suppression / restauration d'une question ----
    #
    # Toujours save_current() d'abord : les champs affichés et pas encore
    # enregistrés (cf. choix_statut ci-dessus) appartiennent à la question en
    # cours, quelle que soit l'action demandée.
    shiny::observeEvent(input$action_question, {
      idx <- nav$current()
      action <- input$action_question
      if (identical(action, "ajouter")) {
        shiny::showModal(shiny::modalDialog(
          title = "Ajouter une question",
          shiny::radioButtons(
            ns("ajout_position"), "Position",
            choices = c("Après la question affichée" = "apres", "À la fin du sujet" = "fin")
          ),
          shiny::selectInput(
            ns("ajout_section"), "Section de l'article",
            choices = stats::setNames(names(SECTIONS_ARTICLE), SECTIONS_ARTICLE), selected = "transversal"
          ),
          shiny::numericInput(
            ns("ajout_difficulte"), "Difficulté (1 à 10)", min = 1, max = 10, step = 1,
            value = rv$sujet_meta$parametres$difficulte_cible %||% 5
          ),
          shiny::textInput(ns("ajout_notion"), "Notion évaluée (texte libre)", width = "100%"),
          shiny::p(class = "small text-muted mb-0",
                   "L'énoncé, les cinq propositions et leurs justifications se rédigent ensuite directement dans la question créée."),
          footer = shiny::tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("ajout_confirmer"), "Ajouter", class = "btn-primary")
          )
        ))
      } else if (action %in% c("supprimer", "restaurer")) {
        save_current(idx)
        rv$questions_final <- marquer_suppression(rv$questions_final, idx, identical(action, "supprimer"))
      }
    })

    shiny::observeEvent(input$ajout_confirmer, {
      idx <- nav$current()
      save_current(idx)
      res <- ajouter_question(
        rv$questions_final, rv$final,
        apres = if (identical(input$ajout_position, "apres")) idx,
        notion = input$ajout_notion, difficulte = input$ajout_difficulte,
        section_article = input$ajout_section
      )
      rv$questions_final <- res$questions_final
      rv$final <- res$final
      shiny::removeModal()
      nav$goto(res$numero)
    })

    output$bouton_suppression <- shiny::renderUI({
      shiny::req(rv$imported)
      supprimee <- question_supprimee(nav$current())
      shiny::tags$button(
        type = "button",
        class = paste("btn btn-sm w-100 mt-2 mb-2", if (supprimee) "btn-outline-success" else "btn-outline-danger"),
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                          ns("action_question"), if (supprimee) "restaurer" else "supprimer"),
        if (supprimee) "Restaurer la question" else "Supprimer la question"
      )
    })

    output$bandeau_question <- shiny::renderUI({
      shiny::req(rv$imported)
      idx <- nav$current()
      q <- rv$questions_final[rv$questions_final$numero == idx, , drop = FALSE]
      shiny::tagList(
        if (isTRUE(q$supprimee[1])) {
          shiny::tagList(
            # La question reste consultable et modifiable, mais visiblement hors sujet.
            shiny::tags$style(shiny::HTML(sprintf("#%s { opacity: 0.5; }", ns("carte_question")))),
            shiny::div(
              class = "alert alert-danger py-2",
              shiny::strong("Question supprimée. "),
              "Elle ne figure plus dans les exports ni dans le score d'accord. Le bouton « Restaurer la question » de la barre latérale la rétablit telle quelle."
            )
          )
        },
        if (isTRUE(q$ajoutee[1])) {
          shiny::div(
            class = "alert alert-info py-2",
            shiny::strong("Question ajoutée. "),
            sprintf("Section : %s — difficulté : %s%s. Rédigez l'énoncé et les cinq propositions, puis cochez « Correcte » sous chaque proposition exacte.",
                    SECTIONS_ARTICLE[q$section_article[1]], q$difficulte[1],
                    if (isTRUE(nzchar(q$notion[1]))) paste0(" — notion : ", q$notion[1]) else "")
          )
        }
      )
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
        # Question ajoutée : ni version initiale ni réponse en aveugle à rappeler.
        if (question_ajoutee(idx)) return(NULL)
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
        # Question ajoutée : statut toujours « manuel » (case « Correcte » seule).
        if (question_ajoutee(idx)) return(NULL)
        actuel <- rv$final$statut_source[rv$final$numero == idx & rv$final$lettre == L]
        choix <- c(ia = "Initial", humain = "Vous", manuel = "Modifier")
        shiny::div(
          class = "d-flex gap-1 mb-1",
          lapply(names(choix), function(v) {
            classe <- paste("btn btn-sm", if (identical(actuel, v)) "btn-primary" else "btn-outline-dark")
            shiny::tags$a(
              href = "#", class = classe,
              onclick = sprintf("Shiny.setInputValue('%s', '%s|%s|%s', {priority: 'event'}); return false;",
                                 ns("choix_statut"), L, v, idx),
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
          shiny::checkboxInput(session$ns(id_manuel(idx, L)), "Correcte",
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

    list(save_now = function() save_current(shiny::isolate(nav$current())))
  })
}
