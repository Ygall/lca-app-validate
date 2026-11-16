test_that("une question ajoutée s'insère après la question demandée", {
  etat <- etat_exemple()

  ajout <- ajouter_question(etat$questions_final, etat$final, apres = 3L)

  expect_identical(ajout$numero, 15L)
  expect_identical(ajout$questions_final$numero[1:5], c(1L, 2L, 3L, 15L, 4L))
  expect_true(ajout$questions_final$ajoutee[4])
  nouvelles <- ajout$final[ajout$final$numero == 15L, ]
  expect_identical(nouvelles$lettre, LETTRES)
  expect_true(all(nouvelles$statut_source == "manuel"))
  expect_false(any(nouvelles$correcte_final))
})

test_that("sans position, la question ajoutée va en fin de sujet", {
  etat <- etat_exemple()
  ajout <- ajouter_question(etat$questions_final, etat$final)
  expect_identical(utils::tail(ajout$questions_final$numero, 1), 15L)
})

test_that("les métadonnées d'une question ajoutée sont ramenées aux valeurs admises", {
  etat <- etat_exemple()
  ajout <- ajouter_question(etat$questions_final, etat$final, difficulte = 42, section_article = "inconnue")
  ligne <- ajout$questions_final[ajout$questions_final$numero == ajout$numero, ]
  expect_identical(ligne$difficulte, 10L)
  expect_identical(ligne$section_article, "transversal")
})

test_that("les questions conservées sont renumérotées, les supprimées n'ont plus de numéro", {
  etat <- etat_exemple()
  qf <- marquer_suppression(etat$questions_final, 2L)

  tq <- table_questions(qf)

  expect_identical(tq$etiquette[1:4], c("Q1", "✕", "Q2", "Q3"))
  expect_true(is.na(tq$numero_final[2]))
  expect_identical(numeros_conserves(qf), c(1L, 3:14))
  expect_identical(table_questions(marquer_suppression(qf, 2L, FALSE))$etiquette[2], "Q2")
})

test_that("le préfixe numéroté des énoncés suit l'ordre final", {
  expect_identical(
    renumeroter_enonces(c("1) Premier", "3) Troisième", "Sans numéro")),
    c("1) Premier", "2) Troisième", "3) Sans numéro")
  )
  expect_identical(renumeroter_enonces(c("2. Deux", "5. Cinq")), c("1. Deux", "2. Cinq"))
  # Sujet dont les énoncés ne sont pas numérotés : rien n'est ajouté.
  expect_identical(renumeroter_enonces(c("Premier", "Deuxième")), c("Premier", "Deuxième"))
  # « 1.5 mg » n'est pas un numéro de question.
  expect_identical(renumeroter_enonces(c("1.5 mg de produit", "Autre")), c("1.5 mg de produit", "Autre"))
})

test_that("l'export omet les questions supprimées et renumérote les autres", {
  etat <- etat_exemple()
  ajout <- ajouter_question(etat$questions_final, etat$final, apres = 3L, notion = "Biais de classement",
                            difficulte = 6, section_article = "methodes")
  etat$questions_final <- marquer_suppression(ajout$questions_final, 5L)
  etat$final <- ajout$final
  etat$questions_final$enonce[etat$questions_final$numero == 15L] <- "Question ajoutée ?"
  etat$final$correcte_final[etat$final$numero == 15L & etat$final$lettre == "C"] <- TRUE

  json <- build_consolidated_json(etat, "complet")

  expect_length(json$questions, 14)
  expect_identical(vapply(json$questions, function(q) q$numero, integer(1)), 1:14)
  expect_identical(json$sujet$parametres$nombre_questions, 14L)
  # La question d'origine n° 5 a disparu, la n° 6 a pris sa place.
  sans_prefixe <- function(x) sub("^[0-9]+\\) ", "", x)
  enonces <- vapply(json$questions, function(q) q$enonce, character(1))
  expect_false(sans_prefixe(etat$raw_json$questions[[5]]$enonce) %in% sans_prefixe(enonces))
  expect_identical(sans_prefixe(enonces[6]), sans_prefixe(etat$raw_json$questions[[6]]$enonce))
  expect_match(enonces[6], "^6\\) ")

  ajoutee <- json$questions[[4]]
  expect_identical(ajoutee$enonce, "4) Question ajoutée ?")
  expect_identical(ajoutee$type, "QRU")
  expect_identical(ajoutee$nb_propositions_correctes, 1L)
  expect_identical(ajoutee$notion, "Biais de classement")
  expect_identical(ajoutee$section_article, "methodes")
  expect_length(ajoutee$propositions, 5)

  # Contrôle qualité recalculé sur les questions exportées.
  expect_identical(sum(unlist(json$controle_qualite$distribution_propositions_correctes)), 14L)
})

test_that("le nombre de propositions exactes suit l'arbitrage", {
  etat <- etat_exemple()
  qru <- etat$questions_meta$numero[etat$questions_meta$type == "QRU"][1]
  lignes <- etat$final$numero == qru
  etat$final$correcte_final[lignes] <- c(TRUE, TRUE, FALSE, FALSE, FALSE)

  q <- build_consolidated_json(etat, "complet")$questions[[qru]]

  expect_identical(q$nb_propositions_correctes, 2L)
  expect_identical(q$type, "QRM")
})

test_that("les questions supprimées ne comptent pas dans le score d'accord", {
  etat <- etat_exemple()
  expect_identical(compute_agreement_stats(etat)$total, 70L)

  etat$questions_final <- marquer_suppression(etat$questions_final, 5L)
  ajout <- ajouter_question(etat$questions_final, etat$final)
  etat$questions_final <- ajout$questions_final
  etat$final <- ajout$final

  expect_identical(compute_agreement_stats(etat)$total, 65L)
})

test_that("une question ajoutée ou supprimée est signalée comme telle dans le diff", {
  etat <- etat_exemple()
  ajout <- ajouter_question(etat$questions_final, etat$final)
  etat$questions_final <- marquer_suppression(ajout$questions_final, 2L)
  etat$final <- ajout$final

  ajoutee <- compute_question_diff(etat, ajout$numero)
  supprimee <- compute_question_diff(etat, 2L)
  intacte <- compute_question_diff(etat, 1L)

  expect_true(ajoutee$ajoutee && ajoutee$any_modifie)
  expect_true(supprimee$supprimee && supprimee$any_modifie)
  expect_false(intacte$ajoutee || intacte$supprimee || intacte$any_modifie)
  expect_s3_class(render_question_diff(ajoutee), "shiny.tag.list")
  expect_s3_class(render_question_diff(supprimee), "shiny.tag.list")
})

test_that("les écarts au format attendu sont signalés avant export", {
  etat <- etat_exemple()
  expect_length(controles_sujet(etat), 0)

  etat$questions_final <- marquer_suppression(marquer_suppression(etat$questions_final, 1L), 2L)
  ajout <- ajouter_question(etat$questions_final, etat$final)
  etat$questions_final <- ajout$questions_final
  etat$final <- ajout$final

  alertes <- controles_sujet(etat)

  # 13 questions conservées : le compte est bon, seule la question vide est signalée.
  expect_length(alertes, 1)
  expect_match(alertes, "Q13 : énoncé vide")
  expect_match(alertes, "aucune proposition correcte")

  etat$questions_final <- marquer_suppression(etat$questions_final, 3L)
  expect_match(controles_sujet(etat)[1], "12 question")
})
