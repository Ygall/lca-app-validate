test_that("une session exportée puis réimportée redonne le même état", {
  etat <- etat_exemple()
  etat$human_answers$coche_humain[c(1, 3, 7)] <- TRUE
  etat$flags$commentaire[2] <- "Signalé lors du jeu en aveugle"
  etat$final$texte_final[4] <- "Texte réécrit, avec « guillemets » et accents"
  etat$final$correcte_final[4] <- !etat$final$correcte_final[4]
  etat$final$statut_source[4] <- "manuel"
  etat$questions_final$enonce[2] <- "2) Énoncé modifié"
  etat$notions_final <- etat$notions_final[-1, ]

  relu <- aller_retour(etat)

  expect_true(relu$ok)
  expect_identical(relu$type, "session")
  for (champ in CHAMPS_ETAT) {
    expect_equal(relu$etat[[champ]], etat[[champ]], ignore_attr = TRUE, info = champ)
  }
})

test_that("les questions ajoutées et supprimées survivent à l'aller-retour", {
  etat <- etat_exemple()
  ajout <- ajouter_question(etat$questions_final, etat$final, apres = 3L, notion = "Biais",
                            difficulte = 6, section_article = "methodes")
  etat$questions_final <- marquer_suppression(ajout$questions_final, 5L)
  etat$final <- ajout$final
  etat$final$texte_final[etat$final$numero == ajout$numero] <- paste("Proposition", LETTRES)
  etat$final$correcte_final[etat$final$numero == ajout$numero & etat$final$lettre == "B"] <- TRUE

  relu <- aller_retour(etat)

  expect_true(relu$ok)
  expect_equal(relu$etat$questions_final, etat$questions_final, ignore_attr = TRUE)
  expect_equal(relu$etat$final, etat$final, ignore_attr = TRUE)
})

test_that("un fichier déposé est reconnu comme sujet ou comme session", {
  sujet <- lire_fichier_import(chemin_exemple(), chemin_schema())
  expect_true(sujet$ok)
  expect_identical(sujet$type, "sujet")

  fichier <- withr::local_tempfile(fileext = ".json")
  writeLines(session_vers_json(session_vers_liste(etat_exemple()), pretty = TRUE), fichier, useBytes = TRUE)
  session <- lire_fichier_import(fichier, chemin_schema())
  expect_true(session$ok)
  expect_identical(session$type, "session")
})

test_that("un fichier qui n'est ni un sujet valide ni une session est rejeté", {
  fichier <- withr::local_tempfile(fileext = ".json")

  writeLines("pas du json", fichier)
  expect_false(lire_fichier_import(fichier, chemin_schema())$ok)

  writeLines('{"sujet": {}, "questions": []}', fichier)
  rejet <- lire_fichier_import(fichier, chemin_schema())
  expect_false(rejet$ok)
  expect_gt(length(rejet$erreurs), 0)
})

test_that("une session illisible ou trop récente est refusée avec un message", {
  expect_false(lire_session("pas du json")$ok)
  expect_false(lire_session('{"format": "autre"}')$ok)
  expect_match(
    lire_session('{"format": "lca-validate-session", "version": 99}')$erreurs,
    "version plus récente"
  )
  expect_match(
    lire_session('{"format": "lca-validate-session", "version": 1, "sujet": {"questions": [1, 2]}}')$erreurs,
    "illisible"
  )
})

test_that("le texte d'une session n'est jamais pris pour un chemin ou une URL", {
  # jsonlite::fromJSON() lirait le fichier désigné ; lire_session() ne doit pas.
  expect_false(lire_session(chemin_exemple())$ok)
  expect_false(lire_session("https://example.com/session.json")$ok)
})

test_that("les valeurs incohérentes d'un fichier de session sont ignorées", {
  etat <- etat_exemple()
  liste <- session_vers_liste(etat)
  liste$etat$propositions[[1]]$statut_source <- "n'importe quoi"
  liste$etat$propositions[[2]]$correcte_final <- "oui"
  liste$etat$propositions[[3]]$texte_final <- list(1, 2)
  liste$etat$reponses[[1]]$numero <- 999
  liste$etat$questions <- c(liste$etat$questions, list(
    list(numero = 77, enonce = "Fantôme", ajoutee = FALSE),
    list(numero = "abc", enonce = "Sans numéro", ajoutee = TRUE)
  ))
  liste$etat$notions <- c(liste$etat$notions, list(
    list(question = 1, lettre = "Z", id = "x", numero = "001", role = "evaluee"),
    list(question = 1, lettre = "A", id = "x", numero = "1", role = "evaluee")
  ))

  relu <- lire_session(session_vers_json(liste))

  expect_true(relu$ok)
  expect_equal(relu$etat$final, etat$final, ignore_attr = TRUE)
  expect_equal(relu$etat$human_answers, etat$human_answers, ignore_attr = TRUE)
  expect_equal(relu$etat$questions_final, etat$questions_final, ignore_attr = TRUE)
  expect_equal(relu$etat$notions_final, etat$notions_final, ignore_attr = TRUE)
})

test_that("une ancienne sauvegarde .rds se convertit en fichier de session", {
  etat <- etat_exemple()
  etat$human_answers$coche_humain[1:3] <- TRUE
  dossier <- withr::local_tempdir()
  rds <- file.path(dossier, "ancienne.rds")
  # Format des sauvegardes d'avant les fichiers de session : questions_final
  # sans les colonnes d'ajout / suppression.
  saveRDS(list(
    raw_json = etat$raw_json, questions_final = etat$questions_final[, c("numero", "enonce")],
    final = etat$final, human_answers = etat$human_answers, flags = etat$flags,
    notions_final = etat$notions_final, sauvegarde_le = Sys.time()
  ), rds)

  sortie <- convertir_session_rds(rds)

  expect_identical(sortie, file.path(dossier, "ancienne.json"))
  relu <- lire_fichier_import(sortie, chemin_schema())
  expect_true(relu$ok)
  expect_identical(relu$type, "session")
  expect_equal(relu$etat$human_answers, etat$human_answers, ignore_attr = TRUE)
  expect_equal(relu$etat$questions_final, etat$questions_final, ignore_attr = TRUE)
})
