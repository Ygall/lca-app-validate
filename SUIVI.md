# Suivi — App Shiny de validation des sujets LCA

Document de suivi créé le 2026-08-10 (synthèse de fin de session, contexte limite atteint). Mis à jour le 2026-08-13 : tentative d'export SIDES (QTI), test réel sur UNESS, puis **abandon et suppression complète** de cette fonctionnalité une fois la limitation de plateforme confirmée (voir « Piste abandonnée » ci-dessous — à ne pas retenter sans nouvelle information). Mis à jour le 2026-08-14 : boutons Précédent/Suivant déplacés dans la barre latérale, ajout du flag en Partie 1 (+ restitution Partie 2/3), et correction d'un bug réel de perte/corruption de données en Comparaison (voir sections dédiées ci-dessous). À relire en début de prochaine session pour reprendre le fil sans tout redécouvrir.

## Contexte et objectif

App Shiny (R) permettant à l'enseignant de **valider manuellement** un sujet de lecture critique d'article (LCA) généré par le skill Claude `lca-create`, avant diffusion aux étudiants :

1. **Jeu en aveugle** — répondre aux questions comme un étudiant, sans voir le corrigé initial.
2. **Comparaison** — arbitrer proposition par proposition (initial / ma réponse / manuel), éditer texte/justification/énoncé.
3. **Vue d'ensemble** — diff visuel entre le corrigé initial d'origine et l'état final arbitré, score d'accord initial/humain (kappa de Cohen).
4. **Export** — JSON consolidé, docx (sujet vierge + corrigé), pptx (diapos de correction). Pas d'export vers SIDES/UNESS (piste explorée puis abandonnée, voir plus bas).

Pas de dépôt git sur ce projet (`git init` jamais fait — à faire si l'utilisateur le souhaite un jour).

## Architecture

```
app.R                       # page_navbar 5 onglets, construit rv (reactiveValues central)
R/
  mod_import.R              # import JSON + panneau de reprise de session (.rds)
  mod_jeu_aveugle.R         # Partie 1
  mod_comparaison.R         # Partie 2
  mod_diff_overview.R       # Partie 3
  mod_export.R              # Partie 4 — 4 tuiles : JSON / Sujet / Corrigé / Diapos
  mod_question_nav.R        # sélecteur de question partagé (grille 3 colonnes, cliquable)
  utils_json.R              # parse + validation (jsonvalidate) + reconstruction JSON
  utils_diff.R              # diff ligne à ligne, kappa de Cohen
  utils_state.R             # init_rv(), chargement d'un sujet dans rv
  utils_persistence.R       # sauvegarde/reprise/suppression de session .rds
  utils_export.R            # rendu Quarto (docx/pptx) + post-traitement pptx (repositionnement titre)
quarto/
  corrige_template.qmd            # docx (vision étudiant/complet)
  diapos_correction_template.qmd  # pptx (2 diapos/question)
  _export_helpers.R               # helpers R communs aux deux templates (raw OOXML)
www/diff.css                # styles diff + grille de sélection de questions
schema/schema-sujet-lca.json # copie du schéma officiel (validation stricte à l'import)
saves/                       # sessions .rds sauvegardées (contient déjà une vraie session utilisateur :
                              # rolland-2026-dry-january-france.rds — NE PAS SUPPRIMER)
data/exemple_sujet.json      # fixture de test (sujet Roland 2025 méningiome, 14 questions, avec notions ; conforme au schéma ; remplace l'ancienne fixture à l'ancien format le 2026-11-02)
```

## Décisions clés (et pourquoi)

- **R Shiny + bslib**, pas Python : cohérent avec l'environnement (session R active, skill shiny-bslib).
- **Persistance en `.rds`** (pas de base de données) : autosave à chaque navigation entre questions + à l'import ; panneau de reprise dans l'onglet Import (déplacé depuis Partie 1 à la demande de l'utilisateur) listant les sessions disponibles avec titre + progression, bouton Supprimer avec confirmation modale.
- **Modèle de données** : `rv$propositions_json` (référence initiale immuable) vs `rv$final` (état arbitré, éditable) vs `rv$human_answers` (réponses de l'utilisateur en aveugle) vs `rv$questions_final` (énoncés éditables, séparés de `rv$questions_meta` original) — permet de diffèrer systématiquement original/modifié pour l'export et la Vue d'ensemble.
- **Sélecteur de question unique et partagé** (`mod_question_nav.R`) entre les 3 parties : grille de tuiles cliquables (3 par ligne, largeur de la barre latérale inchangée), couleur verte = répondu/modifié, contour bleu = question courante, plus boutons Précédent/Suivant **intégrés sous la grille dans la barre latérale** (2026-08-14 : auparavant à côté des actions de chaque partie, dans le contenu principal — déplacé à la demande de l'utilisateur).
- **Partie 2 (Comparaison)** : statut Initial/Humain/Manuel choisi via tuiles cliquables (commit immédiat dans `rv$final`, pas d'attente de la navigation — plus robuste que l'ancien système basé sur un input radio à resynchroniser). Propositions sur 3 colonnes, présentation compacte (juste "A)" + mini-cases colorées Initial/Vous, pas de texte verbeux). Note terminologie (2026-08-13) : "IA" renommé en "Initial" dans toute l'UI — la première version d'un sujet n'est pas nécessairement générée par IA (échanges de fichiers entre humains aussi). La clé interne `statut_source == "ia"` (donnée persistée dans les .rds existants) n'a volontairement pas été renommée, pour ne pas casser la lecture des sessions déjà sauvegardées.
- **Score d'accord initial/humain** : tableau de contingence 2×2 (initial correct/incorrect × coché/non coché) + pourcentage d'accord + kappa de Cohen, affiché en un seul bloc compact en Vue d'ensemble.
- **Flags de proposition** (2026-08-14) : `rv$flags` (numero, lettre, commentaire) permet, en Partie 1 uniquement, de cocher une case de signalement à gauche de chaque case à cocher de réponse (juste un booléen — pas de texte à saisir ; stocké comme une chaîne fixe non vide côté données pour rester compatible avec la détection existante). Restitué en Partie 2 par un badge "Flag" (point rouge) en haut à droite de la card de la proposition concernée, et en Partie 3 par un badge similaire sur la proposition **si son flag est encore actif ET que la question n'a reçu aucun arbitrage depuis** (`compute_question_diff()$propositions$flag_non_resolu`), plus un compteur global "Flags non résolus" dans les indicateurs de Vue d'ensemble. Un flag est considéré traité dès que la question a été modifiée en Comparaison, quelle que soit la proposition réellement touchée.

- **Référentiel de notions (2026-11-02)** : le skill `lca-create` rattache désormais chaque proposition pertinente à une ou plusieurs notions du référentiel Boussole LCA (`propositions[].notions[]` = `id`, `numero`, `role` evaluee/associee), plus `sujet.referentiel_notions` (version/commit), `sujet.parametres.notions_ciblees` et un bloc racine `fiche_article`. `schema/schema-sujet-lca.json` est maintenant la copie du nouveau schéma du skill (**les JSON à l'ancien format sont rejetés** : `referentiel_notions` obligatoire + justifications devant commencer par une citation « …, « Absence de mention » ou « Sans objet » ; les sessions .rds déjà sauvegardées ne sont pas revalidées et restent reprenables). `schema/boussole_lca_notions.json` est la copie du référentiel (143 notions) : il ne sert qu'à retrouver le libellé à partir de l'`id` (stable) ; le `numero` affiché est celui du sujet. À recopier depuis `~/.claude/skills/lca-create/assets/` quand le skill évolue. Modèle : `rv$notions_json` (référence initiale immuable, table longue question/lettre/id/numero/role, issue de `notions_depuis_raw()`) vs `rv$notions_final` (état arbitré), hors de `rv$propositions_json`/`rv$final` ; `rv$referentiel` (copie du référentiel) est posé dans `app.R`. Les deux tables sont persistées dans le .rds (une session antérieure est reconstruite depuis `raw_json`). **Partie 2** : sous chaque justification, pastilles de notions (clic sur le libellé = bascule évaluée/associée, × = retirer) + liste déroulante des 143 notions groupées par domaine ; une notion ajoutée est « évaluée » si la proposition n'en a pas encore, sinon « associée » ; les actions remontent par un unique input événementiel `notion_action` (« lettre|ajouter|retirer|basculer|id »), appliqué par `modifier_notions()` (fonction pure, `utils_notions.R`) avec commit et sauvegarde immédiats (comme les tuiles de statut ; ne touche ni aux zones de texte ni à `rv$final`). Un avertissement signale les écarts entre numéros cités entre parenthèses dans la justification et notions rattachées. **Partie 3** : la répartition (carte « Répartition des notions théoriques », histogramme HTML/CSS empilé évaluée/associée, ★ = ciblée) suit `rv$notions_final` ; un changement de notions compte comme modification de la proposition (compteurs, tuile verte, détail « Notions initiales/modifiées »). **Export JSON complet** : réécrit `notions` depuis l'état arbitré (tableau vide si tout a été retiré d'une proposition qui en avait ; champ absent sinon) et injecte `sujet.referentiel_notions` si un sujet à l'ancien format reçoit des notions.
- **Limite connue (antérieure au nouveau format)** : le JSON « complet » exporté contient un bloc racine `validation` que le schéma (`additionalProperties: false`) refuse — il n'est donc pas réimportable tel quel malgré la mention « réimportable » ; à traiter si le besoin apparaît.

## Bugs réels trouvés et corrigés en cours de route (pas juste des ajustements de goût)

- **`checkboxGroupInput` limité à 300px** (CSS par défaut de Shiny) → `width = "100%"` explicite.
- **Perte de réponse en changeant d'onglet via la barre de nav** (au lieu des boutons Précédent/Suivant internes) → hook `observeEvent(input$main_nav)` au niveau app.R qui force la sauvegarde de l'onglet quitté.
- **pptx corrompu après repositionnement du titre** : le post-traitement XML (dézippage/édition/rezippage) utilisait `list.files()` sans `all.files = TRUE`, qui ignore silencieusement `_rels/.rels` (fichier commençant par un point) — pourtant le fichier racine indispensable à l'ouverture du paquet OPC. PowerPoint proposait une réparation puis n'affichait rien.
- **Cache navigateur sur `diff.css`** : les modifications de style ne s'appliquaient pas visuellement malgré un fichier à jour sur disque → ajout d'un paramètre de version basé sur `mtime` dans l'URL du lien CSS (`app.R`).
- **Régression QTI** : l'ajout de `<assessmentTest>` autour de `<testPart>` (tentative d'amélioration standards-based) a cassé l'import SIDES ("Error parsing pool.xml no testPart found") — revenu à `<testPart>` en racine, qui est ce que leur parseur attend réellement. Leçon : sans accès à la doc SIDES, ne pas sur-corriger au-delà de ce que l'outil de référence fournissait déjà.
- **(2026-08-14) Perte de modifications non enregistrées en cliquant sur une tuile Initial/Vous/Modifier (Comparaison)** : le clic ne committait que le statut de la lettre cliquée dans `rv$final`, jamais les autres champs (texte/justification/case "Correcte" manuelle) actuellement affichés mais pas encore sauvegardés (la sauvegarde n'a lieu qu'à la navigation). Le rechargement réactif des `textAreaInput` (déclenché par ce même changement de `rv$final`) écrasait alors ces champs avec leur dernière valeur *persistée*, donnant l'impression que "la question se reset". Corrigé en appelant `save_current(idx)` en tout premier dans le handler `choix_statut`, avant d'appliquer le changement de statut lui-même.
- **(2026-08-14) Race condition grave : contenu d'une question écrasé par celui d'une autre en Comparaison**, reproduite en direct pendant cette session et confirmée comme la cause du bug utilisateur "la question 2 a été remplacée par la question 3". Mécanisme : `updateTextAreaInput()` est asynchrone (message envoyé au navigateur, pas de garantie de prise en compte immédiate côté serveur). Juste après une navigation interne (Suivant/Précédent), `input$texte_A` etc. contiennent encore, l'instant suivant côté serveur, le texte de la question **quittée**. Si l'utilisateur change alors très vite d'onglet dans la barre de navigation principale, le hook `save_now()` d'app.R lit ces valeurs périmées et les enregistre sous le numéro de la **nouvelle** question — corruption croisée, persistée par l'autosave. Corrigé par un drapeau `chargement_recent` (dans `mod_comparaison.R` et `mod_jeu_aveugle.R`) : passe à `TRUE` juste après un chargement de nouvelle question, repasse à `FALSE` dès qu'un écho d'un champ concerné est reçu ; `save_now()` est un no-op tant qu'il est `TRUE` (le save de la question quittée a de toute façon déjà été fait, de façon fiable, par la navigation elle-même). **Risque résiduel accepté** : un clic manuel sur une tuile de statut ou sur "Vérifier mes réponses" survenant dans cette même fenêtre de course (quelques dizaines de ms) n'est pas protégé — jugé trop rare pour justifier la complexité additionnelle.
- **Incident de données réel (2026-08-14)** : en reproduisant le bug ci-dessus pour le diagnostiquer, la vraie session sauvegardée de l'utilisateur (`lca-portela-2026-azithromycine-roflumilast-bpco.rds`, sujet romi/azithro) a été involontairement utilisée comme terrain de test au lieu du fixture `data/exemple_sujet.json`, et corrompue (texte de test inséré, contamination croisée Q1/Q2, un flag résiduel). Réparée manuellement (Q1/Q2 remises à l'état Initial, flags nettoyés) ; le JSON source et toutes les autres questions étaient intacts. **Leçon pour la suite : ne jamais tester via le bouton "Reprendre" sur une vraie session utilisateur — toujours passer par une session jetable construite à partir du fixture.**

## Piste abandonnée — Export SIDES (QTI), historique complet

**Fonctionnalité explorée puis intégralement retirée du code le 2026-08-13, à la demande explicite de l'utilisateur, une fois la conclusion ci-dessous établie. Ne pas la réimplémenter sans nouvelle information changeant la conclusion.**

Chronologie de l'investigation (utile si le sujet ressort un jour) :

1. Un premier export QTI 2.1 (`pool.xml` + `QUESTION_N.xml` en zip) avait été construit en reproduisant un convertisseur Word→QTI existant de l'utilisateur (cf. `ressources/texte-vers-qti.zip`), avec ajout de justifications en `modalFeedback`. Résultat observé lors d'un test antérieur : SIDES importait bien le contenu, mais reconnaissait le résultat comme "SQI" (séquence de questions isolées) + questions individuelles, jamais comme "LCA" groupée.
2. Un export alternatif au format **Moodle XML** avait aussi été tenté (reproduisant la structure d'un vrai export LCA natif de SIDES fourni par l'utilisateur : `type="description"` + `sidesId` partagé entre toutes les questions du groupe). Mais ce fichier de référence, même généré nativement par SIDES, s'est révélé **non ré-importable tel quel** une fois testé sur UNESS.
3. À la demande de l'utilisateur (session du 2026-08-13), simplification en une tuile unique "Export SIDES" reprenant le format QTI (seule base dont on savait qu'elle s'importait effectivement — la page d'import `studio/testElement/import-qti`, capturée en HTML dans `ressources/`, accepte `.zip` ET `.xml` mais seul le `.zip` fonctionne en pratique).
4. **Premier test réel sur UNESS** : rejet par une erreur Zod exploitable côté serveur — `assessmentItem.modalFeedback` doit être un tableau d'objets dont `identifier` ET `outcomeIdentifier` valent l'une de deux constantes fixes et closes : `"COMMENT_QUESTION"` ou `"QUESTION_CORRECTION_HELPER"` (pas des identifiants arbitraires pilotés par `responseProcessing` comme en QTI standard). Corrigé en conséquence (deux blocs `modalFeedback` toujours fournis ensemble, contenu = `commentaire_pedagogique` du sujet + détail VRAI/FAUX par proposition).
5. **Second test réel sur UNESS, avec la correction** : l'import réussit cette fois pleinement — contenu, commentaires pédagogiques et justifications s'affichent correctement dans l'aperçu Studio. Mais le "Type d'élément d'épreuve" reste bloqué sur **"Séquence de questions isolées (SQI)"**, non modifiable, alors que "Lecture critique d'article (LCA)" existe bien dans la liste déroulante du même champ.
6. **Conclusion établie par l'utilisateur, qui connaît déjà l'interface Studio** (deux faits confirmés indépendamment, pas des hypothèses) :
   - Le type d'un élément d'épreuve se choisit **à la création** (un élément vide créé manuellement dans Studio, hors import, permet bien de sélectionner LCA) — l'import QTI générique (`import-qti`) crée systématiquement un élément de type SQI, quel que soit le contenu du fichier envoyé. **Il n'existe donc aucune structure QTI capable de produire du LCA par cette voie.**
   - Un élément d'épreuve de type LCA, une fois créé, **ne propose aucun mécanisme d'import** en son sein (pas de bouton d'import QTI ou autre dans son onglet Contenu) — tout doit être ressaisi à la main, question par question. **Il n'existe donc aucun raccourci d'automatisation possible côté fichier pour peupler un LCA.**
7. Une piste de contournement (piloter le navigateur pour remplir directement le formulaire Studio à partir du JSON, une fois l'utilisateur connecté) a été évoquée puis explicitement écartée par l'utilisateur ("c'était pour savoir").
8. **Décision finale** : suppression complète de `utils_export_sides.R` (et des versions antérieures `utils_qti.R`, `utils_moodle_xml.R`) et de la tuile "Export SIDES" dans `mod_export.R`. L'app ne propose plus d'export vers SIDES/UNESS ; les enseignants doivent recréer l'épreuve LCA à la main dans Studio (les exports JSON/docx/pptx existants servent de référence pour cette saisie manuelle).

## Autres pistes d'amélioration évoquées (non implémentées, juste discutées)

- Déploiement (Posit Connect / shinyapps.io) si besoin d'un accès hors RStudio.
- Tests automatisés (`shinytest2`) du parcours complet.
- Diff plus fin (mot-à-mot) en Vue d'ensemble — **tentative faite puis annulée** à la demande de l'utilisateur ("ne fonctionne pas"), revenu au diff ligne entière initial/modifié. Ne pas retenter sans nouvelle demande explicite.
- Templates docx/pptx personnalisés aux couleurs du CHU Rennes (actuellement gabarits génériques, décision assumée).

## Prérequis machine (déjà en place, à ne pas re-questionner)

- Quarto CLI installé (`C:\Program Files\Quarto\bin`) — nécessaire pour les exports docx/pptx.
- Package R `jsonvalidate` installé (pour la validation stricte du JSON à l'import).
- Package R `zip`, `xml2` utilisés pour la génération/l'édition d'archives.
