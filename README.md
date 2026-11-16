# lcavalidate — validation des sujets LCA

Application Shiny pour valider à la main un sujet de lecture critique d'article
(LCA) produit par le skill `lca-create`, avant diffusion aux étudiants :

1. **Jeu en aveugle** — répondre aux questions comme un étudiant, sans le corrigé.
2. **Comparaison** — arbitrer chaque proposition (version initiale, votre
   réponse, ou réécriture), modifier énoncés, justifications et notions,
   **ajouter ou supprimer une question**.
3. **Vue d'ensemble** — écarts entre le sujet initial et le sujet final, score
   d'accord (κ de Cohen), répartition des notions.
4. **Export** — JSON consolidé, sujet et corrigé Word, diapositives PowerPoint.

## Lancer l'application sur son poste

```r
if (!requireNamespace("lcavalidate", quietly = TRUE)) pak::pak("Ygall/lca-app-validate")
library(lcavalidate)
lancer_app()
```

`pak` s'installe par `install.packages("pak")` ;
`remotes::install_github("Ygall/lca-app-validate")` convient aussi. Pour mettre
à jour, relancer `pak::pak("Ygall/lca-app-validate")`.

Les exports Word et PowerPoint demandent [Quarto](https://quarto.org) (fourni
avec RStudio et Positron). Sans lui, l'application fonctionne et propose
l'export JSON et l'export de la session.

## Sessions de travail

L'application ne conserve rien sur la machine qui l'exécute. Une session
(sujet, réponses en aveugle, arbitrages, questions ajoutées ou supprimées) vit
à deux endroits, tous deux chez l'utilisateur :

- **dans le navigateur** : elle y est enregistrée automatiquement à chaque
  modification, et l'accueil propose de la reprendre à la visite suivante, sur
  le même navigateur du même appareil ;
- **dans un fichier de session** (`.json`), obtenu par le bouton « Exporter la
  session » : c'est la sauvegarde durable, celle qui permet de reprendre plus
  tard, sur un autre appareil, ou de transmettre son travail.

À l'accueil, on démarre une session vierge en important un sujet, ou on
reprend une session en déposant son fichier. « Fermer la session » l'efface du
navigateur : sur un poste partagé, exporter puis fermer.

Les sauvegardes `.rds` des versions antérieures (dossier `saves/`) se
convertissent en fichier de session :

```r
lcavalidate::convertir_session_rds("saves/mon-sujet.rds")
```

## Déployer sur un serveur

`app.R`, à la racine du dépôt, lance l'application à partir des sources ; il
sert aux déploiements sur Posit Connect, Posit Connect Cloud, shinyapps.io ou
Shiny Server. L'application n'écrit rien sur le serveur et n'a besoin d'aucun
stockage persistant ni d'aucun contrôle d'accès : chaque visiteur travaille sur
sa propre session, dans son navigateur.

Depuis R, avec un compte déjà enregistré par `rsconnect::connectUser()` ou
`rsconnect::setAccountInfo()` :

```r
rsconnect::deployApp(appDir = ".", appName = "lca-validate")
```

Depuis le dépôt GitHub (Posit Connect Cloud, ou publication « git-backed » de
Posit Connect), le serveur lit `manifest.json`. Il est à régénérer après tout
changement de fichiers ou de dépendances :

```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

`.rscignore` tient hors du déploiement ce qui ne sert pas à l'application
(`ressources/`, `saves/`, tests). Quarto doit être présent sur le serveur pour
les exports Word et PowerPoint (à vérifier selon l'hébergeur) ; à défaut, ces
exports sont désactivés et l'application le signale.

## Développement

```r
pkgload::load_all()   # charger le package depuis les sources
lancer_app()
testthat::test_local()
```

Le schéma du sujet et le référentiel de notions (`inst/schema/`) sont des
copies de ceux du skill `lca-create`, à recopier quand le skill évolue.
`SUIVI.md` tient l'historique des décisions.
