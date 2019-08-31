
<!-- README.md is generated from README.Rmd. Please edit that file -->
dimRactivite
============

L'objectif du package est de standardiser l'import dans R des données PMSI avec le package pmeasyr permettant de créer un environement de travail synthétique comprenant plusieurs années et plusieurs établissements. Une fois les données intégrées, des fonctions de générations automatiques de tableaux de bord sont disponibles permettant de suivre de l'activté des établissements.

Des informations complèmentaires non contenues dans les formats officiels peuvent être intégrées, en particulier des informations sur les structures, permettant de générer des tableaux de bord compréhensibles et de décliner le calcul d'indicateurs sur les différents niveaux d'analyse d'un groupe hospitalier.

#### Installation

Vous pouvez installer le package directement depuis github:
``` r
# install.packages("devtools")
devtools::install_github("24p11/dimRactivite")
```


#### Démos

Des scripts de démonstration sont inclus dans le package afin de montrer l'utilisation des différentes fonctions :
```
+ demos
  +--- import_pmeasyr.R
  +--- generation_tableaux_de_bord.R
```

Un scénario complet d'import et de génération peut être trouver dans la vignette [Génération de tableaux de bord PMSI avec pmeasyr](https://24p11.github.io/dimRactivite/articles/generation-tableaux-de-bord-pmsi.html) .

Organisation du file system
---------------------------
Comme préconisé par G.Pressiat dans la documentation du package pmeasyr [les archives PMSI](https://guillaumepressiat.github.io/pmeasyr/archives.html), dimRactivite utilisera un dossier unique comprenant l'ensemble des fichiers zippés en entrée et en sortie de GENRSA. Les fichiers contenus dans ce dossier seront anylisés, les fichiers .RData seront crées s'ils n'existent pas encore, puis les données seront automatiquement intégrées dans R (par défaut la remontée la plus récente est prise en compte).

L'adresse de ce dossier est renseigné dans l'option du fichier de configuration ``` path ```  .

Import des données PMSI
-----------------------

Cette procédure, qui procède par plusieurs étapes, utilise les fichiers GENRSA avec les fonction d'imports pmeasyr (cf [doc irsa](https://guillaumepressiat.github.io/pmeasyr/import-des-donnees.html#rsa)):

- in
    * rss (pmeasyr::irum type 4, transposition des diagnostics, intégration du tra avec pmeasyr::inner_tra)
- out
    * rsa (pmeasyr::irsa type 3, intégration du tra avec pmeasyr::inner_tra)
    * tra
    * ano (sauvegardé sous la forme vano avec données de valorisation)
    * ium
    * diap
    * porg
    * pie

La procédure réalise ensuite des opérations de transformation et de valorisation : 
 - intégration du tra (cf inner_tra)
 - ajout au rum des variables de fichier ium et de typologies des autorisations, 
 - valorisation des rsa ,
 - valorisation des rum (cf fonction vvr_rum_repa)
 - ajout des données de facturation à l'ano (vvr_ano_mco)

Ces fonctions font appel aux données de référentiels suivantes (disponibles dans le package ***referentiels***) :
 - nomenclature_uma
 - tarifs_mco_ghs
 - tarifs_mco_supplements 



Génération des tableaux de bord
-----------------------

NB : les exemples donnés ici ne sont accéssibles que sur l'intranet de l'APHP.

Ces tableaux permettent de suivre précisément les données d'activité pour chaque niveau de la structure du groupe ou de l'établissement. Il reposent donc beaucoup sur le fichier stucture qui est intégré à la fin des imports et qui permet de décrire selon plusieurs niveaux de regroupements les uma.

On distingue un permier ensemble de tableaux de bord permettant de suivre l'évolution anuelle d'un seul indicateurs sur l'ensemble des niveaux de structure, en prenant toujours le principe de distinguer hopsitalisation complète et hospitalisation partielle. On dispose de tableaux de suivi de l'activité (ex : [intranet aphp](http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordGeneral&_mois=07&_annee=2019&_type=mens) ), des recettes avec une répartition par service (ex : [intranet aphp]http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordValorisation&_mois=07&_annee=2019&_type=mens).

Un autre ensemble de tableaux permet de suivre l'évolution annuelle pour chaque niveau (groupe hospitalier, établissement, pole, service,...) d'un ensemble d'indicateurs. Le package permet de le calcul d'environ 200 indicateurs répartis dans tableaux de bord thématiques (activité, DIM, médical) (ex [intranet aphp](http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordActivite&_service=Lariboisiere&_mois=07&_annee=2019&_type=mens))


Paramètres
------------------
L'ensemble des paramètres nécessaires au bon fonctionnement du package (chemin des fichiers, nombre d'année d'antériorité pour génération des tableaux de bord, variables du fichier structure,... ) est décrit dans un fichier yaml (un exemple de fichier se trouve dans le dossier demos)

#### Accès aux options
L'ensemble des paramètres seront chargés lors de l'appel du package. Il est possible de recharger ces paramètres après modification :
```r
upate_options()
```

Les options du package sont accessibles une fois le package installé par la fonction :
```r
getOption("dimRactivite.option_name")
```

où option_name est le nom de l'option.

