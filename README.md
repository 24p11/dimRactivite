
<!-- README.md is generated from README.Rmd. Please edit that file -->
dimRactivite
============

L'objectif du package est de standardiser la production de tableaux de bord et de données permettatn le suivi de l'activité en utilisant les données PMSI et le package pmeasyr. Il permet le suivi de l'activité sur un ensemble d'établissements et sur plusieurs années.

Installation
------------

Vous pouvez installer le package directement depuis github:
``` r
# install.packages("devtools")
devtools::install_github("24p11/dimRactivite")
```

#### Imports de données


Le package dimRactivite propose des fonctions permettant des systématiser les imports pmeasyr (seule la partie MCO a été développée à ce jour) afin de fusionner les données de différentes années et de différents établissements. Ces données pourront ensuite ensuite être anlysées pour réaliser le suivi de l'activité des établissements.

dimRactivite utilise le format de lecture pmeasyr irum type 6 et irsa type 1 pour générer un objet unique .RData par remontée et qui comprend les tableaux de données suivants : rsa - rsa\_v - rum - rum\_v - diagnostics - actes - vano . Ces objets sont ensuite importés dans R et concaténées pour former un enviroment de travail comprenant l'ensemble des données disponibles.

Comme préconisé par G.Pressiat dans la documentation du package pmeasyr, dimRactivite utilisera un dossier unique comprenant l'ensemble des fichiers zippés en entrée et sortie de GENRSA. Les fichiers contenus dans ce dossier seront anylisées, les fichiers .RData seront crées s'ils n'existent pas encore, puis les données seront automatiquement intégrées dans R (par défaut la remontée la plus récente est prise en compte).

Pour ce faire on utilise les standards de nommage des fichiers entrée et sortie de GENRSA contenus dans l'archive zippée et qui sont de la forme : FINESS.AAAA.MM.ext .

Enfin des informations complèmentaires non contenus dans les formats officiels peuvent être intégrées, en particulier des informations sur les structures permettant de générer des tableaux de bord compréhensibles. Pour les structures et les indentités patients, des formats xlsx sont utilisés. L'intégration de ces nouvelles variables est réalisée à la fin de la procédure d'import.


#### Génration de tableaux de bord


Une fois les données intégrées, des fonctions de générations automatiques de tableaux de bord sont disponibles. Pour l'instant, ces tableaux de bords sont produits sous forme de fichiers csv.

La procédure utilise les niveaux de structure tels qu'ils sont défnis dans les paramètres du fichier structure pour décliner les différents niveaux de calcul des indicateurs de suivi.

On distingue un permier ensemble de tableaux de bord permettant de suivre l'évolution anuelle d'un seul indicateurs sur l'ensmble des niveaux de structure, en prenant toujours le principe de distinguer hopsitalisation complète et hospitalisation partielle. On dispose de tableaux de suivi de l'activité, des recettes et du nombre de journées. Puis un autre ensemble de tableaux permettent de suivre l'évolution annuelle pour un niveau de structure (groupe hospitalier, établissement, pole, service,...) d'un ensemble d'indicateurs. Le package permet de le calcul d'environ 200 indicateurs répartis dans tableaux de bord thématiques (activité, DIM, médical).

Paramètres
------------------
L'ensemble des paramètres nécessaires au bon fonctionnement du package (chemin des fichiers, nombre d'année d'antériorité pour génération des tableaux de bord, variables du fichier structure,... ) sont décrit dans un fichier yaml.


Démos
------------------
Des scripts de démonstration sont inclus dans le package afin de montrer l'utilisation des différentes fonctions :
```
+ demos
  +--- import_pmeasyr.R
  +--- generation_tableaux_de_bord.R
```
