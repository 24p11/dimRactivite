
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

Imports de données
------------------

Le package dimRactivite propose des fonctions permettant des systématiser les imports pmeasyr (seule la partie MCO a été développée à ce jour) afin de fusionner les données de différentes années et de différents établissements. Ces données pourront ensuite ensuite être anlysées pour réaliser le suivi de l'activité des établissements.

dimRactivite utilise le format de lecture pmeasyr irum type 6 et irsa type 1 pour générer un objet unique .RData par remontée et qui comprend les tableaux de données suivants : rsa - rsa\_v - rum - rum\_v - diagnostics - actes - vano . Ces objets sont ensuite importés dans R et concaténées pour former un enviroment de travail comprenant l'ensemble des données disponibles.

Comme préconisé par G.Pressiat dans la documentation du package pmeasyr, dimRactivite utilisera un dossier unique comprenant l'ensemble des fichiers zippés en entrée et sortie de GENRSA. Les fichiers contenus dans ce dossier seront anylisées, les fichiers .RData seront crées s'ils n'existent pas encore, puis les données seront automatiquement intégrées dans R (par défaut la remontée la plus récente est prise en compte).

Pour ce faire on utilise les standards de nommage des fichiers entrée et sortie de GENRSA qui sont de la forme : FINESS.AAAA.MM.ext

Génration de tableaux de bord
------------------

Une fois les données intégrées, des fonctions de générations automatiques de tableaux de bord sont disponibles. Pour l'instant, ces tableaux de bords sont produits sous forme de fichiers csv.

Paramètres
------------------
L'ensemble des paramètres nécessaires au bon fonctionnement du package (chemin des fichiers, nombre d'année d'antériorité pour génération des tableaux de bord,... ) sont décrit dans un fichier yaml.


Démos
------------------
Des scripts de démonstration sont inclus dans le package afin de montrer l'utilisation des différentes fonctions :
```
+ demos
  ---- import_pmeasyr.R
  ---- generation_tableaux_de_bord.R
```
