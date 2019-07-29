
<!-- README.md is generated from README.Rmd. Please edit that file -->
dimRactivite
============

L'objectif du package est de standardiser la production de tableaux de bord et de données d'activité en utilisant les données PMSI et le package pmeasyr.

Installation
------------

You can install dimRactivite from github with:

``` r
# install.packages("devtools")
devtools::install_github("24p11/dimRactivite")
```

Imports de données
------------------

Le package propose des fonctions permettant des systématiser les imports pmeasyr en utilisant le modèle de file system proposer par G.Pressiat. Il utilise le format de lecture irum type 6 et irsa type 1 pour générer un objet unique .RData qui dont le chargement sera accélérer et qui comprend: - rsa\_v - rum - rum\_v - diagnostics - actes

Dans la perspective du suivi de l'activité de plusieurs établissement

``` bash
+-- path
    +-- nofiness
        +-- AAAA
           +-- MM 
```

Example
-------

Dans la patie démos plusieurs exemples d'utilisations pour générer rapidement un ensemble de tableaux de bord.

``` r
## basic example code
```
