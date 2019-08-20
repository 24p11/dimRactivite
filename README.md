
<!-- README.md is generated from README.Rmd. Please edit that file -->
dimRactivite
============

L'objectif du package est de standardiser la production de tableaux de bord et de données d'activité en utilisant les données PMSI et le package pmeasyr.

Installation
------------

Vous pouvez installer le package directement depuis github:
``` r
# install.packages("devtools")
devtools::install_github("24p11/dimRactivite")
```

Imports de données
------------------

Le package propose des fonctions permettant des systématiser les imports pmeasyr et générer des tableaux de bord paramétrables à partir des objects R crées à partir de ces imports. Il utilise le format de lecture pmeasyr irum type 6 et irsa type 1 pour générer un objet unique .RData qui comprend: - rsa\_v - rum - rum\_v - diagnostics - actes .

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
