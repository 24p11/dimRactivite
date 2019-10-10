
dimRactivite
============

L'objectif du package est de standardiser avec pmeasyr l'import dans R des données PMSI pour créer un environement de travail synthétique comprenant plusieurs années et plusieurs établissements. Une fois les données intégrées, des fonctions de générations automatiques de tableaux de bord sont disponibles permettant de suivre l'activté des établissements.

Les fonctions d'imports et d'analyse sont construites sur le principe que chaque établissement produit tous les mois des fichiers de facturation mis en entrée du logiciel ATIH GENRSA utilisé pour transmettre les données PMSI aux tutelles (remontées mensuelles).  L’analyse de ces fichiers par GENRSA produit 2 types d’archives (.zip) le in (les fichiers d’entrée) et le out (fichiers préparés pour être envoyés). Le package généralise l’import des fichiers zippés en sortie de GENRSA (in et out) dans un environnement de travail R. Au cours de cet import les RSA et les RUM sont valorisés permettant l'analyse de recettes T2A séjours des établissements.

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

Un scénario complet d'import et de génération peut être trouvé dans la vignette [Génération de tableaux de bord PMSI avec pmeasyr](https://24p11.github.io/dimRactivite/articles/generation-tableaux-de-bord-pmsi.html) .

Organisation du file system
---------------------------
Comme préconisé par G.Pressiat dans la documentation du package pmeasyr [les archives PMSI](https://guillaumepressiat.github.io/pmeasyr/archives.html), dimRactivite utilisera un dossier unique comprenant l'ensemble des fichiers zippés en entrée et en sortie de GENRSA pour le MCO. Les fichiers contenus dans ce dossier seront analysés, un fichier .RData par remontée et par établissement sera crée s'il n'existe pas encore, puis les données seront automatiquement chargées dans R à partir des fichiers .RData (par défaut la remontée la plus récente est prise en compte).

L'adresse de ce dossier est renseigné dans l'option du fichier de configuration ```path```  .

Import des données PMSI
-----------------------

Cette procédure, qui procède par plusieurs étapes, utilise les fichiers GENRSA avec les fonctions d'imports pmeasyr (cf [doc irsa](https://guillaumepressiat.github.io/pmeasyr/import-des-donnees.html#rsa)):

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
 - intégration du tra (cf [```inner_tra```](https://im-aphp.github.io/pmeasyr/reference/inner_tra.html))
 - ajout au rum des variables de fichier ium et de typologies des autorisations, 
 - valorisation des rsa ,
 - valorisation des rum (cf fonction [```vvr_rum_repa```](https://24p11.github.io/dimRactivite/reference/vvr_rum_repa.html))
 - ajout des données de facturation à l'ano (vvr_ano_mco)

Ces fonctions font appel aux données de référentiels suivantes (disponibles dans les packages ***nomensland*** et ***referentiels***) ou dans les tableaux de données de dimRactive:
 - ```nomenclature_uma``` (dimRactivite)
 - ```tarifs_mco_ghs``` (nomensland)
 - ```tarifs_mco_supplements``` (nomensland)

En complément, on calcule également les différents éléments de valorisation avec les tarifs de l'année antérieure peremttant de suivre l'impact du changement de version de tarifs sur la valorisation.

Au moment de l'import des données dans l'environement de travail (cf fonction ```load_RData()```) on peut également importer les données de l'année n-1 produite à mois de la remontée (version non consolidées des données à n-1).

Pour plus d'information sur les données importées et les variables calculées vous pouvez vous reporter à la partie [Méthodes](https://24p11.github.io/dimRactivite/articles/methodes.html) de la documentation du package

A la suite de ces imports des informations complémentaires peuvent être nécessaires pour analyser les données et en particulier des informations descriptives des UMA permettant de les nommer et de les rattacher aux structures de l'établissement. Nous proposons l'utilisation d'un fichier xlsx comprenant comprenant les nom de colonnes suivants :
 - nofiness : numéro nofiness
 - hopital : nom de l'hopital
 - pole : nom du pôle
 - service : nom du service
 - libelle : om de l'UMA
 - cdurm : numéro de l'UMA (= nom de la variable UMA dans pmeasyr) 

D'autres niveaux de regroupement peuvent être ajoutés qui pourront être utilisés avec les fonctions du package.
 

Génération des tableaux de bord
-----------------------

NB : les lien donnés ici ne sont accéssibles que sur l'intranet de l'APHP. Des exemples sous form de fichier xls sont fournis dans les dossiers ```demos``` et ```vignettes``` .

Ces tableaux permettent de suivre précisément les données d'activité pour chaque niveau de la structure du groupe ou de l'établissement. Il reposent donc beaucoup sur le fichier stucture qui est intégré à la fin des imports et qui permet de décrire selon plusieurs niveaux de regroupements les uma.

On distingue un permier ensemble de tableaux de bord permettant de suivre l'évolution anuelle d'un seul indicateur sur l'ensemble des niveaux de structure, en prenant toujours le principe de distinguer hopsitalisation complète et hospitalisation partielle. On dispose de tableaux de suivi de l'activité (ex : [Tableau De Bord General](http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordGeneral&_mois=07&_annee=2019&_type=mens) ), des recettes avec une répartition par service (ex : [Tableau De Bord Valorisation](http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordValorisation&_mois=07&_annee=2019&_type=mens)).

Un autre ensemble de tableaux permet de suivre l'évolution annuelle pour chaque niveau (groupe hospitalier, établissement, pole, service,...) d'un ensemble d'indicateurs. Le package permet de le calcul d'environ 200 indicateurs répartis dans tableaux de bord thématiques (activité, DIM, médical) (ex [Tableau De Bord Activite](http://msi.sls.aphp.fr/tdb/index.php?_tbl=TableauDeBordActivite&_service=Lariboisiere&_mois=07&_annee=2019&_type=mens)). 

Ces tableaux sont donc produits à partir des données PMSI importées et des informations sur les structures. Pour les tableaux d'activité on utilise un fichier de paramètres afin  de lister précisément pour chaque niveau de structure et pour chaque tableau thématique l'ensemble des indicateurs qui devront être calculés. Ce processus permet de personnaliser les tableaux de bord en fonction du type d'activité de chaque service ou pôle. Vous trouverez plus de détail sur cette procédure dans le paragraphe [Paramètres](#détail-sur-la-valorisation-des-RUM).

Des exemples de tableaux de bord sont fournis sous forme de fichiers xls sans aucun formatage, ils ont été produits par les scripts de démonstration (```demos```, ```vignettes```) avec des données tirées au sort, et des structures fictives. Ces fichiers peuvent soit être mis en forme avec dans des templates xlsx, intégrés par un site web, ou utilisé par une application shiny.

Paramètres
------------------
L'ensemble des paramètres nécessaires au bon fonctionnement du package (chemin des fichiers, nombre d'années d'antériorité pour génération des tableaux de bord, variables du fichier structure,... ) est décrit dans un fichier yaml (un exemple de fichier se trouve dans le dossier demos)

#### Accès aux options
Le package utilise les fonctionnalités offertes par R pour stocker des variables permettant d’exécuter les différentes fonctions (cf documentation R [option settings](https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html)).

On utilisera ici un fichier au yaml qui sera interprété par la fonction ```update_option``` pour mettre à jour les options locales.

```r
upate_options("path/to/file")
```

Les options du package sont accessibles une fois le package installé par la fonction :
```r
getOption("dimRactivite.option_name")
```

où option_name est le nom de l'option.

#### Liste des options


- ```path``` = chemin des vers le dossier contenant les archives PMSI
- ```path_tdb_files``` = chemin vers un dossier permettant d'enregistrer les tableaux de bord

- ```extensions``` = extension des fichiers qui seront reconnus par la fonction ```scan_path```.
  * Valeurs par défaut :  rum,  rss,  rsa, rha, ano, tra, ium, leg, med, medatu, pie, porg, diap

- ```fichiers_imco```: liste des types de fichiers nécessaires pour importer une archive
  * valeurs par défaut :
     - ```in``` : rss
     - ```out```: rsa, tra, ano, ium, diap, porg, pie

- ```services_exclus``` = liste des services présents dans le fichier structure que l'on souhaite extraire de l'analyse pour les tableaux de bord (fonction ```get_data```)

- ```fichier_structure``` = chemin vers le fichier structure (au format xlsx). Le fichier doit contenir au moins une colonne cdurm (qui correspond au numéro d'UMA dans la nommenclature ```pmeasyr```) qui permettra la jointure avec la table rum. Pour utiliser les tableaux de bord globaux, il sera nécessaire d'avoir également une colonne ```hopital```, ```pole``` et ```service```.

- ```gestion_doublons_rum ``` = règle de gestion des doublons pour compter les séjours dans les indicateurs d'activité
   * valeurs par défaut
      - service: service
      - pole: service
      - Centre de Traitement des Brûlés: pole

- ```profondeur_tdb``` = profondeur en année par défaut des tableaux de bord.
   * valeur par défaut = 5

#### Fichier indicateurs

Afin de générer les tableaux de bords de suivi d'indicateurs un fichier de paramètrage est utilisé. Le principe est le suivant : 
- la liste de l'ensemble des indicateurs disponibles se trouve en ligne (chaque indicateur est indentifié par une nom unique abrégé), cf [liste des indicateurs](https://24p11.github.io/dimRactivite/articles/indicateurs.html) 
- les différents niveaux de structure en colonne.
- la lettre ```o``` est ajoutée à l'intersection pour identifier le fait que tel indicateur doit être calculé

Le fichier comprend également une colonne supplémentaire pour décrire les tableaux de bord auxquels appartiennent chaque indicateur. Un indicateur pouvant appartenir à plusieurs tableaux de bords, il est possible de renseigner plusieurs nom de tableaux de bord séparé par des virgules ```,```.

Enfin d'autres variables sont également utilisées.

Au final il comprend les colonnes suivantes:
- ```niv``` : variable numérique de 1 à 4 utilisée pour la mise en forme 
- ```libelle``` : libellé de l'indicateur;
- ```var``` : indentifiant unique de l'indicateur (il n'est pas possible que 2 indicateurs aient le même identifiant);
- ```variable``` : variables nécessaires au calcul de l'indicateur;
- ```dataframe```: sources de données nécessaire (dataframe dans lesquels se trouve les variables nécessaires), donné à titre indicatif;
- ```referentiel``` : référentiels nécessaires au calcul de l'indicateur, donné à titre indicatif;
- ```	tdb ``` : liste des tableaux de bord auxquel l'indicateur appartient séparé par des ``` , ``` ;
- les noms des colonnes suivantes sont liées au fichier strucutre et correspondent aux différents niveaux de l'établissement ou du groupe pour lequel des tableaux de suvis sont nécessaires. On peut également ici ajouter de façon indépendante du fichier structure des nom de tableaux de bord que l'on souhaite calculer. Dans le fichier donné à titre d'exemple un niveau GH a été ajouté qui n'est pas décrit dans le fichier structure.

Ce fichier est utilisé pour la génération des tableaux de bord par la fonction ```get_indicateurs()```qui prend comme argument le type du tableau de bord et le nom de la structure auquel il s'applique et qui renvoie la liste des indicateurs à calculer. Un exemple détaillé de construction d'un tableau de bord utilisant cette procédure est décrit dans le script  ```demos/exemple_tableaux_de_bord.R ``` .

Ce fichier pivot est donc particulièrement sensible et à manier avec précaution. Un fichier exemple pouvant servir de caneva est fourni dans les dossiers ```demos``` et ```vignettes``` . Les différents scripts permettent de décrire leur utilisation pour produire des données de façon systématique (```generation_tableaux_de_bord```) ou à la demande (``` demos/exemple_tableaux_de_bord```)
