library(tidyverse)
library(RCurl)
library(jsonlite)
library(referentiels)
library(nomensland)
library(dimRactivite)

dimRactivite::update_options(file.path("demos","options.yaml"))

#####################################################################################################################
##Paramètre des imports
#####################################################################################################################
profondeur_imports = 2


#####################################################################################################################
##Création fichier .RData par remontée
#####################################################################################################################

#Liste des fichiers présents dans le dossier racine
#Analyse des fichiers et vérification de la présence des fichiers en fonction de leur extentions (ext imco + .zip + .RData)
#Par défaut (option maj_dernier_mois =TRUE) les fichiers .RData de la remontée la plus récente est considéré comme manquant
remontees_dispo<-analyse_dossier_remontees( maj_dernier_mois = FALSE )
View(remontees_dispo)

#Voir la liste des fichiers RData non disponibles qui seront générés à l'étape suivante
print( remontees_dispo %>% filter( RData != 1 ) )



#Création et sauvegarde des .RData de remontée (si manquant)
#Mise à jour de la liste des remontées disponibles
remontees_dispo <- save_remontees( remontees_dispo )


#####################################################################################################################
#Import des fichiers sauvegardés dans le .RData
#####################################################################################################################

sel_remontees_import<-remontees_dispo%>%filter( as.numeric(annee) > max( as.numeric(remontees_dispo$annee) ) - profondeur_imports, RData == 1 )%>%
  mutate( mois  =  as.numeric(mois) )%>%
  group_by( finess, annee )%>% filter( mois == max(mois) )%>% mutate(mois = ifelse(annee == 2019,7,mois ))


#import des remontées
load_RData( sel_remontees_import, extra = TRUE )
load_med( sel_remontees_import, fichiers_genrsa )
load_dmi( sel_remontees_import, fichiers_genrsa )


# ajout des structures dans les rum
structures <- readxl::read_excel(file.path("demos","structures.xlsx"),
                                 col_types = c( "text" , "text" , "text" , "text", "text" ),
                                 col_names = c('hopital','cdurm','libelle_um','service','pole'),
                                 skip = 1
)


#vérification que toutes les UMA sont bien renseignées dans le fichier structure
#verif_structure(rum,fichier_structure)

rum <- rum %>% inner_join( ., structures ) %>%
          mutate( pole = ifelse(is.na(pole), 'Erreurs', pole ),
                  service = ifelse(is.na(service), 'Erreur service non renseigné', service ) )

rum <- rum%>%filter(as.numeric(moissor)%in% 1:mois)%>% dplyr::sample_n(.,10000,replace = FALSE)
