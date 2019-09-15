library(tidyverse)
library(RCurl)
library(jsonlite)
library(referentiels)
library(referime)
library(dimRactivite)

#####################################################################################################################
##Paramètre des imports
#####################################################################################################################
profondeur_imports = 6


#####################################################################################################################
##Création fichier .RData par remontée
#####################################################################################################################

#Liste des fichiers présents dans le dossier racine
#Analyse des fichiers et vérification de la présence des fichiers en fonction de leur extentions (ext imco + .zip + .RData)
#Par défaut (option maj_dernier_mois =TRUE) les fichiers .RData de la remontée la plus récente est considéré comme manquant
remontees_dispo<-analyse_dossier_remontees()
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
  group_by( finess, annee )%>% filter( mois == max(mois) )


#import des remontées
load_RData( sel_remontees_import )
load_med( sel_remontees_import, fichiers_genrsa )
load_dmi( sel_remontees_import, fichiers_genrsa )


# ajout des structures dans les rum
fichier_structure <- readxl::read_excel(getOption("dimRactivite.structures")$fichier,
                                        col_types = c( "text" , "text" , "text" , "text" ,"text" ,
                                                       "text" , "text" , "text" , "text" ),
                                        col_names = getOption("dimRactivite.structures")$colonnes,
                                        skip = 1
)

#vérification que toutes les UMA sont bien renseignées dans le fichier structure
verif_structure(rum,fichier_structure)

rum <- rum %>% left_join( ., fichier_structure ) %>%
          mutate( pole = ifelse(is.na(pole), 'Erreurs', pole ),
                  service = ifelse(is.na(service), 'Erreur service non renseigné', service ) )
