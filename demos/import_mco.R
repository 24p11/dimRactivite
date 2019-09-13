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
fichiers_genrsa <- scan_path()

#Analyse des fichiers et vérification de la présence des fichiers en fonction de leur extentions (ext imco + .zip + .RData)
#Par défaut (option recent =TRUE) les fichiers .RData de la remontée la plus récente est considéré comme manquant
remontees_dispo<-analyse_fichiers_remontees(fichiers_genrsa)

#Par défaut, les .RData de l'année en cours sont considérés comme à faire


View(remontees_dispo%>%arrange(annee,mois,finess))



#Création et sauvegarde des .RData de remontée (si manquant)
#Mise à jour de la liste des remontées disponibles
remontees_dispo <- save_remontees(remontees_dispo,fichiers_genrsa)


#####################################################################################################################
#Import des fichiers sauvegardés dans le .RData
#####################################################################################################################

sel_remontees_import<-remontees_dispo%>%filter(as.numeric(annee)> max(as.numeric(remontees_dispo$annee))-profondeur_imports, RData==1)%>%
  mutate(mois  =  as.numeric(mois) )%>%
  group_by( finess, annee )%>% filter(mois == max(mois) )


#Selection des remontées
load_RData(sel_remontees_import)
load_med( sel_remontees_import, fichiers_genrsa )
load_dmi( sel_remontees_import, fichiers_genrsa )
