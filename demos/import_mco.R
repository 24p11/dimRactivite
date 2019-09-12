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
fichiers_genrsa <- dimRactivite::scan_path()

#Analyse des fichiers et vérification de la présence des fichiers en fonction de leur extentions (ext imco + .zip + .RData)
#Par défaut (option recent =TRUE) les fichiers .RData de la remontée la plus récente est considéré comme manquant
remontees_dispo<-analyse_fichiers_remontees(fichiers_genrsa)

#Par défaut, les .RData de l'année en cours sont considérés comme à faire


View(remontees_dispo%>%arrange(annee,mois,finess))


#Préparation de la création des ficihers à créer par la fonction save_remontee
#Exemple de de selection éventuelle de certaines remontées (par défaut tous les .RData qui n'existent pas seront crées)
#dossiers_remontees<-remontees_dispo%>%filter(finess == p$finess,annee == p$annee, mois == p$mois)
dossiers_remontees<-remontees_dispo


#Création et sauvegarde des .RData de remontée (si manquant)
#Mise à jour de la liste des remontées disponibles
remontees_dispo <- save_remontees(dossiers_remontees,fichiers_genrsa)


#####################################################################################################################
#Import des fichiers sauvegardés dans le .RData
#####################################################################################################################

sel_remontees_import<-remontees_dispo%>%filter(as.numeric(annee)> max(as.numeric(remontees_dispo$annee))-profondeur_imports, RData==1)%>%
  mutate(mois  =  as.numeric(mois) )%>%
  group_by( finess, annee )%>% filter(mois == max(mois) )


#Selection des remontées
load_RData(sel_remontees_import)
