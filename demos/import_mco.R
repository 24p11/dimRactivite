
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
remontees_dispo<-maj_variable_RData(remontees_dispo)

View(remontees_dispo%>%arrange(annee,mois,finess))


#Préparation de la création des ficihers à créer par la fonction save_remontee
#Exemple de de selection éventuelle de certaines remontées (par défaut tous les .RData qui n'existent pas seront crées)
#dossiers_remontees<-remontees_dispo%>%filter(finess == p$finess,annee == p$annee, mois == p$mois)
dossiers_remontees<-remontees_dispo


#Création et sauvegarde des .RData de remontée (si manquant)
save_remontees(dossiers_remontees,fichiers_genrsa)


#####################################################################################################################
#Import des fichiers sauvegardés dans le .RData
#####################################################################################################################

#Mise à jour de la liste des remontées disponibles
# par défaut les .RData du mois le plus récent sont systématiquement considérées comme manquantes
remontees_dispo<-update_remontees_dispo()

sel_remontees_import<-remontees_dispo%>%filter(as.numeric(annee)> max(as.numeric(remontees_dispo$annee))-profondeur_imports, RData==1)%>%
  group_by(finess,annee)%>%summarise(mois = max(as.numeric(mois)))

#Selection des remontées
load_all(sel_remontees_import)

#Chargement du fichier structure
fichier_structure <- readxl::read_excel("demos/structures.xlsx",
                                        col_types = c( "text" , "text" , "text" , "text" ,"text" ,
                                                       "text" , "text" , "text" , "text" ),
                                        col_names = c('nofiness','hopital','cdurm','uma_locale','uma_locale2',
                                                      'libelle_um','service','regroupement1','pole'),
                                        skip = 1
                                        )
verif_structure(rum,fichier_structure)

rum <- rum %>% left_join( ., fichier_structure ) %>%
      mutate(pole = ifelse(is.na(pole),'Erreurs',pole),
             service = ifelse(is.na(service),'Erreur service non renseigné',service))

save(list = c('rum','rum_v','diagnostics','actes','rsa','rsa_v','vano'),
     file = paste0(getOption("dimRactivite.path"),'/Rpmeasyr.RData') )

