library(tidyverse)
library (RCurl)
library(jsonlite)
library(referentiels)
library(referime)

#Si fichier genrsatemporaire
tmp = TRUE

#Année et mois en cours
mois=as.numeric(format(Sys.Date()-50,'%m'));Mois=format(Sys.Date()-50,'%m')
annee=as.numeric(format(Sys.Date()-55,'%Y'));



#Chargement du fichier structure
fichier_structure <- readxl::read_excel("demos/structures.xlsx",
                                        col_types = c( "text" , "text" , "text" , "text" ,"text" ,
                                                       "text" , "text" , "text" , "text" ),
                                        col_names = c('nofiness','hopital','cdurm','uma_locale','uma_locale2',
                                                      'libelle_um','service','regroupement1','pole'),
                                        skip = 1
)
verif_structure(rum,fichier_structure)

if(tmp == TRUE){
  
  rum1 <- rum %>% filter( ansor == annee ) %>% rename(uma_locale = cdurm) %>% left_join( ., fichier_structure %>% dplyr::distinct(uma_locale,.keep_all =TRUE) ) %>%
    mutate(pole = ifelse(is.na(pole),'Erreurs',pole),
           service = ifelse(is.na(service),'Erreur service non renseigné',service))
  
  rum2 <- rum  %>% filter( ansor != annee ) %>% left_join( ., fichier_structure ) %>%
    mutate(pole = ifelse(is.na(pole),'Erreurs',pole),
           service = ifelse(is.na(service),'Erreur service non renseigné',service))
  
  rum <- dplyr::bind_rows( rum1, rum2 )
  
  rm(rum1,rum2)
  
}else{
  rum <- rum %>% left_join( ., fichier_structure ) %>%
    mutate(pole = ifelse(is.na(pole),'Erreurs',pole),
           service = ifelse(is.na(service),'Erreur service non renseigné',service))
}


load('~/GH_PMSI/DATA/WD/identites_historique.RData')


nipipp <- read_csv2("~/GH_PMSI/DATA/NIP-IPP-ORBIS/LRB-SLS_Liste_NIP_IPP_20161122.csv", 
                    skip = 1,
                    col_names = c('nom','prenom','dnn','aphp','ipp','tri','nip'),
                    cols(
                      nom =  col_character(),
                      prenom = col_character(),
                      dnn =  col_character(),
                      aphp = col_character(),
                      ipp = col_character(),
                      tri = col_character(),
                      nip =  col_character())
)%>%
  dplyr::select(nip,ipp)

identites_mensuelle <- read_csv2("~/GH_PMSI/DATA/WD/identites_mensuelle.csv",
                                 skip = 1,
                                 col_names = c('nas','nip','nom','prenom','date_naissance'),
                                 cols(
                                   nas = col_character(),
                                   nip = col_character(),
                                   nom = col_character(),
                                   prenom = col_character(),
                                   date_naissance = col_date(format = "")
                                 ))

identites_mensuelle<-right_join(nipipp,identites_mensuelle,by=c('ipp'='nip'))

identites<-bind_rows(identites_historique%>%dplyr::select(names(identites_mensuelle)),identites_mensuelle)
identites = distinct(identites,nas,.keep_all = TRUE)
identites_historique <- identites
save(identites_historique,file = '~/GH_PMSI/DATA/WD/identites_historique.RData')

rsa<-dplyr::right_join(identites,rsa)
rum<-dplyr::right_join(identites,rum)


rum_save <- rum
rsa_save <- rsa

rum<-inner_join(rum,rum_v)
rsa<-inner_join(rsa,rsa_v)
#Sauvegarde de l'objet final disponible dans le working directory
save(rsa, rum, diagnostics, actes, vano,  file = '~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_dispose.RData')

rum<-rum_save
rsa<-rsa_save
save( rsa, rsa_v, rum, rum_v, diagnostics, actes, vano, file = '~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr.RData')
rm(rum_save,rsa_save);gc()


