##
##Tableaux de bord intranet
##
################################################

#Génération de tableaux de bord d'activité après chargement des données pmeasyr
#si besoin : load(paste0(getOption("dimRactivite.path"),"/Rpmeasyr.RData"))

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

#Chemins pour les sorties écriture des fichiers de sortie
path_res = getOption('dimRactivite.path_tdb_files')

#Chargement du fichier structure
fichier_structure <- readxl::read_excel("demos/structures.xlsx",
                                        col_types = c( "text" , "text" , "text" , "text" ,"text" ,
                                                       "text" , "text" , "text" , "text" ),
                                        col_names = c('nofiness','hopital','cdurm','uma_locale','uma_locale2',
                                                      'libelle_um','service','regroupement1','pole'),
                                        skip = 1
)

str<-c(unique(fichier_structure$hopital),unique(fichier_structure$pole),unique(fichier_structure$service))


###Liste des indicateurs disponibles, légendes, indicateurs à calculer pour chaque niveau de regroupement (service/pole/hopial)
references<-read.table('demos/indicateurs.xls',
                       sep='\t',
                       header=F,fill=T,
                       na.strings = c("NA",""),stringsAsFactor=F,fileEncoding = 'latin1')

names(references)<-prep_string(references[1,])
references<-references[-1,]%>%as_tibble(.)
#labels = df simple avec les légendes
labels<-as.data.frame(references[!duplicated(references$var),c('var','libelle','niv')])
row.names(labels)<-labels$var
labels<-labels[,-1]

#Présents dans fichiers structure, absent référentiels
prep_string(str)[!prep_string(str)%in%names(references)]
#Présents référentiels, absents fichier structure
names(references)[!names(references)%in%c(prep_string(str),'tdb','niv','libelle','var','variable','dataframe','referentiel')]



exclusionPdiff<-c('Taux de re-admissions precoces','IGS moyen')

#####################################################################################################################
#Prération des DMS base nationale
#####################################################################################################################
ghm_dms_nationales<-referime::get_table("ghm_dms_nationales")

#Merge rsa, dms pour les cas ou la référence dans ghm_dms_nationales = ghs
rsa_dms1<-inner_join(rsa%>%unite("ghm",gpcmd,gptype,gpnum,gpcompx,sep="")%>%
                       rename(ghs = noghs)%>%
                       select(nofiness,cle_rsa,ansor,anseqta,ghs,ghm),
                     ghm_dms_nationales%>%filter(ghs!=""))
#Merge rsa, dms pour les cas ou la référence dans ghm_dms_nationales = ghm
rsa_dms2<-inner_join(rsa%>%unite("ghm",gpcmd,gptype,gpnum,gpcompx,sep="")%>%
                       select(nofiness,cle_rsa,ansor,anseqta,ghm),
                     ghm_dms_nationales%>%filter(ghs==""))
rsa_dms<-bind_rows(rsa_dms1%>%select(nofiness,cle_rsa,ansor,dms_n),
                   rsa_dms2%>%select(nofiness,cle_rsa,ansor,dms_n)
                   )
rm(rsa_dms1,rsa_dms2)   
#####################################################################################################################
#Prération des données pour le suivi de l'activité de cancérologie
#####################################################################################################################
cancer_diag <- selection_cancer_diag(diagnostics)
cancer_pat <- selection_cancer_pat(cancer_diag , vano%>%inner_join(., rum%>%select(nofiness,cle_rsa,ansor,nas)) %>%
                                     select( nas, noanon ) %>%
                                     distinct(nas,noanon))
cancer_rsa <- dplyr::inner_join(cancer_pat,
                                rsa%>%inner_join(., vano%>%select(nofiness,cle_rsa,ansor,noanon))%>%
                                  select(noanon,nofiness,cle_rsa,ansor, moissor, nas, dp, gpcmd, gptype, gpnum, gpcompx))%>%
  attribution_type_M4(.)%>%
  attribution_statut_nx_patient(.)

#######################################################################################################################
###Tableaux de bord généraux
#######################################################################################################################
df<-get_data(inner_join(rum,rum_v), a =(annee-3):annee, m = 1:mois )


tdb <- get_activite_sejours( df, structure )
path_file=paste0(path_res,'/',annee,'/','TableauDeBordGeneral',annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )

tdb <- get_activite_recettes( df, structure )
path_file=paste0(path_res,'/',annee,'/','TableauDeBordValorisation',annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )


df <- df %>% filter( as.numeric(moissor) == mois )

tdb <- get_activite_sejours( df, structure )
path_file=paste0(path_res,'/',annee,'/','TableauDeBordGeneral',annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )

tdb <- get_activite_recettes( df, structure )
path_file=paste0(path_res,'/',annee,'/','TableauDeBordValorisation',annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )



tdb<-list()
#######################################################################################################################
#Tableaux de bords thématiques
#######################################################################################################################
niveau = "hopital"
vals = fichier_structure%>%select(!!niveau)%>%unique()%>%flatten_chr()

for (val in vals ){
  
  tdb[[val]]<-make_tdb( val, niveau, annee, mois )
  
}


niveau = "pole"
vals = fichier_structure%>%select(!!niveau)%>%unique()%>%flatten_chr()

for (val in vals ){
  
  tdb[[val]]<-make_tdb( val, niveau, annee, mois )
  
}


niveau = "service"
vals = fichier_structure%>%select(!!niveau)%>%unique()%>%flatten_chr()

for ( val in vals ){
  
  tdb[[val]]<-make_tdb( val, niveau, annee, mois )
  
}




df<-get_data(inner_join(rum,rum_v), a =(annee-5):annee, m = 1:mois )
tdb_v[['hopitaux']] <- round( with( df, tapply( valopmctmonotime1, list(hopital,ansor,typehosp), sum, na.rm=T ) ) )
