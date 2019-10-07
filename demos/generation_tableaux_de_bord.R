##
##Tableaux de bord intranet
##
################################################

#Génération de tableaux de bord d'activité après chargement des données pmeasyr
#si besoin :
load("~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr.RData")

library(tidyverse)
library (RCurl)
library(jsonlite)
library(referentiels)
library(referime); library(nomensland)
library(dimRactivite)


#Règler la profondeur des tableaux de bord
set_option("profondeur_tdb",1)


#Année et mois en cours
mois=as.numeric(format(Sys.Date()-50,'%m'));Mois=format(Sys.Date()-50,'%m')
annee=as.numeric(format(Sys.Date()-55,'%Y'));

#Chemins pour les sorties écriture des fichiers de sortie
path_res = getOption('dimRactivite.path_tdb_files')

#Chargement du fichier structure


###Liste des indicateurs disponibles, légendes, indicateurs à calculer pour chaque niveau de regroupement (service/pole/hopial)
references<-readxl::read_excel(file.path('demos','indicateurs.xlsx'),col_names = F)
names(references)<-prep_string(references[1,])
references<-references[-1,]%>%as_tibble(.)
#labels = df simple avec les légendes
labels<-as.data.frame(references[!duplicated(references$var),c('var','libelle','niv')])
row.names(labels)<-labels$var
labels<-labels[,-1]

#Présents dans fichiers structure, absent référentiels
str<-c(unique(structures$hopital),unique(structures$pole),unique(structures$service))
prep_string(str)[!prep_string(str)%in%names(references)]
#Présents référentiels, absents fichier structure
names(references)[!names(references)%in%c(prep_string(str),'tdb','niv','libelle','var','variable','dataframe','referentiel')]

tarifs_mco_ghs <- nomensland::get_table("tarifs_mco_ghs")
#####################################################################################################################
#Prération des DMS base nationale
#####################################################################################################################
ghm_dms_nationales<-nomensland::get_table("ghm_dms_nationales")
ghm_dms_nationales<-dplyr::bind_rows(ghm_dms_nationales,
                                     ghm_dms_nationales%>%filter(anseqta == "2018")%>%mutate(anseqta = "2019")
)
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
df<-get_data(inner_join(rum,rum_v), a =(annee- getOption('dimRactivite.profondeur_tdb')):annee, m = 1:mois )


tdb <- get_activite_sejours( df, structures )
path_file=file.path(path_res,paste0("TableauDeBordGeneral",annee,stringr::str_pad(mois,2,"left","0"),"cum.xls"))  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )

tdb <- get_activite_recettes( df, structures )
path_file=file.path(path_res,paste0("TableauDeBordValorisation",annee,stringr::str_pad(mois,2,"left","0"),"cum.xls")) 
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )


df <- df %>% filter( as.numeric(moissor) == mois )

tdb <- get_activite_sejours( df, structures )
path_file=file.path(path_res,paste0("TableauDeBordGeneral",annee,stringr::str_pad(mois,2,"left","0"),"mens.xls"))  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )

tdb <- get_activite_recettes( df, structures )
path_file=file.path(path_res,paste0("TableauDeBordValorisation",annee,stringr::str_pad(mois,2,"left","0"),"mens.xls")) 
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "utf-8" )



tdb<-list()
#######################################################################################################################
#Tableaux de bords thématiques
#######################################################################################################################
val = "GH"
tdb[[val]]<-make_tdb( val , niveau = NULL, annee, mois )

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


#Tableau de bord synthétique de suivi d'indicateurs et de recettes

tarifs_ghs<-nomensland::get_table( "tarifs_mco_ghs" )

tarifs_ghs_n1<-full_join(tarifs_ghs %>% distinct( anseqta, ghs,.keep_all = TRUE ) %>%select( anseqta, ghs, tarif_base ),
                         tarifs_ghs %>% distinct( anseqta, ghs,.keep_all = TRUE ) %>% mutate(anseqta = as.character(as.numeric(anseqta)-1)) %>% select( anseqta, ghs, tarif_base ),
                         by=c("ghs","anseqta"),suffix = c('',"_n_1")
)

tarifs_ghs_n1<-tarifs_ghs_n1%>%mutate(tarif_base_n_1 = if_else(is.na(tarif_base_n_1),tarif_base,tarif_base_n_1))%>%
  select(-tarif_base)%>%rename(tarif_base = tarif_base_n_1 )

rum_v2 <- dplyr::left_join(rum_v, rum_v_nonconsol %>% dplyr::select( ansor, nofiness, nas, norum, coefpmctmonotime1, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2, cdghm ), 
                           by = c("nofiness", "nas", "norum", "ansor") ,
                           suffix = c( "", "_nonconsol" )) %>% rename(cdghm_nonconsol = cdghm)


#####################################################################################################################
#Préparation tableau de données
#####################################################################################################################

df <- get_data( inner_join(rum,rum_v2), a = (annee-1):annee, m = 1:mois ) %>% mutate( ansor = as.character(ansor))

df <- dplyr::left_join( df,
                        rum_v_tarifsante %>% dplyr::select( ansor, nofiness, cle_rsa, nas, norum, valotime, valopmctmono, valopmctmonotime1, valopmctmonotime2 ), 
                        suffix = c( "", "_tarifsante" ),
                        by = c("nofiness", "nas", "norum", "cle_rsa", "ansor") ) %>% 
  dplyr::left_join( ., inner_join(rsa,rsa_v) %>% dplyr::mutate( rec_exbh = rec_exh + rec_exb ) %>% 
                      dplyr::select( ansor, anseqta, nofiness, cle_rsa, nas, ghs=noghs, rec_base, rec_exbh, duree ) ) %>%
  dplyr::left_join(.,rsa_dms)

df_n<- df %>% filter( as.numeric(ansor) == annee )
df_n<- df_n %>% dplyr::left_join( ., tarifs_ghs  %>% distinct( anseqta, ghs,.keep_all = TRUE ) %>% select(anseqta, ghs, tarif_base ) )


df_n1<- df %>% filter( as.numeric(ansor) == annee -1 )
df_n1 <- df_n1 %>% dplyr::left_join( ., tarifs_ghs_n1  %>% distinct( anseqta, ghs,.keep_all = TRUE ) %>% select(anseqta, ghs, tarif_base ) )


df<-bind_rows(df_n,df_n1)

df<-df %>% mutate(service = factor(service),pole = factor(pole),hopital = factor(hopital), ansor = factor(ansor) )




tdb_indicateurs <- get_tdb_indicateurs( df )
path_file=paste0(getOption('dimRactivite.path_tdb_files'),'/',annee,'/','TableauDeBordIndicateurs_',annee,stringr::str_pad(mois,2,"left","0"),'.xls')  
write.table( tdb_indicateurs, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )


df <- df %>% filter( typehosp=="C" )

tdb_detail_recettes <- get_tdb_detail_recettes( df )
path_file=paste0(getOption('dimRactivite.path_tdb_files'),'/',annee,'/','TableauDeBordDetailValorisation_Resume_',annee,stringr::str_pad(mois,2,"left","0"),'.xls')  
write.table( tdb_detail_recettes, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )



