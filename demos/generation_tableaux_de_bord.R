##
##Tableaux de bord intranet
##
################################################

#Génération de tableaux de bord d'activité après chargement des données pmeasyr
#si besoin : load("~/GH_PMSI/DATA/WD/Rpmeasyr.RData")

library(tidyverse)
library (RCurl)
library(jsonlite)
library(referentiels)


#Année et mois en cours
mois=as.numeric(format(Sys.Date()-50,'%m'));Mois=format(Sys.Date()-50,'%m')
annee=as.numeric(format(Sys.Date()-55,'%Y'));

#Chemins pour l'es sorties'écriture des fichiers de sortie
path_res="~/GH_PMSI/ACTIVITE_MSI/ANALYSES/TABLEAUX_DE_BORD/"


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


exclusionPdiff<-c('Taux de re-admissions precoces','IGS moyen')




#######################################################################################################################
###Tableaux de bord généraux
#######################################################################################################################
df <- get_data(rum, a = (annee-5):annee, m = 1:mois )
tdb <- get_activite_sejours( df, structure )

path_file=paste0(path_res,'/',annee,'/','TableauDeBordGeneral',annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )


df <- get_data(df, a = (annee-5):annee, m = mois )
tdb <- get_activite_sejours( df, structure )

path_file=paste0(path_res,'/',annee,'/','TableauDeBordGeneral',annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )

df<-get_data(inner_join(rum,rum_v), a =(annee-5):annee, m = 1:mois )
tdb <- get_activite_recettes( df, structure )

path_file=paste0(path_res,'/',annee,'/','TableauDeBordValorisation',annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )


df <- get_data(df, a = (annee-5):annee, m = mois )
tdb <- get_activite_recettes( df, structure )

path_file=paste0(path_res,'/',annee,'/','TableauDeBordValorisation',annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')  
write.table( tdb, file=path_file, sep='\t', row.names=T, col.names=NA, na='' , fileEncoding = "latin1" )



#####################################################################################################################
#Prération des données pour le suivi de l'activité de cancérologie
#####################################################################################################################
cancer_diag <- selection_cancer_diag(diagnostics)
cancer_pat <- selection_cancer_pat(cancer_diag)
cancer_rsa <- dplyr::inner_join(cancer_pat,
                                rsa%>%mutate(nda = substr(nas,1,9))%>%
                                  select(nofiness,cle_rsa,ansor, moissor,nip,ipp,nas,nda,dp,gpcmd,gptype,gpnum,gpcompx)%>%
                                  mutate(ipp = dplyr::if_else( (is.na(ipp) | ipp=='') ,nda,ipp)))%>%
  attribution_type_M4(.)%>%
  attribution_statut_nx_patient(.)

#######################################################################################################################
#Tableaux de bords thématiques
#######################################################################################################################
niveau = "hopital"

for (hopital in unique(fichier_structure$hopital) ){

  #Nom des tableaux de bord disponibles
  noms_tableaux = references%>%filter(!!sym(PrepCharacter(hopital)) == 'o', !is.na(tdb))%>%mutate(tdb = str_split(tdb,','))%>%select(tdb)
  noms_tableaux =  unique(unlist(noms_tableaux))


  #Données cumulées
  df<-get_data(rum,ref='ansor',1:mois,(annee-5):annee,hopital,niveau,regles=T)

  df<-left_join(inner_join(df,rum_valo),
                rsa%>%mutate(anseqta = as.numeric(anseqta))%>%select(nofiness,cle_rsa,ansor,noghs,
                                                                     anseqta,duree,rec_base,nbrum,noseqrum,
                                                                     ghm,duree,dtent,dtsort,nbjrbs,sejinfbi,
                                                                     rec_exh,rec_exb))



  for (nom_tableau in noms_tableaux){

    print(paste(hopital,nom_tableau))

    inds=get_indicateurs(nom=nom_tableau,val=hopital)

    Tab<-get_tdb(df=df,indicateurs=inds)

    write.table(Tab,file = paste0(path_res,'/',annee,'/',nom_tableau,PrepCharacter(hopital),annee,mois,'cum.xls'),sep='\t',row.names=F,na='')


  }

  #Données mensuelles
  df<-df%>%filer(moissor == mois)

  for (nom_tableau in noms_tableaux){

    inds=get_indicateurs(nom=nom_tableau,val=hopital)

    Tab<-get_tdb(df=df,indicateurs=inds)

    write.table(Tab,file = paste0(path_res,'/',annee,'/',nom_tableau,PrepCharacter(hopital),annee,mois,'mens.xls'),sep='\t',row.names=F,na='')

    print(paste(hopital,nom_tableau))
  }




}



#Niveau hopital
for (hopital in unique(fichier_structure$hopital) ){

  val = hopital
  niveau = "hopital"

  df<-get_data(rum,ref='ansor',1:mois,(annee-5):annee,val,niveau,regles=F)

  df<-left_join(inner_join(df,rum_valo),
                left_join(rsa%>%mutate(anseqta = as.numeric(anseqta))%>%select(nofiness,cle_rsa,ansor,
                                                                               anseqta,duree,rec_base,nbrum,noseqrum,
                                                                               ghm,duree,dtent,dtsort,nbjrbs,sejinfbi,
                                                                               rec_exh,rec_exb),
                          referentiel_ghm_tarfis%>%rename(noghs = ghs, dms_bn = dms)%>%select(anseqta,ghm,noghs,dms_bn,bb,bh))%>%
                  select(nofiness,cle_rsa,ansor,anseqta,duree,rec_base,nbrum,noseqrum,
                         noghs,dms_bn,bb,bh,nbjrbs,sejinfbi,rec_exh,rec_exb)
  )

  tabs = references%>%filter(!!sym(PrepCharacter(hopital)) == 'o')%>%mutate(tdb = str_split(tdb,','))%>%select(tdb)
  tabs =  unique(unlist(tabs))

  for (nom_tableau in tabs)){

    inds=get_indicateurs(nom=nom_tableau,val=val)

    Tab<-get_tdb(df=df,indicateurs=inds)

    print(nom_tableau)
  }


}

#source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/fonctions.R'))
#rum<-rum%>%group_by(nofiness,ansor,cle_rsa)%>%mutate(daentr = min(d8eeue),dasor = max(d8soue))%>%ungroup()
#df<-rum%>%filter(as.numeric(ansor)%in%(annee-5):annee,as.numeric(moissor)%in%1:mois,!!sym(niveau)==val)

#Calcul des dates entrée et sortie du séjour
#df<-df%>%group_by(nas)%>%mutate(dtent = min(d8eeue),dtsor = max(d8soue))%>%ungroup()


nom_tableau='TableauDeBordActivite'

