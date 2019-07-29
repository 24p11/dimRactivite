##
##Tableaux de bord intranet
##
################################################
#source(paste(PathR,'PROGRAMMES/FCT_SERVICES/ACTIVITES_TRANSVERSALES/TABLEAUX_DE_BORD/UpDateTableauxDeBord.R',sep=''))

# Principe general :
#   les tableaux de bord sont calcules par la fonction GetTableauDeBord, ecrits au format csv et envoyes au serveur intranet
#   les arguments de la fonction sont un tableau de donnees de type sejour

library(tidyverse)
library (RCurl)
library(jsonlite)
library(referentiels)


mois=as.numeric(format(Sys.Date()-50,'%m'));Mois=format(Sys.Date()-50,'%m')
annee=as.numeric(format(Sys.Date()-55,'%Y'));

#Chemins pour les sorties
path=paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/')
path_res="~/GH_PMSI/ACTIVITE_MSI/ANALYSES/TABLEAUX_DE_BORD/"


###liste des indicateurs disponibles, légendes, indicateurs à calculer pour chaque niveau de regroupement (service/pole/hopial)
references<-read.table(paste0(path,'indicateurs.xls',sep=''),
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


#Fichier structure
fichier_structure <- readxl::read_excel('~/GH_PMSI/DATA/Structures/structures.xlsx',
                                        col_types = c( "text" , "text" , "text" , "text" ,"text" ,
                                                       "text" , "text" , "text" , "text" )
)

names(fichier_structure) <- c('nofiness','hopital','cdurm','uma_locale','uma_locale2',
                              'libelle_um','service','regroupement1','pole')




#chargement des fonctions
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/fonctions.R'))

#Import des données pmeasyt
load("~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr.RData")


#Prération des données pour le suivi de l'activité de cancérologie
cancer_diag <- selection_cancer_diag(diagnostics)
cancer_pat <- selection_cancer_pat(cancer_diag)
cancer_rsa <- dplyr::inner_join(cancer_pat,
                                rsa%>%mutate(nda = substr(nas,1,9))%>%
                                  select(nofiness,cle_rsa,ansor, moissor,nip,ipp,nas,nda,dp,gpcmd,gptype,gpnum,gpcompx)%>%
                                  mutate(ipp = dplyr::if_else( (is.na(ipp) | ipp=='') ,nda,ipp)))%>%
  attribution_type_M4(.)%>%
  attribution_statut_nx_patient(.)



###Tableaux de bord généraux
services = unique(fichier_structure$service)
poles = unique(fichier_structure$pole)
df<-get_data(rum,a =(annee-5):annee,m = 1:mois,regles =F)


df<-get_data(inner_join(rum,rum_valo),a =(annee-5):annee,m = 1:mois,regles =F)

get_activite_sejours
get_activite_recettes
tdb_j <-list()

tdb_j[['services']] <- round( with( df, tapply( dureesejpart, list(service,ansor,typehosp), sum, na.rm=T ) ) )
tdb_j[['poles']]  <- round( with( df, tapply( dureesejpart, list(pole,ansor,typehosp), sum, na.rm=T ) ) )
tdb_j[['hopitaux']] <- round( with( df, tapply( dureesejpart, list(hopital,ansor,typehosp), sum, na.rm=T ) ) )
tdb_j[['gh']] <- t(round( with( df, tapply( dureesejpart, list(ansor,typehosp), sum, na.rm=T ) ) ))

tdb_j <- order_by_structure(tdb_j)

tdb_j_final<-cbind(get_diff(tdb_j$hc),NA,get_diff(tdb_j$hp))



#Niveau hopital

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

