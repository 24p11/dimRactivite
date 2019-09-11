#Exepmple de code permettant de générer des tableaux de bord pour suivre l'évolution annuelle d'un certain nombre d'indicateurs
#Format de sortie de la forme : liste d'indicateurs (lignes) x années (colonnes)

# Import des référentiels
#####################################

#Liste des indicateurs disponibles, légendes, indicateurs à calculer pour chaque niveau de regroupement (service/pole/hopial)
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


#Prération des DMS base nationale
ghm_dms_nationales<-referime::get_table("ghm_dms_nationales")

#Il y a 2 type d'appariement différents pour les dms, en effet certain GHM sont associés à plusieurs ghs.
#On réalise donc
# - un appariement par GHS lorsque le GHS est présent dans la table de référence
# - un appariement par GHM lorsque le GHS ,nest présent pas dans la table de référence
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




# Selection des données
#####################################

#Données cumulées de l'année et mois en cours
mois=as.numeric(format(Sys.Date()-50,'%m'));Mois=format(Sys.Date()-50,'%m')
annee=as.numeric(format(Sys.Date()-55,'%Y'));

#Selection des rum
df<-get_data(rum, ref = "ansor", m = 1:mois, a = (annee-3):annee, val = "Lariboisière", niveau = "hopital", opt = T )%>%
  #Création d'un pivot qui correspond aux colonnes du tableau
  mutate(pivot = factor(ansor,levels = (annee-5):annee))

#Ajout des données complémentaires nécessaires aux tableaux de bord
df<-left_join(inner_join(df,rum_v),
              inner_join(rsa,rsa_v)%>%mutate(anseqta = as.numeric(anseqta))%>%select(nofiness,cle_rsa,ansor,noghs,
                                                                                     anseqta,duree,rec_base,nbrum,noseqrum,
                                                                                     ghm,duree,nbjrbs,sejinfbi,rec_totale,rec_base,
                                                                                     rec_exh,rec_exb))%>%
  left_join(.,rsa_dms)%>%
  left_join(.,vano%>%inner_join(., df%>%select(nofiness,cle_rsa,ansor,nas)) %>% select( nas, noanon )%>% distinct(nas,noanon))

#Liste des indicateurs à calculer
inds=get_indicateurs(nom="TableauDeBordDIM",val="Lariboisiere")




#Génération du tableau de bord
#####################################
tdb<-get_tdb(df=df,indicateurs=inds)
