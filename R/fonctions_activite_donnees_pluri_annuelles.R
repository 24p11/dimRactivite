
#' Création d'un tableau indicateurs x pivot, comprenant une liste d'inficateurs
#'
#' @param df un tibble de type séjours
#' @param indicateurs vecteur, liste des indicateurs à calculer
#' @param pivot string, la variable pivot a utiliser dans df pour réaliser le tableau croisé
#'
#' @return tableau de bord
#' @export
#'
#' @examples
get_tdb<-function(df, indicateurs, pivot = 'ansor'){

  df<-df%>%mutate(pivot = !!sym(pivot))%>%mutate(pivot = as.factor(pivot))



  #-------------------------------------------------------------------
  #Utile pour le calcul du nombre de lit utilisé sur la période d'étude
  #--------------------------------------------------------------------
  #Les bornes de dates pour chaque niveau de la variable pivot (ex : année 2014, borne inf = 01-01-2014, borne sup = 31-12-2014)
  df<-df%>%group_by(pivot)%>%mutate(date_min_pivot = as.Date(paste0(min(as.numeric(df$ansor)),'-', min(as.numeric(df$moissor)),'-01')),
                                    date_max_pivot = as.Date(paste0(max(as.numeric(df$ansor)),'-', max(as.numeric(df$moissor))+1,'-01'))-1)%>%
    ungroup()

  if( 'd8eeue' %in% names(df) ){
    df<-df%>%mutate(d8eeue2 = as.Date(ifelse(date_min_pivot < d8eeue,d8eeue,date_min_pivot),origin = '1970-01-01'),
                    d8soue2 = as.Date(ifelse(date_max_pivot > d8soue,d8soue,date_max_pivot),origin = '1970-01-01' ),
                    dureesejpart2 = ifelse(d8soue2 > d8eeue2,d8soue2 - d8eeue2,0 ))


  }else{
    df<-df%>%mutate(dtent2 = as.Date(ifelse(date_min_pivot < dtent,dtent,date_min_pivot),origin = '1970-01-01'),
                    dtsort2 = as.Date(ifelse(date_max_pivot > dtsort,dtsort,date_max_pivot),origin = '1970-01-01' ),
                    dureesejpart2 = ifelse(dtsort2 > dtent2,dtsort2 - dtent2,0 ))


  }
  #Les nouvelles dates entrée/sortie du résumé à l'intérieur des bornes pour chaque niveau de pivot, + nouvelle durée du résumé

  #Données utiles pour la valorisation
  df<-left_join(df%>%mutate(noghs = as.numeric(noghs), anseqta = as.numeric(anseqta)),
                referentiel_ghm_tarfis%>%rename(noghs = ghs, dms_bn = dms)%>%select(anseqta,ghm,ghs,dms_bn,bb,bh))

  tb<-list()

  ###########################################################################
  #Nb de sejours en hospitalisation complete
  #  selection sur le type dossier (diff I,S)
  ###########################################################################
  if('NbPat'%in%indicateurs){
    tb[['NbPat']] <- with(df,tapply(ipp, pivot, nb_unique))

  }

  ###########################################################################
  #Nb de sejours en hospitalisation complete
  #  selection sur le type dossier (diff I,S)
  ###########################################################################
  #A voir comment selectionner la variable TypeDossier

  if('HCtot'%in%indicateurs){
    tb[['HCtot']] <- table( df %>% dplyr::filter( typehosp=="C", doublon==1 ) %>% dplyr::select(pivot) )
  }

  if('NbJHC'%in%indicateurs){

    tb[['NbJHC']]<-with( df%>% dplyr::filter( typehosp=="C" ,doublon==1 ),
                         tapply( duree, pivot, sum ) )
  }


  ###########################################################################
  #Nb de sejours HC 0 nuit
  ###########################################################################
  if('HC0'%in%indicateurs){

    tb[['HC0']]<-table(df %>% dplyr::filter(typehosp=="C", duree == 0, doublon==1) %>%select(pivot) )

  }

  ###########################################################################
  #Nb de sejours HC 1 nuit
  ###########################################################################

  if('HC1'%in%indicateurs){
    tb[['HC1']]<-table(df %>% dplyr::filter(duree == 1, doublon==1) %>%select(pivot) )
  }

  ###########################################################################
  #Nb de sejours HC 2 nuit
  ###########################################################################

  if('HC2'%in%indicateurs){

    tb[['HC2']]<-table(df %>% dplyr::filter(duree > 1, doublon==1) %>%select(pivot) )
  }

  ###########################################################################
  #Nb d'hosptialisations partielles
  ###########################################################################
  if('HPTot'%in%indicateurs){

    tb[['HPTot']]<-table(df %>% dplyr::filter(typehosp=="P", doublon==1) %>%select(pivot) )
  }

  ###########################################################################
  #Nb Seances
  ############################################################################

  if('Seances'%in%indicateurs){

    tb[['Seances']]<-table(df %>% dplyr::filter(cdghm%in%ListesGHM$GhmSeances$GHM, doublon==1) %>%select(pivot) )

  }
  ###########################################################################
  #
  ############################################################################
  if('SeancesDialyses'%in%indicateurs){

    tb[['SeancesDialyses']]<-table(df %>% dplyr::filter(cdghm%in%ListesGHM$GhmSeances$GHM[ListesGHM$GhmSeances$TYPE=='Dialyse'],
                                                        doublon==1) %>%
                                     select(pivot) )
  }

  ###########################################################################
  #Nb Seances de chimiotherapie
  ###########################################################################
  if('SeancesChimio'%in%indicateurs){

    tb[['SeancesChimio']]<-table(df %>% dplyr::filter(cdghm%in%ListesGHM$GhmSeances$GHM[ListesGHM$GhmSeances$TYPE=='Chimio'],
                                                      doublon==1) %>%
                                   select(pivot) )
  }

  ###########################################################################
  #Nb Seances de radiotherapie
  ###########################################################################
  if('SeancesRadio'%in%indicateurs){

    tb[['SeancesRadio']]<-table(df %>% dplyr::filter(cdghm%in%ListesGHM$GhmSeances$GHM[ListesGHM$GhmSeances$TYPE=='Radio'],
                                                     doublon==1) %>%
                                  select(pivot) )
  }
  ###########################################################################
  #Nb Seances preparation radiotherapie
  ###########################################################################
  if('SeancesPrepaRadio'%in%indicateurs){

    tb[['SeancesPrepaRadio']]<-table(df %>% dplyr::filter(cdghm%in%ListesGHM$GhmSeances$GHM[ListesGHM$GhmSeances$TYPE=='PrepaRadio'],
                                                          doublon==1) %>%
                                       select(pivot) )
  }
  ###########################################################################
  #Nb d'hopitaux de jour medicaux
  ###########################################################################
  if('HPMed'%in%indicateurs){

    tb[['HPMed']]<-table(df %>% dplyr::filter(typehosp=="P", substr(cdghm,3,3)=='M', doublon==1) %>% select(pivot) )
  }

  ###########################################################################
  #Nb d'hopitaux de jour chirurgicaux
  ###########################################################################

  if('HPChir'%in%indicateurs){
    tb[['HPChir']]<-table(df %>% dplyr::filter(typehosp=="P", substr(cdghm,3,3)=='C', doublon==1) %>% select(pivot) )
  }



  if('HPMedTech'%in%indicateurs){
    tb[['HPMedTech']]<-table(df %>% dplyr::filter(typehosp=="P", substr(cdghm,3,3)=='K', doublon==1) %>% select(pivot) )
  }



  if('NbIVG'%in%indicateurs){
    tb[['NbIVG']]<-table(df %>% dplyr::filter(cdghm=='14Z08Z', doublon==1) %>% select(pivot) )
  }


  ###########################################################################
  ###########################################################################
  ###########
  ########### Activite chirurgicale
  ###########
  ###########################################################################
  ###########################################################################



  #Activite des blocs operatoires
  ############################################################################
  #Source IPOP

  #Volume des patients HC
  #if('VolBloc'%in%indicateurs){
  #  tb[['VolBloc']]<-table(DataIpop$pivot)
  #}
  #Volume des patients HP
  #if('VolBlocHP'%in%indicateurs){
  #  tb[['VolBlocHP']]<-table(DataIpop$pivot[which(!DataIpop$typehosp=="C")])
  #}

  #Pourcentage de séjours chirurgicaux
  ############################################################################
  if('pSejChirHC'%in%indicateurs){

    tb[['pSejChirHC']]<-round(
      table(df %>% dplyr::filter(typehosp=="C", substr(cdghm,3,3)=='C', doublon==1) %>% select(pivot) )*100/
        tb[['HCtot']],1
    )
  }

  ###########################################################################
  #Indicateurs chirurgie ambulatoire
  ###########################################################################

  # UHambu<-GetParametresFromAllSites('UHchirurgieAmbu')
  #UmAmbu <- fichier_structure$uma_locale[substr(fichier_structure$uma_locale,4,4)=='C']

  # ServicesChirurgie <- GetParametresFromAllSites('ServChirurgie')

  #ServicesChirurgie <- unique(fichier_structure$service[substr(fichier_structure$uma_locale,4,4)=='C'])

  # uma à la place des UH ?

  #Pourcentage des sejours chirurgicaux en ambulatoire
  if('pSejChir0j'%in%indicateurs){

    tb[['pSejChir0j']]<-round(
      table(df %>% dplyr::filter(duree==0, substr(cdghm,3,3)=='C', doublon==1) %>%  dplyr::select(pivot) )*100/
        table(df %>% dplyr::filter( substr(cdghm,3,3)=='C', doublon==1)  %>%  dplyr::select(pivot)),1
    )
  }

  #if('pSejChirOjUCA'%in%indicateurs){
  #  tb[['pSejChir0jUCA']]<-round(
  #    table(df$pivot[substr(df$cdghm,3,3)=='C'&df$duree_sej==0&df$uma_locale2%in%UHambu])*100/
  #      table(df$pivot[substr(df$cdghm,3,3)=='C'&df$duree_sej==0])
  #  )
  #}
  #Activite unite chirurige ambulatoire :
  #if('ActiviteUniteChirAmbu'%in%indicateurs){
  #  tb[['NbSejUCA']]<-table(df$pivot[df$uma_locale2%in%UHambu])
  #  tb[['pSejChirUCA']]<-round(table(df$pivot[df$uma_locale2%in%UHambu&substr(df$cdghm,3,3)=='C'])*100/tb[['NbSejUCA']])
  #  tb[['NbSejUCAservChir']]<-table(df$pivot[df$uma_locale2%in%UHambu&df$service%in%ServicesChirurgie])

  #}
  #Pourcentage d'actes marqueurs realises en ambulatoire
  #if('TitreActesMarqueurs'%in%indicateurs){

  # NOSactes<-subset(Actes,ACTE%in%ListesCCAM$ActesChirurgieAmbulatoireJuillet2012$ACTE)$NOS
  #AM<-get_liste(listes%>%filter(thematique == "Chir ambu : 55 GM")%>%select(nom_abrege)%>%flatten_chr(),var='actes')
  #sejAM <-df%>%filter(grepl(paste(AM,collapse = '|'),actes))

  #tb[['nbSejMarqueurs']]<-table(sejAM$pivot)

  #tb[['pSejMarqueurs0j']]<-round(
  #table(sejAM%>%filter(duree==0, doublon==1)%>%select(pivot))*100/tb[['nbSejMarqueurs']],1
  #)
  #}

  #Taux de rehospitalisation en chirurgie ambulatoire
  #TODO
  #if('TxRehopsChirAmbu'%in%indicateurs){
  #  tb[['TxRehopsChirAmbu']]<-NA
  #}
  #Nombre d'alerte cusum en chirurgie ambulatoire
  #TODO
  #if('NbAlerteCuSum'%in%indicateurs){
  #  tb[['NbAlerteCuSum']]<-NA
  #}
  if('Greffes'%in%indicateurs){

    tb[['Greffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,
                                                                ORGANE%in%c('AutoGreffeCellulesSouches',
                                                                            'AllogreffeCellulesSouches'))$GHM,
                                                doublon==1) %>%
                             select(pivot) )

  }


  if('AutoGreffes'%in%indicateurs){

    tb[['AutoGreffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,
                                                                    ORGANE%in%c('AutoGreffeCellulesSouches'))$GHM,
                                                    doublon==1) %>% select(pivot) )

  }

  if('AlloGreffes'%in%indicateurs){

    tb[['AlloGreffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,
                                                                    ORGANE%in%c('AllogreffeCellulesSouches'))$GHM,
                                                    doublon==1) %>%
                                 select(pivot) )
  }

  if('Transplantations'%in%indicateurs){

    tb[['Transplantations']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,
                                                                         ORGANE%in%c('Rein','Pancreas'))$GHM,
                                                         doublon==1) %>%
                                      select(pivot) )
  }
  if('TransplantationsRenales'%in%indicateurs){

    tb[['TransplantationsRenales']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,ORGANE%in%c('Rein'))$GHM,
                                                                doublon==1) %>%
                                             select(pivot) )
  }
  if('TransplantationsPancreas'%in%indicateurs){

    tb[['TransplantationsPancreas']]<-table(df %>% dplyr::filter(cdghm%in%subset(ListesGHM$GhmGreffes,
                                                                                 ORGANE%in%c('Pancreas'))$GHM,
                                                                 doublon==1) %>%
                                              select(pivot) )
  }

  if('TherapieCOD'%in%indicateurs){
    tb[['TherapieCOD']]<-table(df%>%filter(uma_locale2%in%c(76885), doublon==1)%>%select(pivot))
  }

  if('DiagnosticCOD'%in%indicateurs){
    tb[['DiagnosticCOD']]<-table(df%>%filter(uma_locale2%in%c(76689,76208), doublon==1)%>%select(pivot))

  }

  if('TherapieInflammatoire'%in%indicateurs){
    tb[['TherapieInflammatoire']]<-table(df%>%filter(uma_locale2%in%c(76686,76684), doublon==1)%>%select(pivot))

  }

  if('DiagnosticInflammatoire'%in%indicateurs){
    tb[['DiagnosticInflammatoire']]<-table(df%>%filter(uma_locale2%in%c(76687,76206), doublon==1)%>%select(pivot))

  }

  if('PCHIR'%in%indicateurs){

    tb[['PCHIR']]<-round(
      table(df %>% dplyr::filter(typehosp=="C", substr(cdghm,3,3)=='C', doublon==1) %>%  dplyr::select(pivot) )*100/
        table(df %>% dplyr::filter(typehosp=="C", doublon==1)  %>%  dplyr::select(pivot)),1
    )

  }

  ###########################################################################
  #Duree moyenne de sejour
  ###########################################################################

  #TODO
  if('DMS'%in%indicateurs){
    tb[['DMS']]<- round(with(df%>%filter(duree>0)%>%distinct(nofiness,ansor,cle_rsa,.keep_all = TRUE),
                             tapply(duree, pivot, mean)),1)
  }

  ###########################################################################
  #Duree moyenne des resumes
  ###########################################################################
  if('DMR'%in%indicateurs){
    tb[['DMR']]<-round(with(df%>%filter(duree>0),
                            tapply(dureesejpart, pivot, sum))/
                         table(df%>%distinct(nofiness,cle_rsa,ansor,service,.keep_all = T)%>%select(pivot)),
                       1)
  }
  ###########################################################################
  #Index de performance
  ###########################################################################
  if('IP'%in%indicateurs){
    tb[['IP']]<-as.data.frame(t(sapply(as.numeric(levels(df$pivot)),function(x)IP_SEJOUR(df%>%filter(ansor == x)))))

    names(tb[['IP']])<-as.numeric(levels(df$pivot))
  }

  ###########################################################################
  #Index de performance service
  ###########################################################################
  if('IP_SERV'%in%indicateurs){
    tb[['IP_SERV']]<-as.data.frame(t(sapply(as.numeric(levels(df$pivot)),function(x)IP(df%>%filter(ansor == x)))))

    names(tb[['IP_SERV']])<-as.numeric(levels(df$pivot))
  }
  ###########################################################################
  #Niveau de severite
  ###########################################################################
  if('NS'%in%indicateurs){
    tb[['NS']]<-round(
      table(df%>%filter(duree>3,substr(cdghm,6,6)%in%2:4, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }
  if('NS_2'%in%indicateurs){
    tb[['NS_2']]<-round(
      table(df%>%filter(duree>3,substr(cdghm,6,6)==2, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }


  if('NS_3'%in%indicateurs){
    tb[['NS_3']]<-round(
      table(df%>%filter(duree>3,substr(cdghm,6,6)==3, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }

  if('NS_4'%in%indicateurs){
    tb[['NS_4']]<-round(
      table(df%>%filter(duree>3,substr(cdghm,6,6)==4, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }



  #Nombre de journnees de reanimation et USI en reanimation
  #	info dans dataframe recettes

  ###########################################################################
  if('nbsuprea'%in%indicateurs){

    tb[['nbsuprea']]<-with(df,tapply(nbsuprea_repa,pivot,sum))

  }


  if('nssir'%in%indicateurs){

    tb[['nssir']]<-with(df,tapply(nssir_repa,pivot,sum))

  }


  #Pourcentage des journees effectuees dans une UH de reanimation avec suplement rea
  #	UH reanimation declaree dans la variable DataHop
  ############################################################################

  if('p_nbsuprea'%in%indicateurs){

    tb[['p_nbsuprea']]<-round(tb[['nbsuprea']]*100/( tb[['nbsuprea']]+ tb[['nssir']]))

  }


  #Nombre de journees soins intensif hors reanimation
  ############################################################################
  if('nsstf'%in%indicateurs){

    tb[['nsstf']]<-with(df,tapply(nssir_repa+nsstf_repa,pivot,sum))
  }


  if('stf_hr'%in%indicateurs){

    tb[['stf_hr']]<-tapply(df$nsstf_repa,df$pivot,sum)

  }

  #Nombre de journees surveillance continue
  ############################################################################
  if('nbsupsrc'%in%indicateurs){

    tb[['nbsupsrc']]<-tapply(df$nbsupsrc_repa,df$pivot,sum)

  }

  #Pourcentage des journees avec Sup USC
  ############################################################################
  if('p_nbsupsrc'%in%indicateurs){

    tb[['p_nbsupsrc']]<-round(tb[['nbsupsrc']]*100/
                                with(df%>%filter(substr(typeaut,1,2)%in% c("03","14")),
                                     tapply(dureesejpart,pivot,sum)),
                              1)
  }

  #Nombre de dialyse
  ############################################################################
  #TODO
  if('dialhosp'%in%indicateurs){

    tb[['dialhosp']]<-with(df%>%mutate(adial = stringr::str_count(actes, actesdialyse)),
                           tapply(adial,pivot,sum))
  }

  #Poids moyen du cas traite :
  ##Moyenne des tarifs du GHS
  ############################################################################

  if('rec_base'%in%indicateurs){
    tb[['rec_base']]<-
      round(
        with(df%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_base,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_hc'%in%indicateurs){
    tb[['rec_base_hc']]<-
      round(
        with(df%>%filter(typehosp=='C')%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_base,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa'%in%indicateurs){
    tb[['rec_base_repa']]<-
      round(
        with(df,tapply(valopmctmonotime1,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa_hc'%in%indicateurs){
    tb[['rec_base_repa_hc']]<-
      round(
        with(df%>%filter(typehosp=="C"),
             tapply(valopmctmonotime1,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa_mono_uma'%in%indicateurs){
    tb[['rec_base_repa_mono_uma']]<-
      round(
        with(df%>%filter(typehosp=="C",nbrum==1),
             tapply(valopmctmonotime1,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa_multi_uma'%in%indicateurs){
    tb[['rec_base_repa_multi_uma']]<-
      round(
        with(df%>%filter(typehosp=="C",nbrum!=1),
             tapply(valopmctmonotime1,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_hp'%in%indicateurs){
    tb[['rec_base_hp']]<-
      round(
        with(df%>%filter(typehosp=="P"),
             tapply(valopmctmonotime1,pivot,sum,na.rm=T))

      )
  }

  if('pmct_hc'%in%indicateurs){

    tb[['pmct_hc']] <- round( tb[['rec_base_hc']] / sum(df$doublon[df$typehosp=="C"]) )

  }

  if('pmct_repa_hc'%in%indicateurs){

    tb[['pmct_repa_hc']] <- round( tb[['rec_base_repa_hc']] / sum(df$doublon[df$typehosp=="C"]) )

  }

  if('pmct_mono_uma'%in%indicateurs){

    tb[['pmct_mono_uma']]<- round( tb[['rec_base_repa_mono_uma']] / sum(df$doublon[df$typehosp=="C"&df$nbrum==1]) )

  }

  if('pmct_repa_multi_uma'%in%indicateurs){

    tb[['pmct_repa_multi_uma']]<-round( tb[['rec_base_repa_multi_uma']] / sum(df$doublon[df$nbrum!=1]) )
  }


  #PMCT des HDJ
  ############################################################################
  if('pmct_hp'%in%indicateurs){

    tb[['pmct_hp']]<-     round( tb[['rec_base_hp']] / sum(df$doublon[df$typehosp=="P"]) )
  }
  #Somme des supplements
  ############################################################################

  if('SuppTotal'%in%indicateurs){

    tb[['SuppTotal']]<-round(
      with(df,tapply(rec_sup_repa,pivot,sum,na.rm=T))

    )
  }

  if('SuppSIRea'%in%indicateurs){

    tb[['SuppSIRea']]<-round(
      with(df,tapply(rec_sir_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppSIhorsRea'%in%indicateurs){

    tb[['SuppSIhorsRea']]<-round(
      with(df,tapply(rec_stf_hr_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppRea'%in%indicateurs){

    tb[['SuppRea']]<-round(
      with(df,tapply(rec_rea_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppSRC'%in%indicateurs){

    tb[['SuppSRC']]<-round(
      with(df,tapply(rec_src_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppReaPED'%in%indicateurs){

    tb[['SuppReaPED']]<-round(
      with(df,tapply(rec_rep_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppNN1'%in%indicateurs){

    tb[['SuppNN1']]<-round(
      with(df,tapply(rec_nn1_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppNN2'%in%indicateurs){

    tb[['SuppNN2']]<-round(
      with(df,tapply(rec_nn2_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppNN3'%in%indicateurs){

    tb[['SuppNN3']]<-round(
      with(df,tapply(rec_nn3_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppCaisHyp'%in%indicateurs){

    tb[['SuppCaisHyp']]<-round(
      with(df,tapply(rec_caishyp_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppPO'%in%indicateurs){

    tb[['SuppPO']]<-round(
      with(df,tapply(rec_po_tot_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppANT'%in%indicateurs){

    tb[['SuppANT']]<-round(
      with(df,tapply(rec_ant_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppRDT'%in%indicateurs){

    tb[['SuppRDT']]<-round(
      with(df,tapply(rec_rdt_tot_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppRAP'%in%indicateurs){

    tb[['SuppRAP']]<-round(
      with(df,tapply(rec_rap_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppDial'%in%indicateurs){

    tb[['SuppDial']]<-round(
      with(df,tapply(rec_dialhosp_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      with(df,tapply(rec_aph_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      with(df,tapply(rec_aph_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      with(df,tapply(rec_aph_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppSDC'%in%indicateurs){

    tb[['SuppSDC']]<-round(
      with(df,tapply(rec_sdc_repa,pivot,sum,na.rm=T))

    )
  }
  if('SuppRehosp'%in%indicateurs){

    tb[['SuppRehosp']]<-round(
      with(df,tapply(rec_rehosp_ghm_repa,pivot,sum,na.rm=T))

    )
  }
  ############################################################################
  ####Exploitation des donnees de suivi
  ############################################################################

  #Tx EXH des CRH HC
  ############################################################################

  #if('ExhCrhHc8'%in%indicateurs){
  #
  #  if(nrow(DataCrh)==0){
  #    tb[['ExhCrhHc8']]<-NA
  #  }

  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$crh-DataCrh$ds<9)
  #    DataCrh$P8<-NA
  #    DataCrh$P8[o]<-'O'
  #    o<-which(DataCrh$type_sej=='HC')
  #    tb[['ExhCrhHc8']]<-tapply(DataCrh$P8[o],DataCrh$pivot[o],function(x){round(length(which(!is.na(x)))*100/length(x))})
  #  }
  #}

  #Tx EXH des CRH HC
  ############################################################################

  #if('ExhCrhHc15'%in%indicateurs){
  #
  #  if(nrow(DataCrh)==0)tb[['ExhCrhHc15']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$crh-DataCrh$ds<16)
  #    DataCrh$P15<-NA
  #    DataCrh$P15[o]<-'O'
  #    o<-which(DataCrh$type_sej=='HC'&DataCrh$SERVICE%in%ServiceCRH)
  #    tb[['ExhCrhHc15']]<-tapply(DataCrh$crh[o],DataCrh$pivot[o],function(x){round(length(which(!is.na(x)))*100/length(x))})
  #  }
  #}

  #Delais de production HCnm
  ############################################################################
  #if('DelaisCrhHc'%in%indicateurs){
  #  if(nrow(DataCrh)==0)tb[['DelaisCrhHc']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$type_sej=='HC')
  #    DataCrh$del<-NA
  #    DataCrh$del[o]<-DataCrh$crh[o]-DataCrh$ds[o]
  #    tb[['DelaisCrhHc']]<-round(tapply(DataCrh$del[o],DataCrh$pivot[o],mean),1)
  #  }
  #}

  #Tx EXH des CRH HdJ
  ############################################################################

  #if('ExhCrhHp8'%in%indicateurs){
  #
  #  if(nrow(DataCrh)==0)tb[['ExhCrhHp8']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$crh-DataCrh$ds<9)
  #    DataCrh$P8<-NA
  #    DataCrh$P8[o]<-'O'
  #    o<-which(DataCrh$type_sej!='HC')
  #    tb[['ExhCrhHp8']]<-tapply(DataCrh$P8[o],DataCrh$pivot[o],function(x){round(length(which(!is.na(x)))*100/length(x))})
  #  }
  #}

  #Tx EXH des CRH HdJ
  ############################################################################

  #if('ExhCrhHp15'%in%indicateurs){
  #
  #  if(nrow(DataCrh)==0)tb[['ExhCrhHp15']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$crh-DataCrh$ds<16)
  #    DataCrh$P15<-NA
  #    DataCrh$P15[o]<-'O'
  #    o<-which(DataCrh$type_sej!='HC')
  #    tb[['ExhCrhHp15']]<-tapply(DataCrh$crh[o],DataCrh$pivot[o],function(x){round(length(which(!is.na(x)))*100/length(x))})
  #  }
  #}

  #Delais de production HdJ
  ############################################################################
  #if('DelaisCrhHp'%in%indicateurs){
  #  if(nrow(DataCrh)==0)tb[['DelaisCrhHp']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0){
  #    o<-which(!is.na(DataCrh$crh)&DataCrh$type_sej!='HC')
  #    DataCrh$del<-NA
  #    DataCrh$del[o]<-DataCrh$crh[o]-DataCrh$ds[o]
  #    tb[['DelaisCrhHp']]<-round(tapply(DataCrh$del[o],DataCrh$pivot[o],mean),1)
  #  }
  #}



  #Tx EXH des CRO
  ############################################################################

  #if('ExhCRO'%in%indicateurs){
  #
  #  if(nrow(DataCrh)==0)tb[['ExhCRO']]<-table(DataCrh$pivot)
  #  if(nrow(DataCrh)!=0)tb[['ExhCRO']]<-tapply(DataCrh$cro,DataCro$pivot,function(x){round(length(which(!is.na(x)))*100/length(x))})
  #}

  #Tx EXH codage des actes chir
  ############################################################################
  #J0
  #if('pActesChirJ0'%in%indicateurs){
  #  tb[['pActesChirJ0']]<-round(table(tmpAB$pivot[which(tmpAB$Del<1)])*100/table(tmpAB$pivot),digit=1)
  #}

  #J3
  #if('pActesChirJ0'%in%indicateurs){
  #  tb[['pActesChirJ3']]<-round(table(tmpAB$pivot[which(tmpAB$Del<4)])*100/table(tmpAB$pivot),digit=1)
  #}



  #Exhaustivite du codage 1 et 15 du mois en HC et HP
  ############################################################################
  #if('ExhCodageHC1'%in%indicateurs){
  #  tmp<-DataExh[as.numeric(format(DataExh$date_exh,'%d'))==1&DataExh$hc_att!=0,]
  #  tmp$p<-tmp$hc_lie*100/tmp$hc_att
  #  tb[['ExhCodageHC1']]<- round(tapply(tmp$p,tmp$pivot,mean))
  #}
  #if('ExhCodageHC15'%in%indicateurs){
  #  tmp<-DataExh[as.numeric(format(DataExh$date_exh,'%d'))==15&DataExh$hc_att!=0,]
  #  tmp$p<-tmp$hc_lie*100/tmp$hc_att
  #  tb[['ExhCodageHC15']]<- round(tapply(tmp$p,tmp$pivot,mean))
  #}

  #if('ExhCodageHP1'%in%indicateurs){
  #
  #  tmp<-DataExh[as.numeric(format(DataExh$date_exh,'%d'))==1&DataExh$hdj_att!=0,]
  #  tmp$p<-tmp$hdj_lie*100/tmp$hdj_att
  #  tb[['ExhCodageHP1']]<-  round(tapply(tmp$p,tmp$pivot,mean))
  #}
  #if('ExhCodageHP15'%in%indicateurs){
  #  tmp<-DataExh[as.numeric(format(DataExh$date_exh,'%d'))==15&DataExh$hdj_att!=0,]
  #  tmp$p<-tmp$hdj_lie*100/tmp$hdj_att
  #  tb[['ExhCodageHP15']]<-  round(tapply(tmp$p,tmp$pivot,mean))
  #}


  #if('pSejCibles'%in%indicateurs){
  #  tb[['pSejCibles']]<-round(
  #    table(df$pivot[!duplicated(df$nas)&df$nas%in%NdaCiblage])*100/
  #      table(df$pivot[df$typehosp=="C"]),digit=1
  #  )
  #}
  #########################################################################
  ###Activite de cancerologie
  #########################################################################
  if('ActiviteCancero'%in%indicateurs){

    df_cancer<-inner_join(df,cancer_rsa%>%select(-dp,-nip,-nda,-nas))


    tb[['NbPat']]<-with(df_cancer,tapply(ipp, pivot, nb_unique))
    tb[['NbNxPat']]<-with(df_cancer%>%filter(nx_pat == 'O'),tapply(ipp, pivot, nb_unique))
    tb[['NbSejHC']]<- table(df_cancer%>%dplyr::filter(typehosp=="C",doublon == 1)%>%dplyr::select(pivot))
    tb[['NbSejHP']]<-table(df_cancer%>%dplyr::filter(typehosp=="P",doublon == 1)%>%dplyr::select(pivot))
    #tb[['pSejHC']]<-round(table(DataCancero$pivot[!duplicated(DataCancero$NORSS)&DataCancero$TypeDossier%in%c('A','N')])*100/
    #                        table(df$pivot[!df$typehosp=="P"]))
    #
    #tb[['pSejHP']]<-round(table(DataCancero$pivot[which(DataCancero$TypeDossier%in%c('I','S'))])*100/
    #                        table(df$pivot[df$typehosp=="P"]))
    tb[['NbSejChir']]<-table(df_cancer%>%dplyr::filter(substr(cdghm,3,3)=="C",doublon == 1)%>%dplyr::select(pivot))

    tb[['NbPatientsParAppareilInca']]<-with(df_cancer,tapply(ipp, list(APPAREIL,pivot), nb_unique))

    if(length(dimnames( tb[['NbPatientsParAppareilInca']])[[1]])>0){

      dimnames( tb[['NbPatientsParAppareilInca']])[[1]]<-paste(' -',tolower(dimnames( tb[['NbPatientsParAppareilInca']])[[1]]))
    }

    tb[['ActiviteParAppareilInca']]<-with( df_cancer %>% filter( doublon == 1 ),
                                           tapply( cle_rsa, list( APPAREIL, pivot ), nb_unique ) )

    if(length(dimnames( tb[['ActiviteParAppareilInca']])[[1]])>0){

      dimnames( tb[['ActiviteParAppareilInca']])[[1]]<-paste(' -',tolower(dimnames( tb[['ActiviteParAppareilInca']])[[1]]))
    }
  }
  #########################################################################
  #####Soins Palliatifs
  #########################################################################
  if('SP'%in%indicateurs){

    df_sp <- df%>% filter( dp=='Z515'| dr=='Z515'| grepl('Z515',das) )

    tb[['SejSp']] <- table( df_sp %>% filter(doublon==1) %>% select(pivot) )

    tb[['JourneesSp']] <- with( df_sp, tapply( dureesejpart, pivot, sum, na.rm=T ) )

    tb[['NbPatSP']] <- with( df_sp, tapply(ipp, pivot, nb_unique) )

    tb[['SejSpGhmNonSP']]<-table( df_sp %>% filter( substr(cdghm,1,5)!='23Z02', doublon == 1 )%>% select(pivot) )

    tb[['SejSpLitsStandards']]<-table( df_sp%>%filter( noghs%in%c('7991','7992') )%>% select(pivot) )
    tb[['SejSpLitsDedies']]<-table( df_sp%>%filter(noghs%in%c('7993'))%>%select(pivot))
    tb[['NbPatSpLitsDedies']]<-with(df_sp%>%filter(noghs%in%c('7993')),tapply(ipp,pivot,nb_unique))
    tb[['NbLitsDediesUtilises']]<-round(with(df_sp,tapply(dureesejpart2,pivot,sum,na.rm=T))/
                                          with(df_sp,tapply(date_max_pivot-date_min_pivot,pivot,min)),
                                        1)

    tb[['SejSpUsp']]<-table(df_sp%>%filter(noghs%in%c('7994'))%>%select(pivot))

    tb[['JourneesSpLitsStandards']]<- with(df_sp%>%filter(noghs%in%c('7991','7992')),
                                           tapply(dureesejpart,pivot,sum,na.rm=T))
    tb[['JourneesSpLitsDedies']]<- with(df_sp%>%filter(noghs%in%c('7993')),
                                        tapply(dureesejpart,pivot,sum,na.rm=T))

    tb[['JourneesSpUsp']]<- with(df_sp%>%filter(noghs%in%c('7994')),
                                 tapply(dureesejpart,pivot,sum,na.rm=T))

    tb[['NbDCD']]<-table( df %>% filter( mdsoue == 9 ) %>%
                            distinct( nas, .keep_all = T ) %>% select( pivot ) )

    tb[['NbDCD_SP']]<-table( df_sp %>% filter( mdsoue == 9 ) %>% select( pivot ) )

    tb[['NbDCDNonSP']]<-tb[['NbDCD']] -  tb[['NbDCD_SP']]

    tb[['NbDCDcancer']]<-table(df_cancer%>%filter(mdsoue == 9) %>%select(pivot))

    tb[['NbDCDspGhmNonSP']]<-table(df%>%filter(mdsoue == 9,substr(cdghm,1,5)!='23Z02') %>% select(pivot))

    tb[['NbDCDspGhmSP']]<-table(df%>%filter(mdsoue == 9,substr(cdghm,1,5)=='23Z02') %>% distinct(nas,.keep_all = T)%>%select(pivot))

    tb[['NbDCDspLitsStandards']]<-table( df%>% filter( mdsoue == 9, noghs %in% c('7991','7992')) %>% select(pivot) )

    tb[['NbDCDspLitsDedies']]<-table( df %>% filter(mdsoue == 9 , noghs%in%c('7993') ) %>% select(pivot) )

    tb[['NbDCDspUsp']]<-table( df %>% filter(mdsoue == 9, noghs%in%c('7994') ) %>% select(pivot) )

  }
  ########################################################################################
  # Infectieux
  ########################################################################################
  # if('Infectieux'%in%Indicateurs){
  #
  #   tb[['NbSejoursInfectieux']]<-table(Data$A[!duplicated(Data$NIP)&Data$NOS%in%InDiagnostics(ListesCIM$InfectionsOtoctones$CODE)&Data$NDA%in%NDA_HC])
  #    tb[['NbSejoursInfectieuxU2I']]<-table(Data$A[!duplicated(Data$NDA)&Data$NDA%in%u2i$NDA&Data$NDA%in%NDA_HC])
  #    tb[['NbSejoursInfectionBMR']]<-table(Data$A[!duplicated(Data$NIP)&Data$NOS%in%InDiagnostics(ListesCIM$ResistanceAtb$CODE)&Data$NDA%in%NDA_HC])
  #    tb[['NbSejoursPortageBMR']]<-rep(NA,length(levels(Data$A)))
  #  }

  ########################################################################################
  # Douleur
  ########################################################################################

  if('Douleur'%in%indicateurs){

    tmp<-df%>%filter(dp%in%ListesCIM$Douleur$CODE|dr%in%ListesCIM$Douleur$CODE|
                       grepl(paste(ListesCIM$Douleur$CODE,collapse = '|'),das))

    tb[['PatientsDouleur']]<-with( tmp,tapply(ipp,pivot,nb_unique) )

    tb[['SejoursDouleur']]<-table( tmp%>% filter(doublon ==1) %>% select(pivot))

  }

  ########################################################################################
  #Autres pathologies chroniques
  ########################################################################################
  if('AutresPathologiesChroniques'%in%indicateurs){

    ###VIH
    tmp<-df%>%filter(dp%in%ListesCIM$VIH$CODE|dr%in%ListesCIM$VIH$CODE|
                       grepl(paste(ListesCIM$VIH$CODE,collapse = '|'),das))

    tb[['PatientsVIH']]<- with( tmp, tapply(ipp,pivot,nb_unique) )

    tb[['SejoursVIH_HC']]<-table( tmp%>%filter(typehosp=='C',doublon ==1) %>% select(pivot) )

    tb[['SejoursVIH_HP']]<-table(tmp%>%filter(typehosp=='P')%>%select(pivot))

    #Diabète
    tmp<-df%>%filter(dp%in%ListesCIM$Diabete$CODE|dr%in%ListesCIM$Diabete$CODE|
                       grepl(paste(ListesCIM$Diabete$CODE,collapse = '|'),das))

    tb[['PatientsDiabete']]<-with(tmp, tapply(ipp,pivot,nb_unique) )

    tb[['SejoursDiabete_HC']]<-table(tmp %>% filter(typehosp=='C', doublon==1 ) %>% select(pivot) )

    tb[['SejoursDiabete_HP']]<-table(tmp %>% filter(typehosp=='P') %>% select(pivot) )

    #Insuffisance rénale chronique
    tmp<-df%>%filter(dp%in%ListesCIM$InsuffisanceRenaleChronique$CODE|dr%in%ListesCIM$InsuffisanceRenaleChronique$CODE|
                       grepl(paste(ListesCIM$InsuffisanceRenaleChronique$CODE,collapse = '|'),das))

    tb[['PatientsInsRenaleChronique']]<-with( tmp, tapply(ipp,pivot,nb_unique) )

    tb[['SejoursInsRenaleChronique_HC']]<-table( tmp %>% filter(typehosp=='C', doublon==1) %>% select(pivot) )

    tb[['SejoursInsRenaleChronique_HP']]<-table( tmp %>% filter(typehosp=='P') %>% select(pivot) )

    #Insuffisance respiratoire chronique
    tmp<-df%>%filter(dp%in%ListesCIM$InsuffisanceRespiratoireChronique$CODE|dr%in%ListesCIM$InsuffisanceRespiratoireChronique$CODE|
                       grepl(paste(ListesCIM$InsuffisanceRespiratoireChronique$CODE,collapse = '|'),das))

    tb[['PatientsInsRespiratoireChronique']]<-with( tmp, tapply(ipp,pivot,nb_unique) )

    tb[['SejoursInsRespiratoireChronique_HC']]<-table( tmp %>% filter(typehosp=='C', doublon ==1) %>% select(pivot) )

    tb[['SejoursInsRespiratoireChronique_HP']]<-table( tmp %>% filter(typehosp=='P') %>% select(pivot) )

  }
  #########################################################################################
  #Reanimation
  #########################################################################################
  ###Pourcentage de patients ventilés

  if('IndicateursRea'%in%indicateurs){

    tmp_rea<-df %>% dplyr::filter(typeaut=="01A")

    tb[['HCtotAutRea']]<-table(tmp_rea%>% filter(doublon == 1) %>%select(pivot) )

    ##Re-hospitalisation precoses
    #df3<-merge(Data[o,c('NIP','SERVICE','DateEntreeResume','DateSortieResume','A')],
    #           Sejours2[,c('NIP','SERVICE','DateEntreeResume','DateSortieResume','ModeSortieResume')],
    #           by=c('NIP','SERVICE'),suffixes = c('.1','.2'))
    #df3<-df3[df3$DateSortieResume.1<df3$DateEntreeResume.2,]
    #df3$delai<-difftime(df3$DateEntreeResume.2,df3$DateSortieResume.1,units="hours")

    #tb[['NbRehsopitPrecoceRea']]<-table(df3$pivot[df3$delai<48])
    #tb[['TauxRehsopitPrecoceRea']]<-round(table(df3$pivot[df3$delai<48])*100/as.numeric(tb[['HCtot2']]),2)

    tb[['IGSmoyen']]<-round( with( tmp, tapply( as.numeric(igs), pivot, mean, na.rm=T) ), 1 )

    tb[['IGS0']]<-table(tmp %>% filter( as.numeric(igs)==0 ) %>% select( pivot ) )

    l_actes = paste(c(as.character(ListesCCAM$VentilationIntubation$ACTE),
                      as.character(ListesCCAM$VentilationTracheotomie$ACTE),
                      as.character(ListesCCAM$VNI$ACTE)),
                    collapse = '|')

    tb[['NbSejoursPatientVentiles']]<- table(df %>% filter( grepl( l_actes, actes ) )
                                             %>% filter( doublon == 1 )
                                             %>% select( pivot ) )

    tb[['pPatientVentiles']]<- round( table(df %>%filter(grepl(l_actes,actes))
                                            %>%filter(doublon == 1)
                                            %>%select(pivot) ) *100 / tb[['HCtotAutRea']],
                                      1)

    ###Nombre de patients ventiles plus de 48h
    tb[['NbSejoursPatientVentiles48h']]<- table( tmp %>% mutate(nb_actes = stringr::str_count(actes, l_actes))
                                                 %>% filter(nb_actes > 1)
                                                 %>% filter(doublon == 1)
                                                 %>% select(pivot) )


    tb[['NbJourneesVentilesInt']]<-with( tmp %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                         tapply(nb_actes,pivot,sum,na.rm=T))

    l_actes = paste(ListesCCAM$VentilationIntubation$ACTE,collapse = '|')

    tb[['NbSejoursPatientVentilesInt']] <- table( tmp %>% filter(grepl(l_actes,actes))
                                                  %>% filter(doublon == 1)
                                                  %>% select(pivot) )



    l_actes = paste(ListesCCAM$VNI$ACTE,collapse = '|')

    tb[['NbSejoursPatientVentilesMasque']]<-table( tmp %>% filter(grepl(l_actes,actes))
                                                   %>% filter(doublon == 1)
                                                   %>% select(pivot) )

    l_actes = paste(ListesCCAM$VentilationTracheotomie$ACTE,collapse = '|')
    tb[['NbSejoursPatientVentilesTrac']]<- table( tmp %>% filter(grepl(l_actes,actes))
                                                  %>% filter(doublon == 1)
                                                  %>% select(pivot) )

    ###Pourcentage de patients dialyses
    l_actes = paste(c(ListesCCAM$ActesDialyseIRADiscontinue$ACTE,
                      ListesCCAM$ActesDialyseIRCDiscontinue$ACTE,
                      ListesCCAM$ActesDialyseIRAContinue$ACTE),
                    collapse = '|')

    tb[['NbSejoursPatientsDialyses']]<-table(df %>% filter( grepl( l_actes, actes ) )
                                             %>% filter( doublon == 1 )
                                             %>% select( pivot ) )

    tb[['pPatientsDialyses']]<-round( tb[['NbSejoursPatientsDialyses']]*100 / tb[['HCtotAutRea']] )

    ###Nb patients EER  + EER continue
    l_actes = paste( c(ListesCCAM$ActesDialyseIRADiscontinue$ACTE,
                       ListesCCAM$ActesDialyseIRCDiscontinue$ACTE),
                     collapse = '|')

    tb[['NbjourneesEERcontinue']]<-with( tmp %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                         tapply(nb_actes,pivot,sum,na.rm=T))

    ##Nb seance EER discontinue (seances de dialyse)
    l_actes = paste( ListesCCAM$ActesDialyseContinue$ACTE,
                     collapse = '|')

    tb[['NbSeancesDialyses']]<- with( tmp %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                      tapply(nb_actes,pivot,sum,na.rm=T))


    ##Nb d'actes d'apherese

    l_actes = paste( ListesCCAM$ActesApherese$ACTE,
                     collapse = '|')

    tb[['NbApherese']]<-with( tmp %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                              tapply(nb_actes,pivot,sum,na.rm=T))


    ###Nombre de patients sous catecholamine
    l_actes = paste( c('EQLF001','EQLF003'),
                     collapse = '|')

    tb[['NbSejoursPatientCatecholamine']]<-table(df %>% filter( grepl( l_actes, actes ) )
                                                 %>% filter( doublon == 1 )
                                                 %>% select( pivot ) )

    ###Nombre suppleance pour prelevement d'organe
    nosSuppPvtOrgane<-InActes(c())
    tb[['NbSejoursPatientsSuppPvtOrgane']]<-table(df %>% filter( grepl( 'EQMF003', actes ) )
                                                  %>% filter( doublon == 1 )
                                                  %>% select( pivot ) )

    ###Pourcentage de deces en reanimation

    tb[['pDcdRea']]<- round(table( df %>% filter(mdsoue == 9, typeaut=="01A" ) %>% select(pivot) )*100/
                              tb[['HCtotAutRea']],
                            digit=1)

    tb[['pDcdHopRea']]<-round(table( inner_join(df, tmp_rea%>%select(nofiness,cle_rsa,ansor)%>%distinct(nofiness,cle_rsa,ansor))
                                     %>%filter(mdsoue == 9, typeaut=="01A" )
                                     %>% select(pivot) )*100/
                                tb[['HCtotAutRea']],
                              digit=1)

  }
  #########################################################################################
  #Tableau de Bord DIM
  #########################################################################################

  ###########################################################################
  #Sejours extremes (bornes)
  ###########################################################################
  if('NbSejDMSp30j'%in%indicateurs){

    tb[['NbSejDMSp30j']]<-table( df%>%filter(duree > 30) %>% filter(doublon == 1) %>% select(pivot) )

  }

  if('NbSejBH'%in%indicateurs){

    tmp = df%>%filter(duree>bh)

    tb[['NbSejBH']]<-table( tmp %>% filter(doublon == 1) %>% select(pivot) )
  }

  if('pSejBH'%in%indicateurs){
    tb[['pSejBH']]<-round(tb[['NbSejBH']] * 100 / tb[['HCtot']]
                          ,digit=1)
  }

  if('nb_j_bh'%in%indicateurs){

    tb[['nb_j_bh']]<-with( tmp , tapply(nbjrbs,pivot,sum,na.rm=T) )
  }

  if('nb_j_bh_repa'%in%indicateurs){

    tb[['nb_j_bh_repa']]<-with( tmp%>% mutate(nbjsup = nbjrbs*coeftime), tapply(nbjsup,pivot,sum,na.rm=T) )
  }

  #if('pSejBHrevus'%in%indicateurs){
  #  tb[['pSejBHrevus']]<-
  #    tapply(df$duree_sej[!duplicated(df$norss)&df$duree_sej>Data$BH&df$nas%in%Nestor$nas],
  #           df$pivot[!duplicated(df$norss)&df$duree_sej>Data$BH&df$nas%in%Nestor$nas],sum)
  #
  #}

  if('NbSejSupDMS'%in%indicateurs){

    tb[['NbSejSupDMS']]<-table( df %>% filter(duree>dms_bn, doublon == 1 ) %>% select(pivot) )

  }

  if('pSejSupDMS'%in%indicateurs){
    tb[['pSejSupDMS']]<-round( tb[['NbSejSupDMS']]*100 / tb[['HCtot']], 1)

  }

  #if('pSejDMSrevus'%in%indicateurs){
  #  tb[['pSejDMSrevus']]<-round(
  #    table(df$pivot[!duplicated(df$nas)&df$duree_sej>Data$DMS&df$nas%in%nas_hc&df$nas%in%Nestor$nas])*100/
  #      table(df$pivot[!duplicated(df$nas)&df$duree_sej>Data$DMS&df$nas%in%nas_hc])
  #  )
  #}
  ###NbSejRevus
  #if('NbSejRevus'%in%indicateurs){
  #  tb[['NbSejRevus']]<-
  #    table(df$pivot[df$nas%in%c(NdaCiblage, NdaErreurs,NdaOptimisation)&df$nas%in%nas_hc])
  #}

  #if('pSejRevus'%in%indicateurs){
  #  tb[['pSejRevus']]<-round(
  #    table(df$pivot[df$nas%in%c(NdaCiblage, NdaErreurs,NdaOptimisation)&df$nas%in%nas_hc])*100/
  #      table(df$pivot[df$nas%in%nas_hc])
  #  )
  #}

  # if('pSejErreurs'%in%indicateurs){
  #   tb[['pSejErreurs']]<-round(
  #     table(df$pivot[df$nas%in%NdaErreurs&df$nas%in%nas_hc])*100/
  #       table(df$pivot[df$nas%in%nas_hc])
  #   )
  # }
  #
  # if('pSejOptimisation'%in%indicateurs){
  #   tb[['pSejOptimisation']]<-round(
  #     table(df$pivot[df$nas%in%NdaOptimisation&df$nas%in%nas_hc])*100/
  #       table(df$pivot[df$nas%in%nas_hc]))
  # }
  #
  # if('NbSejCiblage'%in%indicateurs){
  #   tb[['NbSejCiblage']]<-table(df$pivot[df$nas%in%NdaCiblage&df$nas%in%nas_hc])
  # }
  #
  # if('pSejCiblage'%in%indicateurs){
  #
  #   tb[['pSejCiblage']]<-round(
  #     table(df$pivot[df$nas%in%NdaCiblage&df$nas%in%nas_hc])*100/
  #       table(df$pivot[df$nas%in%nas_hc]))
  #
  # }

  if('pSejBB'%in%indicateurs){

    tb[['pSejBB']]<-round( table( df%>%filter( sejinfbi>0, doublon == 1 )%>%select(pivot) )*100 /  tb[['HCtot']], digit=1)

  }
  if('MontantEXB'%in%indicateurs){

    tb[['MontantEXB']]<-with(df, tapply(rec_exb,pivot,sum,na.rm=T) )
  }

  ###Montant des pertes dues aux sejours duree > BH

  if('pertes_bh'%in%indicateurs){

    tb[['pertes_bh']]<- round(with(df%>%mutate(     rec_moy = rec_base / dms_bn,
                                                    diff = rec_exh - (nbjrbs * rec_moy) ),
                                   tapply(diff,ansor,sum,na.rm=T)))


  }

  if('pertes_bh_repa'%in%indicateurs){

    tb[['pertes_bh_repa']]<- round(with(df%>%mutate(nbjsup = nbjrbs*coeftime,
                                                    rec_moy = rec_base / dms_bn,
                                                    diff = rec_exh - (nbjsup * rec_moy) ),
                                        tapply(diff,ansor,sum,na.rm=T)))


  }




  TB<-sapply(names(tb),function(x)tb[[x]]<<-prep_tb(tb[[x]]))


  t<-as.data.frame(t(rep(NA,(length(levels(df$pivot))+2))))

  names(t)<-c('niv','libelle',c((min(as.numeric(levels(df$pivot))):max(as.numeric(levels(df$pivot))))))

  levels_pivot = as.character(c((min(as.numeric(levels(df$pivot))):max(as.numeric(levels(df$pivot))))))

  for(i in indicateurs){

    if(i%in%references$var){
      t[i,'niv']<-references%>%filter(var==i)%>%select(niv)%>%flatten_chr()
      t[i,'libelle']<-references%>%filter(var==i)%>%select(libelle)%>%flatten_chr()
    }

    if(i%in%names(tb)){

      if(is.null(length(dim(tb[[i]]))))t[i,levels_pivot]<-'-'

      if(!is.null(length(dim(tb[[i]])))){

        #valeur indicateur = vector (ex : nb hospitalisation)
        if(length(dim(tb[[i]]))<2 & length(dim(tb[[i]])) > 0 ){

          t[i,levels_pivot]<-as.character(prep_tb(tb[[i]]))
        }

        #valeur indicateur = tableau (ex : nb hosp. par type de cancer)
        if(length(dim(tb[[i]]))==2 & length(dim(tb[[i]])) > 0 ){

          if(nrow(tb[[i]])==1) t[i,levels_pivot]<-as.character(tb[[i]])

          if(nrow(tb[[i]])>1){

            t2<-as.data.frame.matrix(tb[[i]])

            t2$niv=references%>%filter(var==i)%>%select(niv)%>%flatten_chr()

            t2$libelle<-row.names(t2)

            t<-rbind(t,t2 [,c('niv','libelle',levels_pivot)])
          }
        }
      }
    }
  }
  t<-diff_tb(t)
  lignesexclusionPdiff<-which(t$LIBELLE%in%exclusionPdiff)
  if(length(lignesexclusionPdiff))t[lignesexclusionPdiff,'P.Diff']<-'-'

  #t$Cus['IP']<-GetCumIP(format(max(Data$DateSortieSejour,'%m%Y')))
  return(t[-1,])

}


#' Tableau de bord général d'activité avec un compte des séjours, et séparant HDJ/HC
#'
#' @param df un tableau de données de type séjours rum/rsa
#'
#' @return tableau de bord d'activité général en séjours
#' @export
#'
#' @examples

get_activite_sejours<-function(df){
    tdb <-list()
    tdb[['services']] <- table(df %>%  distinct(nofiness,cle_rsa,ansor,service,.keep_all = T)%>%
                                 select(service,ansor,typehosp))
    tdb[['poles']] <- table(df %>%  distinct(nofiness,cle_rsa,ansor,service,.keep_all = T)%>%
                              select(pole,ansor,typehosp))
    tdb[['hopitaux']]  <- table(df %>%  distinct(nofiness,cle_rsa,ansor,.keep_all = T)%>%
                                  select(hopital,ansor,typehosp))
    tdb[['gh']] <- table(df %>% distinct(nofiness,cle_rsa,ansor,.keep_all = T)%>%
                           select(typehosp,ansor))

    tdb <- order_by_structure(tdb)

    tdb_final<-cbind(get_diff(tdb$hc),NA,get_diff(tdb$hp))

    return(tdb_final)

}

#' Tableau de bord général d'activité en recettes, et séparant HDJ/HC
#'
#' @param df un tableau de données de type séjours rum/rsa
#'
#' @return tableau de bord d'activité général en recettes
#' @export
#'
#' @examples

get_activite_recettes<-function(df){
  tdb_v <-list()

  tdb_v[['services']] <- round( with( df, tapply( valopmctmonotime1, list(service,ansor,typehosp), sum, na.rm=T ) ) )
  tdb_v[['poles']]  <- round( with( df, tapply( valopmctmonotime1, list(pole,ansor,typehosp), sum, na.rm=T ) ) )
  tdb_v[['hopitaux']] <- round( with( df, tapply( valopmctmonotime1, list(hopital,ansor,typehosp), sum, na.rm=T ) ) )
  tdb_v[['gh']] <- t(round( with( df, tapply( valopmctmonotime1, list(ansor,typehosp), sum, na.rm=T ) ) ))

  tdb_v <- order_by_structure(tdb_v)

  tdb_v_final<-cbind(get_diff(tdb_v$hc),NA,get_diff(tdb_v$hp))

  return(tdb_final)

}
