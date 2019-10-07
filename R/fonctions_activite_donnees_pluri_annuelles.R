
#' Création d'un tableau indicateurs x pivot, comprenant une liste d'indicateurs
#'
#' @param df un tibble de type séjours
#' @param indicateurs vecteur, liste des indicateurs à calculer
#' @param pivot string, la variable pivot a utiliser dans df pour réaliser le tableau croisé, 
#'peut être soit un facteur soit une date, si une date la variable unit_pivot doit être renseignée
#' @param unit_pivot string, la variable pivot a utiliser dans df pour réaliser le tableau croisé
#'
#' @return dataframe contenant les statistiques demandées dans indicateurs mise en forme
#' 
#' @examples
#' \dontrun{
#' 
#'    get_tdb(df, indicateurs) -> tdb
#'    
#' }
#' 
#' @export get_tdb
#' @usage get_tdb(df, indicateurs)
#' @export
#' 
get_tdb<-function(df, indicateurs, pivot = 'pivot', unit_pivot = NULL){

  if(pivot == 'pivot'){
    
    stopifnot(is.factor(df$pivot))
    
  }else{
    
    stopifnot(!is.null(unit_pivot))
    
    df<-df%>%mutate(pivot = !!sym(pivot))%>%mutate(pivot = factor(as.numeric(format(pivot,unit_pivot))))
    
  }
  

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
                    dureesejpart2 = ifelse(d8soue2 > d8eeue2, d8soue2 - d8eeue2, 0 ) )


  }else{
    df<-df%>%mutate(dtent2 = as.Date(ifelse(date_min_pivot < dtent, dtent, date_min_pivot ),origin = '1970-01-01'),
                    dtsort2 = as.Date(ifelse(date_max_pivot > dtsort, dtsort, date_max_pivot ),origin = '1970-01-01' ),
                    dureesejpart2 = ifelse(dtsort2 > dtent2,dtsort2 - dtent2,0 ) )


  }
  #Les nouvelles dates entrée/sortie du résumé à l'intérieur des bornes pour chaque niveau de pivot, + nouvelle durée du résumé



  tb<-list()

  ###########################################################################
  #Nb de sejours en hospitalisation complete
  #  selection sur le type dossier (diff I,S)
  ###########################################################################
  if('NbPat'%in%indicateurs){
    tb[['NbPat']] <- with(df,tapply(noanon, pivot, nb_unique))

  }

  ###########################################################################
  #Nb de sejours en hospitalisation complete
  #  selection sur le type dossier (diff I,S)
  ###########################################################################
  #A voir comment selectionner la variable TypeDossier

  if('HCtot'%in%indicateurs){
    tb[['HCtot']] <- table( df %>% dplyr::filter( typehosp=="C", doublon==1 ) %>% dplyr::select( pivot ) )
  }

  if('nb_jour_hc_sej'%in%indicateurs){

    tb[['nb_jour_hc_sej']]<-with( df%>% dplyr::filter( typehosp=="C" ,doublon==1 ),
                         tapply( duree, pivot, sum ) )
  }
  
  if('nb_jour_hc'%in%indicateurs){
    
    tb[['nb_jour_hc']]<-with( df%>% dplyr::filter( typehosp=="C" ,doublon==1 ),
                         tapply( dureesejpart, pivot, sum ) )
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
  if("HC2det"%in%indicateurs){
    tmp<-df%>%dplyr::filter(duree > 1)%>%
      mutate(ds_det = dplyr::case_when(duree == 2 ~ '2 jours',
                                       duree == 3  ~ '3 jours',
                                       duree > 3 & duree < 6 ~ '3-5 jours',
                                       duree>=6 & duree < 11 ~ '6-10 jours',
                                       duree>10 & duree < 21  ~ '11-20 jours',
                                       duree>20 & duree < 31  ~ '21-30 jours',
                                       duree>30 ~ '> 30 jours'))%>%
      mutate(ds_det = factor(ds_det,levels = c('2 jours','3 jours','3-5 jours','6-10 jours','11-20 jours','21-30 jours','> 30 jours')))
               
            
    
    tb[['HC2det']]<-with(tmp,tapply(doublon, list(ds_det,pivot), sum))
    
    if(length(dimnames( tb[['HC2det']])[[1]])>0){
      
      dimnames( tb[['HC2det']])[[1]]<-paste('    - ',tolower(dimnames( tb[['HC2det']])[[1]]))
    }
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

    tb[['Seances']]<-table(df %>% dplyr::filter(cdghm%in%ghmseances$ghm, doublon==1) %>%select(pivot) )

  }
  ###########################################################################
  #
  ############################################################################
  if('SeancesDialyses'%in%indicateurs){

    tb[['SeancesDialyses']]<-table(df %>% dplyr::filter(cdghm%in%ghmseances$ghm[ghmseances$type=='Dialyse'],
                                                        doublon==1) %>%
                                     select(pivot) )
  }

  ###########################################################################
  #Nb Seances de chimiotherapie
  ###########################################################################
  if('SeancesChimio'%in%indicateurs){

    tb[['SeancesChimio']]<-table(df %>% dplyr::filter(cdghm%in%ghmseances$ghm[ghmseances$type=='Chimio'],
                                                      doublon==1) %>%
                                   select(pivot) )
  }

  ###########################################################################
  #Nb Seances de radiotherapie
  ###########################################################################
  if('SeancesRadio'%in%indicateurs){

    tb[['SeancesRadio']]<-table(df %>% dplyr::filter(cdghm%in%ghmseances$ghm[ghmseances$type=='Radio'],
                                                     doublon==1) %>%
                                  select(pivot) )
  }
  ###########################################################################
  #Nb Seances preparation radiotherapie
  ###########################################################################
  if('SeancesPrepaRadio'%in%indicateurs){

    tb[['SeancesPrepaRadio']]<-table(df %>% dplyr::filter(cdghm%in%ghmseances$ghm[ghmseances$type=='PrepaRadio'],
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

    tb[['Greffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,
                                                                organe%in%c('AutoGreffeCellulesSouches',
                                                                            'AllogreffeCellulesSouches'))$ghm,
                                                doublon==1) %>%
                             select(pivot) )

  }


  if('AutoGreffes'%in%indicateurs){

    tb[['AutoGreffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,
                                                                    organe%in%c('AutoGreffeCellulesSouches'))$ghm,
                                                    doublon==1) %>% select(pivot) )

  }

  if('AlloGreffes'%in%indicateurs){

    tb[['AlloGreffes']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,
                                                                    organe%in%c('AllogreffeCellulesSouches'))$ghm,
                                                    doublon==1) %>%
                                 select(pivot) )
  }

  if('Transplantations'%in%indicateurs){

    tb[['Transplantations']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,
                                                                         organe%in%c('Rein','Pancreas'))$ghm,
                                                         doublon==1) %>%
                                      select(pivot) )
  }
  if('TransplantationsRenales'%in%indicateurs){

    tb[['TransplantationsRenales']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,organe%in%c('Rein'))$ghm,
                                                                doublon==1) %>%
                                             select(pivot) )
  }
  if('TransplantationsPancreas'%in%indicateurs){

    tb[['TransplantationsPancreas']]<-table(df %>% dplyr::filter(cdghm%in%subset(ghmgreffes,
                                                                                 organe%in%c('Pancreas'))$ghm,
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
    tb[['DMS']]<- round( with( df %>% filter( duree > 0, doublon == 1 ), tapply( duree, pivot, mean, na.rm = T ) ), 1 )
  }
  
  if('DMS_centrale'%in%indicateurs){
    tb[['DMS_centrale']]<- round( with( df %>% filter( duree > 0, duree < dms_n +30, doublon == 1 ) ,
                                        tapply( duree, pivot, mean, na.rm = T ) ), 1 )
  }

  ###########################################################################
  #Duree moyenne des resumes
  ###########################################################################
  if('DMR'%in%indicateurs){
    tb[['DMR']]<-round( with( df %>% filter( duree > 0 ), tapply( dureesejpart, pivot, sum, na.rm = T ) )/
                         table( df %>% filter( duree > 0, doublon == 1 ) %>% select( pivot ) ),
                       1)
  }
  
  if('DMR_centrale'%in%indicateurs){
    tb[['DMR_centrale']]<-round( with( df %>% filter( duree > 0, duree < dms_n +30 ), tapply( dureesejpart, pivot, sum, na.rm = T ) )/
                          table( df %>% filter( duree > 0, duree < dms_n +30, doublon == 1 ) %>% select( pivot ) ),
                        1)
  }
  
  ###########################################################################
  #Index de performance
  ###########################################################################
  if('IP'%in%indicateurs){
    tb[['IP']]<-as.data.frame(t(sapply(as.numeric(levels(df$pivot)),function(x)IP_SEJOUR(df%>%filter(pivot == x)))))

    names(tb[['IP']])<-as.numeric(levels(df$pivot))
  }

  ###########################################################################
  #Index de performance service
  ###########################################################################
  if('IP_SERV'%in%indicateurs){
    tb[['IP_SERV']]<-as.data.frame(t(sapply(as.numeric(levels(df$pivot)),function(x)IP_SERVICE(df%>%filter(pivot == x)))))

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

    tb[['nbsuprea']]<-round(with(df,tapply(nbsuprea_repa,pivot,sum)))

  }


  if('nssir'%in%indicateurs){

    tb[['nssir']]<- round(with(df,tapply(nssir_repa,pivot,sum)))

  }


  #Pourcentage des journees effectuees dans une UH de reanimation avec suplement rea
  #	UH reanimation declaree dans la variable DataHop
  ############################################################################

  if('p_nbsuprea'%in%indicateurs){

    tb[['p_nbsuprea']]<-round(round(tb[['nbsuprea']]*100/( tb[['nbsuprea']]+ tb[['nssir']])))

  }


  #Nombre de journees soins intensif hors reanimation
  ############################################################################
  if('nsstf'%in%indicateurs){

    tb[['nsstf']]<- round(with(df,tapply(nssir_repa+nsstf_repa,pivot,sum)))
  }


  if('stf_hr'%in%indicateurs){

    tb[['stf_hr']]<-round(tapply(df$nsstf_repa,df$pivot,sum))

  }

  #Nombre de journees surveillance continue
  ############################################################################
  if('nbsupsrc'%in%indicateurs){

    tb[['nbsupsrc']]<-round(tapply(df$nbsupsrc_repa,df$pivot,sum))

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

    tb[['dialhosp']]<-with(df%>%mutate(adial = stringr::str_count(actes, paste(actes_dialyse_sup$acte,collapse = "|"))),
                           tapply(adial,pivot,sum))
  }

  #Poids moyen du cas traite :
  ##Moyenne des tarifs du GHS
  ############################################################################

  
  if('rec_totale'%in%indicateurs){
    tb[['rec_totale']]<-
      round(
        with(df%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_totale,pivot,sum,na.rm=T))
        
      )
  }
  if('rec_totale_hc'%in%indicateurs){
    tb[['rec_totale_hc']]<-
      round(
        with(df%>%filter(typehosp=='C')%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_totale,pivot,sum,na.rm=T))
        
      )
  }
  
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
 
  if('rec_supp'%in%indicateurs){
    tb[['rec_supp']]<-
      round(
        with(df%>%filter(typehosp=='C')%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_totale-rec_base,pivot,sum,na.rm=T))
        
      )
  } 
  if('rec_supp_repa'%in%indicateurs){
    tb[['rec_supp_repa']]<-
      round(
        with(df%>%filter(typehosp=='C')%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
             tapply(rec_sup_repa,pivot,sum,na.rm=T))
        
      )
  } 
  
  if('rec_totale_repa'%in%indicateurs){
    tb[['rec_totale_repa']]<-
      round(
        with(df,tapply(valopmctmonotime1,pivot,sum,na.rm=T))
        
      )
  }

  if('rec_base_repa'%in%indicateurs){
    tb[['rec_base_repa']]<-
      round(
        with(df,tapply(valopmctmonotime1-rec_sup_repa,pivot,sum,na.rm=T))

      )
  }
  
  if('rec_totale_repa_hc'%in%indicateurs){
    tb[['rec_totale_repa_hc']]<-
      round(
        with(df%>%filter(typehosp=="C"),tapply(valopmctmonotime1,pivot,sum,na.rm=T))
        
      )
  }
  if('rec_base_repa_hc'%in%indicateurs){
    tb[['rec_base_repa_hc']]<-
      round(
        with(df%>%filter(typehosp=="C"),
             tapply(valopmctmonotime1-rec_sup_repa,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa_mono_uma'%in%indicateurs){
    tb[['rec_base_repa_mono_uma']]<-
      round(
        with(df%>%filter(typehosp=="C",nbrum==1),
             tapply(valopmctmonotime1-rec_sup_repa,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_repa_multi_uma'%in%indicateurs){
    tb[['rec_base_repa_multi_uma']]<-
      round(
        with(df%>%filter(typehosp=="C",nbrum!=1),
             tapply(valopmctmonotime1-rec_sup_repa,pivot,sum,na.rm=T))

      )
  }

  if('rec_base_hp'%in%indicateurs){
    tb[['rec_base_hp']]<-
      round(
        with(df%>%filter(typehosp=="P"),
             tapply(rec_base,pivot,sum,na.rm=T))

      )
  }
  
  if('rmct_hc'%in%indicateurs){
    
    tb[['rec_totale_hc']]<- round(
      with(df%>%filter(typehosp=='C')%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T),
           tapply(rec_totale,pivot,sum,na.rm=T))
    )
    
    tb[['rmct_hc']] <- round( tb[['rec_totale_hc']] / tb[['HCtot']] ) 
  }
  
  if('rmct_repa_hc'%in%indicateurs){
    
    tb[['rmct_repa_hc']] <- round( tb[['rec_totale_repa_hc']] / tb[['HCtot']] )
    
  }
  

  if('pmct_hc'%in%indicateurs){

    tb[['pmct_hc']] <- round( tb[['rec_base_hc']] / tb[['HCtot']] )

  }

  if('pmct_repa_hc'%in%indicateurs){

    tb[['pmct_repa_hc']] <- round( tb[['rec_base_repa_hc']] / tb[['HCtot']] )

  }

  if('pmct_repa_mono_uma'%in%indicateurs){

    tb[['pmct_repa_mono_uma']]<- round( tb[['rec_base_repa_mono_uma']] / 
                                          table( df %>% dplyr::filter( typehosp=="C", nbrum==1 ) %>% dplyr::select( pivot ) ) 
                                       )
                                          

  }

  if('pmct_repa_multi_uma'%in%indicateurs){

    tb[['pmct_repa_multi_uma']]<- round( tb[['rec_base_repa_mono_uma']] / 
                                           table( df %>% dplyr::filter( typehosp=="C", nbrum!=1,  doublon==1 ) %>% dplyr::select( pivot ) ) 
    )
  }


  #PMCT des HDJ
  ############################################################################
  if('pmct_hp'%in%indicateurs){

    tb[['pmct_hp']]<-     round( tb[['rec_base_hp']] / tb[['HPTot']]  )
  }
  
  
  #Recette journalières
  ############################################################################
  tb[['rec_totale_jour_hc']]<- round( tb[['rec_totale_hc']] / tb[['nb_jour_hc']] )

  tb[['rec_base_jour_hc']]<- round( tb[['rec_base_hc']] / tb[['nb_jour_hc']] )
  

  tb[['rec_totale_jour_hc_repa']]<- round( tb[['rec_totale_repa_hc']] / tb[['nb_jour_hc']] )
  
  tb[['rec_base_jour_hc_repa']]<- round( tb[['rec_base_repa_hc']] / tb[['nb_jour_hc']] )
  
  ##Détails recettes
  if('0_nuit_nb_sej'%in%indicateurs){
    
    nb_journees <- with( df %>% filter( typehosp == 'C' ), tapply( dureesejpart, pivot, sum, na.rm=T ) )
    rescettes_totales <- with( df %>% filter( typehosp == 'C' ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) )    
    
    tb[['0_nuit_nb_sej']] <- table( df %>% filter( typehosp == 'C', duree == 0 ) %>% select( pivot ) )

    tb[['0_nuit_recettes']] <- round( with( df %>% filter( typehosp == 'C', duree == 0 ), tapply( valopmctmonotime1, pivot,sum, na.rm=T ) ) )

    tb[['0_nuit_pourc_recettes']] <-  round( tb[['0_nuit_recettes']] * 100 / rescettes_totales, 1 )
    
    tb[['0_nuit_recettes_jour']]  <- round( tb[['0_nuit_recettes']] /  tb[['0_nuit_nb_sej']] )
    
    
    tb[['1_nuit_nb_sej']] <- table( df %>% filter( duree == 1 ) %>% select( pivot ) ) 
     
    tb[['1_nuit_pourc_journees']] <-  round( tb[['1_nuit_nb_sej']] * 100 /  nb_journees, 1 )
    
    tb[['1_nuit_recettes']] <- round( with( df %>% filter( duree == 1 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['1_nuit_pourc_recettes']]<- round( tb[['1_nuit_recettes']] * 100 / rescettes_totales, 1 )
    
    tb[['1_nuit_recettes_jour']] <- round( tb[['1_nuit_recettes']] /  tb[['1_nuit_nb_sej']] )
    
    
    
    tb[['2_nuits_nb_journees']] <- with( df %>% filter( duree > 1 , duree < dms_n + 30 ), tapply( dureesejpart, pivot, sum, na.rm=T ) ) 
    
    tb[['2_nuits_pourc_journees']] <- round( tb[['2_nuits_nb_journees']]*100 /  nb_journees, 1 )
    
    tb[['2_nuits_recettes']] <- round ( with( df %>% filter(duree > 1 , duree < dms_n + 30 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['2_nuits_pourc_recettes']] <- round( tb[['2_nuits_recettes']] * 100 / rescettes_totales, 1)
    
    tb[['2_nuits_recettes_jour']] <- round(tb[['2_nuits_recettes']] /  tb[['2_nuits_nb_journees']] )
    
    
    tb[['sej_longs_nb_journees']] <- with( df %>% filter( duree >= dms_n +30 ), tapply( dureesejpart, pivot, sum, na.rm=T ) ) 
    
    tb[['sej_longs_pourc_journees']] <-  round( tb[['sej_longs_nb_journees']]*100 /  nb_journees, 1 )
    
    tb[['sej_longs_recettes']] <- round( with( df %>% filter( duree >= dms_n +30 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['sej_longs_pourc_recettes']] <- round( tb[['sej_longs_recettes']] * 100 / rescettes_totales, 1)
    
    tb[['sej_longs_recettes_jour']] <- round( tb[['sej_longs_recettes']] /  tb[['sej_longs_nb_journees']] )
    
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
  if('ActiviteCancero'%in%indicateurs|"SP"%in%indicateurs){

    df_cancer<-inner_join(df,cancer_rsa%>%select(-dp,-noanon,-nas))


    tb[['NbPat']]<-with(df_cancer,tapply(noanon, pivot, nb_unique))
    tb[['NbNxPat']]<-with(df_cancer%>%filter(nx_pat == 'O'),tapply(noanon, pivot, nb_unique))
    tb[['NbSejHC']]<- table(df_cancer%>%dplyr::filter(typehosp=="C",doublon == 1)%>%dplyr::select(pivot))
    tb[['NbSejHP']]<-table(df_cancer%>%dplyr::filter(typehosp=="P",doublon == 1)%>%dplyr::select(pivot))
    #tb[['pSejHC']]<-round(table(DataCancero$pivot[!duplicated(DataCancero$NORSS)&DataCancero$TypeDossier%in%c('A','N')])*100/
    #                        table(df$pivot[!df$typehosp=="P"]))
    #
    #tb[['pSejHP']]<-round(table(DataCancero$pivot[which(DataCancero$TypeDossier%in%c('I','S'))])*100/
    #                        table(df$pivot[df$typehosp=="P"]))
    tb[['NbSejChir']]<-table(df_cancer%>%dplyr::filter(substr(cdghm,3,3)=="C",doublon == 1)%>%dplyr::select(pivot))

    tb[['NbPatientsParAppareilInca']]<-with(df_cancer,tapply(noanon, list(appareil,pivot), nb_unique))

    if(length(dimnames( tb[['NbPatientsParAppareilInca']])[[1]])>0){

      dimnames( tb[['NbPatientsParAppareilInca']])[[1]]<-paste(' -',tolower(dimnames( tb[['NbPatientsParAppareilInca']])[[1]]))
    }

    tb[['ActiviteParAppareilInca']]<-with( df_cancer %>% filter( doublon == 1 ),
                                           tapply( cle_rsa, list( appareil, pivot ), nb_unique ) )

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

    tb[['NbPatSP']] <- with( df_sp, tapply(noanon, pivot, nb_unique) )

    tb[['SejSpGhmNonSP']]<-table( df_sp %>% filter( substr(cdghm,1,5)!='23Z02', doublon == 1 )%>% select(pivot) )

    tb[['SejSpLitsStandards']]<-table( df_sp%>%filter( noghs%in%c('7991','7992') )%>% select(pivot) )
    tb[['SejSpLitsDedies']]<-table( df_sp%>%filter(noghs%in%c('7993'))%>%select(pivot))
    tb[['NbPatSpLitsDedies']]<-with(df_sp%>%filter(noghs%in%c('7993')),tapply(noanon,pivot,nb_unique))
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

  ######################################################################### 
  #####Case mix
  #########################################################################
  if('TitreCaseMixCMD_HC'%in%indicateurs){
    
    tmpHC <- df %>% dplyr::filter( typehosp=="C")
    tmp<-tmpHC%>%mutate(cmd := substr(ghm,1,2))
    nb<-table(tmp$cmd)
    nb<-nb[order(nb,decreasing = T)]
    nbc<-cumsum(nb[order(nb,decreasing = T)])*100/sum(nb)

    tmp<- tmp %>% mutate(cmd = ifelse(cmd %in% names(nbc[nbc<91]),cmd,'Autres'))%>%
      mutate(cmd = factor(cmd,levels= c(names(nbc[nbc<91]),'Autres')))
    
    tb[['CaseMixCMD_HC']]<-with(tmp,tapply(doublon, list(cmd,pivot), sum))
    
    if(length(dimnames( tb[['CaseMixCMD_HC']])[[1]])>0){
      
      dimnames( tb[['CaseMixCMD_HC']])[[1]]<-paste(' -',tolower(dimnames( tb[['CaseMixCMD_HC']])[[1]]))
    }
    
    tmp<-tmpHC%>%mutate(type := substr(ghm,3,3))
    
    tb[['CaseMixType_HC']]<-with(tmp,tapply(doublon, list(type,pivot), sum))
    
    if(length(dimnames( tb[['CaseMixType_HC']])[[1]])>0){
      
      dimnames( tb[['CaseMixType_HC']])[[1]]<-paste(' -',dimnames( tb[['CaseMixType_HC']])[[1]])
    }
    
    tmp<-tmpHC%>%mutate(racine := substr(ghm,1,5))
    nb<-table(tmp$racine)
    nb<-nb[order(nb,decreasing = T)]
    nbc<-cumsum(nb[order(nb,decreasing = T)])*100/sum(nb)
    
    tmp<- tmp %>% mutate(racine = ifelse(racine %in% names(nbc[nbc<91]),racine,'Autres'))%>%
      mutate(racine = factor(racine,levels= c(names(nbc[nbc<91]),'Autres')))
    
    tb[['CaseMixGHM_HC']]<-with(tmp,tapply(doublon, list(racine,pivot), sum))
    
    if(length(dimnames( tb[['CaseMixGHM_HC']])[[1]])>0){
      
      dimnames( tb[['CaseMixGHM_HC']])[[1]]<-paste(' -',tolower(dimnames( tb[['CaseMixGHM_HC']])[[1]]))
    }
    
    

    
    
  }
  ########################################################################################
  # Infectieux
  ########################################################################################
   if('Infectieux'%in%indicateurs){
     tmp<-df%>%filter(dp%in%infections_otoctones$code|dr%in%infections_otoctones$code|
                        grepl(paste(infections_otoctones$code,collapse = '|'),das))
     
     tb[['NbSejoursInfectieux']]<-table( tmp%>% filter(doublon ==1) %>% select(pivot))
     
     tmp<-df%>%filter(dp%in%resistance_atb$code|dr%in%resistance_atb$code|
                        grepl(paste(resistance_atb$code,collapse = '|'),das))
     
      tb[['NbPateintsBMR']]<-with( tmp,tapply(noanon,pivot,nb_unique) )
      tb[['NbSejoursBMR']]<-table( tmp%>% filter(doublon ==1) %>% select(pivot))
    }

  ########################################################################################
  # Douleur
  ########################################################################################

  if('Douleur'%in%indicateurs){

    tmp<-df%>%filter(dp%in%douleur$code|dr%in%douleur$code|
                       grepl(paste(douleur$code,collapse = '|'),das))

    tb[['PatientsDouleur']]<-with( tmp,tapply(noanon,pivot,nb_unique) )

    tb[['SejoursDouleur']]<-table( tmp%>% filter(doublon ==1) %>% select(pivot))

  }

  ########################################################################################
  #Autres pathologies chroniques
  ########################################################################################
  if('AutresPathologiesChroniques'%in%indicateurs){

    ###VIH
    tmp<-df%>%filter(dp%in%vih$code|dr%in%vih$code|
                       grepl(paste(vih$code,collapse = '|'),das))

    tb[['PatientsVIH']]<- with( tmp, tapply(noanon,pivot,nb_unique) )

    tb[['SejoursVIH_HC']]<-table( tmp%>%filter(typehosp=='C',doublon ==1) %>% select(pivot) )

    tb[['SejoursVIH_HP']]<-table(tmp%>%filter(typehosp=='P')%>%select(pivot))

    #Diabète
    tmp<-df%>%filter(dp%in%diabete$code|dr%in%diabete$code|
                       grepl(paste(diabete$code,collapse = '|'),das))

    tb[['PatientsDiabete']]<-with(tmp, tapply(noanon,pivot,nb_unique) )

    tb[['SejoursDiabete_HC']]<-table(tmp %>% filter(typehosp=='C', doublon==1 ) %>% select(pivot) )

    tb[['SejoursDiabete_HP']]<-table(tmp %>% filter(typehosp=='P') %>% select(pivot) )

    #Insuffisance rénale chronique
    tmp<-df%>%filter(dp%in%insuffisance_renale_chronique$code|dr%in%insuffisance_renale_chronique$code|
                       grepl(paste(insuffisance_renale_chronique$code,collapse = '|'),das))

    tb[['PatientsInsRenaleChronique']]<-with( tmp, tapply(noanon,pivot,nb_unique) )

    tb[['SejoursInsRenaleChronique_HC']]<-table( tmp %>% filter(typehosp=='C', doublon==1) %>% select(pivot) )

    tb[['SejoursInsRenaleChronique_HP']]<-table( tmp %>% filter(typehosp=='P') %>% select(pivot) )

    #Insuffisance respiratoire chronique
    tmp<-df%>%filter(dp%in%insuffisance_respiratoire_chronique$code|dr%in%insuffisance_respiratoire_chronique$code|
                       grepl(paste(insuffisance_respiratoire_chronique$code,collapse = '|'),das))

    tb[['PatientsInsRespiratoireChronique']]<-with( tmp, tapply(noanon,pivot,nb_unique) )

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

    tb[['IGSmoyen']]<-round( with( tmp_rea, tapply( as.numeric(igs), pivot, mean, na.rm=T) ), 1 )

    tb[['IGS0']]<-table(tmp_rea %>% filter( as.numeric(igs)==0 ) %>% select( pivot ) )

    l_actes = paste(c(as.character(actes_ventilation_intubation$acte),
                      as.character(actes_ventilation_tracheotomie$acte),
                      as.character(actes_vni$acte)),
                    collapse = '|')

    tb[['NbSejoursPatientVentiles']]<- table(df %>% filter( grepl( l_actes, actes ) )
                                             %>% filter( doublon == 1 )
                                             %>% select( pivot ) )

    tb[['pPatientVentiles']]<- round( table(df %>%filter(grepl(l_actes,actes))
                                            %>%filter(doublon == 1)
                                            %>%select(pivot) ) *100 / tb[['HCtotAutRea']],
                                      1)

    ###Nombre de patients ventiles plus de 48h
    tb[['NbSejoursPatientVentiles48h']]<- table( tmp_rea %>% mutate(nb_actes = stringr::str_count(actes, l_actes))
                                                 %>% filter(nb_actes > 1)
                                                 %>% filter(doublon == 1)
                                                 %>% select(pivot) )


    tb[['NbJourneesVentilesInt']]<-with( tmp_rea %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                         tapply(nb_actes,pivot,sum,na.rm=T))

    l_actes = paste(actes_ventilation_intubation$acte,collapse = '|')

    tb[['NbSejoursPatientVentilesInt']] <- table( tmp_rea %>% filter(grepl(l_actes,actes))
                                                  %>% filter(doublon == 1)
                                                  %>% select(pivot) )



    l_actes = paste(actes_vni$acte,collapse = '|')

    tb[['NbSejoursPatientVentilesMasque']]<-table( tmp_rea %>% filter(grepl(l_actes,actes))
                                                   %>% filter(doublon == 1)
                                                   %>% select(pivot) )

    l_actes = paste(actes_ventilation_tracheotomie$acte,collapse = '|')
    tb[['NbSejoursPatientVentilesTrac']]<- table( tmp_rea %>% filter(grepl(l_actes,actes))
                                                  %>% filter(doublon == 1)
                                                  %>% select(pivot) )

    ###Pourcentage de patients dialyses
    l_actes = paste(c(actes_dialyse_ira_discontinue$acte,
                      actes_dialyseirc_discontinue$acte,
                      actes_dialyse_ira_continue$acte),
                    collapse = '|')

    tb[['NbSejoursPatientsDialyses']]<-table(df %>% filter( grepl( l_actes, actes ) )
                                             %>% filter( doublon == 1 )
                                             %>% select( pivot ) )

    tb[['pPatientsDialyses']]<-round( tb[['NbSejoursPatientsDialyses']]*100 / tb[['HCtotAutRea']] )

    ###Nb patients EER  + EER continue
    l_actes = paste( actes_dialyse_continue$acte ,
                     collapse = '|' )

    tb[['NbjourneesEERcontinue']]<-with( tmp_rea %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                         tapply(nb_actes,pivot,sum,na.rm=T))

    ##Nb seance EER discontinue (seances de dialyse)
    l_actes = paste( c(actes_dialyse_ira_discontinue$acte,
                       actes_dialyseirc_discontinue$acte),
                     collapse = '|' )

    tb[['NbSeancesDialyses']]<- with( tmp_rea %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                                      tapply(nb_actes,pivot,sum,na.rm=T))


    ##Nb d'actes d'apherese

    l_actes = paste(actes_apherese$acte,
                     collapse = '|')

    tb[['NbApherese']]<-with( tmp_rea %>% mutate(nb_actes = stringr::str_count(actes, l_actes)),
                              tapply(nb_actes,pivot,sum,na.rm=T))


    ###Nombre de patients sous catecholamine
    l_actes = paste( c('EQLF001','EQLF003'),
                     collapse = '|')

    tb[['NbSejoursPatientCatecholamine']]<-table(df %>% filter( grepl( l_actes, actes ) )
                                                 %>% filter( doublon == 1 )
                                                 %>% select( pivot ) )

    ###Nombre suppleance pour prelevement d'organe

    tb[['NbSejoursPatientsSuppPvtOrgane']]<-table(df %>% filter( rec_po_tot_repa > 0)  
                                                  %>% filter( doublon == 1 )
                                                  %>% select( pivot ) )

    ###Pourcentage de deces en reanimation

    tb[['pDcdRea']]<- round(table( df %>% filter(mdsoue == 9, typeaut=="01A" ) %>% select(pivot) )*100/
                              tb[['HCtotAutRea']],
                            digit=1)

    tb[['pDcdHopRea']]<-round(table( inner_join(df, tmp_rea %>% select(nofiness,cle_rsa,ansor)%>%distinct(nofiness,cle_rsa,ansor))
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
  if('nb_sej_sup30'%in%indicateurs){

    tb[['nb_sej_sup30']]<-table( df%>%filter(duree > 30) %>% filter(doublon == 1) %>% select(pivot) )

  }

  if('nb_sej_bh'%in%indicateurs){

   

    tb[['nb_sej_bh']]<-table( df %>% filter(nbjrbs>0) %>% filter(doublon == 1) %>% select(pivot) )
  }

  if('p_sej_bh'%in%indicateurs){
    tb[['p_sej_bh']]<-round(tb[['nb_sej_bh']] * 100 / 
                              table( df %>% dplyr::filter( typehosp=="C", doublon==1 ) %>% dplyr::select(pivot) )
                          ,digit=1)
  }

  if('nb_jour_bh'%in%indicateurs){

    tb[['nb_jour_bh']] <- with( df , tapply( nbjrbs, pivot, sum, na.rm=T ) )
  }

  if('nb_jour_bh_repa'%in%indicateurs){

    tb[['nb_jour_bh_repa']] <- round( with( df %>% mutate( nbjsup = nbjrbs*coeftime ), tapply( nbjsup, pivot, sum, na.rm=T ) ) )
  }

  #if('pSejBHrevus'%in%indicateurs){
  #  tb[['pSejBHrevus']]<-
  #    tapply(df$duree_sej[!duplicated(df$norss)&df$duree_sej>Data$BH&df$nas%in%Nestor$nas],
  #           df$pivot[!duplicated(df$norss)&df$duree_sej>Data$BH&df$nas%in%Nestor$nas],sum)
  #
  #}

  if('nb_sej_sup_dms'%in%indicateurs){

    tb[['nb_sej_sup_dms']] <- table( df %>% filter( duree > dms_n, doublon == 1 ) %>% select(pivot) )

  }
  
  if('nb_jour_sup_dms'%in%indicateurs){
    
    tb[['nb_jour_sup_dms']] <-  round( with( df %>% filter( duree > dms_n, doublon == 1 ), tapply( duree-dms_n, pivot, sum, na.rm=T ) ) )
     
    
  }
  
  if('nb_jour_sup_dms_repa'%in%indicateurs){
    
    tb[['nb_jour_sup_dms_repa']] <-  round( with( df %>% filter( duree>dms_n, doublon == 1 ),
                                              tapply( dureesejpart - ( dms_n*coeftime ), pivot, sum, na.rm = T ) ) )
    
    
  }
  

  if('p_sej_sup_dms'%in%indicateurs){
    
    tb[['p_sej_sup_dms']] <- round( tb[['nb_sej_sup_dms']] * 100 / tb[['HCtot']], 1 )

  }
  
  if('p_jour_sup_dms'%in%indicateurs){
    
    tb[['p_jour_sup_dms']] <-  round(  tb[['nb_jour_sup_dms']]*100 / tb[['nb_jour_hc']], 1 )
    
    
  }
  
  if('p_jour_sup_dms_repa'%in%indicateurs){
    
    tb[['p_jour_sup_dms_repa']] <-  round(  tb[['nb_jour_sup_dms_repa']]*100 / tb[['nb_jour_hc']] )
    
    
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

  if('nb_sej_bb'%in%indicateurs){
    
    tb[['nb_sej_bb']]<- table( df %>% filter( sejinfbi>0, doublon == 1 ) %>% select(pivot) )
    
  }
  if('p_sej_bb'%in%indicateurs){

    tb[['p_sej_bb']]<-round( table( df %>% filter( sejinfbi>0, doublon == 1 ) %>% select(pivot) )*100 /  tb[['HCtot']], digit=1)

  }
  if('MontantEXB'%in%indicateurs){

    tb[['MontantEXB']]<-with(df, tapply( rec_exb, pivot, sum, na.rm=T ) )
  }

  ###Montant des pertes dues aux sejours duree > BH

  if('pertes_bh'%in%indicateurs){

    tb[['pertes_bh']]<- round(with(df %>% mutate( rec_moy = rec_base / dms_n,
                                                    diff = rec_exh - (nbjrbs * rec_moy) ),
                                   tapply( diff, pivot, sum, na.rm=T )))


  }

  if('pertes_bh_repa'%in%indicateurs){

    tb[['pertes_bh_repa']]<- round(with(df%>%mutate(nbjsup = nbjrbs*coeftime,
                                                    rec_moy = (valopmctmonotime1-rec_sup_repa) / dms_n,
                                                    diff = rec_exh*coeftime - (nbjsup * rec_moy * coeftime) ),
                                        tapply(diff,pivot,sum,na.rm=T)))


  }




  TB<-sapply(names(tb),function(x)tb[[x]]<<-prep_tb(tb[[x]]))


  t<-as.data.frame(t(rep(NA,(length(levels(df$pivot))+2))))
  
  levels_pivot = as.character(c((min(as.numeric(levels(df$pivot))):max(as.numeric(levels(df$pivot))))))

  names(t)<-c('niv','libelle',levels_pivot)



  for(i in indicateurs){

    if(i%in%references$var){
      lib = references%>%filter(var==i)%>%select(libelle)
        if(!is.na(lib)){
          t[i,'niv']<-references%>%filter(var==i)%>%distinct()%>%select(niv)%>%flatten_chr()
          t[i,'libelle']<-lib
        }
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

            t<-rbind(t,t2[,c('niv','libelle',levels_pivot)])
          }
        }
      }
    }
  }
  t<-diff_tb(t)
  #lignesexclusionPdiff<-which(t$LIBELLE%in%exclusionPdiff)
  #if(length(lignesexclusionPdiff))t[lignesexclusionPdiff,'P.Diff']<-'-'

  #t$Cus['IP']<-GetCumIP(format(max(Data$DateSortieSejour,'%m%Y')))
  return(t[-1,])

}


#' Calcul de la part liée à la variation de la répartition dans les niveaux de sévérité
#' dans l'évolution des recettes T2A entre 2 années.
#'
#' @param df tibble, contenant l'ensemble des données nécessaires au calcul
#' @param pivot string, le niveau de structure pour lequel on souhaite faire le calcul
#'
#' @return
#' @export detail_diff_recettes
#'
#' @examples
detail_diff_recettes <- function( df, pivot ) {
  
  
  df<-df%>%mutate(pivot:=!!sym(pivot))
  
  annee_ = max(as.numeric(df$ansor))
  
  
  giac1 <- dplyr::filter(df) %>%
    dplyr::group_by( nas, pivot ) %>% 
    dplyr::summarise( ghm = first(cdghm),
                      ghs = first(ghs),
                      tarif_base = first( tarif_base ),
                      coefpmctmonotime1 = sum( coefpmctmonotime1 ),
                      ansor = first( ansor ) ) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate( racine = substr( ghm, 1, 5) ) %>%
    dplyr::group_by( ansor, pivot, racine ) %>%
    dplyr::mutate( nrac_sej = length(ghs), 
                   nrac_coef = sum( coefpmctmonotime1 ) ,
                   rec_base = sum( tarif_base * coefpmctmonotime1 )
    ) %>% 
    dplyr::ungroup(.) %>%
    dplyr::group_by( ansor, pivot, racine, ghs ) %>%
    dplyr::mutate( nghs = length(ghs), 
                   repanum = length(ghs) / nrac_sej * tarif_base ) %>% 
    dplyr::ungroup(.) %>%
    dplyr::distinct( ansor, pivot, ghm, ghs, .keep_all=TRUE ) %>%
    dplyr::group_by( ansor, pivot, racine ) %>%
    dplyr::summarise( repanum_rac = sum(repanum),
                      nrac_sej = dplyr::first(nrac_sej),
                      nrac_coef = dplyr::first(nrac_coef),
                      rec_base = first(rec_base)
    ) %>% dplyr::ungroup(.) 
  
  
  
  giac<-dplyr::full_join( giac1%>%filter( as.numeric(ansor) == annee_-1 )%>%select( -ansor ),
                          giac1%>%filter( as.numeric(ansor) == annee_ )%>%dplyr::select( -ansor ),
                          suffix = c( "_n_1", "_n" ),
                          by = c( "racine", "pivot" )
  )
  ##Resultats differents pour giac1 et pour giac??? --> je n'ai pas trouvé de différence cf ci dessous
  
  # a <- tapply(giac1$repanum_rac,list(giac1$pivot,giac1$ansor),sum,na.rm=T)
  # b <- tapply(giac1$nrac_sej,list(giac1$pivot,giac1$ansor),sum,na.rm=T)  
  
  
  # a_ <- cbind(tapply(giac$repanum_rac_n_1,giac$pivot,sum,na.rm=T),tapply(giac$repanum_rac_n,giac$pivot,sum,na.rm=T))
  # b_ <- cbind(tapply(giac$nrac_sej_n_1,giac$pivot,sum,na.rm=T),tapply(giac$nrac_sej_n,giac$pivot,sum,na.rm=T))
  
  # a - a_
  # b - b_
  
  
  #res2<-giac%>%
  # group_by( pivot ) %>% summarise( "rec_base" = sum( rec_base_n, na.rm=T ),
  #                                  "sum_coeff" = sum(nrac_coef_n,na.rm=T) ,
  #                                  "nb_sej" = sum(nrac_sej_n,na.rm=T) ) 
  
  #df2%>%  dplyr::mutate( racine = substr( cdghm, 1, 5) ) %>%
  #  dplyr::group_by( ansor, pivot, racine ) %>%
  #  dplyr::distinct(nas,.keep_all=TRUE)%>%
  #  dplyr::summarise( nrac_sej = length(nas), 
  #                    nrac_coef = sum( coefpmctmonotime1 ) ,
  #                   rec_base = sum( tarif_base )
  # )->tmp2
  
  #tdb_v[['services']] <- round( with( df, tapply(  tarif_base * coefpmctmonotime1 , list(pivot,ansor), sum, na.rm=T ) ) )
  
  #tdb_v[['sej']] <- round( with( df2, tapply(  nas , list(pivot,ansor), function(x) length( unique(x) ) ) ) )
  
  #tdb_v[['coef']] <- with( df2, tapply(  coefpmctmonotime1 , list(pivot,ansor), sum, na.rm=T ) ) 
  
  
  pc<-giac%>%group_by(pivot)%>%mutate( s_nrac = sum(nrac_sej_n, na.rm=T) )%>%ungroup()%>%filter(is.na(nrac_sej_n) | is.na(nrac_sej_n_1) | nrac_sej_n_1 < 5 )%>%
    group_by(pivot)%>%
    mutate(p_calc = round( (1-sum(nrac_sej_n,na.rm=T)/s_nrac )*100, 1 ) ) %>% dplyr::distinct(pivot, p_calc)  %>% ungroup(.)
  
  
  res<-giac%>%filter( ! is.na(nrac_sej_n) ,  ! is.na(nrac_sej_n_1) , nrac_sej_n_1 > 4 ) %>%
    mutate( diff = ( nrac_coef_n * repanum_rac_n ) - ( nrac_coef_n * repanum_rac_n_1 ) )%>%
    group_by( pivot ) %>% summarise( "part_ns" = round( sum( diff, na.rm=T )  ) ) %>% ungroup(.)
  
  res2 <- dplyr::full_join( pc , res )
  
  row_n <- res2$pivot
  
  res2 <- as.data.frame( res2 %>% select( - pivot ) )
  
  row.names(res2) <-   row_n
  
  return(res2)
  
}

#' Tableau de bord de suivi d'indicateurs
#'
#' @param df un tiblle de type rum/rsa avec les variables suivantes : 
#' - structure : service, pole hôpital
#' - dédoublonnage : doublon (cf fonction get_data)
#' - rum/rsa : cle_rsa, ansor, moissor, dureesejpart, nofiness
#' - rmu_v : coeftime, valopmctmonotime1
#' - référentiels : dms_n
#'
#' @return matrix, tableau de bord de suivi d'indicateur
#' @export
#'
#' @examples
get_tdb_indicateurs <- function( df ){
#####################################################################################################################
#Tableau de bord indicateurs
#####################################################################################################################

  tdb <- NULL
  
  #Hospitalisation partielle
  #############################################################
  
  
  #Nombre de séjours HP
  #------------------------------------------------------------
  
  df_hp<- df%>%filter(typehosp == "P")
  tdb[['services']] <- get_diff( round( with( df_hp, tapply(  doublon , list(service,ansor), sum, na.rm=T ) ) ) ) [ , -1 ]
  tdb[['poles']] <- get_diff( round( with( df_hp, tapply(  doublon, list(pole,ansor), sum, na.rm = T) ) ) ) [ , -1 ]
  tdb[['hopitaux']] <- get_diff( round( with( df_hp, tapply(  doublon , list(hopital,ansor), sum, na.rm=T ) ) ) ) [ , -1 ]
  tdb[['gh']] <- get_diff( as.data.frame(t (round( with( df_hp, tapply(  doublon , list(ansor),  sum, na.rm=T ) ) ) ) ) ) [ , -1 ]
  
  #Recettes HP
  #------------------------------------------------------------

  tdb[['services']] <-   cbind( tdb[['services']] , get_diff( round( with( df_hp, tapply( valopmctmonotime1, list(service,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['poles']] <-  cbind( tdb[['poles']] , get_diff( round( with( df_hp, tapply( valopmctmonotime1, list(pole,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['hopitaux']] <-  cbind( tdb[['hopitaux']] , get_diff( round( with( df_hp, tapply( valopmctmonotime1, list(hopital,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['gh']] <-  cbind( tdb[['gh']] , get_diff( as.data.frame(t (round( with( df_hp, tapply(  valopmctmonotime1 , list(ansor), sum, na.rm=T ) ) ) ) ) ) [ , -1 ] )
  
  
  df<- df%>%filter(typehosp == "C")
  
  #Nombre de séjours HC
  #------------------------------------------------------------
  
  tdb[['services']] <-  cbind( tdb[['services']], 'sep' = NA , get_diff( round( with( df, tapply(  doublon , list(service,ansor), sum, na.rm=T ) ) ) ) [ , -1 ])
  tdb[['poles']] <- cbind( tdb[['poles']], 'sep' = NA , get_diff( round( with( df, tapply(  doublon, list(pole,ansor), sum, na.rm = T) ) ) ) [ , -1 ] )
  tdb[['hopitaux']] <- cbind(tdb[['hopitaux']], 'sep' = NA , get_diff( round( with( df, tapply(  doublon , list(hopital,ansor), sum, na.rm=T ) ) ) ) [ , -1 ] )
  tdb[['gh']] <- cbind(tdb[['gh']], 'sep' = NA , get_diff( as.data.frame(t (round( with( df, tapply(  doublon , list(ansor),  sum, na.rm=T ) ) ) ) ) ) [ , -1 ] )
  
  #Recettes HC
  #------------------------------------------------------------
  
  tdb[['services']] <-   cbind( tdb[['services']] , get_diff( round( with( df, tapply( valopmctmonotime1, list(service,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['poles']] <-  cbind( tdb[['poles']] , get_diff( round( with( df, tapply( valopmctmonotime1, list(pole,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['hopitaux']] <-  cbind( tdb[['hopitaux']] , get_diff( round( with( df, tapply( valopmctmonotime1, list(hopital,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['gh']] <- cbind( tdb[['gh']] , get_diff( as.data.frame(t (round( with( df, tapply(  valopmctmonotime1 , list(ansor),  sum, na.rm=T ) ) ) ) ) ) [ , -1 ] )
  
  #PMCT
  #------------------------------------------------------------
  
  tmp <- round( with( df, tapply( valopmctmonotime1, list(service,ansor), sum , na.rm=T ) ) / with( df, tapply( doublon , list(service,ansor), sum , na.rm=T ) ) )
  tdb[['services']] <-   cbind( tdb[['services']] , round( get_diff( tmp ) , 1 ) [ , -1 ] ) 
  
  tmp <- round( with( df, tapply( valopmctmonotime1, list(pole,ansor), sum , na.rm=T ) ) / with( df, tapply( doublon , list(pole,ansor), sum , na.rm=T ) ) )
  tdb[['poles']] <-   cbind( tdb[['poles']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- round( with( df, tapply( valopmctmonotime1, list(hopital,ansor), sum , na.rm=T ) ) / with( df, tapply( doublon , list(hopital,ansor), sum , na.rm=T ) ) )
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- get_diff( as.data.frame( t( round( with( df, tapply(  valopmctmonotime1 , list(ansor), sum, na.rm=T ) )
                                            /
                                              with( df, tapply( doublon , list(ansor), sum, na.rm=T ) )
  ) ) ) )
  
  tdb[['gh']] <- cbind( tdb[['gh']] , tmp[ , - 1 ] )
  
  #Nombre de journnées
  #------------------------------------------------------------
  
  tdb[['services']] <-   cbind( tdb[['services']] , get_diff( round( with( df, tapply( dureesejpart, list(service,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['poles']] <-  cbind( tdb[['poles']] , get_diff( round( with( df, tapply( dureesejpart, list(pole,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['hopitaux']] <-  cbind( tdb[['hopitaux']] , get_diff( round( with( df, tapply( dureesejpart, list(hopital,ansor), sum, na.rm=T ) ) ) )[, -1 ] )
  tdb[['gh']] <-  cbind( tdb[['gh']] , get_diff( as.data.frame(t (round( with( df, tapply(  dureesejpart , list(ansor), sum, na.rm=T ) ) ) ) ) ) [ , -1 ] )
  
  
  #Recettes journalieres
  #------------------------------------------------------------
 
  tmp <- round( with( df, tapply( valopmctmonotime1, list(service,ansor), sum , na.rm=T ) ) / with( df, tapply( dureesejpart , list(service,ansor), sum , na.rm=T ) ) )
  tdb[['services']] <-   cbind( tdb[['services']] , round( get_diff( tmp ) , 1 ) [ , -1 ] ) 
  
  tmp <- round( with( df, tapply( valopmctmonotime1, list(pole,ansor), sum , na.rm=T ) ) / with( df, tapply( dureesejpart , list(pole,ansor), sum , na.rm=T ) ) )
  tdb[['poles']] <-   cbind( tdb[['poles']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- round( with( df, tapply( valopmctmonotime1, list(hopital,ansor), sum , na.rm=T ) ) / with( df, tapply( dureesejpart , list(hopital,ansor), sum , na.rm=T ) ) )
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- get_diff( as.data.frame( t( round( with( df, tapply(  valopmctmonotime1 , list(ansor), sum, na.rm=T ) )
                                            /
                                              with( df, tapply( dureesejpart , list(ansor), sum, na.rm=T ) )
  ) ) ) )
  
  tdb[['gh']] <- cbind( tdb[['gh']] , tmp[ , - 1 ] )
  
  
  #DMR
  #------------------------------------------------------------
  
  tmp <- round( with( df %>% filter( duree > 0 ), tapply( dureesejpart, list( service, ansor ), sum ) )/
                 table( df %>% filter( duree>0 ) %>% distinct( nofiness, cle_rsa, ansor, service, .keep_all = T )%>%select( service, ansor ) ),
               1) %>% get_diff(.)
  
  tdb[['services']] <- cbind( tdb[['services']] , tmp[ , - 1 ] )
  
  tmp <- round( with( df %>% filter( duree > 0 ), tapply( dureesejpart, list( pole, ansor ), sum ) )/
                 table( df %>% filter( duree>0 ) %>% distinct( nofiness, cle_rsa, ansor, service, .keep_all = T ) %>% select( pole, ansor ) ),
               1) %>% get_diff(.)
  
  tdb[['poles']] <- cbind( tdb[['poles']] , tmp[ , - 1 ] )
  
  tmp <- round( with( df %>% filter( duree > 0 ), tapply( dureesejpart, list( hopital, ansor ), sum ) )/
                 table( df %>% filter( duree > 0 ) %>% distinct( nofiness, cle_rsa, ansor, service, .keep_all = T ) %>% select( hopital, ansor ) ),
               1 ) %>% get_diff(.)
  
  tdb[['hopitaux']] <- cbind( tdb[['hopitaux']] , tmp[ , - 1 ] )
  
  
  tmp <- get_diff( as.data.frame( t( round( with( df %>% filter( duree>0 ), tapply(  dureesejpart , list(ansor), sum, na.rm=T ) )
                                                          /
                                              with( df %>% filter( duree>0 ), tapply(  doublon , list(ansor), sum, na.rm=T ) )
                                          ) ) ) )
                                                        
  tdb[['gh']] <- cbind( tdb[['gh']] , tmp[ , - 1 ] )
  
  
  #DMS
  #------------------------------------------------------------
  
  tmp <-  get_diff( round( with( df %>% filter( duree>0, doublon == 1 ), tapply( duree, list( service , ansor ), mean ) ) , 1 ) )
  tdb[['services']] <-    cbind( tdb[['services']] ,  tmp[ , - 1 ] )
  
  tmp <-  get_diff( round(with(df%>%filter( duree>0, doublon == 1 ), tapply( duree, list( pole, ansor ), mean ) ), 1 ) )
  tdb[['poles']] <-    cbind( tdb[['poles']] ,  tmp[ , - 1 ] )
  
  tmp <-  get_diff( round( with( df %>% filter( duree>0, doublon == 1 ), tapply( duree, list( hopital, ansor ), mean ) ), 1 ) )
  tdb[['hopitaux']] <-    cbind( tdb[['hopitaux']] ,  tmp[ , - 1 ] )
  
  tmp <- get_diff( as.data.frame( t( round( with( df %>% filter( duree>0, doublon == 1 ), tapply(  duree , list(ansor), mean, na.rm=T ) ) ) ) ) )
  
  tdb[['gh']] <- cbind( tdb[['gh']] , tmp[ , - 1 ] )
  
  
  
  tmp <- with( df, tapply(dureesejpart, list(service,ansor), sum , na.rm=T ) ) / with( df, tapply( dms_n*coeftime , list(service,ansor), sum , na.rm=T ) )
  tdb[['services']] <-   cbind( tdb[['services']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- with( df, tapply(dureesejpart, list(pole,ansor), sum , na.rm=T ) ) / with( df, tapply( dms_n*coeftime , list(pole,ansor), sum , na.rm=T ) )
  tdb[['poles']] <-   cbind( tdb[['poles']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- with( df, tapply(dureesejpart, list(hopital,ansor), sum , na.rm=T ) ) / with( df, tapply( dms_n*coeftime , list(hopital,ansor), sum , na.rm=T ) )
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , round( get_diff( tmp ) , 1 ) [ , -1 ] )
  
  tmp <- get_diff( as.data.frame( t( round( with( df, tapply(  dureesejpart , list(ansor), sum, na.rm=T ) )
                                            /
                                              with( df, tapply(  dms_n*coeftime , list(ansor), sum, na.rm=T ) )
  ) ) ) )
  
  tdb[['gh']] <- cbind( tdb[['gh']] , tmp[ , - 1 ] )
  
  services = unique( structures%>%filter(service%in%dimnames(tdb$services)[[1]])%>%select(service) )$service
  poles = unique(  structures%>%filter(pole%in%dimnames(tdb$poles)[[1]])%>%select(pole) )$pole
  
  tdb_ind <- rbind("Groupe Hospitalier" = NA, tdb$hopitaux, "Total GH" = tdb$gh )
  
  
  for (p in poles){
    
    serv =services[services%in%structures$service[structures$pole==p]&
                     services%in%row.names(tdb$services)]
    
    tdb_ind<-rbind(tdb_ind,NA,NA)
    
    row.names(tdb_ind)[nrow(tdb_ind)]<-p
    
    
    tdb_ind = rbind(tdb_ind,tdb$services[serv,], "Total pole" = tdb$poles[p,])
    
    if(length(serv)==1){
      row.names(tdb_ind)[nrow(tdb_ind)-1]<-serv
      
    }
    
    
    
  }
  return(tdb_ind)
}

get_tdb_detail_recettes <- function( df, resume = TRUE ){
  #####################################################################################################################
  #Prération tarifs et tarif année antiérieure
  #####################################################################################################################
  
  

  #####################################################################################################################
  #Tableau de bord analyse recettes
  #####################################################################################################################
  
  tdb <- NULL
  tdb_res <- NULL
  
  
  #Diférentiel séjours
  
  tdb[['services']] <- get_diff( round( with( df, tapply(  nas , list(service,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb[['poles']] <- get_diff( round( with( df, tapply(  nas , list(pole,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb[['hopitaux']] <- get_diff( round( with( df, tapply(  nas , list(hopital,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb[['gh']] <- get_diff( as.data.frame(t (round( with( df, tapply(  nas , list(ansor), function(x) length( unique(x) ) ) ) ) ) ) ) [ , -1 ]
  
  tdb_res[['services']] <- get_diff( round( with( df, tapply(  nas , list(service,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb_res[['poles']] <- get_diff( round( with( df, tapply(  nas , list(pole,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb_res[['hopitaux']] <- get_diff( round( with( df, tapply(  nas , list(hopital,ansor), function(x) length( unique(x) ) ) ) ) ) [ , -1 ]
  tdb_res[['gh']] <- get_diff( as.data.frame(t (round( with( df, tapply(  nas , list(ansor), function(x) length( unique(x) ) ) ) ) ) ) ) [ , -1 ]
  
  #Différentiels recettes
  tmp_diff_rec_serv <- get_diff( round( with( df, tapply( valopmctmonotime1, list(service,ansor), sum, na.rm=T ) ) ) )
  tmp_diff_rec_serv[,'diff'][which(is.na(tmp_diff_rec_serv[,'diff']))]<-0
  tdb[['services']] <-   cbind( tdb[['services']] , tmp_diff_rec_serv[, -1 ] )
  tdb_res[['services']] <-   cbind( tdb_res[['services']] , tmp_diff_rec_serv[, -1 ] )
  
  tmp_diff_rec_pole <- get_diff( round( with( df, tapply( valopmctmonotime1, list(pole,ansor), sum, na.rm=T ) ) ) )
  tmp_diff_rec_pole[,'diff'][which(is.na(tmp_diff_rec_pole[,'diff']))]<-0
  tdb[['poles']] <-  cbind( tdb[['poles']] , tmp_diff_rec_pole[, -1 ] )
  tdb_res[['poles']] <-  cbind( tdb_res[['poles']] , tmp_diff_rec_pole[, -1 ] )
  
  tmp_diff_rec_hop <- get_diff( round( with( df, tapply( valopmctmonotime1, list(hopital,ansor), sum, na.rm=T ) ) ) )
  tmp_diff_rec_hop[,'diff'][which(is.na(tmp_diff_rec_hop[,'diff']))]<-0
  tdb[['hopitaux']] <-  cbind( tdb[['hopitaux']] , tmp_diff_rec_hop[, -1 ] )
  tdb_res[['hopitaux']] <-  cbind( tdb_res[['hopitaux']] , tmp_diff_rec_hop[, -1 ] )
  
  tmp_diff_rec_gh <- get_diff( as.data.frame( t (round( with( df, tapply(  valopmctmonotime1 , list(ansor), sum, na.rm=T ) ) ) ) ) ) 
  tdb[['gh']] <-  cbind( tdb[['gh']] , tmp_diff_rec_gh[, -1 ] )
  tdb_res[['gh']] <-  cbind( tdb_res[['gh']] , tmp_diff_rec_gh[, -1 ] )
  
  ##Part tarifs dans différentiel
  tdb[['services']] <-   cbind( tdb[['services']] , "part_tarifs" = NA ) 
  
  tdb[['poles']] <-   cbind( tdb[['poles']] , "part_tarifs" = NA ) 
  
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , "part_tarifs" = NA ) 
  
  tdb[['gh']] <-  cbind( tdb[['gh']] , "part_tarifs" = NA )
  
  
  ##Extrêmes bas (séjours 0 et 1 nuit)
  
  tmp <- with( df %>% filter( duree < 2 ) ,tapply(  nas , list(service,ansor), function(x) length( unique(x) ) ) ) 
  tdb[['services']] <-   cbind( tdb[['services']] ,"sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree < 2 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(service,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_courts_serv <- round( get_diff( tmp ) )
  tmp_sej_courts_serv[which(is.na(tmp_sej_courts_serv[,'diff'])),'diff']<-0
  tdb[['services']] <-   cbind( tdb[['services']] , tmp_sej_courts_serv [, c( -1, -4 )  ] ) 
  tdb_res[['services']] <-   cbind( tdb_res[['services']] ,"sep"=NA, "diff_sej_courts" = tmp_sej_courts_serv [, "diff"  ] ) 
  
  tmp <- with( df %>% filter( duree < 2 ) ,tapply(  nas , list(pole,ansor), function(x) length( unique(x) ) ) ) 
  tdb[['poles']] <-   cbind( tdb[['poles']] ,"sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree < 2 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(pole,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_courts_pole <- round( get_diff( tmp ) )
  tmp_sej_courts_pole[which(is.na(tmp_sej_courts_pole[,'diff'])),'diff']<-0
  tdb[['poles']] <-   cbind( tdb[['poles']] , tmp_sej_courts_pole [, c( -1, -4 )  ] ) 
  tdb_res[['poles']] <-   cbind( tdb_res[['poles']] , "sep"=NA,"diff_sej_courts" = tmp_sej_courts_pole [, "diff" ] ) 
  
  tmp <- with( df %>% filter( duree < 2 ) ,tapply(  nas , list(hopital,ansor), function(x) length( unique(x) ) ) ) 
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] ,"sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree < 2 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(hopital,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_courts_hop <- round( get_diff( tmp ) )
  tmp_sej_courts_hop[which(is.na(tmp_sej_courts_hop[,'diff'])),'diff']<-0
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , tmp_sej_courts_hop [, c( -1, -4 )  ] ) 
  tdb_res[['hopitaux']] <-   cbind( tdb_res[['hopitaux']] , "sep"=NA, "diff_sej_courts" = tmp_sej_courts_hop [, "diff" ] ) 
  
  tmp <- with( df %>% filter( duree < 2 ) ,tapply(  nas , list(ansor), function(x) length( unique(x) ) ) ) 
  tdb[['gh']] <-   cbind( tdb[['gh']] ,"sep" = NA, get_diff( as.data.frame( t( tmp ) ) ) [, c( -1, -4 )  ] )
  
  
  tmp <- with( df %>% filter( duree < 2 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(ansor), sum, na.rm = T ) ) 
  
  tmp_sej_courts_gh <- round( get_diff ( as.data.frame( t( tmp ) ) ) )
  tdb[['gh']] <-   cbind( tdb[['gh']] , tmp_sej_courts_gh [, c( -1, -4 )  ] ) 
  tdb_res[['gh']] <-   cbind( tdb_res[['gh']] , "sep"=NA, "diff_sej_courts" = tmp_sej_courts_gh [, "diff" ] ) 
  
  ##Extrêmes hauts (séjours dms + 30 j)
  
  tmp <- with( df %>% filter( duree > dms_n + 30 ) ,tapply(  nas , list(service,ansor), function(x) length( unique(x) ) ) ) 
  tdb[['services']] <-   cbind( tdb[['services']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(service,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_longs_serv <- round( get_diff( tmp ) )
  tmp_sej_longs_serv[which(is.na(tmp_sej_longs_serv[,'diff'])),'diff']<-0
  tdb[['services']] <-   cbind( tdb[['services']] , tmp_sej_longs_serv [, c( -1, -4 )  ] ) 
  tdb_res[['services']] <-   cbind( tdb_res[['services']] , "diff_sej_longs" = tmp_sej_longs_serv [, "diff" ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 ) ,tapply(  nas , list(pole,ansor), function(x) length( unique(x) ) ) ) 
  tdb[['poles']] <-   cbind( tdb[['poles']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(pole,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_longs_pole <- round( get_diff( tmp ) )
  tmp_sej_longs_pole[which(is.na(tmp_sej_longs_pole[,'diff'])),'diff']<-0
  tdb[['poles']] <-   cbind( tdb[['poles']] , tmp_sej_longs_pole [, c( -1, -4 )  ] ) 
  tdb_res[['poles']] <-   cbind( tdb_res[['poles']] , "diff_sej_longs" = tmp_sej_longs_pole [, "diff"  ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 ) ,tapply(  nas , list(hopital,ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(hopital,ansor), sum, na.rm = T ) ) 
  
  tmp_sej_longs_hop <- round( get_diff( tmp ) )
  tmp_sej_longs_hop[which(is.na(tmp_sej_longs_hop[,'diff'])),'diff']<-0
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , tmp_sej_longs_hop [, c( -1, -4 )  ] ) 
  tdb_res[['hopitaux']] <-   cbind( tdb_res[['hopitaux']] , "diff_sej_longs" = tmp_sej_longs_hop [,  "diff" ] ) 
  
  
  tmp <- with( df %>% filter( duree > dms_n + 30 ) ,tapply(  nas , list(ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['gh']] <-   cbind( tdb[['gh']] , "sep" = NA, get_diff( as.data.frame( t( tmp ) ) ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df %>% filter( duree > dms_n + 30 )%>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(ansor), sum, na.rm = T ) ) 
  
  tmp_sej_longs_gh <- round( get_diff( as.data.frame( t( tmp ) ) ) )
  tdb[['gh']] <-   cbind( tdb[['gh']] , tmp_sej_longs_gh [, c( -1, -4 )  ] ) 
  tdb_res[['gh']] <-   cbind( tdb_res[['gh']] ,  "diff_sej_longs" = tmp_sej_longs_gh [, "diff"  ] )
  
  #Sejours 2N et dms + < 30 jours
  
  df2 <- df %>% filter(duree >= 2, duree <= dms_n + 30 )
  
  tmp <- with( df2 ,tapply(  nas , list(service,ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['services']] <-   cbind( tdb[['services']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] )
  
  tmp <- with( df2 %>% mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(service,ansor), sum, na.rm = T ) ) 
  
  tmp_s2n_serv <- round( get_diff( tmp ) )
  tmp_s2n_serv[which(is.na(tmp_s2n_serv[,'diff'])),'diff']<-0
  tdb[['services']] <-   cbind( tdb[['services']] , tmp_s2n_serv [, c( -1, -4 )  ] ) 
  tdb_res[['services']] <-   cbind( tdb_res[['services']] , "diff_sej_2N" = tmp_s2n_serv [, "diff"  ] ) 
  
  
  tmp <- with( df2 ,tapply(  nas , list(pole,ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['poles']] <-   cbind( tdb[['poles']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df2 %>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(pole,ansor), sum, na.rm = T ) ) 
  
  tmp_s2n_pole <- round( get_diff( tmp ) )
  tmp_s2n_pole[which(is.na(tmp_s2n_pole[,'diff'])),'diff']<-0
  tdb[['poles']] <-   cbind( tdb[['poles']] , tmp_s2n_pole [, c( -1, -4 )  ] ) 
  tdb_res[['poles']] <-   cbind( tdb_res[['poles']] ,  "diff_sej_2N" =  tmp_s2n_pole [, "diff"  ] ) 
  
  tmp <- with( df2 ,tapply(  nas , list(hopital,ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , "sep" = NA, get_diff( tmp ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df2 %>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(hopital,ansor), sum, na.rm = T ) ) 
  
  tmp_s2n_hop <- round( get_diff( tmp ) )
  tmp_s2n_hop[which(is.na(tmp_s2n_hop[,'diff'])),'diff']<-0
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] , tmp_s2n_hop [, c( -1, -4 )  ] ) 
  tdb_res[['hopitaux']] <-   cbind( tdb_res[['hopitaux']] , "diff_sej_2N" = tmp_s2n_hop [, "diff"  ] ) 
  
  
  
  
  tmp <- with( df2 ,tapply(  nas , list(ansor), function(x) length( unique(x) ) ) ) 
  
  tdb[['gh']] <-   cbind( tdb[['gh']] , "sep" = NA, get_diff( as.data.frame( t( tmp ) ) ) [, c( -1, -4 )  ] ) 
  
  tmp <- with( df2 %>%mutate(valopmctmonotime1 = if_else(ansor == as.character(annee),valopmctmonotime1_tarifsante,valopmctmonotime1)) ,
               tapply(  valopmctmonotime1 , list(ansor), sum, na.rm = T ) ) 
  
  tmp_s2n_gh <- round( get_diff( as.data.frame( t( tmp ) ) ) )
  tmp_s2n_gh[which(is.na(tmp_s2n_gh[,'diff'])),'diff']<-0
  tdb[['gh']] <-   cbind( tdb[['gh']] , tmp_s2n_gh [, c( -1, -4 )  ] ) 
  tdb_res[['gh']] <-   cbind( tdb_res[['gh']] , "diff_sej_2N"  = tmp_s2n_gh [, "diff"  ] ) 
  
  ##Recalcul de la part tarifs
  #tmp_diff_tarifs_serv <- with( df%>%mutate(diff_tarifs = round( valopmctmonotime1 - valopmctmonotime1_tarifsante ) ), tapply(  diff_tarifs, list(service,ansor), sum, na.rm=T )  )[, -1 ]
  #tmp_diff_tarifs_serv[which(is.na(tmp_diff_tarifs_serv))]<-0
  tdb[['services']][,"part_tarifs"] <- tmp_diff_rec_serv[,'diff'] - tmp_s2n_serv[,'diff']- tmp_sej_longs_serv[,'diff'] - tmp_sej_courts_serv[,'diff']
  tdb[['poles']][,"part_tarifs"] <- tmp_diff_rec_pole[,'diff'] - tmp_s2n_pole[,'diff']- tmp_sej_longs_pole[,'diff'] - tmp_sej_courts_pole[,'diff']
  tdb[['hopitaux']][,"part_tarifs"] <- tmp_diff_rec_hop[,'diff'] - tmp_s2n_hop[,'diff']- tmp_sej_longs_hop[,'diff'] - tmp_sej_courts_hop[,'diff']
  tdb[['gh']][,"part_tarifs"] <- tmp_diff_rec_gh[,'diff'] - tmp_s2n_gh[,'diff']- tmp_sej_longs_gh[,'diff'] - tmp_sej_courts_gh[,'diff']
  
  
  ## Différences liées niveaux de sévérité
  tmp_diff_ns_serv <- detail_diff_recettes ( df2, "service" )
  tmp_diff_ns_serv[,'part_ns'][which(is.na(tmp_diff_ns_serv[,'part_ns']))]<-0
  tdb[['services']] <- cbind( tdb[['services']],"sep" = NA, tmp_diff_ns_serv )
  tdb_res[['services']] <- cbind( tdb_res[['services']],"sep" = NA, "part_ns" = tmp_diff_ns_serv[,-1] )
  
  tmp_diff_ns_pole <- detail_diff_recettes (  df2, "pole" )
  tmp_diff_ns_pole[,'part_ns'][which(is.na(tmp_diff_ns_pole[,'part_ns']))]<-0
  tdb[['poles']]<- cbind( tdb[['poles']],"sep" = NA, tmp_diff_ns_pole )
  tdb_res[['poles']]<- cbind( tdb_res[['poles']],"sep" = NA, "part_ns" = tmp_diff_ns_pole[,-1] )
  
  tmp_diff_ns_hop <- detail_diff_recettes (  df2, "nofiness" )
  tmp_diff_ns_hop[,'part_ns'][which(is.na(tmp_diff_ns_hop[,'part_ns']))]<-0
  tdb[['hopitaux']]<- cbind( tdb[['hopitaux']], "sep" = NA, tmp_diff_ns_hop )
  tdb_res[['hopitaux']]<- cbind( tdb_res[['hopitaux']], "sep" = NA, "part_ns" = tmp_diff_ns_hop[,-1] )
  
  tmp_diff_ns_gh <- detail_diff_recettes ( df2 %>%mutate(pivot = "GH"), "pivot" )
  tdb[['gh']]<- cbind( tdb[['gh']], "sep" = NA, tmp_diff_ns_gh )
  tdb_res[['gh']]<- cbind( tdb_res[['gh']], "sep" = NA, "part_ns" = tmp_diff_ns_gh[,-1] )
  
  tmp_diff_supp_serv <- get_diff( round( with( df2 ,
                                               tapply( rec_sup_repa, list(service,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tmp_diff_ex_serv <- get_diff( round( with( df2 , 
                                             tapply( rec_exbh, list(service,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tdb[['services']] <-   cbind( tdb[['services']] ,
                                "part_supp" = tmp_diff_supp_serv,
                                "part_ex" = tmp_diff_ex_serv,
                                "part_activite" = tmp_s2n_serv[,'diff'] -
                                  tmp_diff_ns_serv[,'part_ns'] - 
                                  tmp_diff_supp_serv - 
                                  tmp_diff_ex_serv ) 
  
  tdb_res[['services']] <-   cbind( tdb_res[['services']] ,
                                    "part_supp" = tmp_diff_supp_serv,
                                    "part_ex" = tmp_diff_ex_serv,
                                    "part_activite" = tmp_s2n_serv[,'diff'] -
                                      tmp_diff_ns_serv[,'part_ns'] - 
                                      tmp_diff_supp_serv - 
                                      tmp_diff_ex_serv ) 
  
  
  tmp_diff_supp_pole <- get_diff( round( with( df2 ,
                                               tapply( rec_sup_repa, list(pole,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tmp_diff_ex_pole <- get_diff( round( with( df2 , 
                                             tapply( rec_exbh, list(pole,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tdb[['poles']] <-   cbind( tdb[['poles']] ,
                             "part_supp" = tmp_diff_supp_pole,
                             "part_ex" = tmp_diff_ex_pole,
                             "part_activite" = tmp_s2n_pole[,'diff'] -
                               tmp_diff_ns_pole[,'part_ns'] - 
                               tmp_diff_supp_pole - 
                               tmp_diff_ex_pole ) 
  
  tdb_res[['poles']] <-   cbind( tdb_res[['poles']] ,
                                 "part_supp" = tmp_diff_supp_pole,
                                 "part_ex" = tmp_diff_ex_pole,
                                 "part_activite" = tmp_s2n_pole[,'diff'] -
                                   tmp_diff_ns_pole[,'part_ns'] - 
                                   tmp_diff_supp_pole - 
                                   tmp_diff_ex_pole ) 
  
  tmp_diff_supp_hop <- get_diff( round( with( df2 ,
                                              tapply( rec_sup_repa, list(hopital,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tmp_diff_ex_hop <- get_diff( round( with( df2 , 
                                            tapply( rec_exbh, list(hopital,ansor), sum, na.rm=T ) ) ) )[,'diff']
  
  tdb[['hopitaux']] <-   cbind( tdb[['hopitaux']] ,
                                "part_supp" = tmp_diff_supp_hop,
                                "part_ex" = tmp_diff_ex_hop,
                                "part_activite" = tmp_s2n_hop[,'diff'] -
                                  tmp_diff_ns_hop[,'part_ns'] - 
                                  tmp_diff_supp_hop - 
                                  tmp_diff_ex_hop ) 
  
  
  tdb_res[['hopitaux']] <-   cbind( tdb_res[['hopitaux']] ,
                                    "part_supp" = tmp_diff_supp_hop,
                                    "part_ex" = tmp_diff_ex_hop,
                                    "part_activite" = tmp_s2n_hop[,'diff'] -
                                      tmp_diff_ns_hop[,'part_ns'] - 
                                      tmp_diff_supp_hop - 
                                      tmp_diff_ex_hop ) 
  
  tmp <- with( df2 , tapply( rec_sup_repa, list(ansor), sum, na.rm=T ) )
  tmp_diff_supp_gh <- round( tmp[2] - tmp[1] )
  
  tmp <- with( df2 , tapply( rec_exbh, list(ansor), sum, na.rm=T ) )
  tmp_diff_ex_gh <-  round( tmp[2] - tmp[1] )
  
  tdb[['gh']] <-   cbind( tdb[['gh']] ,
                          "part_supp" = tmp_diff_supp_gh,
                          "part_ex" = tmp_diff_ex_gh,
                          "part_activite" = tmp_s2n_gh[,'diff'] -
                            tmp_diff_ns_gh[,'part_ns'] - 
                            tmp_diff_supp_gh - 
                            tmp_diff_ex_gh ) 
  
  
  tdb_res[['gh']] <-   cbind( tdb_res[['gh']] ,
                              "part_supp" = tmp_diff_supp_gh,
                              "part_ex" = tmp_diff_ex_gh,
                              "part_activite" = tmp_s2n_gh[,'diff'] -
                                tmp_diff_ns_gh[,'part_ns'] - 
                                tmp_diff_supp_gh - 
                                tmp_diff_ex_gh ) 
  
  tdb_res[['services']] <-  cbind( tdb_res[['services']],
                                   "sep"= NA,
                                   "part_tarifs" = tmp_diff_rec_serv[,'diff'] - tmp_s2n_serv[,'diff']- tmp_sej_longs_serv[,'diff'] - tmp_sej_courts_serv[,'diff'] )
  tdb_res[['poles']]<-  cbind( tdb_res[['poles']],
                               "sep"= NA,
                               "part_tarifs" = tmp_diff_rec_pole[,'diff'] - tmp_s2n_pole[,'diff']- tmp_sej_longs_pole[,'diff'] - tmp_sej_courts_pole[,'diff'] )
  tdb_res[['hopitaux']] <-  cbind( tdb_res[['hopitaux']],
                                   "sep"= NA,
                                   "part_tarifs" = tmp_diff_rec_hop[,'diff'] - tmp_s2n_hop[,'diff']- tmp_sej_longs_hop[,'diff'] - tmp_sej_courts_hop[,'diff'] )
  tdb_res[['gh']] <-  cbind( tdb_res[['gh']],
                             "sep"= NA,
                             "part_tarifs" = tmp_diff_rec_gh[,'diff'] - tmp_s2n_gh[,'diff']- tmp_sej_longs_gh[,'diff'] - tmp_sej_courts_gh[,'diff'] )
  
  tmp <- round( with( df  ,tapply( valopmctmonotime1 - valopmctmonotime1_nonconsol , list(service), sum, na.rm =T ) ) )
  tdb[['services']] <- cbind( tdb[['services']] , "sep" = NA, "diff_M12" = tmp)
  tdb_res[['services']] <- cbind( tdb_res[['services']] ,"sep" = NA, "diff_M12" = tmp)
  
  tmp <- round( with( df  ,tapply( valopmctmonotime1 - valopmctmonotime1_nonconsol , list(pole), sum, na.rm =T ) ) )
  tdb[['poles']] <- cbind( tdb[['poles']] ,"sep" = NA, "diff_M12" = tmp)
  tdb_res[['poles']] <- cbind( tdb_res[['poles']] ,"sep" = NA, "diff_M12" = tmp)
  
  tmp <- round( with( df  ,tapply( valopmctmonotime1 - valopmctmonotime1_nonconsol , list(hopital), sum, na.rm =T ) ) ) 
  tdb[['hopitaux']] <- cbind( tdb[['hopitaux']] ,"sep" = NA, "diff_M12" = tmp)
  tdb_res[['hopitaux']] <- cbind( tdb_res[['hopitaux']] ,"sep" = NA, "diff_M12" = tmp)
  
  tdb[['gh']] <- cbind( tdb[['gh']] ,"sep" = NA, "diff_M12" = round( sum( df$valopmctmonotime1 - df$valopmctmonotime1_nonconsol, na.rm=T ) ) )
  tdb_res[['gh']] <- cbind( tdb_res[['gh']] ,"sep" = NA, "diff_M12" = round( sum( df$valopmctmonotime1 - df$valopmctmonotime1_nonconsol, na.rm=T ) ) )
  
  
  services = unique( structures%>%filter(service%in%dimnames(tdb$services)[[1]])%>%select(service) )$service
  poles = unique(  structures%>%filter(pole%in%dimnames(tdb$poles)[[1]])%>%select(pole) )$pole
  
  tdb_ <- rbind("Groupe Hospitalier" = NA, tdb$hopitaux, "Total GH" = tdb$gh )
  tdb_res_ <- rbind("Groupe Hospitalier" = NA, tdb_res$hopitaux, "Total GH" = tdb_res$gh )
  
  for (p in poles){
    
    serv =services[services%in%structures$service[structures$pole==p]&
                     services%in%row.names(tdb$services)]
    
    tdb_<-rbind(tdb_,NA,NA)
    tdb_res_<-rbind(tdb_res_,NA,NA)
    row.names(tdb_)[nrow(tdb_)]<-p
    row.names(tdb_res_)[nrow(tdb_res_)]<-p
    
    tdb_ = rbind(tdb_,tdb$services[serv,], "Total pole" = tdb$poles[p,])
    tdb_res_ = rbind(tdb_res_,tdb_res$services[serv,], "Total pole" = tdb_res$poles[p,])
    
    if(length(serv)==1){
      row.names(tdb_)[nrow(tdb_)-1]<-serv
      row.names(tdb_res_)[nrow(tdb_res_)-1]<-serv
    }
    
    
    
  }
  
  if(resume){
    return(tdb_res_)
  }else{
    return(tdb_)
  }
  
}


#' Tableau de bord général d'activité avec un compte des séjours, et séparant HDJ/HC
#'
#' @param df un tableau de données de type séjours rum/rsa
#'
#' @return tableau de bord d'activité général en séjours
#' @examples
#' \dontrun{
#' 
#'    get_activite_sejours( df, structure ) -> tdb
#' }
#' 
#' @export get_activite_sejours
#' @usage make_tdb(val, niveau, annee, mois )
#' @export 

get_activite_sejours<-function( df, structure ){
  
    tdb <-list()
    
    df<-df%>%mutate(typehosp = factor(typehosp))
    
    tdb[['services']] <- table( df %>%  distinct( nofiness, cle_rsa, ansor, service, .keep_all = T)%>%
                                 select(service, ansor, typehosp) )
    
    tdb[['poles']] <- table( df %>%  distinct( nofiness, cle_rsa, ansor, service, .keep_all = T )%>%
                              select( pole, ansor, typehosp ) )
    
    tdb[['hopitaux']]  <- table( df %>%  distinct(nofiness, cle_rsa, ansor, .keep_all = T)%>%
                                  select(hopital, ansor, typehosp ) )
    
    tdb[['gh']] <- table(df %>% distinct( nofiness, cle_rsa, ansor, .keep_all = T )%>%
                           select( typehosp, ansor ) )

    tdb <- dimRactivite:::order_by_structure( tdb, structure )

    tdb_final<-cbind( get_diff(tdb$hc), NA, get_diff(tdb$hp) )

    return(tdb_final)

}

#' Tableau de bord général d'activité en recettes, et séparant HDJ/HC
#'
#' @param df un tableau de données de type séjours rum/rsa
#'
#' @return tableau de bord d'activité général en recettes
#' @examples
#' \dontrun{
#' 
#'    get_activite_recettes( df, structure ) -> tdb
#' }
#' @export get_activite_recettes
#' @usage make_tdb(val, niveau, annee, mois )
#' @export 

get_activite_recettes<-function( df, structures ){
  
  tdb_v <-list()
  
  df<-df%>%mutate(typehosp = factor(typehosp))

  tdb_v[['services']] <- round( with( df, tapply( valopmctmonotime1, list(service,ansor,typehosp), sum, na.rm=T ) ) )
  
  tdb_v[['poles']]  <- round( with( df, tapply( valopmctmonotime1, list(pole,ansor,typehosp), sum, na.rm=T ) ) )
  
  tdb_v[['hopitaux']] <- round( with( df, tapply( valopmctmonotime1, list(hopital,ansor,typehosp), sum, na.rm=T ) ) )
  
  tdb_v[['gh']] <- t(round( with( df, tapply( valopmctmonotime1, list(ansor,typehosp), sum, na.rm=T ) ) ))

  tdb_v <- order_by_structure( tdb_v, structures )

  tdb_v_final<-cbind( get_diff(tdb_v$hc), NA, get_diff(tdb_v$hp) )

  return(tdb_v_final)

}

#' Création des tableaux de bord
#'
#' @param val 
#' @param niveau
#' @param annee
#' @param mois 
#'
#' @return tableau de bord d'activité
#' @examples
#' \dontrun{
#' 
#'    make_tdb(df, indicateurs) -> tdb
#' }
#' @export make_tdb
#' @usage make_tdb(val, niveau, annee, mois )
#' @export 
make_tdb <- function( val, niveau, annee, mois ){
  
  #Nom des tableaux de bord disponibles
  
  if (!prep_string(val)%in%names(references)) {
    message(
      "\n",
      toString(niveau)," " , toString(val), " non référencé dans le fichier d'indicateurs \n"
    )
    return(NA)
  }
  
  #Vérifer que la variable de profondeur des tableaux de bord est bien disponible
  if(is.null(getOption('dimRactivite.profondeur_tdb'))){
    set_option(profondeur_tdb,5)
  }
  
  noms_tableaux = references%>%filter(!!sym(prep_string(val)) == 'o', !is.na(tdb))%>%mutate(tdb = str_split(tdb,','))%>%select(tdb)
  noms_tableaux =  unique(unlist(noms_tableaux))
  
  tdb<-list()
  #Données cumulées
  df<-get_data( rum, ref = "ansor", m = 1:mois, a = (annee-getOption('dimRactivite.profondeur_tdb')):annee, val, niveau, opt = T )%>%
    mutate( pivot = factor( ansor, levels = ( annee - getOption('dimRactivite.profondeur_tdb')):annee ) )
  
  if (nrow(df)==0) {
    message(
      "\n",
      toString(niveau)," " , toString(val), " aucune donnée retrouvée dans les rum \n"
    )
    return(NA)
  }
  
  
  df<-dplyr::left_join(inner_join(df,rum_v),
                inner_join(rsa,rsa_v)%>%mutate(anseqta = as.numeric(anseqta))%>%select(nofiness,cle_rsa,ansor,noghs,
                                                                                       anseqta,duree,rec_base,nbrum,noseqrum,
                                                                                       ghm,duree,nbjrbs,sejinfbi,rec_totale,rec_base,
                                                                                       rec_exh,rec_exb))%>%
    dplyr::left_join(.,rsa_dms)%>%
    dplyr::left_join(.,vano%>%inner_join(., df%>%select(nofiness,cle_rsa,ansor,nas)) %>% select( nas, noanon )%>% distinct(nas,noanon))
  
  #Données utiles pour la valorisation
  df<-dplyr::left_join( df %>% mutate(  anseqta = as.character(anseqta) ),
                        tarifs_mco_ghs%>%rename( noghs = ghs, bb = borne_basse, bh = borne_haute )%>%select( anseqta, ghm, noghs, bb, bh ) )
  
  
  
  for (nom_tableau in noms_tableaux){
    
    tdb[[nom_tableau]]<-list()
    
    print(paste(val,nom_tableau))
    
    inds=get_indicateurs(nom=nom_tableau,val=val)
    
    tdb[[nom_tableau]][['cum']]<-get_tdb(df=df,indicateurs=inds)
    
    write.table(tdb[[nom_tableau]][['cum']],file = file.path(getOption('dimRactivite.path_tdb_files'),annee,paste0(nom_tableau,prep_string(val),annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')),sep='\t',row.names=F,na='')
    
    
  }
  
  #Données mensuelles
  df <- df %>% filter( as.numeric(moissor) == mois )
  
  if (nrow(df)==0) {
    message(
      "\n",
      toString(niveau)," " , toString(val), " aucune donnée retrouvée dans les rum mensuels (",toString(mois),") \n"
    )
    return(NA)
  }
  
  
  for (nom_tableau in noms_tableaux){
    
    inds=get_indicateurs(nom=nom_tableau,val=val)
    
    tdb[[nom_tableau]][['mens']]<-get_tdb(df=df,indicateurs=inds)
    
    write.table(tdb[[nom_tableau]][['mens']],file = file.path(getOption('dimRactivite.path_tdb_files'),annee,paste0(nom_tableau,prep_string(val),annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')),sep='\t',row.names=F,na='')
  }
  
  
  
  return(tdb)
  
}

