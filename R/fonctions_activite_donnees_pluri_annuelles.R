
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
    
    df<-df%>%dplyr::mutate(pivot = !!sym(pivot))%>%
      dplyr::mutate(pivot = factor(as.numeric(format(pivot,unit_pivot))))
    
  }
  

  #-------------------------------------------------------------------
  #Utile pour le calcul du nombre de lit utilisé sur la période d'étude
  #--------------------------------------------------------------------
  #Les bornes de dates pour chaque niveau de la variable pivot (ex : année 2014, borne inf = 01-01-2014, borne sup = 31-12-2014)
  df<-df%>%dplyr::group_by(pivot)%>%
    dplyr::mutate(date_min_pivot = as.Date(paste0(min(as.numeric(df$annee)),'-', min(as.numeric(df$mois)),'-01')),
                                    date_max_pivot =  ifelse(max(as.numeric(df$mois)) == 12,
                                                            as.Date(paste0(max(as.numeric(df$annee)),'-12-31'))+1,
                                                            as.Date(paste0(max(as.numeric(df$annee)),'-', max(as.numeric(df$mois))+1,'-01'))))%>%

        dplyr::ungroup()%>%
    dplyr::mutate(date_max_pivot = as.Date(date_max_pivot,origin = "1970-01-01"))

  if( 'dateentree_rum' %in% names(df) ){
    
    df<-df%>%dplyr::mutate(dateentree_rum2 = as.Date(ifelse(date_min_pivot < dateentree_rum,dateentree_rum,date_min_pivot),origin = '1970-01-01'),
                    datesortie_rum2 = as.Date(ifelse(date_max_pivot > datesortie_rum,datesortie_rum,date_max_pivot),origin = '1970-01-01' ),
                    duree_rum2 = ifelse(datesortie_rum2 > dateentree_rum2, datesortie_rum2 - dateentree_rum2, 0 ) )


  }else{
    df<-df%>% dplyr::mutate(dtent2 = as.Date(ifelse(date_min_pivot < dtent, dtent, date_min_pivot ),origin = '1970-01-01'),
                    dtsort2 = as.Date(ifelse(date_max_pivot > dtsort, dtsort, date_max_pivot ),origin = '1970-01-01' ),
                    duree_rum2 = ifelse(dtsort2 > dtent2,dtsort2 - dtent2,0 ) )


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
    tb[['HCtot']] <- table( df %>% dplyr::filter( type_hospum_1=="C", doublon==1 ) %>% dplyr::select( pivot ) )
  }

  if('nb_jour_hc_sej'%in%indicateurs){

    tb[['nb_jour_hc_sej']]<-with( df%>% dplyr::filter( type_hospum_1=="C" ,doublon==1 ),
                         tapply( duree, pivot, sum ) )
  }
  
  if('nb_jour_hc'%in%indicateurs){
    
    tb[['nb_jour_hc']]<-with( df%>% dplyr::filter( type_hospum_1=="C" ,doublon==1 ),
                         tapply( duree_rum, pivot, sum ) )
  }


  ###########################################################################
  #Nb de sejours HC 0 nuit
  ###########################################################################
  if('HC0'%in%indicateurs){

    tb[['HC0']]<-table(df %>% dplyr::filter(type_hospum_1=="C", duree == 0, doublon==1) %>%select(pivot) )

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
  #Séjours multi-UMA
  ###########################################################################      
  if('nb_sej_multi_uma'%in%indicateurs){
    
    tb[['nb_sej_multi_uma']]<-table(df %>% dplyr::filter(rum > 1 , doublon==1 ) %>%select(pivot) ) 
  }
  
  if('p_sej_multi_uma'%in%indicateurs){
    
    tb[['p_sej_multi_uma']]<- round( table(df %>% dplyr::filter(rum > 1 , doublon==1 ) %>%select(pivot) ) *100 / tb[['HCtot']] )
  }
  
  ###########################################################################
  #Nb d'hosptialisations partielles
  ###########################################################################
  if('HPTot'%in%indicateurs){

    tb[['HPTot']]<-table(df %>% dplyr::filter(type_hospum_1=="P", doublon==1) %>%select(pivot) )
  }

  ###########################################################################
  #Nb Seances
  ############################################################################

  if('Seances'%in%indicateurs){

    tb[['Seances']]<-table(df %>% dplyr::filter(ghm%in%ghmseances$ghm, doublon==1) %>%select(pivot) )

  }
  ###########################################################################
  #
  ############################################################################
  if('SeancesDialyses'%in%indicateurs){

    tb[['SeancesDialyses']]<-table(df %>% dplyr::filter(ghm%in%ghmseances$ghm[ghmseances$type=='Dialyse'],
                                                        doublon==1) %>%
                                     select(pivot) )
  }

  ###########################################################################
  #Nb Seances de chimiotherapie
  ###########################################################################
  if('SeancesChimio'%in%indicateurs){

    tb[['SeancesChimio']]<-table(df %>% dplyr::filter(ghm%in%ghmseances$ghm[ghmseances$type=='Chimio'],
                                                      doublon==1) %>%
                                   select(pivot) )
  }

  ###########################################################################
  #Nb Seances de radiotherapie
  ###########################################################################
  if('SeancesRadio'%in%indicateurs){

    tb[['SeancesRadio']]<-table(df %>% dplyr::filter(ghm%in%ghmseances$ghm[ghmseances$type=='Radio'],
                                                     doublon==1) %>%
                                  select(pivot) )
  }
  ###########################################################################
  #Nb Seances preparation radiotherapie
  ###########################################################################
  if('SeancesPrepaRadio'%in%indicateurs){

    tb[['SeancesPrepaRadio']]<-table(df %>% dplyr::filter(ghm%in%ghmseances$ghm[ghmseances$type=='PrepaRadio'],
                                                          doublon==1) %>%
                                       select(pivot) )
  }
  ###########################################################################
  #Nb d'hopitaux de jour medicaux
  ###########################################################################
  if('HPMed'%in%indicateurs){

    tb[['HPMed']]<-table(df %>% dplyr::filter(type_hospum_1=="P", substr(ghm,3,3)=='M', doublon==1) %>% select(pivot) )
  }

  ###########################################################################
  #Nb d'hopitaux de jour chirurgicaux
  ###########################################################################

  if('HPChir'%in%indicateurs){
    tb[['HPChir']]<-table(df %>% dplyr::filter(type_hospum_1=="P", substr(ghm,3,3)=='C', doublon==1) %>% select(pivot) )
  }



  if('HPMedTech'%in%indicateurs){
    tb[['HPMedTech']]<-table(df %>% dplyr::filter(type_hospum_1=="P", substr(ghm,3,3)=='K', doublon==1) %>% select(pivot) )
  }



  if('NbIVG'%in%indicateurs){
    tb[['NbIVG']]<-table(df %>% dplyr::filter(ghm=='14Z08Z', doublon==1) %>% select(pivot) )
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
  #  tb[['VolBlocHP']]<-table(DataIpop$pivot[which(!DataIpop$type_hospum_1=="C")])
  #}

  #Pourcentage de séjours chirurgicaux
  ############################################################################
  if('nb_sej_chir'%in%indicateurs){
    
    tb[['nb_sej_chir']]<-round(
      table(df %>% dplyr::filter( substr(ghm,3,3)=='C', doublon==1) %>% select(pivot) )
    )
  }
  

  if('p_sej_chir_hc'%in%indicateurs){

    tb[['p_sej_chir_hc']]<-round(
      table(df %>% dplyr::filter(type_hospum_1=="C", substr(ghm,3,3)=='C', doublon==1) %>% select(pivot) )*100/
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
  if('nb_sej_chir_0j'%in%indicateurs){

    tb[['nb_sej_chir_0j']]<-round(
      table(df %>% dplyr::filter(duree==0, substr(ghm,3,3)=='C', doublon==1) %>%  dplyr::select(pivot) )
    )
  }
  
  if('p_sej_chir_0j'%in%indicateurs){
    
    tb[['p_sej_chir_0j']]<-round(
      table(df %>% dplyr::filter(duree==0, substr(ghm,3,3)=='C', doublon==1) %>%  dplyr::select(pivot) )*100/
        table(df %>% dplyr::filter( substr(ghm,3,3)=='C', doublon==1)  %>%  dplyr::select(pivot)),1
    )
  }

  #if('pSejChirOjUCA'%in%indicateurs){
  #  tb[['pSejChir0jUCA']]<-round(
  #    table(df$pivot[substr(df$ghm,3,3)=='C'&df$duree_sej==0&df$uma_locale2%in%UHambu])*100/
  #      table(df$pivot[substr(df$ghm,3,3)=='C'&df$duree_sej==0])
  #  )
  #}
  #Activite unite chirurige ambulatoire :
  #if('ActiviteUniteChirAmbu'%in%indicateurs){
  #  tb[['NbSejUCA']]<-table(df$pivot[df$uma_locale2%in%UHambu])
  #  tb[['pSejChirUCA']]<-round(table(df$pivot[df$uma_locale2%in%UHambu&substr(df$ghm,3,3)=='C'])*100/tb[['NbSejUCA']])
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

    tb[['Greffes']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,
                                                                organe%in%c('AutoGreffeCellulesSouches',
                                                                            'AllogreffeCellulesSouches'))$ghm,
                                                doublon==1) %>%
                             select(pivot) )

  }


  if('AutoGreffes'%in%indicateurs){

    tb[['AutoGreffes']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,
                                                                    organe%in%c('AutoGreffeCellulesSouches'))$ghm,
                                                    doublon==1) %>% select(pivot) )

  }

  if('AlloGreffes'%in%indicateurs){

    tb[['AlloGreffes']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,
                                                                    organe%in%c('AllogreffeCellulesSouches'))$ghm,
                                                    doublon==1) %>%
                                 select(pivot) )
  }

  if('Transplantations'%in%indicateurs){

    tb[['Transplantations']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,
                                                                         organe%in%c('Rein','Pancreas'))$ghm,
                                                         doublon==1) %>%
                                      select(pivot) )
  }
  if('TransplantationsRenales'%in%indicateurs){

    tb[['TransplantationsRenales']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,organe%in%c('Rein'))$ghm,
                                                                doublon==1) %>%
                                             select(pivot) )
  }
  if('TransplantationsPancreas'%in%indicateurs){

    tb[['TransplantationsPancreas']]<-table(df %>% dplyr::filter(ghm%in%subset(ghmgreffes,
                                                                                 organe%in%c('Pancreas'))$ghm,
                                                                 doublon==1) %>%
                                              select(pivot) )
  }

  if('TherapieCOD'%in%indicateurs){
    tb[['TherapieCOD']]<-table(df%>%filter(cdurm%in%c(76885), doublon==1)%>%select(pivot))
  }

  if('DiagnosticCOD'%in%indicateurs){
    tb[['DiagnosticCOD']]<-table(df%>%filter(cdurm%in%c(76689,76208), doublon==1)%>%select(pivot))

  }

  if('TherapieInflammatoire'%in%indicateurs){
    tb[['TherapieInflammatoire']]<-table(df%>%filter(cdurm%in%c(76686,76684), doublon==1)%>%select(pivot))

  }

  if('DiagnosticInflammatoire'%in%indicateurs){
    tb[['DiagnosticInflammatoire']]<-table(df%>%filter(cdurm%in%c(76687,76206), doublon==1)%>%select(pivot))

  }

  if('PCHIR'%in%indicateurs){

    tb[['PCHIR']]<-round(
      table(df %>% dplyr::filter(type_hospum_1=="C", substr(ghm,3,3)=='C', doublon==1) %>%  dplyr::select(pivot) )*100/
        table(df %>% dplyr::filter(type_hospum_1=="C", doublon==1)  %>%  dplyr::select(pivot)),1
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
    tb[['DMR']]<-round( with( df %>% filter( duree > 0 ), tapply( duree_rum, pivot, sum, na.rm = T ) )/
                         table( df %>% filter( duree > 0, doublon == 1 ) %>% select( pivot ) ),
                       1)
  }
  
  if('DMR_centrale'%in%indicateurs){
    tb[['DMR_centrale']]<-round( with( df %>% filter( duree > 0, duree < dms_n +30 ), tapply( duree_rum, pivot, sum, na.rm = T ) )/
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
      table(df%>%filter(duree>3,substr(ghm,6,6)%in%2:4, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }
  if('NS_2'%in%indicateurs){
    tb[['NS_2']]<-round(
      table(df%>%filter(duree>3,substr(ghm,6,6)==2, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }


  if('NS_3'%in%indicateurs){
    tb[['NS_3']]<-round(
      table(df%>%filter(duree>3,substr(ghm,6,6)==3, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }

  if('NS_4'%in%indicateurs){
    tb[['NS_4']]<-round(
      table(df%>%filter(duree>3,substr(ghm,6,6)==4, doublon==1)%>%select(pivot))*100/
        table(df%>%filter(duree>3)%>%select(pivot)),
      1)
  }



  #Nombre de journnees de reanimation et USI en reanimation
  #	info dans dataframe recettes

  ###########################################################################
  if('nbsuprea'%in%indicateurs){

    tb[['nbsuprea']]<-round(with(df,tapply(supp_rea_repa,pivot,sum)))

  }


  if('nssir'%in%indicateurs){

    tb[['nssir']]<- round(with(df,tapply(supp_si_repa,pivot,sum)))

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

    tb[['nsstf']]<- round( with(df, tapply( supp_si_repa+supp_stf_repa, pivot, sum ) ) ) 
  }


  if('stf_hr'%in%indicateurs){

    tb[['stf_hr']]<- round( tapply( df$supp_stf_repa, df$pivot, sum ) )

  }

  #Nombre de journees surveillance continue
  ############################################################################
  if('nbsupsrc'%in%indicateurs){

    tb[['nbsupsrc']]<-round( tapply( df$supp_src_repa, df$pivot, sum ) )

  }

  #Pourcentage des journees avec Sup USC
  ############################################################################
  if('p_nbsupsrc'%in%indicateurs){

    tb[['p_nbsupsrc']]<-round( tb[['nbsupsrc']]*100/
                                with( df %>% dplyr::filter( substr( type_rum_1, 1, 2 ) %in% c( "03", "14" ) ),
                                     tapply( duree_rum, pivot, sum ) ),
                              1)
  }

  #Nombre de dialyse
  ############################################################################
  #TODO
  if('dialhosp'%in%indicateurs){

    tb[['dialhosp']]<-with(df%>%dplyr::mutate( adial = stringr::str_count(actes, paste( actes_dialyse_sup$acte, collapse = "|"))),
                           tapply( adial, pivot, sum ) )
  }

  #Poids moyen du cas traite :
  ##Moyenne des tarifs du GHS
  ############################################################################

  
  if('rec_totale'%in%indicateurs){
    tb[['rec_totale']]<-
      round(
        with(df %>% dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_totale, pivot, sum, na.rm=T ) )
        
      )
  }
  if('rec_totale_hc'%in%indicateurs){
    tb[['rec_totale_hc']]<-
      round(
        with( df%>% dplyr::filter( type_hospum_1=='C' )%>%
                dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_totale, pivot, sum, na.rm=T ) )
        
      )
  }
  
  if('rec_base'%in%indicateurs){
    tb[['rec_base']]<-
      round(
        with(df %>% dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_base, pivot, sum, na.rm=T ) )

      )
  }

  if('rec_base_hc'%in%indicateurs){
    tb[['rec_base_hc']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=='C' ) %>% 
                dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_base, pivot, sum, na.rm=T ) )

      )
  }
 
  if('rec_supp'%in%indicateurs){
    tb[['rec_supp']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=='C' ) %>%
                dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_totale-rec_base, pivot, sum, na.rm=T ) )
        
      )
  } 
  if('rec_supp_repa'%in%indicateurs){
    tb[['rec_supp_repa']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=='C' ) %>%
                dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
             tapply( rec_sup_repa, pivot, sum, na.rm=T ) )
        
      )
  } 
  
  if('rec_totale_repa'%in%indicateurs){
    tb[['rec_totale_repa']]<-
      round(
        with( df, tapply( valopmctmonotime1, pivot, sum, na.rm=T ) )
        
      )
  }

  if('rec_base_repa'%in%indicateurs){
    tb[['rec_base_repa']]<-
      round(
        with( df, tapply( valopmctmonotime1 - rec_sup_repa, pivot, sum, na.rm=T ) )

      )
  }
  
  if('rec_totale_repa_hc'%in%indicateurs){
    tb[['rec_totale_repa_hc']]<-
      round(
        with( df%>% dplyr::filter( type_hospum_1=="C" ),
              tapply( valopmctmonotime1, pivot, sum, na.rm=T ) )
        
      )
  }
  if('rec_base_repa_hc'%in%indicateurs){
    tb[['rec_base_repa_hc']]<-
      round(
        with( df%>%dplyr::filter( type_hospum_1=="C" ),
             tapply( valopmctmonotime1 - rec_sup_repa, pivot, sum, na.rm=T ) )

      )
  }

  if('rec_base_repa_mono_uma'%in%indicateurs){
    tb[['rec_base_repa_mono_uma']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=="C", rum==1 ),
             tapply( valopmctmonotime1 - rec_sup_repa, pivot, sum, na.rm=T ) )

      )
  }

  if('rec_base_repa_multi_uma'%in%indicateurs){
    tb[['rec_base_repa_multi_uma']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=="C", rum!=1 ),
             tapply( valopmctmonotime1 - rec_sup_repa, pivot, sum, na.rm=T ) )

      )
  }

  if('rec_base_hp'%in%indicateurs){
    tb[['rec_base_hp']]<-
      round(
        with( df %>% dplyr::filter( type_hospum_1=="P" ),
             tapply( rec_base, pivot, sum, na.rm=T ) )

      )
  }
  
  if('rmct_hc'%in%indicateurs){
    
    tb[['rec_totale_hc']]<- round(
      
      with( df %>% dplyr::filter( type_hospum_1=='C' )%>%
              dplyr::distinct( finess, annee, cle_rsa, .keep_all = T ),
           tapply( rec_totale, pivot, sum, na.rm=T ) )
      
    )
    
    tb[['rmct_hc']] <- round( tb[['rec_totale_hc']] / tb[['HCtot']] ) 
  }
  
  if('rmct_repa_hc'%in%indicateurs){
    
    tb[['rec_totale_repa_hc']]<- round(
        
        with( df %>% dplyr::filter( type_hospum_1=="C" ),
              tapply( valopmctmonotime1, pivot, sum, na.rm=T ) )
        
      )
    
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
                                          table( df %>% dplyr::filter( type_hospum_1=="C", rum==1 ) %>% 
                                                   dplyr::select( pivot ) ) 
                                       )
                                          

  }

  if('pmct_repa_multi_uma'%in%indicateurs){

    tb[['pmct_repa_multi_uma']]<- round( tb[['rec_base_repa_mono_uma']] / 
                                           table( df %>% dplyr::filter( type_hospum_1=="C", rum!=1,  doublon==1 ) %>% 
                                                    dplyr::select( pivot ) ) 
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
    
    nb_journees <- with( df %>% filter( type_hospum_1 == 'C' ), tapply( duree_rum, pivot, sum, na.rm=T ) )
    
    
    rescettes_totales <- with( df %>% filter( type_hospum_1 == 'C' ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) )
    
    
    tb[['0_nuit_nb_sej']] <- table( df %>% filter( type_hospum_1 == 'C', duree == 0 ) %>% select( pivot ) )

    
    tb[['0_nuit_recettes']] <- round( with( df %>% filter( type_hospum_1 == 'C', duree == 0 ), tapply( valopmctmonotime1, pivot,sum, na.rm=T ) ) )

    
    tb[['0_nuit_pourc_recettes']] <-  round( tb[['0_nuit_recettes']] * 100 / rescettes_totales, 1 )
    
    
    tb[['0_nuit_recettes_jour']]  <- round( tb[['0_nuit_recettes']] /  tb[['0_nuit_nb_sej']] )
    
    
    tb[['1_nuit_nb_sej']] <- table( df %>% filter( duree == 1 ) %>% select( pivot ) ) 
     
    tb[['1_nuit_pourc_journees']] <-  round( tb[['1_nuit_nb_sej']] * 100 /  nb_journees, 1 )
    
    tb[['1_nuit_recettes']] <- round( with( df %>% filter( duree == 1 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['1_nuit_pourc_recettes']]<- round( tb[['1_nuit_recettes']] * 100 / rescettes_totales, 1 )
    
    tb[['1_nuit_recettes_jour']] <- round( tb[['1_nuit_recettes']] /  tb[['1_nuit_nb_sej']] )
    
    
    
    tb[['2_nuits_nb_journees']] <- with( df %>% filter( duree > 1 , duree < dms_n + 30 ), tapply( duree_rum, pivot, sum, na.rm=T ) ) 
    
    tb[['2_nuits_pourc_journees']] <- round( tb[['2_nuits_nb_journees']]*100 /  nb_journees, 1 )
    
    tb[['2_nuits_recettes']] <- round ( with( df %>% filter(duree > 1 , duree < dms_n + 30 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['2_nuits_pourc_recettes']] <- round( tb[['2_nuits_recettes']] * 100 / rescettes_totales, 1)
    
    tb[['2_nuits_recettes_jour']] <- round(tb[['2_nuits_recettes']] /  tb[['2_nuits_nb_journees']] )
    
    
    tb[['sej_longs_nb_journees']] <- with( df %>% filter( duree >= dms_n +30 ), tapply( duree_rum, pivot, sum, na.rm=T ) ) 
    
    tb[['sej_longs_pourc_journees']] <-  round( tb[['sej_longs_nb_journees']]*100 /  nb_journees, 1 )
    
    tb[['sej_longs_recettes']] <- round( with( df %>% filter( duree >= dms_n +30 ), tapply( valopmctmonotime1, pivot, sum, na.rm=T ) ) )
    
    tb[['sej_longs_pourc_recettes']] <- round( tb[['sej_longs_recettes']] * 100 / rescettes_totales, 1)
    
    tb[['sej_longs_recettes_jour']] <- round( tb[['sej_longs_recettes']] /  tb[['sej_longs_nb_journees']] )
    
  }
  

  #Somme des supplements
  ############################################################################

  if('SuppTotal'%in%indicateurs){

    tb[['SuppTotal']]<-round(
      
      with( df, tapply( rec_sup_repa, pivot, sum, na.rm=T ) )

    )
  }

  if('SuppSIRea'%in%indicateurs){

    tb[['SuppSIRea']]<-round(
      
      with( df, tapply( rec_si_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppSIhorsRea'%in%indicateurs){

    tb[['SuppSIhorsRea']]<-round(
      
      with( df, tapply( rec_stf_hr_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppRea'%in%indicateurs){

    tb[['SuppRea']]<-round(
      
      with( df, tapply( rec_rea_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppSRC'%in%indicateurs){

    tb[['SuppSRC']]<-round(
      
      with( df, tapply( rec_src_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppReaPED'%in%indicateurs){

    tb[['SuppReaPED']]<-round(
      
      with( df, tapply( rec_rep_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppNN1'%in%indicateurs){

    tb[['SuppNN1']]<-round(
      
      with( df, tapply( rec_nn1_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppNN2'%in%indicateurs){

    tb[['SuppNN2']]<-round(
      
      with( df, tapply( rec_nn2_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppNN3'%in%indicateurs){

    tb[['SuppNN3']]<-round(
      
      with( df, tapply( rec_nn3_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppCaisHyp'%in%indicateurs){

    tb[['SuppCaisHyp']]<-round(
      
      with( df, tapply( rec_caishyp_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppPO'%in%indicateurs){

    tb[['SuppPO']]<-round(
      
      with( df, tapply( rec_po_tot_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppANT'%in%indicateurs){

    tb[['SuppANT']]<-round(
      
      with( df, tapply( rec_ant_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppRDT'%in%indicateurs){

    tb[['SuppRDT']]<-round(
      
      with( df, tapply( rec_rdt_tot_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppRAP'%in%indicateurs){

    tb[['SuppRAP']]<-round(
      
      with( df, tapply( rec_rap_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppDial'%in%indicateurs){

    tb[['SuppDial']]<-round(
      
      with( df, tapply( rec_dialhosp_repa, pivot, sum, na.rm=T ))

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      
      with( df, tapply( rec_aph_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      
      with( df, tapply( rec_aph_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppApherese'%in%indicateurs){

    tb[['SuppApherese']]<-round(
      
      with( df, tapply( rec_aph_repa, pivot, sum, na.rm=T ) )

    )
  }
  if('SuppSDC'%in%indicateurs){

    tb[['SuppSDC']]<-round(
      
      with( df, tapply( rec_sdc_repa, pivot, sum, na.rm=T ) ) 

    )
  }
  if('SuppRehosp'%in%indicateurs){

    tb[['SuppRehosp']]<-round(
      
      with( df, tapply( rec_rehosp_ghm_repa, pivot, sum, na.rm=T ) )

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
  #      table(df$pivot[df$type_hospum_1=="C"]),digit=1
  #  )
  #}
  #########################################################################
  ###Activite de cancerologie
  #########################################################################
  
  if( 'ActiviteCancero' %in% indicateurs | "SP" %in% indicateurs ){

    df_cancer<-inner_join( df, cancer_rsa %>% select( -dp, -noanon, -nas ) )


    tb[['NbPat']]<-with( df_cancer, tapply( noanon, pivot, nb_unique ) )
    
    
    tb[['NbNxPat']]<-with( df_cancer %>% filter( nx_pat == 'O' ), tapply( noanon, pivot, nb_unique ) )
    
    
    tb[['NbSejHC']]<- table( df_cancer %>% dplyr::filter( type_hospum_1=="C", doublon == 1 ) %>% dplyr::select( pivot ) )
    
    
    tb[['NbSejHP']]<- table( df_cancer %>% dplyr::filter( type_hospum_1=="P", doublon == 1 ) %>% dplyr::select( pivot ) )
    
    
    #tb[['pSejHC']]<-round(table(DataCancero$pivot[!duplicated(DataCancero$NORSS)&DataCancero$TypeDossier%in%c('A','N')])*100/
    #                        table(df$pivot[!df$type_hospum_1=="P"]))
    #
    #tb[['pSejHP']]<-round(table(DataCancero$pivot[which(DataCancero$TypeDossier%in%c('I','S'))])*100/
    #                        table(df$pivot[df$type_hospum_1=="P"]))
    
    tb[['NbSejChir']]<-table( df_cancer %>% dplyr::filter( substr( ghm, 3, 3 ) == "C", doublon == 1 )%>%
                                dplyr::select( pivot ) )
 
    
    tb[['NbPatientsParAppareilInca']]<-with( df_cancer, tapply( noanon, list( appareil, pivot ), nb_unique ) )

    
    if(length(dimnames( tb[['NbPatientsParAppareilInca']])[[1]])>0){

      dimnames( tb[['NbPatientsParAppareilInca']])[[1]]<-paste(' -', tolower( dimnames( tb[['NbPatientsParAppareilInca']])[[1]]))
    }

    tb[['ActiviteParAppareilInca']]<-with( df_cancer %>% filter( doublon == 1 ),
                                           tapply( cle_rsa, list( appareil, pivot ), nb_unique ) )

    if(length(dimnames( tb[['ActiviteParAppareilInca']])[[1]])>0){

      dimnames( tb[['ActiviteParAppareilInca']])[[1]]<-paste(' -', tolower( dimnames( tb[['ActiviteParAppareilInca']])[[1]]))
    }
  }
  #########################################################################
  #####Soins Palliatifs
  #########################################################################
  if('SP'%in%indicateurs){

    df_sp <- df%>% filter( dp=='Z515'| dr=='Z515'| grepl('Z515',das) )
    
    if(nrow(df_sp)>10){
    
        tb[['SejSp']] <- table( df_sp %>% filter(doublon==1) %>% select(pivot) )
        
    
        tb[['JourneesSp']] <- with( df_sp, tapply( duree_rum, pivot, sum, na.rm=T ) )
        
    
        tb[['NbPatSP']] <- with( df_sp, tapply(noanon, pivot, nb_unique) )
        
    
        tb[['SejSpGhmNonSP']]<-table( df_sp %>% dplyr::filter( substr(ghm,1,5)!='23Z02', doublon == 1 )%>% 
                                        dplyr::select(pivot) )
        
    
        tb[['SejSpLitsStandards']]<-table( df_sp %>% dplyr::filter( ghs%in%c('7991','7992') ) %>% dplyr::select(pivot) )
        
        
        tb[['SejSpLitsDedies']]<-table( df_sp %>% dplyr::filter(ghs%in%c('7993'))%>% dplyr::select(pivot))
        
        
        tb[['NbPatSpLitsDedies']]<-with( df_sp %>% dplyr::filter( ghs%in%c('7993') ), tapply( noanon, pivot, nb_unique ) )
        
        
        tb[['NbLitsDediesUtilises']]<-round( with( df_sp,tapply( duree_rum2, pivot, sum, na.rm=T ) ) /
                                              with( df_sp, tapply( date_max_pivot - date_min_pivot, pivot, min ) ),
                                            1)
    
        tb[['SejSpUsp']]<-table(df_sp %>% filter( ghs %in% c('7994') ) %>% select( pivot ) )
        
        
        tb[['JourneesSpLitsStandards']]<- with( df_sp %>% dplyr::filter( ghs %in% c('7991','7992') ),
                                               tapply( duree_rum, pivot, sum, na.rm=T ) )
        
        
        tb[['JourneesSpLitsDedies']]<- with( df_sp %>% dplyr::filter( ghs %in% c('7993') ),
                                            tapply( duree_rum, pivot, sum, na.rm=T ) )
        
        
    
        tb[['JourneesSpUsp']]<- with( df_sp %>% dplyr::filter( ghs %in% c('7994') ),
                                     tapply( duree_rum, pivot, sum, na.rm=T ) )
        
        
    
        tb[['NbDCD']]<- table( df %>% dplyr::filter( modesortie_rum == 9 ) %>%
                                dplyr::distinct( nas, .keep_all = T ) %>% select( pivot ) )
        
        
     
        tb[['NbDCD_SP']]<- table( df_sp %>% dplyr::filter( modesortie_rum == 9 ) %>% dplyr::select( pivot ) )
        
    
        tb[['NbDCDNonSP']]<- tb[['NbDCD']] -  tb[['NbDCD_SP']]
        
    
        tb[['NbDCDcancer']]<- table( df_cancer %>% dplyr::filter(modesortie_rum == 9) %>% dplyr::select(pivot) )
        
    
        tb[['NbDCDspGhmNonSP']]<-table( df%>% dplyr::filter(modesortie_rum == 9,substr(ghm,1,5)!='23Z02') %>% dplyr::select(pivot))
    
        
        tb[['NbDCDspGhmSP']]<-table( df %>% dplyr::filter(modesortie_rum == 9,substr(ghm,1,5)=='23Z02') %>% dplyr::distinct(nas,.keep_all = T)%>%select(pivot))
    
        
        tb[['NbDCDspLitsStandards']]<-table( df%>% dplyr::filter( modesortie_rum == 9, ghs %in% c('7991','7992')) %>% dplyr::select(pivot) )
    
        
        tb[['NbDCDspLitsDedies']]<-table( df %>% dplyr::filter(modesortie_rum == 9 , ghs%in%c('7993') ) %>% dplyr::select(pivot) )
    
        
        tb[['NbDCDspUsp']]<-table( df %>% dplyr::filter(modesortie_rum == 9, ghs%in%c('7994') ) %>% dplyr::select(pivot) )
    
    }

  }

  ######################################################################### 
  #####Case mix
  #########################################################################
  if('TitreCaseMixCMD'%in%indicateurs){
    
    if(!exists("cmd")){
      cmd <<- get_cmd()
    }

    tmp <- df%>%dplyr::mutate(cmd := substr(ghm,1,2)) %>% left_join(.,cmd)%>% unite(cmd, cmd, libelle_cmd)
    
    nb <- table(tmp$cmd)
    
    nb <- nb[order(nb,decreasing = T)]
    
    nbc <- cumsum(nb[order(nb,decreasing = T)])*100/sum(nb)

    tmp<- tmp %>% dplyr::mutate(cmd = ifelse(cmd %in% names(nbc[nbc<91]),cmd,'Autres'))%>%
      dplyr::mutate(cmd = factor(cmd,levels= c(names(nbc[nbc<91]),'Autres')))
    
    res<-with(tmp,tapply(doublon, list(cmd,pivot), sum))
    
    res2<-round(with(tmp,tapply(valopmctmonotime1, list(cmd,pivot), sum)))
    
    tb[['CaseMixCMD']] = NULL
    for(i in dimnames(res)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb[['CaseMixCMD']]<-rbind(tb[['CaseMixCMD']],tmp)
    }

    
    tmp <- df %>% dplyr::mutate(type := substr(ghm,3,3))%>% dplyr::mutate(type = ifelse(type == 'C',"Chirurgie",
                                                                        ifelse(type == "M", "Médecine",
                                                                        ifelse(type == "K", "Médico_technique",
                                                                        "Autres"))))
    
    res <- with( tmp, tapply( doublon, list( type, pivot ), sum ) )
    
    res2 <- round( with( tmp, tapply( valopmctmonotime1, list( type, pivot ), sum ) ) )
     
    tb[['CaseMixType']]<-NULL
    
    for(i in dimnames(res)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb[['CaseMixType']]<-rbind(tb[['CaseMixType']],tmp)
    }
    
    if(!exists("racines")){
      
      racines = get_racines()
    }
    
    tmp<-df%>%dplyr::mutate(racine := substr(ghm,1,5))%>%
      dplyr::left_join( ., racines )%>%
      tidyr::unite( racine, racine, libelle,sep = " ")%>%
      dplyr::mutate( racine = paste0( substr( racine, 1, 50 ), ifelse( nchar( racine ) > 51, "...", "" ) ) )
    
    nb<-table(tmp$racine)
    
    nb<-nb[order(nb,decreasing = T)]
    
    nbc<-cumsum(nb[order(nb,decreasing = T)])*100/sum(nb)
    
    tmp<- tmp %>% dplyr::mutate( racine = ifelse( racine %in% names( nbc[nbc<91] ), racine, 'Autres' ) )%>%
      dplyr::mutate( racine = factor( racine, levels= c( names( nbc[nbc<91] ), 'Autres' ) ) )
    
    res<-with( tmp, tapply( doublon, list(racine,pivot), sum ) )
    
    res2<-round( with( tmp, tapply( valopmctmonotime1, list( racine, pivot ), sum ) ) ) 
    

    tb[['CaseMixGHM']] = NULL
    for(i in dimnames(res)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb[['CaseMixGHM']]<-rbind(tb[['CaseMixGHM']],tmp)
    }

    
    

    
    
  }
  ########################################################################################
  # Infectieux
  ########################################################################################
   if('Infectieux'%in%indicateurs){
     
     
     tmp<- df%>% dplyr::filter(dpdurum%in%infections_otoctones$code|drdurum%in%infections_otoctones$code|
                        grepl( paste( infections_otoctones$code, collapse = '|'), dasdurum ) )
     
     
     tb[['NbSejoursInfectieux']]<-table( tmp%>% filter(doublon ==1) %>% dplyr::select(pivot) )
     
     
     tmp<-df%>%filter(dpdurum %in% resistance_atb$code | drdurum %in% resistance_atb$code|
                        grepl( paste( resistance_atb$code, collapse = '|'), dasdurum ) )
     
      tb[['NbPateintsBMR']] <- with( tmp, tapply( noanon, pivot, nb_unique ) )
      
      tb[['NbSejoursBMR']] <- table( tmp%>% dplyr::filter(doublon ==1) %>% dplyr::select( pivot ) )
      
    }

  ########################################################################################
  # Douleur
  ########################################################################################

  if('Douleur'%in%indicateurs){

    tmp<-df%>%filter( dpdurum%in%douleur$code | drdurum%in%douleur$code |
                       grepl( paste( douleur$code, collapse = '|' ), dasdurum ) )

    tb[['PatientsDouleur']]<-with( tmp,tapply( noanon, pivot, nb_unique) )

    tb[['SejoursDouleur']]<-table( tmp%>% filter(doublon ==1) %>% dplyr::select( pivot ) )

  }

  ########################################################################################
  #Autres pathologies chroniques
  ########################################################################################
  if('AutresPathologiesChroniques'%in%indicateurs){

    ###VIH
    tmp<-df%>%filter(dpdurum %in% vih$code | drdurum %in% vih$code |
                       grepl( paste( vih$code,collapse = '|'), dasdurum ) )
    

    tb[['PatientsVIH']]<- with( tmp, tapply( noanon, pivot, nb_unique) )
    
    
    tb[['SejoursVIH_HC']]<-table( tmp %>% dplyr::filter(type_hospum_1=='C',doublon ==1) %>% dplyr::select( pivot ) )
    

    tb[['SejoursVIH_HP']]<- table( tmp%>% dplyr::filter(type_hospum_1=='P') %>% dplyr::select( pivot ) )
    

    #Diabète
    tmp <- df%>%dplyr::filter( dpdurum %in% diabete$code | drdurum %in% diabete$code |
                       grepl( paste( diabete$code, collapse = '|'), dasdurum ) )
    

    tb[['PatientsDiabete']]<-with(tmp, tapply( noanon, pivot, nb_unique) )
    

    tb[['SejoursDiabete_HC']]<-table( tmp %>% dplyr::filter(type_hospum_1=='C', doublon==1 ) %>% dplyr::select(pivot) )
    

    tb[['SejoursDiabete_HP']]<-table( tmp %>% dplyr::filter(type_hospum_1=='P') %>% dplyr::select(pivot) )

    #Insuffisance rénale chronique
    tmp<-df%>%filter( dpdurum %in% insuffisance_renale_chronique$code| drdurum %in% insuffisance_renale_chronique$code |
                       grepl(paste(insuffisance_renale_chronique$code,collapse = '|'),dasdurum))
    

    tb[['PatientsInsRenaleChronique']]<-with( tmp, tapply( noanon, pivot, nb_unique ) )
    

    tb[['SejoursInsRenaleChronique_HC']]<-table( tmp %>% dplyr::filter( type_hospum_1=='C', doublon==1 ) %>% dplyr::select(pivot) )

    
    tb[['SejoursInsRenaleChronique_HP']]<-table( tmp %>% dplyr::filter( type_hospum_1=='P' ) %>% dplyr::select(pivot) )
    

    #Insuffisance respiratoire chronique
    tmp<-df%>%filter( dpdurum %in% insuffisance_respiratoire_chronique$code | drdurum %in% insuffisance_respiratoire_chronique$code|
                       grepl( paste( insuffisance_respiratoire_chronique$code, collapse = '|' ), dasdurum ) )
    

    tb[['PatientsInsRespiratoireChronique']]<-with( tmp, tapply(noanon,pivot,nb_unique) )
    

    tb[['SejoursInsRespiratoireChronique_HC']]<-table( tmp %>% dplyr::filter(type_hospum_1=='C', doublon ==1) %>% dplyr::select(pivot) )

    tb[['SejoursInsRespiratoireChronique_HP']]<-table( tmp %>% dplyr::filter(type_hospum_1=='P') %>% dplyr::select(pivot) )

  }
  #########################################################################################
  #Reanimation
  #########################################################################################
  ###Pourcentage de patients ventilés

  if('IndicateursRea'%in%indicateurs){

    tmp_rea<-df %>% dplyr::filter(type_rum_1=="01A")

    tb[['HCtotAutRea']]<-table( tmp_rea %>% dplyr::filter( doublon == 1 ) %>% dplyr::select( pivot ) )

    ##Re-hospitalisation precoses
    #df3<-merge(Data[o,c('NIP','SERVICE','DateEntreeResume','DateSortieResume','A')],
    #           Sejours2[,c('NIP','SERVICE','DateEntreeResume','DateSortieResume','ModeSortieResume')],
    #           by=c('NIP','SERVICE'),suffixes = c('.1','.2'))
    #df3<-df3[df3$DateSortieResume.1<df3$DateEntreeResume.2,]
    #df3$delai<-difftime(df3$DateEntreeResume.2,df3$DateSortieResume.1,units="hours")

    #tb[['NbRehsopitPrecoceRea']]<-table(df3$pivot[df3$delai<48])
    #tb[['TauxRehsopitPrecoceRea']]<-round(table(df3$pivot[df3$delai<48])*100/as.numeric(tb[['HCtot2']]),2)

    tb[['IGSmoyen']]<-round( with( tmp_rea, tapply( as.numeric(igs), pivot, mean, na.rm=T) ), 1 )

    tb[['IGS0']]<-table(tmp_rea %>% dplyr::filter( as.numeric(igs)==0 ) %>% dplyr::select( pivot ) )

    l_actes = paste(c(as.character(actes_ventilation_intubation$acte),
                      as.character(actes_ventilation_tracheotomie$acte),
                      as.character(actes_vni$acte)),
                    collapse = '|')
    

    tb[['NbSejoursPatientVentiles']]<- table(df %>% dplyr::filter( grepl( l_actes, actes ) )%>% 
                                                 dplyr::filter( doublon == 1 )%>% 
                                                 dplyr::select( pivot ) )
    

    tb[['pPatientVentiles']]<- round( table(df %>% dplyr::filter( grepl( l_actes, actes ) ) %>% 
                                              dplyr::filter( doublon == 1 ) %>%
                                              dplyr::select( pivot) ) *100 / tb[['HCtotAutRea']],
                                      1)

    ###Nombre de patients ventiles plus de 48h
    tb[['NbSejoursPatientVentiles48h']]<- table( tmp_rea %>% dplyr::mutate( nb_actes = stringr::str_count(actes, l_actes)) %>% 
                                                   dplyr::filter( nb_actes > 1 ) %>% 
                                                   dplyr::filter( doublon == 1 ) %>% 
                                                   dplyr::select( pivot ) )


    tb[['NbJourneesVentilesInt']]<-with( tmp_rea %>% dplyr::mutate( nb_actes = stringr::str_count( actes, l_actes ) ),
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

    tb[['pDcdRea']]<- round(table( df %>% filter(modesortie_rum == 9, type_rum_1=="01A" ) %>% select(pivot) )*100/
                              tb[['HCtotAutRea']],
                            digit=1)

    tb[['pDcdHopRea']]<-round(table( inner_join(df, tmp_rea %>% select(finess,cle_rsa,annee)%>%distinct(finess,cle_rsa,annee))
                                     %>%filter(modesortie_rum == 9, type_rum_1=="01A" )
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
                              table( df %>% dplyr::filter( type_hospum_1=="C", doublon==1 ) %>% dplyr::select(pivot) )
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
                                              tapply( duree_rum - ( dms_n*coeftime ), pivot, sum, na.rm = T ) ) )
    
    
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



#' Title
#'
#' @param df 
#'
#' @return
#' @export racine_diff_recettes
#'
#' @examples
racine_diff_recettes <- function( df ) {
  
  annee_ = max(as.numeric(df$annee))
  
  
  giac1 <- df %>%
    dplyr::mutate( racine = substr( ghm, 1, 5) ) %>%
    dplyr::group_by( annee, racine ) %>%
    dplyr::summarise( sej_racine = n_distinct(cle_rsa,finess,cle_rsa), 
                      nb_journees = sum(duree_rum),
                      dmr = round( sum(duree_rum) / sej_racine,1),
                      coef_racine = sum( coefpmctmonotime1 ),
                      rec_base_racine = sum( tarif_base * coefpmctmonotime1 ),
                      rec_totale_racine = round( sum( rec_totale * coefpmctmonotime1 ) ) ,
                      rec_moy_racine = round( rec_base_racine/sej_racine ),
                      rec_supp_racine = round( sum ( rec_sup_repa  ) ),
                      rec_ex_racine =  round( sum( rec_exbh * coeftime ) ),
                      diff_tarifs_racine = round(sum(diff_t * coefpmctmonotime1) ) ) %>% 
    dplyr::ungroup(.)
  
  
  
  giac <- dplyr::full_join( giac1 %>% filter( as.numeric( annee ) == annee_-1 )%>% dplyr::select( -annee ),
                            giac1 %>% filter( as.numeric( annee ) == annee_ ) %>%  dplyr::select( -annee ),
                          suffix = c( "_n_1", "_n" ),
                          by = c( "racine" )  )%>%
    replace(., is.na(.), 0)%>%
    
    mutate( diff_sej = sej_racine_n - sej_racine_n_1,
            diff_journnees = nb_journees_n - nb_journees_n_1,
            diff_dmr = dmr_n - dmr_n_1,
            diff_recettes = round ( rec_totale_racine_n - rec_totale_racine_n_1 ),
            diff_ns = ( sej_racine_n * rec_moy_racine_n ) - ( sej_racine_n * rec_moy_racine_n_1 ),
            diff_supp = rec_supp_racine_n - rec_supp_racine_n_1,
            diff_ex = rec_ex_racine_n - rec_ex_racine_n_1,
            diff_tarifs = diff_tarifs_racine_n)%>%
    
    mutate(diff_ns = ifelse ( sej_racine_n < 5 | sej_racine_n_1 < 5 , 0, diff_ns), 
           diff_activite = diff_recettes - diff_ns - diff_supp - diff_ex - diff_tarifs
  )


  return(giac)
  
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
  
  annee_ = max(as.numeric(df$annee))
  
  df<-df%>%mutate(pivot:=!!sym(pivot))
  
  giac1 <- df %>%
    dplyr::mutate( racine = substr( ghm, 1, 5) ) %>%
    dplyr::group_by( pivot, annee, racine ) %>%
    dplyr::summarise( sej_racine = n_distinct( cle_rsa, finess, cle_rsa ), 
                      nb_journees = sum( duree_rum ),
                      coef_racine = sum( coefpmctmonotime1 ) ,
                      rec_base_racine = sum( tarif_base * coefpmctmonotime1 ),
                      rec_totale_racine = round( sum( valopmctmonotime1 ) ) ,
                      rec_moy_racine = round( rec_base_racine/sej_racine ),
                      rec_supp_racine = round( sum ( rec_sup_repa  ) ),
                      rec_ex_racine =  round( sum( rec_exbh * coeftime ) ) ) %>% 
    dplyr::ungroup(.)
  
  
  
  giac <- dplyr::full_join( giac1 %>% filter( as.numeric( annee ) == annee_-1 )%>% dplyr::select( -annee ),
                            giac1 %>% filter( as.numeric( annee ) == annee_ ) %>%  dplyr::select( -annee ),
                            suffix = c( "_n_1", "_n" ),
                            by = c( "pivot", "racine" )  )%>%
    replace(., is.na(.), 0)
  
  res <- giac%>% group_by(pivot)%>%
    
    dplyr::summarise( sej_n = sum( sej_racine_n ),
                      diff_sej = sum( sej_racine_n - sej_racine_n_1 ),
                      p_diff_sej = round( diff_sej / sej_n),
                      nb_journees = sum( nb_journees_n ),
                      diff_nb_journees = nb_journees - sum(nb_journees_n_1),
                      dmr = round( nb_journees / sej_n, 1 ),
                      diff_dmr = dmr - round( sum(nb_journees_n_1) /  sum (sej_racine_n_1), 1 ),
                      recettes_n = sum (rec_totale_racine_n),
                      diff_recettes = round ( sum( rec_totale_racine_n - rec_totale_racine_n_1 ) ),
                      p_diff_recettes = round( diff_recettes / recettes_n ),
                      diff_supp = round( sum( rec_supp_racine_n - rec_supp_racine_n_1 ) ),
                      diff_ex = round( sum (rec_ex_racine_n - rec_ex_racine_n_1) ) )
  
  pc<-giac%>%group_by(pivot)%>%mutate( s_nrac = sum(sej_racine_n, na.rm=T) )%>%ungroup()%>%
    filter( sej_racine_n < 5 | sej_racine_n_1 < 5 )%>%
    group_by(pivot)%>%
    mutate(p_calc = round( (1-sum(sej_racine_n,na.rm=T)/s_nrac )*100, 1 ) ) %>% dplyr::distinct(pivot, p_calc)  %>% ungroup(.)
  
  res2<-giac%>%dplyr::filter(sej_racine_n >= 5 & sej_racine_n_1 >= 5 ) %>%
    dplyr::mutate( diff = ( sej_racine_n * rec_moy_racine_n ) - ( sej_racine_n * rec_moy_racine_n_1 ) )%>%
    dplyr::group_by( pivot ) %>% dplyr::summarise( diff_ns = round( sum( diff, na.rm=T )  ) ) %>% dplyr::ungroup(.) %>%
    dplyr::left_join(.,pc)
  
  res <- dplyr::left_join(res,res2)%>% replace(., is.na(.), 0)%>%
    mutate(diff_activite = diff_recettes - diff_ns - diff_supp - diff_ex )
  

  
  
  return(res)
  
}

#' Title
#'
#' @param df 
#' @param pivot 
#'
#' @return 
#' @export diff_sej_recettes
#'
#' @examples
diff_sej_recettes<-function( df, pivot ){
  
  df<-df%>%mutate(pivot:=!!sym(pivot))
  
  #Diférentiel séjours
  tmp <- round( with( df, tapply(  doublon , list(pivot,annee), sum , na.rm =T ) ) )
  tmp[which(is.na(tmp))]<- 0
  tmp <- get_diff( tmp )
  dimnames(tmp)[[2]]<-paste0("nb_sej_",dimnames(tmp)[[2]])
  res <- tmp [ , -c(1,4) ]
  
  #Différentiels recettes
  tmp <-  round( with( df, tapply( valopmctmonotime1, list( pivot, annee ), sum, na.rm=T ) ) ) 
  tmp[which(is.na(tmp))]<- 0
  tmp <- get_diff( tmp ) 
  dimnames(tmp)[[2]]<-paste0("recettes_totales_",dimnames(tmp)[[2]])
  res<-   cbind( res , tmp[, -c(1,4) ]  )
  
  return(res)
  
  
}

#' Calcul de la part liée à la variation de la répartition dans les niveaux de sévérité
#' dans l'évolution des recettes T2A entre 2 années.
#'
#' @param df tibble, contenant l'ensemble des données nécessaires au calcul
#' @param pivot string, le niveau de structure pour lequel on souhaite faire le calcul
#'
#' @return
#' @export diff_sej_recettes_v2
#'
#' @examples
diff_sej_recettes_v2 <- function( df, pivot ) {
  
  annee_ = max(as.numeric(df$annee))
  
  df<-df%>%mutate(pivot:=!!sym(pivot))
  
  giac1 <- df %>%
    dplyr::mutate( racine = substr( ghm, 1, 5) ) %>%
    dplyr::group_by( pivot, annee ) %>%
    dplyr::summarise( sej_pivot = sum( doublon ), 
                      nb_journees_pivot = sum( duree_rum ),
                      coef_pivot = sum( coefpmctmonotime1 ) ,
                      rec_base_pivot = sum( tarif_base * coefpmctmonotime1 ),
                      rec_totale_pivot = round( sum( valopmctmonotime1 ) ) ,
                      rec_moy_pivot = round( rec_base_pivot/sej_pivot ),
                      rec_supp_pivot = round( sum ( rec_sup_repa  ) ),
                      rec_ex_pivot =  round( sum( rec_exbh * coeftime ) ) ) %>% 
    dplyr::ungroup(.)
  
  
  
  giac <- dplyr::full_join( giac1 %>% filter( as.numeric( annee ) == annee_-1 )%>% dplyr::select( -annee ),
                            giac1 %>% filter( as.numeric( annee ) == annee_ ) %>%  dplyr::select( -annee ),
                            suffix = c( "_n_1", "_n" ),
                            by = c( "pivot" )  )%>%
    replace(., is.na(.), 0)
  
  res <- giac%>% group_by(pivot)%>%
    
    dplyr::summarise( sej_n = sum( sej_pivot_n ),
                      diff_sej = sum( sej_pivot_n - sej_pivot_n_1 ),
                      p_diff_sej = round( diff_sej * 100 / sej_n , 1 ),
                      nb_journees_n = sum( nb_journees_pivot_n ),
                      diff_nb_journees = nb_journees_n - sum(nb_journees_pivot_n_1),
                      dmr = round( nb_journees_n / sej_n, 1 ),
                      diff_dmr = dmr - round( sum(nb_journees_pivot_n_1) /  sum (sej_pivot_n_1), 1 ),
                      recettes_n = sum (rec_totale_pivot_n),
                      diff_recettes = round ( sum( rec_totale_pivot_n - rec_totale_pivot_n_1 ) ),
                      p_diff_recettes = round( diff_recettes * 100 / sum( rec_totale_pivot_n_1 ), 1 )
                      )
  
  
  res <- res %>% replace(., is.na(.), 0)
  
  
  
  return(res)
  
}



#' Title
#'
#' @param df 
#'
#' @return
#' @export get_tdb_detail_recettes
#'
#' @examples 
get_tdb_detail_recettes <- function( df ){
    
  tdb <- NULL
  
  df$gh = "GH"
  
  for(pivot in c('gh','hopital','pole','service')){

    
      tdb[[pivot]] <- dplyr::full_join(diff_sej_recettes_v2(df,pivot)%>%
                                             dplyr::mutate(sep =NA),
                                           diff_sej_recettes_v2(df %>% dplyr::filter( duree < 2 ),pivot)%>%select(pivot,diff_recettes), 
                                           by = "pivot", suffix = c("","_0_1_j") ) %>%
        dplyr::full_join(., diff_sej_recettes_v2(df %>% dplyr::filter( duree > dms_n + 30 ),pivot)%>%select(pivot,diff_recettes), 
                         by = "pivot", suffix = c("","_30j") ) %>%
        dplyr::full_join(., diff_sej_recettes_v2(df%>%dplyr::filter( duree < 2 , duree <= dms_n + 30), pivot )%>%select(pivot,diff_recettes), 
                         by = "pivot", suffix = c("","_s_1j") )%>%
        dplyr::mutate(sep2 = NA)%>%
        dplyr::full_join(., detail_diff_recettes(df, pivot)%>% select( pivot, diff_supp, diff_ex, diff_ns, p_calc, diff_activite) )%>%
        mutate(sep3 = NA)%>%
        full_join(.,as_tibble(round( with(df%>%mutate(pivot = !!sym(pivot)),
                                             tapply(valopmctmonotime1- valopmctmonotime1_tarifsante,
                                                    pivot,sum,na.rm=T)) ),rownames = NA)%>%
                           rownames_to_column(., var = "pivot") %>% 
                           rename(part_tarifs = value ))
      
    }
    
    services = unique(tdb$service%>%mutate(pivot = as.character(pivot))%>%select(pivot)%>%flatten_chr())
    poles = unique(tdb$pole%>%mutate(pivot = as.character(pivot))%>%select(pivot)%>%flatten_chr())
    
    tdb_ <- rbind(NA, tdb$hopital, "Total GH" = tdb$gh )


    
    for (p in poles){
      
      serv =services[services%in%structures$service[structures$pole==p]]
      
      tdb_<-rbind(tdb_,NA,NA)
      tdb_[nrow(tdb_),'pivot']<-p

      tdb_ = dplyr::bind_rows(tdb_,
                              tdb$service%>%filter(pivot%in%serv), 
                              tdb$pole%>%filter(pivot == p)%>%mutate(pivot = paste('Total pole ',p)))

    }
    
    return(tdb_)
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
    
    df<-df%>%mutate(type_hospum_1 = factor(type_hospum_1))
    
    tdb[['services']] <- table( df %>%  distinct( finess, cle_rsa, annee, service, .keep_all = T)%>%
                                 select(service, annee, type_hospum_1) )
    
    tdb[['poles']] <- table( df %>%  distinct( finess, cle_rsa, annee, service, .keep_all = T )%>%
                              select( pole, annee, type_hospum_1 ) )
    
    tdb[['hopitaux']]  <- table( df %>%  distinct(finess, cle_rsa, annee, .keep_all = T)%>%
                                  select(hopital, annee, type_hospum_1 ) )
    
    tdb[['gh']] <- table(df %>% distinct( finess, cle_rsa, annee, .keep_all = T )%>%
                           select( type_hospum_1, annee ) )

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
  
  df<-df%>%mutate(type_hospum_1 = factor(type_hospum_1))

  tdb_v[['services']] <- round( with( df, tapply( valopmctmonotime1, list(service,annee,type_hospum_1), sum, na.rm=T ) ) )
  
  tdb_v[['poles']]  <- round( with( df, tapply( valopmctmonotime1, list(pole,annee,type_hospum_1), sum, na.rm=T ) ) )
  
  tdb_v[['hopitaux']] <- round( with( df, tapply( valopmctmonotime1, list(hopital,annee,type_hospum_1), sum, na.rm=T ) ) )
  
  tdb_v[['gh']] <- t(round( with( df, tapply( valopmctmonotime1, list(annee,type_hospum_1), sum, na.rm=T ) ) ))

  tdb_v <- order_by_structure( tdb_v, structures )

  tdb_v_final<-cbind( get_diff(tdb_v$hc), NA, get_diff(tdb_v$hp) )

  return(tdb_v_final)

}


get_actvite_racines<-function(df){
  
  if(!exists("racines")){
    
    racines <<- get_racines()
  }
  
  if(!exists("cmd")){
    
    cmd <<- get_cmd()
  }
  
    tmp <- df%>%mutate(cmd = substr(ghm,1,2)) %>% left_join(.,cmd)%>% unite(cmd, cmd, libelle_cmd,sep= " ")

    res<- get_diff( with(tmp,tapply(doublon, list(cmd,pivot), sum)) )
    res2<-get_diff( round(with(tmp,tapply(valopmctmonotime1, list(cmd,pivot), sum))) )
    if(nrow(res2)>1) res2[order(abs(res2[,'diff']),decreasing =T),]
    
    tb = NULL
    for(i in dimnames(res2)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb <-rbind(tb,tmp)
    }
    
    tb = rbind("Catégorie majeur de diagnostic" = NA,tb)
    
    
    tmp<-df%>%mutate(type := substr(ghm,3,3))%>%mutate(type = ifelse(type == 'C',"Chirurgie",
                                                                     ifelse(type == "M", "Médecine",
                                                                            ifelse(type == "K", "Médico-technique",
                                                                                   "Indifférenciés"))))
    
    res<- get_diff( with(tmp,tapply(doublon, list(type,pivot), sum)) )
    res2<-get_diff( round(with(tmp,tapply(valopmctmonotime1, list(type,pivot), sum))) )
    if(nrow(res2)>1) res2 = res2[order(abs(res2[,'diff']),decreasing =T),]
    
    tb = rbind(tb,"Type de GHM" = NA)


    for(i in dimnames(res2)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb<-rbind(tb,tmp)
    }
    

    
    tmp<-df%>%mutate(racine := substr(ghm,1,5))%>%left_join(.,racines)%>%
      unite(racine,racine,libelle,sep = " ")%>%
      mutate(racine = paste0(substr(racine,1,50),ifelse(nchar(racine)>51,"...","")))
    nb<-table(tmp$racine)
    nb<-nb[order(nb,decreasing = T)]
    nbc<-cumsum(nb[order(nb,decreasing = T)]*100/sum(nb))
    
    nbc2<-nb[order(nb,decreasing = T)]*100/sum(nb)

    tmp<- tmp %>% mutate(racine = ifelse(racine %in% names(nbc[nbc<95]),racine,'Autres'))%>%
      mutate(racine = factor(racine,levels= c(names(nbc[nbc<95]),'Autres')))
    
    res<- get_diff(with(tmp,tapply(doublon, list(racine,pivot), sum)))
    res2<-get_diff(round(with(tmp,tapply(valopmctmonotime1, list(racine,pivot), sum))))
    if(nrow(res2)>1) res2 = res2[order(abs(res2[,'diff']),decreasing =T),]
  
    tb = rbind(tb,"Racine de GHM" = NA)

    for(i in dimnames(res2)[[1]]){
      
      tmp = rbind(res[i,],res2[i,])
      dimnames(tmp)[[1]]<-c(paste(' -',i),"")
      
      tb<-rbind(tb,tmp)
    }
    

    tb<-bind_cols("niveau"=rep(3,dim(tb)[[1]]),"nom"=dimnames(tb)[[1]],as_tibble(tb))%>%
      dplyr::mutate(niveau = ifelse(grepl("GHM|Catégorie",nom),1,niveau))
    
  
  return(tb)
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
  df<-get_data( rum, ref = "annee", m = 1:mois, a = (annee-getOption('dimRactivite.profondeur_tdb')):annee, val, niveau, opt = T )%>%
    mutate( pivot = factor( annee, levels = ( annee - getOption('dimRactivite.profondeur_tdb')):annee ) )
  
  if (nrow(df)==0) {
    message(
      "\n",
      toString(niveau)," " , toString(val), " aucune donnée retrouvée dans les rum \n"
    )
    return(NA)
  }
  
  
  df<-dplyr::left_join(inner_join(df,rum_v),
                inner_join(rsa,rsa_v)%>%mutate(anseqta = as.numeric(anseqta))%>%select(finess,cle_rsa,annee,ghs,
                                                                                       anseqta,duree,rec_base,rum,noseqrum,
                                                                                       ghm,duree,nbjrbs,sejinfbi,rec_totale,rec_base,
                                                                                       rec_exh,rec_exb))%>%
    dplyr::left_join(.,rsa_dms)%>%
    dplyr::left_join(.,vano%>%inner_join(., df%>%select(finess,cle_rsa,annee,nas)) %>% select( nas, noanon )%>% distinct(nas,noanon))
  
  #Données utiles pour la valorisation
  df<-dplyr::left_join( df %>% mutate(  anseqta = as.character(anseqta) ),
                        tarifs_mco_ghs%>%rename( ghs = ghs, bb = borne_basse, bh = borne_haute )%>%select( anseqta, ghm, ghs, bb, bh ) )
  
  
  
  for (nom_tableau in noms_tableaux){
    
    
    tdb[[nom_tableau]]<-list()
    
    print(paste(val,nom_tableau))
    
    if(nom_tableau == "TableauDeBordCaseMix"){
      
      tdb[[nom_tableau]][['cum']]<-get_actvite_racines(df)
      
      
    }else{
      
    
      inds=get_indicateurs(nom=nom_tableau,val=val)
      
      tdb[[nom_tableau]][['cum']]<-get_tdb(df=df,indicateurs=inds)
    
    }
    
    write.table(tdb[[nom_tableau]][['cum']],file = file.path(getOption('dimRactivite.path_tdb_files'),annee,paste0(nom_tableau,prep_string(val),annee,stringr::str_pad(mois,2,"left","0"),'cum.xls')),sep='\t',row.names=F,na='',fileEncoding = 'latin1')
    
    
  }
  
  #Données mensuelles
  df <- df %>% filter( as.numeric(mois) == mois )
  
  if (nrow(df)==0) {
    message(
      "\n",
      toString(niveau)," " , toString(val), " aucune donnée retrouvée dans les rum mensuels (",toString(mois),") \n"
    )
    return(NA)
  }
  
  
  for (nom_tableau in noms_tableaux){
    
    if(nom_tableau == "TableauDeBordCaseMix"){
      
      tdb[[nom_tableau]][['mens']]<-get_actvite_racines(df)
      
      
    }else{
      
      
      inds=get_indicateurs(nom=nom_tableau,val=val)
      
      tdb[[nom_tableau]][['mens']]<-get_tdb(df=df,indicateurs=inds)
      
    }
    
    
    write.table(tdb[[nom_tableau]][['mens']],file = file.path(getOption('dimRactivite.path_tdb_files'),annee,paste0(nom_tableau,prep_string(val),annee,stringr::str_pad(mois,2,"left","0"),'mens.xls')),sep='\t',row.names=F,na='',fileEncoding = 'latin1')
  }
  
  
  
  return(tdb)
  
}

