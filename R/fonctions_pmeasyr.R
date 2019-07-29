#################
# PMCT MONORUMS #
#################
###Fonction pmctmono : pmct mono urm calculé par année ou 12 mois glissant si mois < 12 / pour les REA: moyenne des couts de sejour complets (pas de repartition au temps)
# A faire : changement nom pmctmono et pmctuma

pmct_mono_uma <- function (rsa, rum, annee, mois) {


  #Calcul : pmct mono-rum (pmctnosup_norea) + sommes recettes mono rum + nb sej monorum
  #Selection des séjours en fonction de la date (12 mois glissants) et du nombre de rum
  rsa %>%dplyr::filter( ansor < annee & moissor < mois ) %>%
    dplyr::filter(nbrum==1)%>%
    dplyr::inner_join(., rum %>% dplyr::select(nas, nofiness, cdurm, typeaut))  %>%
    distinct( nas, .keep_all= TRUE)  %>%
    dplyr::group_by( nofiness, cdurm, typeaut ) %>%
    dplyr::summarise(pmctnosup_norea = mean( rec_base, na.rm = TRUE ),
                     recbeenosup_norea = sum( rec_base, na.rm = TRUE ),
                     nbnosup_norea = length( rec_base ) ) -> temp2

  #Calcul des pmct par uma (non mono rum)
  rsa %>%dplyr::filter(ansor < annee & moissor < mois) %>%
    dplyr::inner_join(., rum %>% dplyr::select(nas, nofiness, cdurm, typeaut))  %>%
    #une seule ligne par séjour et uma
    distinct(  nofiness, cdurm, nas, .keep_all= TRUE)   %>%
    dplyr::group_by( nofiness, cdurm, typeaut ) %>%
    dplyr::summarise( pmctnosup_rea=mean(rec_base, na.rm = TRUE),
                      recbeenosup_rea=sum(rec_base, na.rm = TRUE),
                      nbnosup_rea=length(rec_base)) -> temp3

  tempfinal <-  inner_join(temp2, temp3) %>%
    dplyr::mutate(pmctnosup = dplyr::if_else(substr(typeaut,1,2)=="01",
                                             pmctnosup_rea,
                                             pmctnosup_norea),
                  recbeenosup = dplyr::if_else(substr(typeaut,1,2)=="01",
                                               recbeenosup_rea,
                                               recbeenosup_norea),
                  nbnosup = dplyr::if_else(substr(typeaut,1,2)=="01",
                                           nbnosup_rea,
                                           nbnosup_norea)) %>%
    dplyr::select(-pmctnosup_rea, -pmctnosup_norea, -recbeenosup_rea,
                  -recbeenosup_norea, -nbnosup_rea, -nbnosup_norea)

  return(tempfinal)
}



# rec_totale :Valorisation 100% T2A globale
# rec_base: Valorisation des GHS de base
# rec_bee: Valorisation base + exb + exh
# rec_exb: Valorisation extrême bas (à déduire)
# rec_rehosp_ghm: Valorisation séjours avec rehosp dans même GHM
# rec_mino_sus: Valorisation séjours avec minoration forfaitaire liste en sus
# rec_exh: Valorisation journées extrême haut
# rec_aph: Valorisation actes GHS 9615 en Hospit.
# rec_rap: Valorisation suppléments radiothérapie pédiatrique,
# rec_ant: Valorisation suppléments antepartum,
# rec_rdt_tot: Valorisation actes RDTH en Hospit.,
# rec_rea: Valorisation suppléments de réanimation
# rec_rep: Valorisation suppléments de réa pédiatrique
# rec_nn1: Valorisation suppléments de néonat sans SI
# rec_nn2: Valorisation suppléments de néonat avec SI
# rec_nn3: Valorisation suppléments de réanimation néonat
# rec_po_tot: Valorisation prélévements d organe
# rec_caishyp:Valorisation des actes de caissons hyperbares en sus
# rec_dialhosp: Valorisation suppléments de dialyse
# rec_src: Valorisation suppléments de surveillance continue
# rec_stf: Valorisation suppléments de soins intensifs
# rec_sdc: Valorisation supplément défibrilateur cardiaque
########################################################################
# PMCT MULTIRUMS [NB: ghm rehospitalisation repartit sur le temps ici] #
########################################################################
###Fonction pmctmono (on doit encore repartir les supplements RAP et ANTEPARTUM)
vvr_rum_repa <- function (rsa, rum, pmctmono) {

  #selection de la periode pertinente:
  #rsa %>% dplyr::filter((as.numeric(ansor)==annee & as.numeric(moissor)<=mois)) %>% dplyr::select(norss) -> norss

  #Actes de radiothérapie donnant lieu à facturation d'un supplément
  actesradio <- "AZNL001|QZNL001|ZZNL045|ZZNL046|ZZNL047|ZZNL048|ZZNL050|ZZNL051|ZZNL052|ZZNL053|ZZNL054|ZZNL058|ZZNL059|ZZNL060|ZZNL061|ZZNL062|ZZNL063|ZZNL064|ZZNL065|ZZNL066"
  #Actes de dialyse donnant lieu à facturation d'un supplément
  actesdialyse <- "JVJF003|JVJF004|JVJF008|JVRP004|JVRP007|JVRP008"
  #Actes pose défibrilateur cardiaque
  actessdc <- "DEKA002|DELA004|DELA007|DELF013|DELF014|DELF016|DELF020|DELF900"
  #Actes d'aphérère donnant lieu à facturation d'un supplément
  actesapherese <- "FEFF001|FEFF002|FEJF001|FEJF002|FEJF004|FEJF005|FEJF007|FEJF009|FEPF001|FEPF002|FEPF003|FEPF004|FEPF005|FERP001"

  #creation fichier intermediaire des valorisations:
  rsa %>% dplyr::mutate(ansor = ansor) %>%
    dplyr::select(nas,duree,nbrum,ansor,moissor,dplyr::starts_with("rec_"),
                  nbsuprea,nbsupsi,nbsupstf,nbsuprep,nbsupsrc,nbsupreaped,nbsupnn1,
                  nbsupnn2,nbsupnn3,nbsupcaisson,nb_rdth) -> fullrsatemp1

  #creation table des sommes pour coefficient de repartition (sur pmct à n sans sup):
  dplyr::left_join(rum, fullrsatemp1) %>% dplyr::left_join(.,pmctmono) %>%

    #  somme par rum des actes éligible facturation d'un supplément
    dplyr::mutate(adial = stringr::str_count(actes, actesdialyse),
                  ardt = stringr::str_count(actes, actesradio),
                  aaph = stringr::str_count(actes, actesapherese),
                  asdc = stringr::str_count(actes, actessdc)) ->fullrsatemp1B

  fullrsatemp1B%>%dplyr::group_by(nas) %>%
    dplyr::summarise( # sommes des coefficients de répartition mixtes durée rum + pmctmono
      sumpmctimenosup=sum(pmctnosup*(dureesejpart+1), na.rm = TRUE),
      #sommes pmct monorum
      sumpmctnosup=sum(pmctnosup,na.rm = TRUE),
      nsrea=first(nbsuprea),
      #soins intensifs issus de réa
      nssir=pmax(first(nbsupsi),0) ,
      #somme des journées des rum avec autorisation réanimation
      jrea=sum(dureesejpart[substr(typeaut,1,2)=="01"]+1),
      #calcul des suppléments soins intensifs
      nsstf= pmax(first(nbsupstf),0) ,
      #calcul des suppléments soins intensifs hors réa
      nsstf_hr=pmax(first(nbsupstf)-first(nbsupsi),0),
      #somme des journées des rum avec autorisation soins intensifs (sans les réa)
      jstf=sum(dureesejpart[substr(typeaut,1,2) %in% c("02","16","18")]+1),
      #somme des journées des rum avec autorisation réanimation pédiatrique
      jrep=sum(dureesejpart[substr(typeaut,1,2)=="13"]+1),
      #somme des journées surveillance continue
      jsrc=sum(dureesejpart[substr(typeaut,1,2) %in% c("03","14")]+1),
      #somme des journées éligibles sup NN1
      jnn1=sum(dureesejpart[substr(typeaut,1,2)=="04"]+1),
      #somme des journées éligibles sup NN2
      jnn2=sum(dureesejpart[substr(typeaut,1,2) %in% c("05","06","13")]+1),
      #somme des journées éligibles sup NN3
      jnn3=sum(dureesejpart[substr(typeaut,1,2) %in% c("06","13")]+1),
      #somme des journées éligibles sup hyp1
      jcaishyp1=sum(dureesejpart[substr(typeaut,1,2) %in% c("01","13")]+1),
      #somme des journées éligibles sup hyp2
      jcaishyp2=sum(dureesejpart[substr(typeaut,1,2) %in% c("02","16","18")]+1),
      #somme des journées éligibles sup antépartum
      jant=sum(dureesejpart[substr(typeaut,1,2) %in% c("70","71","73")]+1),
      sdial = sum(adial, na.rm = TRUE),#Varaible déjà existante dans le rsa; vérifier concordance
      srdt = sum(ardt, na.rm = TRUE),
      saph = sum(aaph, na.rm = TRUE),
      ssdc = sum(asdc, na.rm = TRUE) ) -> tempmct1

  #bind des rum d'interet avec les prix/duree des rsa correspondant, les pmctmono et leur somme, puis ventilation supplements et clef de repartition:
  dplyr::left_join(rum, fullrsatemp1) %>% dplyr::left_join(.,pmctmono) %>%
    dplyr::left_join(.,tempmct1) %>%
    dplyr::mutate(  # rec_pmctmono = ifelse(substr(typeaut,1,2)=="01",
      # ((dureesejpart+1)/jrea) * rec_bee*(jrea/(duree+1)) + (pmctnosup / sumpmctnosup) * rec_bee*(((duree+1)-jrea)/(duree+1)),
      # (pmctnosup / sumpmctnosup) * rec_bee),
      # rec_pmctmonotime1 = ifelse(substr(typeaut,1,2)=="01",
      # ((dureesejpart+1)/jrea) * rec_bee*(jrea/(duree+1)) + (pmctnosup*(dureesejpart+1)/sumpmctimenosup) * rec_bee*(((duree+1)-jrea)/(duree+1)),
      # (pmctnosup*(dureesejpart+1)/sumpmctimenosup) * rec_bee),

      #PMCT
      #-----------------------------------------------------

      # rec_time = recettes ghs repartition temps de passage
      rec_time = ((dureesejpart+1)/(duree+nbrum)) * rec_bee,
      #rec_pmctmono = recettes ghs répartition pmct monorum
      rec_pmctmono = (pmctnosup / sumpmctnosup) * rec_bee,
      #rec_pmctmonotime1 = recettes ghs répartition temps + mono rum
      rec_pmctmonotime1 = (pmctnosup*(dureesejpart+1)/sumpmctimenosup) * rec_bee,
      #rec_pmctmonotime2 = recettes ghs avec moyenne 2 répartitions

      rec_pmctmonotime2 = (rec_time+rec_pmctmono)/2,

      #suppléments autorisation 01 (A+B) réanimation
      #-----------------------------------------------------

      #nombre de supplements réanimation avec répartition
      nbsuprea_repa = ifelse(substr(typeaut,1,2)=="01",
                             nbsuprea * ((dureesejpart+1)/jrea),
                             0),
      #rec_rea_repa= recettes sup réa avec clé répartition
      rec_rea_repa = ifelse(substr(typeaut,1,2)=="01",
                            rec_rea * ((dureesejpart+1)/jrea),
                            0),

      #suppléments autorisation 01 (A+B) si en réanimation (sir)
      #-----------------------------------------------------

      #nombre de supplement SI de réanimation avec répartition
      nssir_repa  = ifelse(substr(typeaut,1,2)=="01",
                           nssir * ((dureesejpart+1)/jrea),
                           0),
      #rec_sir_repa = recettes sup si réa avec clé répartition
      rec_sir_repa =  ifelse(substr(typeaut,1,2)=="01" & nsstf > 0 ,
                             rec_stf * ((dureesejpart+2)/(jrea+jrep)*(nssir/nsstf)),
                             0),

      #suppléments autorisation = 02,16,18 : si hors réanimation (stf)
      #-----------------------------------------------------

      #nombre de supplement SI hors rea avec répartition
      nsstf_repa = ifelse(substr(typeaut,1,2)%in% c("02","16","18") & nsstf > 0 ,
                          nsstf_hr * ((dureesejpart+1)/jstf),
                          0),
      #rec_stf_hr_repa = recettes sup si hors réa avec clé répartition
      rec_stf_hr_repa = ifelse(substr(typeaut,1,2) %in% c("02","16","18") & nsstf > 0 ,
                               (rec_stf) * ((dureesejpart+1)/jstf)*(nsstf_hr/nsstf),
                               0),


      #suppléments autorisation = 03 (A+B) ,14 surveillance continue (src)
      #-----------------------------------------------------

      #nombre de supplement surveillance continue
      nbsupsrc_repa = ifelse(substr(typeaut,1,2)%in% c("03","14"),
                             nbsupsrc * ((dureesejpart+1)/jsrc),
                             0),
      #rec_src_repa = recettes sup usc avec clé répartition
      rec_src_repa = ifelse(substr(typeaut,1,2) %in% c("03","14"),
                            rec_src * ((dureesejpart+1)/jsrc),
                            0),

      #suppléments autorisation = 13 (A+B) réanimation pédiatrique (rep)
      #-----------------------------------------------------

      #nombre de supplementd réanimation pédiatrique avec répartition
      nbsuprep_repa = ifelse(substr(typeaut,1,2)=="13",
                             nbsuprep * ((dureesejpart+1)/jrep),
                             0),
      #rec_rep_repa = recettes sup réanimation pédiatrique avec clé répartition
      rec_rep_repa = ifelse(substr(typeaut,1,2)=="13",
                            rec_rep * ((dureesejpart+1)/jrep),
                            0),

      #suppléments autorisation = 04 Néonatologie sans SI (nn1)
      #--------------------------------------------------------

      #nombre de supplements nn1
      nbsupnn1_repa = ifelse(substr(typeaut,1,2)=="04",
                             nbsupnn1 * ((dureesejpart+1)/jnn1),
                             0),

      #rec_nn1_repa = recettes sup nn1 avec clé répartition
      rec_nn1_repa = ifelse(substr(typeaut,1,2)=="04",
                            rec_nn1 * ((dureesejpart+1)/jnn1),
                            0),

      #suppléments autorisation = 05 Soins intensifs en néonatologie (nn2)
      #-------------------------------------------------------------------

      #nombre de supplements nn1
      nbsupnn2_repa = ifelse(substr(typeaut,1,2)=="05",
                             nbsupnn2 * ((dureesejpart+1)/jnn2),
                             0),
      #rec_nn2_repa = recettes sup nn2 avec clé répartition
      rec_nn2_repa = ifelse(substr(typeaut,1,2) %in% c("05"),
                            rec_nn2 * ((dureesejpart+1)/jnn2),
                            0),

      #suppléments autorisation 06 = réanimation néonatale (nn3)
      #--------------------------------------------------------

      #nombre de supplements nn1
      nbsupnn3_repa = ifelse(substr(typeaut,1,2)=="06",
                             nbsupnn3 * ((dureesejpart+1)/jnn3),
                             0),

      #rec_nn3_repa = recettes sup nn3 avec clé répartition
      rec_nn3_repa = ifelse(substr(typeaut,1,2) %in% c("06"),
                            rec_nn3 * ((dureesejpart+1)/jnn3),
                            0),

      #rec_caishyp_repa = recettes sup caisson avec clé répartition
      rec_caishyp_repa = ifelse(substr(typeaut,1,2) %in% c("01","13"),
                                rec_caishyp * ((dureesejpart+1)/jcaishyp1),
                                ifelse(substr(typeaut,1,2) %in% c("02","16","18"),
                                       rec_caishyp * ((dureesejpart+1)/jcaishyp2),
                                       0)
      ),
      #rec_po_tot_repa = Répartition po au dernier rum
      rec_po_tot_repa = ifelse(norum==nbrum,
                               rec_po_tot,
                               0),
      #rec_ant_repa = recettes sup ant avec clé répartition
      rec_ant_repa = ifelse(substr(typeaut,1,2) %in% c("70","71","73"),
                            rec_ant * ((dureesejpart+1)/jant),
                            0 ),

      #rec_rdt_tot_repa = recettes sup radiothérapie avec clé répartition
      rec_rdt_tot_repa = ifelse(grepl(actesradio, actes),
                                rec_rdt_tot * (stringr::str_count(actes, actesradio)/srdt),
                                0),
      #rec_rap_repa = recettes sup radiothérapie pédiatrique avec clé répartition
      rec_rap_repa = ifelse(grepl(actesradio,actes),
                            rec_rap * (stringr::str_count(actes, actesradio)/srdt),
                            0),
      #rec_dialhosp_repa = recettes sup dialyse en hosp. avec clé répartition
      rec_dialhosp_repa = ifelse(grepl(actesdialyse, actes),
                                 rec_dialhosp * (stringr::str_count(actes, actesdialyse)/sdial),
                                 0),
      #rec_aph_repa = recettes sup aphérèse avec clé répartition
      rec_aph_repa = ifelse(grepl(actesapherese,actes),
                            rec_aph * (stringr::str_count(actes, actesapherese)/saph),
                            0),
      #rec_sdc_repa = recettes sdc avec clé répartition
      rec_sdc_repa = ifelse(grepl(actessdc,actes),
                            rec_sdc * (stringr::str_count(actes, actessdc)/ssdc),
                            0),
      #rec_rehosp_ghm_repa = recettes ré-hospitalisation
      rec_rehosp_ghm_repa = rec_rehosp_ghm * ((dureesejpart+1)/(duree+nbrum)),

      #rec_sup_repa = recettes ensemble des suppléments avec clé répartition
      rec_sup_repa = rec_rea_repa + rec_rep_repa +  rec_stf_hr_repa + rec_sir_repa + rec_src_repa +
        rec_nn1_repa + rec_nn2_repa + rec_nn3_repa + rec_caishyp_repa +
        rec_rehosp_ghm_repa + rec_rdt_tot_repa + rec_po_tot_repa +
        rec_dialhosp_repa + rec_aph_repa + rec_ant_repa + rec_rap_repa +
        rec_sdc_repa,
      #valotime = recettes totales avec répartition temporelles
      valotime = rec_time + rec_sup_repa,
      #valopmctmono = recettes totales avec répartition pmct
      valopmctmono = rec_pmctmono + rec_sup_repa,
      #valopmctmonotime1 = recettes totales avec répartition pmctmonotime1
      valopmctmonotime1 = rec_pmctmonotime1 + rec_sup_repa,
      #valopmctmonotime2 = recettes totales avec répartition pmctmonotime2
      valopmctmonotime2 = rec_pmctmonotime2 + rec_sup_repa,
      #coeftime = coéfficients de répartition temporel
      coeftime=(dureesejpart+1)/(duree+nbrum),
      #coefpmctmono = coéfficients de répartition pmct
      coefpmctmono=pmctnosup/sumpmctnosup,
      #coefpmctmonotime1 = coéfficients de répartition pmctmonotime1
      coefpmctmonotime1=pmctnosup*(dureesejpart+1)/sumpmctimenosup,
      #coefpmctmonotime2 = coéfficients de répartition pmctmonotime2
      coefpmctmonotime2=(coeftime+coefpmctmono)/2 ) %>%
    dplyr::select(nofiness,cle_rsa,nas,norum,nbrum,
                  ansor='ansor',
                  moissor='moissor',
                  dplyr::ends_with("_repa"),
                  'coeftime',
                  'valotime',
                  'coefpmctmono',
                  'valopmctmono',
                  'coefpmctmonotime1',
                  'valopmctmonotime1',
                  'coefpmctmonotime2',
                  'valopmctmonotime2') -> temprumfull
  return(temprumfull)
}
