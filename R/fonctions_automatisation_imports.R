#' Récupération de la liste de l'ensemble des fichiers de remontée contenu dans un dossier
#'
#' @param path racine du dossier contenant les archives
#'
#' @return liste des fichiers d'archives disponibles
#' @export
#'
#' @examples fichiers_genrsa <- scan_path(getOption("dimRactivite.path"))
#'
scan_path<-function(path = getOption("dimRactivite.path")){

  fichiers_genrsa =NULL

  ext = getOption("dimRactivite.extensions")


  folders = list.dirs(path)

  for(folder in folders){

    files = list.files(paste0(folder,'/'))

    for(file in files){

      if( stringr::str_sub(file,-3)=='zip'){
        r<-pmeasyr::astat(path = folder,
                          file = file,
                          view = F)
        finals = r$Name
        zfile = file


      }else{

        finals = file
        zfile=NA
      }

      for(final in finals){

        det_final = unlist(str_split(final,'\\.'))

        if(any(det_final%in%ext)){

          fichiers_genrsa = rbind(fichiers_genrsa,data.frame("finess" = det_final[1],
                                                           "annee" = det_final[2],
                                                           "mois" = det_final[3],
                                                           "type" = det_final[4],
                                                           "ext" = det_final[length(det_final)],
                                                           "archive"=zfile,
                                                           "file"=file,
                                                           "filepath"=folder,
                                                           "RData" =NA,
                                                           stringsAsFactors = F)
          )
        }

      }

    }

  }

  return(as_tibble(fichiers_genrsa))


}


#' Import des principaux fichiers de remontées et valorisation des séjours et résumés
#'
#' @param p un noyau pmeasyr
#' @param tarifsante si TRUE utilisation des tarifs de l'année antérieure
#' @param save si TRUE sauvegarde un fichier .RData dans le répertoire des données de remontée
#' @param persist si TRUE renvoie les données
#'
#' @return liste avec principaux fichiers de remontées, et enregistrement sur le disque si besoin
#' @export
#'
#' @examples data2019 = ipmeasyr(p,tarifsante=TRUE,save=TRUE,persist=TRUE)
#'
imco<-function(p, tarifsante = FALSE, save = TRUE, persist = FALSE, pathm12 = NULL){

  if(tarifsante==TRUE) {
    tarifs      <- tarifs_mco_ghs %>% dplyr::distinct(ghs, anseqta, .keep_all = TRUE) %>% dplyr::mutate(anseqta=as.character(as.numeric(anseqta)+1))
    supplements <- tarifs_mco_supplements %>% dplyr::mutate(anseqta=as.character(as.numeric(anseqta)+1))
    suffixe = "tarifs_anterieurs_"
  } else {
    tarifs      <- tarifs_mco_ghs %>% dplyr::distinct(ghs, anseqta, .keep_all = TRUE)
    supplements <- tarifs_mco_supplements
    suffixe = ""
  }

  #Pour le cacul des pmct mono rum on préfère toujours utiliser les 12 derniers mois.
  #Si l'import concerne un mois autre que décembre, on importe également si elles sont dispnibles les données M12 de l'année antérieure
  if(p$mois !=12) deb  = p$annee -1
  fin = p$annee

  #On prévoit de modifier le noyau pour l'import
  p_import = p

  rsa_en_cours = NULL
  rum_en_cours = NULL

  for(a in (p$annee-1):p$annee){

    #On change l'année et le mois du noyau d'import en fonction du contexte
    if(p$annee != a){
      p_import$annee = a
      p_import$mois = 12
        if(is.null(pathm12)){
          p_import$path = paste0(getOption("dimRactivite.path"),'/',p$finess,'/',p_import$annee,'/M',p_import$mois)
          } else {
          p_import$path = pathm12
        }
    }

    #Imports
    rsa <- pmeasyr::irsa(p_import,typi = 3 ,tolower_names = T)
    vrsa <- pmeasyr::vvr_rsa(p_import,tolower_names = T)
    vano <- pmeasyr::vvr_ano_mco(p_import,tolower_names = T)
    porg <- pmeasyr::ipo(p_import,tolower_names = T)
    diap <- pmeasyr::idiap(p_import,tolower_names = T)
    ium <- pmeasyr::iium(p_import,tolower_names = T)
    pie <- pmeasyr::ipie(p_import,tolower_names = T)
    tra <- pmeasyr::itra(p_import ,  tolower_names = T )
    rum <- pmeasyr::irum(p_import, typi = 4 ,  tolower_names = T )

    #Transformation des données

    #Intégration du tra
    rsa$rsa =  pmeasyr::inner_tra( rsa$rsa, tra )
    rum$rum =  pmeasyr::inner_tra( rum$rum, tra )

    #Intégration du type d'UMA au fihcier IUM
    ium = left_join(ium %>% dplyr::mutate(temp=as.integer(annee)-as.integer(substr(dteaut,1,4))) %>%
                      dplyr::arrange(noum,temp) %>% dplyr::filter(temp>=0, !duplicated(noum))  %>%
                      dplyr::select(-temp), nomenclature_uma%>%dplyr::select(typeaut, libelle_typeaut,
                                                                             discipline ))


    #Intégration des type d'UMA au fichier rum
    rum$rum = left_join(rum$rum,ium%>%dplyr::select(noum,typeaut,typehosp,libelle_typeaut,
                                                    discipline),by=c("cdurm" = "noum"))
    #Transformation des diagnostics du rum
    rum <- pmeasyr::tdiag(rum)

    #Valorisation des séjours
    rsa_v <- pmeasyr::vvr_ghs_supp(rsa = vrsa,
                                   tarifs = tarifs%>%mutate(ghs = as.character(ghs), anseqta =  as.character(anseqta)),
                                   supplements =  supplements %>% mutate(anseqta = as.character(anseqta)),
                                   ano = vano,
                                   porg = porg,
                                   diap = diap,
                                   pie = pie,
                                   bee = FALSE)


    rsa_v = pmeasyr::inner_tra( rsa_v , tra )

    # rsa_v <- dplyr::left_join(rsa$rsa, dplyr::distinct( rsa_v, cle_rsa,.keep_all = TRUE) )




    rum$rum =  pmeasyr::inner_tra( rum$rum, tra )

    vano = vano%>%dplyr::select(names(vano)[ ! grepl('^cr',names(vano))])%>%
             dplyr::mutate( ansor = as.character(a) )

    #Objets temporaires à valoriser
    rsa_en_cours = dplyr::bind_rows(dplyr::left_join(rsa$rsa, dplyr::distinct( rsa_v, cle_rsa,.keep_all = TRUE) ), rsa_en_cours)
    rum_en_cours = dplyr::bind_rows(rum$rum, rum_en_cours)

  }

  #Etape 2 : valorisation des rum par séjour avec répartition des recettes par service

  #pmct mono de l'année antétieure
  pmctmono =  pmct_mono_uma(rsa_en_cours, rum_en_cours, p$annee, p$mois)

  #Valorisation des rum
  rum_valo <- vvr_rum_repa(dplyr::left_join(rsa$rsa, dplyr::distinct( rsa_v, cle_rsa,.keep_all = TRUE) ), rum$rum, pmctmono)

  #SAUVEGARDE OBJET PAR MOIS

  i = paste0(p$annee,'_',p$mois)

  if(!tarifsante){
    
    assign( paste0('rum_valo_',suffixe,i), rum_valo )
    assign( paste0('rsa_v_',suffixe,i), rsa )
    assign( paste0('rsa_',i), rsa$rsa )
    assign( paste0('rum_',i), rum)
    assign( paste0('vano_',i), vano )
    assign( paste0('porg_',i), porg )
    assign( paste0('diap_',i), diap )
    assign( paste0('pie_',i), pie )
    assign( paste0('pmctmono_',i), pmctmono )
    
    if(persist){
      return(list( 'rsa' = rsa$rsa,
                   'rsa_v' = rsa_v,
                   'rum' = rum,
                   'rum_valo' = rum_valo,
                   'vano'= vano,
                   'tra'= tra,
                   'pmctmono' = pmctmono,
                   'pie' = pie,
                   'diap' = diap,
                   'porg' = porg  )
      )
    } 
    
    if(save){
    save( list = c(paste0('rum_',i),
                   paste0('rum_valo_',i),
                   paste0('rsa_',i),
                   paste0('rsa_v_',i),
                   paste0('vano_',i),
                   paste0('pmctmono_',i)),
          file = paste0(p$path,"/",p$finess,".",p$annee,".",p$mois,".RData")
    )
    }

  } else {

    assign( paste0('rum_valo_',suffixe,i), rum_valo )
    assign( paste0('rsa_v_',suffixe,i), rsa )
    
    if(persist){
      return(list( 'rsa_v' = rsa_v,
                   'rum_valo' = rum_valo ) )
    } 
    
    if(save){
    save( list = c(paste0('rum_valo_',suffixe,i),
                   paste0('rsa_v_',suffixe,i)),
          file = paste0(p$path,"/",p$finess,".",p$annee,".",p$mois,".",substr(suffixe,1,nchar(suffixe)-1),".RData")
    )
    }

  }

}




#' un ensemble de fonction permettant d'automatiser les imports pmeasyr et d'effectuer des opérations
#' de transformation et de sélection des séjours
#'
#' Warning: utilisation du package pmeasyr

#' Selection des diagnostics de cancerologie pour un objet pmeasyr diagnostics
#'
#' @param df un tibble de type diagnostic
#'
#' @return une sélection de df pour la cancerologie
#' @export
#' @section Warning: utilise la liste inca_cancero
#' @examples

selection_cancer_diag<-function(df = diagnostics){

  dplyr::inner_join(df,dplyr::as_tibble(inca_cancero),by = c("diag" = "CODE"))%>%
    dplyr::mutate(nda = substr(nas,1,9))%>%
    dplyr::inner_join(rum%>%select(nas,ipp,ansor),.)%>%
    dplyr::mutate(ipp = dplyr::if_else( (is.na(ipp) | ipp=='') ,nda,ipp))%>%
    dplyr::distinct( nda,diag,type, .keep_all= TRUE)%>%
    dplyr::select(-norum)


}

#' Attribution d'un diagnostic de cancer par patient pour un objet de type diagnotics en sortie de \code{\link{selection_cancer_diag}}
#'
#' @param df  un tibble de type diagnostic en sortie de \code{\link{selection_cancer_diag}}
#'
#' @return un tibble de type diagnostic comprenant une ligne par patient selectionne comme etant le diagnostic de cancer unique pour l'analyse de l'activite
#' @export
#' @section Warning: utilise lae resultat de la fonction  \code{\link{selection_cancer_diag}}
#' @examples
#'
selection_cancer_pat<-function(df){

  df%>%mutate(score_confiance_diag_cancer = dplyr::if_else(type=='a',1,10))%>%
    group_by(ipp,diag,APPAREIL,ORGANE)%>%
    summarise(score_confiance_diag_cancer = sum(score_confiance_diag_cancer))%>%
    ungroup()%>%
    group_by(ipp)%>%
    filter(score_confiance_diag_cancer == max(score_confiance_diag_cancer))%>%
    distinct(ipp, .keep_all= TRUE)
}


#' Attribution du type de chirurgie M4 pour les objets de type rsa
#' Les rsa de cancerologie sont préalablerment selectionnés en utilisant les fonctions \code{\link{selection_cancer_diag}} et
#' \code{\link{selection_cancer_pat}}. Les objets en sortie de ces deux fonctions sont concaténés avec un objet rsa et mis en entrée de
#' la fonction pour attribution du type M4 de cancer
#'
#' @param df  un tibble de type rsa concaténé avec un tibble diagnostic en sortie de \code{\link{selection_cancer_pat}}
#'
#' @return un tibble de type rsa
#' @export
#'
#' @examples
#'
attribution_type_M4<-function(df){

  df<-df%>% dplyr::mutate(type_chirugie = NA)
  for (nom_liste in c("ChirurgeCancersDigestif","ChirurgieCancersGynecologique",
                      "ChirurgieCancersORL_Maxilo","ChirurgieCancersSein","ChirurgieCancersThorax",
                      "ChirurgieCancersThyoroide","ChirurgieCancersUrologie","CancersSNC",
                      "CancersOsTissusMou","ChirurgieCancersThyroide","CancersDeLaPeau")){

    df <- df %>% dplyr::mutate(type_chirugie = ifelse(dp %in% listes_cim[[nom_liste]]$code & gptype == 'C', nom_liste, type_chirugie))

  }

  return(df)
}

#' Attribution du statut nouveau patient pour les objets de type rsa (utilisé pour la cancérologie)
#' Les rsa de cancerologie sont préalablerment selectionnés en utilisant les fonctions \code{\link{selection_cancer_diag}} et
#' \code{\link{selection_cancer_pat}}. Les objets en sortie de ces deux fonctions sont concaténés avec un objet rsa et mis en entrée de
#'
#' @param df  un tibble de type rsa concaténé avec un tibble diagnostic en sortie de \code{\link{selection_cancer_pat}}
#'
#' @return un tibble de type rsa
#' @export
#'
#' @examples
#'
attribution_statut_nx_patient<-function(df){

  ansor_deb<-max(df$ansor); ansor_fin<-min(df$ansor)

  df<-df%>%dplyr::mutate(nx_pat = 'N')

  for(a in ansor_deb:ansor_fin){
    tmp <- df %>% dplyr::filter(ansor %in% (a-3):(a-1) )%>% dplyr::select(ipp) %>% purrr::flatten_chr() %>% unique()
    df <- df%>%dplyr::mutate(nx_pat = ifelse(! ipp %in% tmp & ansor == a,'O',nx_pat))

  }

  return(df)
}

