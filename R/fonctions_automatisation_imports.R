#' un ensemble de fonction permettant d'automatiser les impots pmeasyr et d'effectuer des opérations
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
    mutate(nda = substr(nas,1,9))%>%
    dplyr::inner_join(rum%>%select(nas,ipp,ansor),.)%>%
    mutate(ipp = dplyr::if_else( (is.na(ipp) | ipp=='') ,nda,ipp))%>%
    distinct( nda,diag,type, .keep_all= TRUE)%>%
    select(-norum)


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
