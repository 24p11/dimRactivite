

`%>%` <- magrittr::`%>%`

#' Chargement des options du package dimRactivité
#'
#' @param file fichier yaml d'options du package dimRactivité
#'
#' Options à renseigner dans le fichier sont:
#' @param services_exclus : lise des services exclus (tels qu'il apparaissent dans le fichier structure) séparé par une virgule
#' @param gestion_doublons_rum : pour certain niveau de structure ou certain libellé de regroupement
#' @return NULL
#' @export update_options
#'
#' @examples

update_options<-function(file = 'options.yaml'){

  dimRactivite_options_ = yaml::read_yaml(file)

  dimRactivite_options = list()

  for( o in names(dimRactivite_options_) ) {


    var_op <- as.name(paste0("dimRactivite.",o))

    val_op <- dimRactivite_options_[[o]]

    if( typeof(val_op) == "character" ){

      val_op <- stringr::str_trim( stringr::str_split(val_op,',')  %>% purrr::flatten_chr() )

    }
    dimRactivite_options[[var_op]]<- val_op

  }


  options(dimRactivite_options)

}

#' set option from val
#'
#' @param opt string, name of the option
#' @param val string/number, value of the option
#'
#' @return
#' @export
#'
#' @examples
set_option<-function(opt,val){

  opt = paste0('dimRactivite.',opt)
  
  options(setNames(list(val),opt) )
}


#' Actions to do when package is called
#'
#' @param libname 
#' @param pkgname 
#'
#' @return
#' @export
#'
#' @examples
.onLoad <- function(libname, pkgname) {

  #update_options()

  invisible()
}


