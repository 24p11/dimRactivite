#' Chargement des options du package dimRactivité
#'
#' @param file fichier yaml d'options du package dimRactivité
#'
#' Options à renseigner dans le fichier sont:
#' @param services_exclus : lise des services exclus (tels qu'il apparaissent dans le fichier structure) séparé par une virgule
#' @param gestion_doublons_rum : pour certain niveau de structure ou certain libellé de regroupement
#'
#' @return NULL
#' @export
#'
#' @examples
#' update_options()
update_options<-function(file = 'demos/options.yaml'){

  dimRactivite_options_ = yaml::read_yaml('demos/options.yaml')

  dimRactivite_options = list()

  for( o in names(dimRactivite_options_) ) {


    var_op <- as.name(paste0("dimRactivite.",o))

    val_op <- dimRactivite_options_[[o]]

    if( typeof(val_op) == "character" ){

      val_op <- stringr::str_trim( stringr::str_split(val_op,',') %>% purrr::flatten_chr())

    }
    dimRactivite_options[[var_op]]<- val_op

  }


  options(dimRactivite_options)

}

dimRactivite_default_options <- list(
  dimRactivite.services_exclus = NULL,
  dimRactivite.gestion_doublons_rum = list(
    "service"= "service",
    "pole"= "service"
  )
)

.onLoad <- function(libname, pkgname) {

  options(dimRactivite_default_options)

  invisible()
}