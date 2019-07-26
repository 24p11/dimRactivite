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
update_options<-function(file = 'options.yaml'){

  dimRactivite_options = yaml::read_yaml(file)
  dimRactivite_options<-list(
    dimRactivite.services_exclus =  stringr::str_trim( stringr::str_split(dimRactivite_options$services_exclus,',') %>% purrr::flatten_chr()),
    dimRactivite.gestion_doublons_rum = dimRactivite_options$gestion_doublons_rum
  )

  dimRactivite.services_exclus =

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
