% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions_complementaires.R
\name{get_indicateurs}
\alias{get_indicateurs}
\title{Génère la liste des indicateurs à calculer en fonction du fichier de paramètre}
\usage{
get_indicateurs(nom, val, df = references)
}
\arguments{
\item{nom}{chaine de catactères, nom du tableau de bord}

\item{val}{chaine de caractères, variable géographique qui sera choisie pour la selection (ex : service, pole,...).}

\item{df}{tiblle de type paramètres des tableaux de bord}
}
\value{
vecteur de nom d'indicateurs
}
\description{
Génère la liste des indicateurs à calculer en fonction du fichier de paramètre
}
\examples{

}
