% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions_automatisation_imports.R
\name{save_remontees}
\alias{save_remontees}
\title{Création des fichiers .RData contenant l'ensemble des données d'une remontée
à partir du tableau d'analyse des fichiers de remontée (analyse_remontee)}
\usage{
save_remontees( remontees )
}
\arguments{
\item{remontees}{tibble en sortie de  analyse_remontee}
}
\value{
ecriture sur le disque d'un .RData de sauvegarde pour chaque remontée
}
\description{
Création des fichiers .RData contenant l'ensemble des données d'une remontée
à partir du tableau d'analyse des fichiers de remontée (analyse_remontee)
}
\examples{
\dontrun{

   save_remontees( remontees )
   
}

}
