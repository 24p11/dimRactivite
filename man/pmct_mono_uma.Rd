% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions_pmeasyr.R
\name{pmct_mono_uma}
\alias{pmct_mono_uma}
\title{Calcul des PMCT mono UMA (moyenne du tarifs GHS des séjours pris en charge dans une seule UMA)
Cet indicateur est calculé à partir des recettes ghs (sans la valorisation des extrêmes ni des suppléments) 
pour les séjours dont la variable nbrum = 1 sauf pour les réanimations (autorisations PMSI 01A et 01B) 
où cette restriction n'a pas été faite.
Le périmètre du calcul est le suivant :
- pour les remontées en année pleine, à M12, les pmct monorum sont calculés sur les 12 mois de l'année,
- pour les remontées infra-annuelle, les pmct monorum sont calculés sur 12 mois glissants
(attention les fichiers de la remontée M12 de l'année antérieure dezippés doit être présente )}
\usage{
pmct_mono_uma(rsa, rum, annee, mois)
}
\arguments{
\item{rsa}{}

\item{rum}{}

\item{annee}{}

\item{mois}{}
}
\description{
Calcul des PMCT mono UMA (moyenne du tarifs GHS des séjours pris en charge dans une seule UMA)
Cet indicateur est calculé à partir des recettes ghs (sans la valorisation des extrêmes ni des suppléments) 
pour les séjours dont la variable nbrum = 1 sauf pour les réanimations (autorisations PMSI 01A et 01B) 
où cette restriction n'a pas été faite.
Le périmètre du calcul est le suivant :
- pour les remontées en année pleine, à M12, les pmct monorum sont calculés sur les 12 mois de l'année,
- pour les remontées infra-annuelle, les pmct monorum sont calculés sur 12 mois glissants
(attention les fichiers de la remontée M12 de l'année antérieure dezippés doit être présente )
}
\examples{
\dontrun{

   pmct_mono_uma(rsa, rum, annee, mois) -> pmcto_mono
   
}
}
