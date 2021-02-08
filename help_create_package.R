library(devtools)
library(tidyverse)
library(fs)
library(referentiels)


create_package(getwd())

devtools::load_all('~/GH_PMSI/INFORMATIQUE/GIT/remi/dimRactivite')

update_options('~/GH_PMSI/INFORMATIQUE/GIT/remi/tableaux_de_bord/options.yaml')


use_build_ignore("help_create_package.R")
devtools::use_package("tidyverse")
devtools::use_package("RCurl")
devtools::use_package("jsonlite")
devtools::use_package("referentiels")
devtools::use_package("dplyr")
devtools::use_package("stringr")
devtools::use_package("yaml")
devtools::use_package("purrr")
devtools::use_package("pmeasyr", "Suggests")

usethis::use_vignette("generation-tableaux-de-bord-pmsi")
usethis::use_vignette("methodes")
usethis::use_vignette("indicateurs")
#.libPaths()
devtools::load_all()
update_options()

devtools::check()

pkgdown::build_site()

use_mit_license("Rémi Flicoteaux")

document()

check()
devtools::build()
devtools::install()
usethis::use_testthat()
library(testthat)
load_all()
devtools::test()

update_options('demos/options.yaml')

load_all()
document()

use_readme_rmd()
## Noyau exemple pour les tests
f = 750100042
a = 2015
m = 12
p= pmeasyr::noyau_pmeasyr(finess =f ,
                          annee = a,
                          mois = m,
                          path   = getOption("dimRactivite.path")
)


#Liste des fichiers présents dans le dossier racine
fichiers_genrsa <- scan_path()
adzipRemonteee(p, fichiers_genrsa)
imco_files_types = getOption("dimRactivite.fichiers_imco")%>%purrr::flatten_chr()
adzipComplet(c(''))



View(cbind(tapply(rum$cle_rsa,list(paste0(rum$ansor,'-',rum$moissor),rum$nofiness),nb_unique)," ",

table(paste0(rsa$ansor,'-',rsa$moissor),rsa$nofiness)))

##TODO ajouter clé primaire objets actes & diagnostics

rum%>%filter(as.numeric(moissor)%in%1:7)%>% distinct(nofiness,cle_rsa,ansor,.keep_all = T) %>%select(pole,ansor)%>%table()

rsa<-rsa%>%inner_join(rum%>%select(nofiness,ansor,cle_rsa,typehosp)%>%distinct(nofiness,ansor,cle_rsa,.keep_all = T))

rsa%>%filter(as.numeric(moissor)%in%1:7)%>%select(nofiness,ansor,typehosp)%>%table()

with(rsa%>%inner_join(.,rsa_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(rec_totale,list(nofiness,ansor,typehosp),sum))->res


with(rum%>%inner_join(.,rum_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(valopmctmonotime1,list(nofiness,ansor,typehosp),sum))->res


with(rsa%>%inner_join(.,rsa_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(rec_bee,list(nofiness,ansor,typehosp),sum))

with(rum%>%inner_join(.,rum_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(valopmctmonotime1-rec_sup_repa,list(nofiness,ansor,typehosp),sum))


with(rsa%>%inner_join(.,rsa_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(rec_stf,list(nofiness,ansor,typehosp),sum))

with(rum%>%inner_join(.,rum_v)%>%filter(as.numeric(moissor)%in%1:7),
     tapply(rec_sir_repa+rec_stf_hr_repa,list(nofiness,ansor,typehosp),sum))


remontees_dispo<-analyse_fichiers_remontees(fichiers_genrsa,maj_dernier_mois =F)
sel_remontees_import<-remontees_dispo%>%filter(as.numeric(annee)> max(as.numeric(remontees_dispo$annee))-profondeur_imports, RData==1)%>%
  mutate(mois  =  as.numeric(mois) )%>%
  group_by( finess, annee )%>% filter(mois == max(mois) )


#test creating xlsx files with format style
#https://stackoverflow.com/questions/50030410/how-to-align-the-cells-of-an-xlsx-file-using-rs-xlsx-package

write.xlsx(USArrests, file="myworkbook.xlsx",
           sheetName="USA-ARRESTS", append=FALSE)

wb<-createWorkbook(type="xlsx")
sheet <- createSheet(wb, sheetName = "Activite mensuelle")
addMergedRegion(sheet, startRow, endRow, startColumn, endColumn)

CellStyle(wb, dataFormat=NULL, alignment=NULL,
          border=NULL, fill=NULL, font=NULL)

format.cells<-getCells(row, colIndex=1:ncol(tdb))

invisible(lapply(all.cells, setCellStyle, cs))

indicateurs_ <- yaml::read_yaml("~/GH_PMSI/INFORMATIQUE/GIT/remi/dimRactivite/vignettes/indicateurs.yml")

names(unlist(lapply(indicateurs_,function(x)grepl("TableauDeBordDIM",x$`tableaux de bord`))))


#Graphique 1 HC




data%>%filter(as.numeric(mois) %in% 1:5)%>%
  group_by(hopital,annee,type_hospit)%>%
  summarise(nb = n())
