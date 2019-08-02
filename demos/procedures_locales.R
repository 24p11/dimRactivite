
load('~/GH_PMSI/DATA/WD/identites_historique.RData')


nipipp <- read_csv2("~/GH_PMSI/DATA/NIP-IPP-ORBIS/LRB-SLS_Liste_NIP_IPP_20161122.csv", 
                    skip = 1,
                    col_names = c('nom','prenom','dnn','aphp','ipp','tri','nip'),
                    cols(
                      nom =  col_character(),
                      prenom = col_character(),
                      dnn =  col_character(),
                      aphp = col_character(),
                      ipp = col_character(),
                      tri = col_character(),
                      nip =  col_character())
)%>%
  dplyr::select(nip,ipp)

identites_mensuelle <- read_csv2("~/GH_PMSI/DATA/WD/identites_mensuelle.csv",
                                 skip = 1,
                                 col_names = c('nas','nip','nom','prenom','date_naissance'),
                                 cols(
                                   nas = col_character(),
                                   nip = col_character(),
                                   nom = col_character(),
                                   prenom = col_character(),
                                   date_naissance = col_date(format = "")
                                 ))

identites_mensuelle<-right_join(nipipp,identites_mensuelle,by=c('ipp'='nip'))

identites<-bind_rows(identites_historique%>%dplyr::select(names(identites_mensuelle)),identites_mensuelle)
identites = distinct(identites,nas,.keep_all = TRUE)
identites_historique <- identites
save(identites_historique,file = '~/GH_PMSI/DATA/WD/identites_historique.RData')

rsa<-dplyr::right_join(identites,rsa)
rum<-dplyr::right_join(identites,rum)
rum_save<-rum

rum<-inner_join(rum,rum_valo)
#Sauvegarde de l'objet final disponible dans le working directory
save(rsa, rum, diagnostics, actes, ano, pmctmono, file = '~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr_dispose.RData')

rum<-rum_save
save(rsa, rum, rum_valo, diagnostics, actes, ano, pmctmono, file = '~/GH_PMSI/DATA/WD/Rpmsi_pmeasyr.RData')
rm(rum_save);gc()


