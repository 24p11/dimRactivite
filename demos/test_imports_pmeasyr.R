devtools::load_all('~/pmeasyr')

p <- pmeasyr::noyau_pmeasyr(
  finess   = '750100042',
  annee    = 2019,
  mois     = 12,
  path     = '~/GH_PMSI/DATA/test/test_pmeasyr/MCO_LMD_OUT_00047_201918',
  progress = F,
  annee_format = "2020")

p %>% pmeasyr::irsa(typi = 4)     -> rsa
pmeasyr::iano_mco(p) %>% 
  dplyr::select_all(tolower) -> vano
vrsa <- pmeasyr::vvr_rsa(p,tolower_names = T)
vano <- pmeasyr::vvr_ano_mco(p,tolower_names = T)
porg <- pmeasyr::ipo(p,tolower_names = T)
diap <- pmeasyr::idiap(p,tolower_names = T)
ium <- pmeasyr::iium(p,tolower_names = T)
pie <- pmeasyr::ipie(p,tolower_names = T)


tarifs      <- nomensland::get_table('tarifs_mco_ghs') %>% dplyr::distinct(ghs, anseqta, .keep_all = TRUE)%>%
  dplyr::filter(anseqta == p$annee )
supplements <- nomensland::get_table('tarifs_mco_supplements')%>%
  dplyr::filter(anseqta == p$annee )


format <- pmeasyr::formats %>% 
  dplyr::filter(champ == 'mco', table == 'rsa_ano', an == 19)


rsa_v <- pmeasyr::vvr_ghs_supp(rsa = vrsa,
                               tarifs = tarifs%>%mutate(ghs = as.character(ghs), anseqta =  as.character(anseqta)),
                               supplements =  supplements %>% mutate(anseqta = as.character(anseqta)),
                               ano = vano,
                               porg = porg,
                               diap = diap,
                               pie = pie,
                               bee = FALSE)

rsa$rsa %>% 
  dplyr::select_all(tolower) %>% 
  dplyr::select(cle_rsa, duree, rsacmd, ghm, typesej, noghs, moissor, ansor, sexe, agean, agejr,
                anseqta, nbjrbs, nbjrexb, sejinfbi, agean, agejr, nbseance,
                dplyr::starts_with('nbsup'), dplyr::starts_with('sup'),
                nb_rdth, nbacte9615,
                echpmsi, prov, dest, schpmsi,
                rdth, nb_rdth, nbrum) %>%
  dplyr::mutate(nbseance = dplyr::case_when(
    rsacmd == '28' & ! ghm %in% c('28Z19Z', '28Z20Z', '28Z21Z', '28Z22Z') ~ nbseance, 
    ghm %in% c('28Z19Z', '28Z20Z', '28Z21Z', '28Z22Z') ~ 1L, 
    TRUE ~ 1L
  )) %>% 
  dplyr::left_join(rsa$rsa_um %>%   dplyr::select_all(tolower) %>% 
                     dplyr::filter(substr(typaut1, 1, 2) == '07') %>% 
                     dplyr::distinct(cle_rsa) %>%
                     dplyr::mutate(uhcd = 1), by = 'cle_rsa') %>%
  dplyr::mutate(monorum_uhcd = (uhcd == 1 & nbrum == 1)) %>%
  dplyr::select(-uhcd)->vrsa



p2 <- pmeasyr::noyau_pmeasyr(
  finess   = '750100042',
  annee    = 2019,
  mois     = 12,
  path     = '~/GH_PMSI/DATA/test/test_pmeasyr/MCO_OUT_00047_201912',
  progress = F)

p2 %>% pmeasyr::irsa(typi = 4)     -> rsa


format <- pmeasyr::formats %>% dplyr::filter(champ == 'mco', table == 'rsa_ano', an == "20")

format <- pmeasyr::formats %>% dplyr::filter(champ == 'mco', table == 'rsa_ano', an == "19")
