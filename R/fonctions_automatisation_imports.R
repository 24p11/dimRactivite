#' Récupération de la liste de l'ensemble des fichiers de remontée contenu dans un dossier
#'
#' @param path racine du dossier contenant les archives
#'
#' @return liste des fichiers d'archives disponibles
#' @export scan_path
#'
#' @examples 
#'
scan_path<-function( path = getOption("dimRactivite.path"), ext = getOption("dimRactivite.extensions") ){

  fichiers_genrsa =NULL
  fichiers_rdata = NULL

  folders = list.dirs(path)

  for(folder in folders){

    files = list.files(paste0(folder,'/'))

    for(file in files){
      
      if(stringr::str_sub(file,-5)=='RData'){

        fichiers_rdata = c( fichiers_rdata, stringr::str_sub(file,1,-7) )
        
      }else{
            
              if( stringr::str_sub(file,-3)=='zip'){
                r<-pmeasyr::astat(path = folder,
                                  file = file,
                                  view = F)
                finals = r$Name
                zfile = file
        
        
              }else{
        
                finals = file
                zfile=NA
                
              }
        
              for(final in finals){
        
                det_final = unlist(stringr::str_split(final,'\\.'))
        
                if(any(det_final%in%ext)){
        
                  fichiers_genrsa = rbind(fichiers_genrsa,data.frame("finess" = det_final[1],
                                                                   "annee" = det_final[2],
                                                                   "mois" = det_final[3],
                                                                   "type" = det_final[4],
                                                                   "ext" = det_final[length(det_final)],
                                                                   "archive"=zfile,
                                                                   "file"=file,
                                                                   "filepath"=folder,
                                                                   "RData" =0,
                                                                   stringsAsFactors = F)
                  )
                }
        
              }
        
      }
      
    }
    
  }  
  
  fichiers_genrsa = as_tibble(fichiers_genrsa)
  

  
  for(rdata in fichiers_rdata){
    
    refs = unlist(stringr::str_split(rdata,'\\.')) 
    
    fichiers_genrsa<-fichiers_genrsa%>%mutate(RData = ifelse(finess == refs[1] & annee==refs[2] & mois ==refs[3],1,RData))
    
  }
  

  return(fichiers_genrsa)


}

#' Lecture des fichiers en sortie de scan_path pour vérification de leur contenu et préparation aux imports
#'
#' @param fichiers_genrsa tibble en sortie de scan_path
#' @param recent logical, si TRUE considère par défaut que le .RData du dernier mois est manquant
#'
#' @return tibble avec liste des fichiers par remontée
#' @export analyse_fichiers_remontees
#'
#' @examples
analyse_fichiers_remontees <- function( fichiers_genrsa, maj_dernier_mois = TRUE ){
  
  imco_files_types = getOption("dimRactivite.fichiers_imco")%>%purrr::flatten_chr()
  
  df<-fichiers_genrsa%>%filter(annee>2012,type%in%imco_files_types)%>%
                                      mutate(annee = as.numeric(annee),
                                            mois = as.numeric(mois),
                                            RData = factor(RData,levels=c('0','1')),
                                            type = factor(type,levels = imco_files_types))
  

  #dossiers<-table(fichiers_genrsa$filepath[o],fichiers_genrsa$type[o])
  dossiers<-table( paste( df$finess,df$annee,df$mois ) , df$type )
  dossiers[which(dossiers>0)]<-1
  dossiers <- cbind(dossiers,'manquants'=rowSums(dossiers)-length(imco_files_types))

  dossiersRdata<-table( paste( df$finess,df$annee,df$mois ) , df$RData )
  dossiersRdata<-dossiersRdata[,'1']
  
  dossiersRdata[which(dossiersRdata>0)]<-1
  
  dossiers <- cbind(dossiers,'RData'=dossiersRdata)
  
  remontees<-dossiers%>%as_tibble()%>%bind_cols('dossier'=row.names(dossiers),.)%>%
    filter(manquants>-3,rss>0,rsa>0)%>%
    separate(dossier,c('finess','annee','mois'))
    #%>%
    #mutate(annee = as.numeric(annee),mois = as.numeric(mois))%>%
    #inner_join(. , 
    #           fichiers_genrsa%>%distinct(finess,annee,mois,archive,.keep_all = T)%>%
    #             group_by(finess,annee,mois)%>% summarise( archives = paste(archive,collapse = ',')) %>%ungroup()%>%
    #             mutate(annee = as.numeric(annee),mois = as.numeric(mois))
    #           )
  
  if(maj_dernier_mois == TRUE){
        remontees <- maj_variable_RData(remontees)
  }
  return(remontees)
  
}

#' Title
#'
#' @param path chemin du dossier dans lequel se trouve les fichiers zipper de remontée
#' @param ext extentinon à intégrer
#'
#' @return tibble, tableau de synthèse des remontées disponibles
#' @export update_remontees_dispo
#'
#' @examples
update_remontees_dispo<-function( path = getOption("dimRactivite.path"), ext = getOption("dimRactivite.extensions") ){
  
  #Liste des fichiers présents dans le dossier racine
  fichiers_genrsa <- scan_path( path, ext )
  
  #Analyse des fichiers et classement par remontée
  remontees_dispo <- analyse_fichiers_remontees( fichiers_genrsa, maj_dernier_mois = FALSE )
  
  
  return(remontees_dispo)
  
  
}


#' Mise à jour de la variabla RData du tableau de synthèse des remontées disponibles pour la dernière remontée disponible
#' force la variable RData à 0 pour que la dernière remontée disponible soit importée
#' @param remontees tibble, tableau de synthèse des remontées disponibles 
#' @param p un noyau pmeasyr
#'
#' @returntibble, tableau de synthèse des remontées disponibles mise à jour
#' @export maj_variable_RData
#'
#' @examples
maj_variable_RData<-function( remontees, p = NULL ){
  
  #Si pas de noyau la variable .RData mise à jour est celle des remontées les plus récentes
  if( is.null(p) ){
  
    if( 1 %in% unique(remontees$RData) ){
      
      recents<-remontees %>% filter( RData==1 , annee == max( as.numeric(annee) ) )%>% 
        group_by( finess, annee )%>%summarise( mois = max( as.numeric(mois) ) )
   
      for(i in 1:nrow(recents)){
        
        p_modif= pmeasyr::noyau_pmeasyr(finess = recents$finess[i] ,
                                  annee =  recents$annee[i],
                                  mois = recents$mois[i],
                                  path   = getOption("dimRactivite.path")
        )
   
        remontees<-maj_variable_RData(remontees,p_modif)
        
      
      }
    }
    
  }else{
  
      remontees<-remontees %>% mutate(RData = ifelse( finess == p$finess &
                                                        annee == p$annee &
                                                        mois == p$mois,
                                                      0,RData)
      )
  }
  
  return(remontees)
}

#' Généralisation de la fonction adzip de pmeasyr pour l'ensemble d'une archive in et out
#' avec contrôle du type de fichier et création des fichiers manquants
#'
#' @param zfichiers vecteur, nom des fichiers zippés
#' @param ext_to_import vecteur, extention des fichiers a importer
#'
#' @return aucun, dézip des archives en fonction du type de fichier voulus et création de fichier vide pour les types de fichier manquant
#' @export adzipComplet
#'
#' @examples
adzipComplet<-function(zfichiers,ext_to_import){

  dz_fichiers<-NULL

  for(zf in zfichiers){

    noms_fichiers<-pmeasyr::astat( path = getOption("dimRactivite.path"),
                                   file = zf,
                                   view = F)$Name

    det_noms = unique(unlist(stringr::str_split(noms_fichiers,'\\.')))

    #Choix des fichiers a importer dasn l'archive en fonction des extensions voulues
    if(grepl('OUT|out',zf)){
      t = 'out'
      types = intersect ( getOption("dimRactivite.fichiers_imco")$`out` , det_noms )
    }
    if(grepl('IN|in',zf)){
      t = 'in'
      types = intersect ( getOption("dimRactivite.fichiers_imco")$`in` , det_noms )
    }


    noms<-noms_fichiers[sapply(noms_fichiers,function(n)grepl(paste(types,collapse = '|'),n))]

    dz_fichiers<-c(dz_fichiers,noms)

    pmeasyr::adezip2(path = getOption("dimRactivite.path"),
                     file = zf,
                     liste = types,
                     pathto = getOption("dimRactivite.path"))


  }


  #Vérification si tous les fichiers nécessaires sont présents
  det_noms = unique(unlist(stringr::str_split(dz_fichiers,'\\.')))
  exts_m<-setdiff(ext_to_import,det_noms)

  #Creation des fichiers manaquants si besoin
  if(length(exts_m)>0){
    
    dz_fichiers<-c(dz_fichiers,exts_m)

    #Récupératation nofiness,annee,mois avec le nom de fichier dans l'archive
    refs<-unlist(stringr::str_split(dz_fichiers[1],'\\.'))[1:3]

    for (ext_m in exts_m){
      file.create(paste0(getOption("dimRactivite.path"),'/',paste(c(refs[1],refs[2],refs[3],ext_m),collapse = '.')))

      
    }
    message(
      "\n",
      'Fichiers créés : ',toString(exts_m)
    )

  }
  
 return(dz_fichiers)


}
#' Généralisation de la fonction adzip de pmeasyr pour l'ensemble d'une remontée (avec plusieurs sites)
#'
#' @param p nouyau pmeasyr 
#' @param fichiers_genrsa liste des fichiers contenue dans le dossier archive
#' @param ext_to_import extension des fichiers à lire et importer
#' @return aucun, dézip des archives en fonction du type de fichier voulus et création de fichier vide pour les types de fichier manquant
#' @export adzipComplet
#'
#' @examples
adzipRemonteee<-function( p, fichiers_genrsa, ext_to_import = NULL ){
  
  imco_files_types = getOption("dimRactivite.fichiers_imco")%>%purrr::flatten_chr()
  
  if(is.null(ext_to_import)){
    
    ext_to_import = getOption("dimRactivite.fichiers_imco")%>%purrr::flatten_chr()
  }
  
  #Fichiers zip (in et out) de la remontée en cours
  files<-fichiers_genrsa%>%dplyr::filter(finess == p$finess,
                                         annee ==  as.numeric(p$annee),
                                         mois == as.numeric(p$mois),
                                         substr(file,nchar(file)-2,nchar(file))=="zip")%>%
    dplyr::group_by(file)%>%dplyr::summarise(types =paste0(type, collapse = ","))
  
  #Dezippage des fichiers archive en fonction de la sélection des types de fichiers (et création d'un fichier vide si manquant)
  dz_files <- adzipComplet(files$file,imco_files_types)
}

#' Vérification de l'exhaustivité du fichier structure
#'
#' @param rum tiblle de type rum
#' @param fichier_structure tibble, structure 
#'
#' @return
#' @export verif_structure
#'
#' @examples
verif_structure<-function(rum,fichier_structure){
  
  manq<-rum%>%filter(! cdurm %in% fichier_structure$cdurm )
  
  if(nrow(manq)==0){
    message(
      "\n",
      "Toutes les UMA sont renseignées dans le fichier structure"
    )
  }else{
    
    message(
      "\n",
      "Les UMA suivantes sont manquantes dans le fichier structure"
    )
    
    print(table(manq$cdurm,format(manq$d8soue,'%Y-%m'),manq$typehosp,manq$nofiness))
    
  }
  
}


#' Création des fichiers .RData contenant l'ensemble des données d'une remontée à partir du tableau d'analyse des fichiers de remontée (analyse_remontee)
#'
#' @param dossiers_remontees tibble en sortie de  analyse_remontee
#' @param fichiers_genrsa tibble en sortie de  scan_path avec analyse de l'ensemble des fichiers du dossier racine

#'
#' @return ecriture sur le disque d'1 .RData de sauvegarde pour chaque remontée
#' @export save_remontees
#'
#' @examples
save_remontees<-function(dossiers_remontees,fichiers_genrsa){
  
  imco_files_types = getOption("dimRactivite.fichiers_imco")%>%purrr::flatten_chr()
  
  for(i in 1:nrow(dossiers_remontees)){
    
    if(dossiers_remontees$RData[i]==0){
      
      #Noyau
      p= pmeasyr::noyau_pmeasyr(finess =dossiers_remontees$finess[i] ,
                                annee = as.numeric(dossiers_remontees$annee[i]),
                                mois = as.numeric(dossiers_remontees$mois[i]),
                                path   = getOption("dimRactivite.path")
      )
      
      #Fichiers zip (in et out) de la remontée en cours
      files<-fichiers_genrsa%>%dplyr::filter(finess == p$finess,
                                             annee ==  as.numeric(p$annee),
                                             mois == as.numeric(p$mois),
                                             substr(file,nchar(file)-2,nchar(file))=="zip")%>%
        dplyr::group_by(file)%>%dplyr::summarise(types =paste0(type, collapse = ","))
      
      #Dezippage des fichiers archive en fonction de la sélection des types de fichiers (et création d'un fichier vide si manquant)
      dz_files<-adzipComplet(files$file,imco_files_types)
      
      #Pour les années incomplètes on décompresse également l'année antérieure si disponible pour calcul des pmct mono uma
      if(as.numeric(dossiers_remontees$mois[i])<12){
        
        pN1= pmeasyr::noyau_pmeasyr(finess =dossiers_remontees$finess[i] ,
                                    annee = as.numeric(dossiers_remontees$annee[i])-1,
                                    mois = 12,
                                    path   = paste0(getOption("dimRactivite.path"))
        )
        
        filesN1<-fichiers_genrsa%>%dplyr::filter(finess == pN1$finess,
                                                 annee ==  pN1$annee,
                                                 mois ==pN1$mois)%>%
          dplyr::group_by(file)%>%dplyr::summarise(types =paste0(type, collapse = ","))
        
        if(nrow(filesN1)>0){
          dz_filesN1<-adzipComplet(filesN1$file,imco_files_types)
        }
      }
      
      dt<-imco(p,persist = T)
      dt<-imco(p,tarifsante = T)      
      
      ###Suppression des fichiers
      pmeasyr::adelete( p, 
                        liste = getOption("dimRactivite.fichiers_imco")[['in']], 
                        type = "in")
      
      pmeasyr::adelete( p, 
                        liste = getOption("dimRactivite.fichiers_imco")[['out']], 
                        type = "out")
      
      
      if(as.numeric(dossiers_remontees$mois[i])<12){
        pmeasyr::adelete( pN1, 
                          liste = getOption("dimRactivite.fichiers_imco")[['in']], 
                          type = "in")
        
        pmeasyr::adelete( pN1, 
                          liste = getOption("dimRactivite.fichiers_imco")[['out']], 
                          type = "out")
      }
      
      
    }
    
    
  }
  
  remontees = update_remontees_dispo()
  return(remontees)
}

#' charge en mémoire les objets définitifs rum,rum_v,rsa,rsa_v,diagnostics, actes, vano
#'
#' @param remontees_sel tibble de type analyse_remontee avec l'ensemble des année à charger
#'
#' @return 
#' @export load_all
#' 
#'
#' @examples 
load_all<- function(remontees_sel){
  

  
    rsa <<- NULL
    rsa_v <<- NULL
    rum <<- NULL
    rum_v <<- NULL
    diagnostics <<- NULL
    actes <<- NULL
    vano <<- NULL
  
  
    for(i in 1:nrow(remontees_sel)){
      
      p= pmeasyr::noyau_pmeasyr(finess =remontees_sel$finess[i] ,
                                annee = remontees_sel$annee[i],
                                mois = remontees_sel$mois[i],
                                path   = getOption("dimRactivite.path")
      )
      
      

      load(paste0(getOption("dimRactivite.path"),'/',p$finess,'.',p$annee,'.',p$mois,'.RData'))
      
      rum <<- bind_rows(rum,get(paste0('rum','_',p$annee,'_',p$mois))[['rum']])
      diagnostics <<- bind_rows(diagnostics,get(paste0('rum','_',p$annee,'_',p$mois))[['diags']])
      actes <<- bind_rows(actes,get(paste0('rum','_',p$annee,'_',p$mois))[['actes']])
      rum_v <<- bind_rows(rum_v,get(paste0('rum_v','_',p$annee,'_',p$mois)))
      rsa <<- bind_rows(rsa,get(paste0('rsa','_',p$annee,'_',p$mois)))
      rsa_v <<- bind_rows(rsa_v,get(paste0('rsa_v','_',p$annee,'_',p$mois)))
      vano <<- bind_rows(vano,get(paste0('vano','_',p$annee,'_',p$mois)))
      
      rm(list=c(paste0('rum','_',p$annee,'_',p$mois),
                paste0('rum_v_',p$annee,'_',p$mois),
                paste0('rsa_',p$annee,'_',p$mois),
                paste0('rsa_v_',p$annee,'_',p$mois),
                paste0('vano_',p$annee,'_',p$mois),
                paste0('pmctmono_',p$annee,'_',p$mois))
      )
      
      
    }
    
    message(
      "\n Importés dans l'espace de travail :",
      "annees : ", toString(paste(unique(rsa$ansor),collapse = '/')) ,
      "rsa, rsa_v  : ",  toString(nrow(rsa)) , " lignes \n",
      "rum, rum_v :", toString(nrow(rum)), " lignes \n"
    )
  
}

#' Import des principaux fichiers de remontées et valorisation des séjours et résumés
#'
#' @param p un noyau pmeasyr
#' @param tarifsante si TRUE utilisation des tarifs de l'année antérieure
#' @param save si TRUE sauvegarde un fichier .RData dans le répertoire des données de remontée
#' @param persist si TRUE renvoie les données
#'
#' @return liste avec principaux fichiers de remontées, et enregistrement sur le disque si besoin
#' @export imco
#'
#' @examples 
#'
imco<-function(p, tarifsante = FALSE, save = TRUE, persist = FALSE, pathm12 = NULL){

  if(tarifsante==TRUE) {
    tarifs      <- referime::get_table('tarifs_mco_ghs') %>% dplyr::distinct(ghs, anseqta, .keep_all = TRUE) %>% dplyr::mutate(anseqta=as.character(as.numeric(anseqta)+1))
    supplements <- referime::get_table('tarifs_mco_supplements') %>% dplyr::mutate(anseqta=as.character(as.numeric(anseqta)+1))
    suffixe = "tarifs_anterieurs_"
  } else {
    tarifs      <- referime::get_table('tarifs_mco_ghs') %>% dplyr::distinct(ghs, anseqta, .keep_all = TRUE)
    supplements <- referime::get_table('tarifs_mco_supplements')
    suffixe = ""
  }

  #Pour le cacul des pmct mono rum on préfère toujours utiliser les 12 derniers mois.
  #Si l'import concerne un mois autre que décembre, on importe également si elles sont dispnibles les données M12 de l'année antérieure
  #deb et fin ne semblent pas utilisés:

  if(p$mois !=12){
    deb  = p$annee -1
  }else{
    deb = p$annee
  }
  
  fin = p$annee

  #On prévoit de modifier le noyau pour l'import
  

  rsa_en_cours = NULL
  rum_en_cours = NULL

  for(a in deb:fin){
  
    p_import = p
    p_import$annee = a
    
    #On change l'année et le mois du noyau d'import en fonction du contexte
    if(p$annee != a){
      p_import$mois = 12
    }
    message(
      "\n",
      "Imports fichiers remontée: ",toString(c(p_import$finess,p_import$annee,p_import$mois)),
      "\n"
    )
    #Imports
    rsa <- pmeasyr::irsa(p_import,typi = 3 ,tolower_names = T)
    vrsa <- pmeasyr::vvr_rsa(p_import,tolower_names = T)
    vano <- pmeasyr::vvr_ano_mco(p_import,tolower_names = T)
    porg <- pmeasyr::ipo(p_import,tolower_names = T)
    diap <- pmeasyr::idiap(p_import,tolower_names = T)
    ium <- pmeasyr::iium(p_import,tolower_names = T)
    pie <- pmeasyr::ipie(p_import,tolower_names = T)
    tra <- pmeasyr::itra(p_import ,  tolower_names = T )
    rum <- pmeasyr::irum(p_import, typi = 4 ,  tolower_names = T )

    #Transformation des données

    #Intégration du tra
    rsa$rsa =  pmeasyr::inner_tra( rsa$rsa, tra )
    
    rum$rum =  pmeasyr::inner_tra( rum$rum, tra %>% dplyr::select(cle_rsa,nas,dtsort), sel=2)%>%
      mutate(ansor = format(dtsort,'%Y'),moissor = format(dtsort,'%m'))%>%
      select(-dtsort)

    #Intégration du type d'UMA au fihcier IUM
    ium = left_join(ium %>% dplyr::mutate(temp=as.integer(annee)-as.integer(substr(dteaut,1,4))) %>%
                      dplyr::arrange(noum,temp) %>% dplyr::filter(temp>=0, !duplicated(noum))  %>%
                      dplyr::select(-temp), nomenclature_uma %>%dplyr::select(typeaut, libelle_typeaut,
                                                                             discipline ))


    #Intégration des type d'UMA au fichier rum
    rum$rum = left_join(rum$rum,ium%>%dplyr::select(noum,typeaut,typehosp,libelle_typeaut,
                                                    discipline),by=c("cdurm" = "noum"))
    #Transformation des diagnostics du rum
    rum <- pmeasyr::tdiag(rum)

    #Valorisation des séjours
    rsa_v <- pmeasyr::vvr_ghs_supp(rsa = vrsa,
                                   tarifs = tarifs%>%mutate(ghs = as.character(ghs), anseqta =  as.character(anseqta)),
                                   supplements =  supplements %>% mutate(anseqta = as.character(anseqta)),
                                   ano = vano,
                                   porg = porg,
                                   diap = diap,
                                   pie = pie,
                                   bee = FALSE)

    rsa_v <- pmeasyr::inner_tra( rsa_v , tra %>% dplyr::select(cle_rsa,nas,dtsort), sel=2)%>%
      mutate(ansor = format(dtsort,'%Y'),moissor = format(dtsort,'%m'))%>%select(-dtsort)%>%
      mutate(nofiness = p_import$nofiness)

    rsa_v2 <- dplyr::left_join(rsa$rsa, dplyr::distinct( rsa_v, cle_rsa, .keep_all = TRUE) )

    

    vano = vano%>%dplyr::select(names(vano)[ ! grepl('^cr',names(vano))])%>%
             dplyr::mutate( ansor = as.character(a) )

    #Objets temporaires à valoriser pour rum et rsa avec année antérieure
    rsa_en_cours = dplyr::bind_rows(rsa_v2, rsa_en_cours)
    rum_en_cours = dplyr::bind_rows(rum$rum, rum_en_cours)

  }

  #Etape 2 : valorisation des rum par séjour avec répartition des recettes par service

  #pmct mono sur les 12 mois antétieurs
  pmctmono =  pmct_mono_uma(rsa_en_cours, rum_en_cours, p$annee, p$mois)

  #Valorisation des rum de la dernière année
  rum_v <- vvr_rum_repa(rsa_v2, rum$rum, pmctmono)

  #SAUVEGARDE OBJET DERNIERE ANNEE

  i = paste0(p$annee,'_',p$mois)
  
  message(
    "\n",
    "Nb séjours importés pour l'année ",    toString(unique(rsa$rsa$ansor)) , " : ",toString(nrow(rsa$rsa)),
    "\n"
  )

  if(!tarifsante){
    
    assign( paste0('rum_v_',suffixe,i), rum_v )
    assign( paste0('rsa_v_',suffixe,i), rsa_v )
    assign( paste0('rsa_',i), rsa$rsa )
    assign( paste0('rum_',i), rum)
    assign( paste0('vano_',i), vano )
    assign( paste0('porg_',i), porg )
    assign( paste0('diap_',i), diap )
    assign( paste0('pie_',i), pie )
    assign( paste0('pmctmono_',i), pmctmono )


    if(save){
    save( list = c(paste0('rum_',i),
                   paste0('rum_v_',i),
                   paste0('rsa_',i),
                   paste0('rsa_v_',i),
                   paste0('vano_',i),
                   paste0('pmctmono_',i)),
          file = paste0(p$path,"/",p$finess,".",p$annee,".",p$mois,".RData")
    )
    }
    
    if(persist){
      return(list( 'rsa' = rsa$rsa,
                   'rsa_v' = rsa_v,
                   'rum' = rum,
                   'rum_v' = rum_v,
                   'vano'= vano,
                   'tra'= tra,
                   'pmctmono' = pmctmono,
                   'pie' = pie,
                   'diap' = diap,
                   'porg' = porg  )
      )
    } 
    

  } else {

    assign( paste0('rum_v_',suffixe,i), rum_v )
    assign( paste0('rsa_v_',suffixe,i), rsa_v )

    
    if(save){
    save( list = c(paste0('rum_v_',suffixe,i),
                   paste0('rsa_v_',suffixe,i)),
          file = paste0(p$path,"/",p$finess,".",p$annee,".",p$mois,".",substr(suffixe,1,nchar(suffixe)-1),".RData")
    )
    }
    
    
    if(persist){
      return(list( 'rsa_v' = rsa_v,
                   'rum_v' = rum_v ) )
    } 

  }

}




