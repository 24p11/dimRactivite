#' Selection de données en fonction de critère temporels et de structure
#'  Applicable sur des objets en sortie de pmeasyr
#' @param df tibble, un tibble en sortie de pmeasyr
#' @param ref chaine de caractères, la variable de référence qui sera utilisée pour la selection temporel
#'    - si ref = ansor : les variables pmeasyr ansor et moisor sont utilisé pour la sélection avec les paramètres m et a
#'    par exemple pour des objets de type rum ou rsa
#' @param m vecteur numérique, un vecteur de nombre entiers indiquant les mois à selectionner
#' @param a vecteur numérique, un vecteur de nombre entiers indiquant les années à selectionner
#' @param val chaine de caractères, variable géographique qui sera choisie pour la selection (ex : service, pole,...).
#'   Si NULL pas de critères géographiques pour la sélection
#' @param niveau chaine de caractères, valeur de la variable géographique à sélectionner
#' @param regles logique, utilisation des règles locales T
#'
#' @return tibble, un tibble contenant la sélection selon les critères indiqués
#' @export get_data
#'
#' @examples
get_data<-function( DF, ref = 'ansor', m, a, val = NULL, niveau = NULL, opt = T ){


  if( ref == 'ansor' ){

    DF <- DF  %>% filter ( as.numeric(ansor) %in% a,
                           as.numeric(moissor) %in% m )%>%
      mutate(ansor = factor(ansor,levels = min(a):max(a)))

  }else{

    DF <- DF  %>% filter ( as.numeric( format( !!sym(ref), '%Y') ) %in% a,
                           as.numeric( format( !!sym(ref), '%m') ) %in% m )

  }


  if( !is.null(niveau) & !is.null(val) ){

    DF <- DF %>% filter ( !!sym(niveau) %in% val )

  }


  if( opt == T ){

    DF <- options_locales( DF, val, niveau )

  }
  
  


  return(DF)
}


#source(paste(PathFonctions,"GetData.R",sep=""))
get_data_glissant<-function( DF, ref = 'ansor', m, a, val = NULL, niveau = NULL, opt = T ){
  #(Data,DateRef,Month,Years,val=NULL,niveau=NULL){
  
  if( ref == 'ansor' ){
    
    DF <- DF  %>% dplyr::mutate(ansor = if_else( as.numeric(moissor) > max(m), 
                                          as.character( as.numeric(ansor) + 1 ),
                                          ansor ) ) %>%
      dplyr::filter( as.numeric(ansor) %in% a ) %>%
      dplyr::mutate(ansor = factor( ansor, levels = min(a):max(a) ) )
  
  }else{
    
    DF <- DF  %>% dplyr::mutate(anref = as.numeric( format( !!sym(ref), '%Y')),
                        moisref = as.numeric( format( !!sym(ref), '%m')))%>%
      dplyr::mutate(anref = if_else( as.numeric(moisref) > max(m), 
                               as.numeric(anref) + 1 , anref ) ) %>%
      dplyr::filter( as.numeric(anref) %in% a )
    
  #  DF <- DF  %>% filter ( as.numeric( format( !!sym(ref), '%Y') ) %in% a,
  #                         as.numeric( format( !!sym(ref), '%m') ) %in% m )
  #  
  }
  
  #if(is.null(niveau)|is.null(val)){
    
  #  DF<-DF[
  #    DF[,DateRef]>=as.Date(paste(min(Years)-1,'-',Month+1,'-','01',sep=''))&
  #      DF[,DateRef]<as.Date(paste(max(Years),'-',Month+1,'-','01',sep='')),
  #    ]
    
  #}
  
  if(!is.null(niveau)&!is.null(val)){
    
   DF <- DF%>%filter(!!sym(niveau) == val)
   
  }
  
  #Data$A<-as.numeric(format(Data[,DateRef],'%Y'))
  #Data$m<-factor(as.numeric(format(Data[,DateRef],'%m')))
  #Data$A[which(as.numeric(format(Data[,DateRef],'%m'))>Month)]<-as.numeric(format(Data[which(as.numeric(format(Data[,DateRef],'%m'))>Month),DateRef],'%Y'))+1
  #Data$A<-factor(Data$A,levels=Years)
  #Data<-ReglesLocalesDataSet(Data,val,niveau)
  
  #print(paste(niveau,val,':',dim(Data)[1],'-',dim(Data)[2]))
    
  if( opt == T ){
    
    DF <- options_locales( DF, val, niveau )
      
  }
    
  return(DF)
}

#' Mise en oeuvre des options locales pour le décompte des séjours dans les données d'activité
#'
#' La fonction ajoute une variable doublon ( TRUE / FALSE ) à un objet de type rum/rsa
#' qui sera utilisée dans les tableaux de bord pour dédoublonner les séjours au moment de leur décompte
#'
#' @param df tibble de type rum/rsa, utilisé entre autres en entrée de \code{\link{get_data}}
#' @param val chaine de caractères, variable géographique qui sera choisie pour la selection (ex : service, pole,...).
#' @param niveau chaine de caractères, variable géographique qui sera choisie pour la selection (ex : service, pole,...).
#'
#' @return tibble de type rum/rsa, le tibble local auquel a été ajoutée.
#' @export options_locales
#'
#'
#' @examples
#'
#'
options_locales<-function(DF,val=NULL,niveau=NULL){

  if( !is.null( getOption("dimRactivite.services_exclus") ) & "service" %in% names(DF) ){

    DF <- DF %>% dplyr::filter( !service %in% getOption("dimRactivite.services_exclus") )
  }
  
  DF <- DF %>% dplyr::mutate( doublon = 1 ) 

  #Dédoublonnage en fonction des paramètres optionnels
  #On cherche dans val ou niveau la valeur d'une variable de déboublonnage
  if( !is.null(niveau) & !is.null( getOption("dimRactivite.gestion_doublons_rum") ) ){

    for(i in names( getOption("dimRactivite.gestion_doublons_rum") ) ){

      n = getOption("dimRactivite.gestion_doublons_rum")[[i]]

      if( niveau == i | val == i  ){

        DF <- DF %>%  group_by( nofiness, ansor, cle_rsa, !!sym(n) ) %>%
                      arrange( nofiness,ansor, cle_rsa, !!sym(n), d8eeue ) %>%
                      mutate( nb_rum = n(),
                              doublon = if_else( row_number() == 1, 1, 0 ) ) %>%
                      ungroup()
      }


    }

  }
  
  if( is.null(niveau) ){
    
    DF <- DF %>%  group_by( nofiness, ansor, cle_rsa ) %>%
      arrange( nofiness,ansor, cle_rsa, d8eeue ) %>%
      mutate( nb_rum = n(),
              doublon = if_else( row_number() == 1, 1, 0 ) ) %>%
      ungroup()
    
  }

  return(DF)

}


#' Génère la liste des indicateurs à calculer en fonction du fichier de paramètre
#'
#' @param nom chaine de catactères, nom du tableau de bord
#' @param val chaine de caractères, variable géographique qui sera choisie pour la selection (ex : service, pole,...).
#' @param df tiblle de type paramètres des tableaux de bord
#'
#' @return vecteur de nom d'indicateurs
#' @export get_indicateurs
#'
#' @examples
#'
get_indicateurs<-function(nom,val,df= references){

  df<- df%>% filter(grepl(nom,tdb))

  #Si pas de niveau identifie attribution des indicateurs par defaut=GH

  if(!prep_string(val)%in%names(df))val='GH'
  indicateurs=df$var[which(df[,prep_string(val)]=='o')]

  return(indicateurs)
}

#' Suppression des caractères spéciaux et accentués
#'
#' @param string chaine de caractères
#'
#' @return 
#' @export prep_string
#'
#' @examples
#'  
#'  
prep_string<-function(string){

  string <- gsub('[^[:alnum:]]','',string)
  string <- gsub("['`^~\"]", " ", string)
  string <- iconv(string, to="ASCII//TRANSLIT//IGNORE")
  string <- gsub("['`^~\"]", "", string)
  return(string)

}




#' Nombbre d'élément unique dans un vecteur
#'
#' @param x vecteur
#'
#' @return integer, nombre d'élément unique
#' @export nb_unique
#'
#' @examples
#'
nb_unique<-function(x){
  return(length(unique(x)))
}



#' Fonction de tableaux de bord
#' Mise en forme du tableau -> remplacement des lignes vides par '-'
#'
#' @param tab tableau résultats de table/tapply un tableau de bord
#'
#' @return tableau modifié
#' @export prep_tb
#'
#' @examples
#'
prep_tb<-function(tab){
  tab[which(is.na(tab)|tab==0|tab==''|tab=='NaN')]<-'-'
  return(tab)
}

#' Calcul de la différence et du pourcentage de différence
#'  entre les deux dernières colonnes d'un tableau de bord
#'
#' @param t  tableau résultats de table/tapply un tableau de bord
#'
#' @return tableau modifié
#' @export diff_tb
#'
#' @examples
diff_tb<-function(t){
  tab<-t
  tab$Diff<-round(as.numeric(tab[,ncol(t)])-as.numeric(tab[,ncol(t)-1]))
  tab$P.Diff<-round(	(
    (as.numeric(tab[,ncol(t)])-as.numeric(tab[,ncol(t)-1]))
    *100)/
      as.numeric(tab[,ncol(t)-1]),
    digit=1)
  tab$P.Diff[which(tab$D.Diff=='Inf')]<-'-'
  return(tab)
}

#' Calcul de l'index de performance SERVICE à partir d'un objet de type séjours rum
#'  L'objet en entrée doit avoir une variable service
#' @param df  objet de type séjours (rum/rsa)
#'
#' @return double, index de performance
#' @export IP_SERVICE
#'
#' @examples
IP_SERVICE<-function(df){
  
  df%>%filter(noghs!=9999,duree>0,dms_n>0)

  NUMERATEUR<-sum(df$dureesejpart,na.rm=T)
  DENOMINATEUR<-sum(df$dms_n*df$coeftime,na.rm=T)
  IP  <-round(NUMERATEUR/DENOMINATEUR,digit=2)
  return(IP)
}
# TODO Merge des 2 fonction de calcul de l'IP en utilisant les règles locales et une variable 'niveau'
#' Calcul de l'index de performance à partir d'un objet de type séjours rum/rsa
#'  Attention pas de dédoublonnage
#' @param df
#'
#' @return 
#' @export IP_SEJOUR
#'
#' @examples
IP_SEJOUR<-function(df){
  df<-df%>%filter(noghs!=9999,duree>0,dms_n>0)%>%
    distinct(ansor,nofiness,cle_rsa,.keep_all = T)

  NUMERATEUR<-sum(df$duree,na.rm=T)
  DENOMINATEUR<-sum(df$dms_n,na.rm=T)
  IP  <-round(NUMERATEUR/DENOMINATEUR,digit=2)
  return(IP)
}

#' Utilisé pour la tableaux de bord généraux
#' Reformatage d'objets en sortie de table/tapply selon l'ordonnancement décidé dans le fichier de structure
#'  Fusion des tableaux
#'        - Nom du niveau de structure (ex nom de service ou de pole)
#'        - Variable de temporalité (par exemple l'année)
#'        - C / P
#' @param tdb list d'objets en sortie de table/tapply avec 3 dimenssions

#'
#' @return concaténation et restructuration des tableaux de données
#' @export order_by_structure
#'
#' @examples
order_by_structure<-function( tdb, structure ){
  
  
  services = unique( structure%>%filter(service%in%dimnames(tdb$services)[[1]])%>%select(service) )$service
  poles = unique(  structure%>%filter(pole%in%dimnames(tdb$poles)[[1]])%>%select(pole) )$pole

  tdb$hc <- rbind( NA, "Groupe Hospitalier" = NA, tdb$hopitaux[,,"C"], "Total GH" = tdb$gh['C',] )
  tdb$hp <- rbind( NA, "Groupe Hospitalier" = NA, tdb$hopitaux[,,"P"], "Total GH" = tdb$gh['P',] )

  for (p in poles){

    serv =services[services%in%structure$service[structure$pole==p]&
                     services%in%row.names(tdb$services[,,"C"])]

    tdb$hc<-rbind(tdb$hc,NA,NA)
    row.names(tdb$hc)[nrow(tdb$hc)]<-p

    tdb$hc = rbind(tdb$hc,tdb$services[serv,,"C"], "Total pole" = tdb$poles[p,,"C"])

    if(length(serv)==1){
      row.names(tdb$hc)[nrow(tdb$hc)-1]<-serv
    }

    tdb$hp<-rbind(tdb$hp,NA,NA)
    row.names(tdb$hp)[nrow(tdb$hp)]<-p

    tdb$hp = rbind(tdb$hp,tdb$services[serv,,"P"], "Total pole" = tdb$pole[p,,"P"])

    if(length(serv)==1){
      row.names(tdb$hp)[nrow(tdb$hp)-1]<-serv
    }


  }
  return(tdb)

}



#' Calcul de la différence et du pourcentage
#' des deux dernières colonnes d'un tableau de bord
#'
#' @param df
#'
#' @return
#' @export get_diff
#'
#' @examples
get_diff<-function(df){
  diff<-df[,ncol(df)]-df[,ncol(df)-1]
  df<-cbind(df,diff)
  p_diff<-round(diff*100/df[,ncol(df)-2],digit=1)
  df<-cbind(df,p_diff)
  return(df)
}

#' un ensemble de fonction permettant d'automatiser les imports pmeasyr et d'effectuer des opérations
#' de transformation et de sélection des séjours
#'
#' Warning: utilisation du package pmeasyr

#' Selection des diagnostics de cancerologie pour un objet pmeasyr diagnostics
#'
#' @param df un tibble de type diagnostic
#'
#' @return une sélection de df pour la cancerologie
#' @export selection_cancer_diag
#' @section Warning: utilise la liste inca_cancero
#' @examples

selection_cancer_diag<-function( diagnostics ){
  
  dplyr::inner_join( diagnostics, dplyr::as_tibble( inca_cancero ), by = c("diag" = "code") )%>%
    dplyr::mutate( nda = substr(nas,1,9) )%>%
    dplyr::distinct( nda, diag, position, .keep_all= TRUE )
  
  
}

#' Attribution d'un diagnostic de cancer par patient pour un objet de type diagnotics en sortie de \code{\link{selection_cancer_diag}}
#'
#' @param df  un tibble de type diagnostic en sortie de \code{\link{selection_cancer_diag}}
#'
#' @return un tibble de type diagnostic comprenant une ligne par patient selectionne comme etant le diagnostic de cancer unique pour l'analyse de l'activite
#' @export selection_cancer_pat
#' @section Warning: utilise lae resultat de la fonction  \code{\link{selection_cancer_diag}}
#' @examples
#'

selection_cancer_pat<-function( df, df_ano ){
  
  df%>%dplyr::mutate(score_confiance_diag_cancer = dplyr::if_else(position==5,1,10))%>%
    dplyr::inner_join(., df_ano )%>%
    dplyr::group_by(noanon,diag,appareil,organe)%>%
    dplyr::summarise(score_confiance_diag_cancer = sum(score_confiance_diag_cancer))%>%
    dplyr::ungroup()%>%
    dplyr::group_by(noanon)%>%
    dplyr::filter(score_confiance_diag_cancer == max(score_confiance_diag_cancer))%>%
    dplyr::distinct(noanon, .keep_all= TRUE)
}


#' Attribution du type de chirurgie M4 pour les objets de type rsa
#' Les rsa de cancerologie sont préalablerment selectionnés en utilisant les fonctions \code{\link{selection_cancer_diag}} et
#' \code{\link{selection_cancer_pat}}. Les objets en sortie de ces deux fonctions sont concaténés avec un objet rsa et mis en entrée de
#' la fonction pour attribution du type M4 de cancer
#'
#' @param df  un tibble de type rsa concaténé avec un tibble diagnostic en sortie de \code{\link{selection_cancer_pat}}
#'
#' @return un tibble de type rsa
#' @export attribution_type_M4
#'
#' @examples
#'
attribution_type_M4<-function(df){
  
  df<-df%>% dplyr::mutate(type_chirurgie = NA)
  for (nom_liste in c("chirurge_cancers_digestif","chirurgie_cancers_gynecologique",
                      "chirurgie_cancers_orl_maxilo","chirurgie_cancers_sein","chirurgie_cancers_thorax",
                      "chirurgie_cancers_urologie","cancers_snc",
                      "cancers_os_tissus_mou","chirurgie_cancers_thyroide","cancers_de_la_peau")){
    
    df <- df %>% dplyr::mutate(type_chirurgie = ifelse(dp %in% get(nom_liste)$code & gptype == 'C', nom_liste, type_chirurgie))
    
  }
  
  return(df)
}

#' Attribution du statut nouveau patient pour les objets de type rsa (utilisé pour la cancérologie)
#' Les rsa de cancerologie sont préalablerment selectionnés en utilisant les fonctions \code{\link{selection_cancer_diag}} et
#' \code{\link{selection_cancer_pat}}. Les objets en sortie de ces deux fonctions sont concaténés avec un objet rsa et mis en entrée de
#'
#' @param df  un tibble de type rsa concaténé avec un tibble diagnostic en sortie de \code{\link{selection_cancer_pat}}
#'
#' @return un tibble de type rsa
#' @export attribution_statut_nx_patient
#'
#' @examples
#'
attribution_statut_nx_patient<-function(df){
  
  ansor_deb<-max(df$ansor); ansor_fin<-min(df$ansor)
  
  df<-df%>%dplyr::mutate(nx_pat = 'N')
  
  for(a in ansor_deb:ansor_fin){
    tmp <- df %>% dplyr::filter(ansor %in% (a-3):(a-1) )%>% dplyr::select(noanon) %>% purrr::flatten_chr() %>% unique()
    df <- df%>%dplyr::mutate(nx_pat = ifelse(! noanon %in% tmp & ansor == a,'O',nx_pat))
    
  }
  
  return(df)
}


#' cmd et libellé
#' retourne un tibble avec la liste des CMD et leur libellé
#' @return tibble cmd
#' @export get_cmd
#'
#' @examples get_cmd()
get_cmd<-function(){
  
  cmd<-rbind(c('01',"affections du système nerveux"),
             c("02","Affections de l'oeil"),
             c("03","Affections des oreilles, du nez, de la gorge, de la bouche et des dents"),
             c("04","Affections de l'appareil respiratoire"),
             c("05","Affections de l'appareil circulatoire"),
             c("06","Affections du tube digestif"),
             c("07","Affections du système hépatobiliaire et du pancréas"),
             c("08","Affections et traumatismes de l'appareil musculosquelettique et du tissu conjonctif"),
             c("09","Affections de la peau, des tissus sous-cutanés et des seins"),
             c("10","Affections endocriniennes, métaboliques et nutritionnelles"),
             c("11","Affections du rein et des voies urinaires"),
             c("12","Affections de l'appareil génital masculin"),
             c("13","Affections de l'appareil génital féminin"),
             c("14","Grossesses pathologiques, accouchements et affections du post-partum"),
             c("15","Nouveau-nés, prématurés et affections de la période périnatale"),
             c("16","Affections du sang et des organes hématopoïétiques"),
             c("17","Affections myéloprolifératives et tumeurs de siège imprécis ou diffus"),
             c("18","Maladies infectieuses et parasitaires"),
             c("19","Maladies et troubles mentaux"),
             c("20","Troubles mentaux organiques liés à l'absorption de drogues ou induits par celles-ci"),
             c("21","Traumatismes, allergies et empoisonnements"),
             c("22","Brûlures"),
             c("23","Facteurs influant sur l'état de santé et autres motifs de recours aux services de santé"),
             c("25","Maladies dues à une infection par le VIH"),
             c("26","Traumatismes multiples graves"),
             c("27","Transplantations d'organes"),
             c("28","Séances"),
             c("90","Erreurs et autres séjours inclassables"))
  dimnames(cmd)[[2]]<-c('cmd','libelle_cmd')
  cmd<-as_tibble(cmd)
  
  return(cmd)
  
}

#' racines de ghm et libellé
#' retourne un tibble avec la liste des CMD et leur libellé
#' @return tibble racines
#' @export get_racines
#'
#' @examples get_racines()
get_racines<-function(){
  
  tarifs_mco_ghs <- referime::get_table("tarifs_mco_ghs")
  
  t_replace = ", niveau 1|en ambulatoire|, très courte durée|, sans complication significative|, sans problème significatif"
  
  racines<-tarifs_mco_ghs%>%
    dplyr::mutate( racine = substr(ghm,1,5) ) %>% arrange( ghs ) %>% distinct (racine, .keep_all = T ) %>%
    dplyr::rename( libelle = libelle_ghm ) %>%
    dplyr::mutate( libelle = str_replace(libelle, t_replace, '') )%>%
    dplyr::select( racine, libelle )
  
  return(racines)
  
}
