is.date <- function(x) inherits(x, 'Date')

#' Selection de données en fonction de critère temporels et géographique (lieu d'hopsitalisation)
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
#' @export
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
#' @export
#'
#'
#' @examples
#'
#'
options_locales<-function(DF,val=NULL,niveau=NULL){

  if( !is.null( getOption("dimRactivite.services_exclus") ) & "service" %in% names(df) ){

    DF <- DF %>% dplyr::filter( !service %in% getOption("dimRactivite.services_exclus") )
  }
  
  DF <- DF %>% dplyr::group_by( nofiness, ansor, cle_rsa, .keep_all = T ) %>% dplyr::mutate( doublon = 1 ) %>% ungroup()

  #Dédoublonnage en fonction des paramètres optionnels
  #On cherche dans val ou niveau la valeur d'une variable de déboublonnage
  if( !is.null(niveau) & !is.null( getOption("dimRactivite.gestion_doublons_rum") ) ){

    for(i in names( getOption("dimRactivite.gestion_doublons_rum") ) ){

      n = getOption("dimRactivite.gestion_doublons_rum")[[i]]

      if( niveau == i | val == i  ){

        DF <- DF %>%  group_by( nofiness, ansor, cle_rsa, !!sym(n) )%>%
                      arrange( nofiness,ansor, cle_rsa, !!sym(n), d8eeue ) %>%
                      mutate( nb_rum = n(),
                              doublon = if_else( row_number() == 1, 1, 0 ) )%>%
                      ungroup()
      }


    }

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
#' @export
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
#' @param text chaine de caractères
#'
#' @return
#' @export text
#'
#' @examples
#'  prep_string( 'rtzei zef ef $$$ eé)&')
#'  [1] "rtzeizefefee"
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
#' @export
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
#' @export
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
#' @export
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
#' @export
#'
#' @examples
IP_SERVICE<-function(df){
  df%>%filter(noghs!=9999,duree>0,dms_n>0)%>%
    distinct( nofiness,ansor,cle_rsa,service, .keep_all= TRUE)

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
#' @export
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
#' @param tdb list d'objets en sortie de table/tapply avec 3 dimenssions
#'
#'        - Nom du niveau de structure (ex nom de service ou de pole)
#'        - Variable de temporalité (par exemple l'année)
#'        - C / P
#'
#' @return concaténation et restructuration des tableaux de données
#' @export
#'
#' @examples
order_by_structure<-function( tdb, structure ){
  
  
  services = unique(fichier_structure$service)
  poles = unique(fichier_structure$pole)

  tdb$hc <- rbind( NA, "Groupe Hospitalier" = NA, tdb$hopitaux[,,"C"], "Total GH" = tdb$gh['C',] )
  tdb$hp <- rbind( NA, "Groupe Hospitalier" = NA, tdb$hopitaux[,,"P"], "Total GH" = tdb$gh['P',] )

  for (p in poles){

    serv =services[services%in%fichier_structure$service[fichier_structure$pole==p]&
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
#' @export
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
#' @export
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
#' @export
#' @section Warning: utilise lae resultat de la fonction  \code{\link{selection_cancer_diag}}
#' @examples
#'

selection_cancer_pat<-function( df, df_ano ){
  
  df%>%mutate(score_confiance_diag_cancer = dplyr::if_else(position==5,1,10))%>%
    inner_join(., df_ano )%>%
    group_by(noanon,diag,appareil,organe)%>%
    summarise(score_confiance_diag_cancer = sum(score_confiance_diag_cancer))%>%
    ungroup()%>%
    group_by(noanon)%>%
    filter(score_confiance_diag_cancer == max(score_confiance_diag_cancer))%>%
    distinct(noanon, .keep_all= TRUE)
}


#' Attribution du type de chirurgie M4 pour les objets de type rsa
#' Les rsa de cancerologie sont préalablerment selectionnés en utilisant les fonctions \code{\link{selection_cancer_diag}} et
#' \code{\link{selection_cancer_pat}}. Les objets en sortie de ces deux fonctions sont concaténés avec un objet rsa et mis en entrée de
#' la fonction pour attribution du type M4 de cancer
#'
#' @param df  un tibble de type rsa concaténé avec un tibble diagnostic en sortie de \code{\link{selection_cancer_pat}}
#'
#' @return un tibble de type rsa
#' @export
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
#' @export
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

