
options(shiny.maxRequestSize=10000*1024^2) 
infofiles <<- NULL
nsites <<- NULL
hopitaux <<- NULL
annee <<- NULL
mois <<- NULL
pmsidata <<- NULL
pmsidataant <<- NULL 
addpie <<- NULL
addporg <<- NULL
adddiap <<- NULL
do.call(file.remove, list(list.files("../temp/", full.names = TRUE)))

function(input, output, session) { 
  
  check <- reactiveValues(ok = NULL)
  
  url <- a("à cette page", href = "http://sls-dbim02.sls.aphp.fr:3838/4115983/servyce/")
  output$urlservyce <- renderUI({tagList("Une interface d'analyse des données par service est disponible ", url)})
  
  output$readme1 <- renderUI({HTML(paste("", "", "Le chargement des données doit être effectué ainsi:", "1) charger les archives genrsa (les fichiers pie, porg ou diap manquants seront créés) selon leur periode de remontée dans l'interface de gauche (plusieurs finess acceptés) et attendre la copie des fichiers,", "2) cliquer sur -Imports et calculs- puis attendre la fin des traitements,", "3) lorsque le traitement est fini, cliquer sur telechargement pour sauvegarder les fichiers au format RData", "", "","Le fichier des structures de l'hopital doit etre un fichier excel contenant un header et des colonnes comme suit:", "1) finess,", "2) libellé de l'hopital,", "3-4-5) 3 colonnes pour les codes des unités (la 3ème doit comporter les unités PMSI),", "6) le libellé des unités PMSI,", "7) libellé des services,", "8) libellé des regroupements de 1er niveau,", "9) libellé des regroupements de 2ème niveau", "", "", sep="<br/>"))})
  
  load <- reactive( {

    courant <- input$courant ; consol <- input$consol ; struct <- input$struct
    if (is.null(courant) | is.null(consol) | is.null(struct)) {return(NULL)}
    
    print(courant)
    
    #Copier ou extraire les fichier 
    for(i in 1:length(courant$datapath)) {
      if(tools::file_ext(courant[[i,'datapath']])=="zip") {
        unzip(courant[[i,'datapath']], junkpaths = TRUE, exdir = "../temp/")
      } else {
        file.copy(courant[[i,'datapath']], paste0("../temp/", courant[[i,'name']]))
      }
    }

    for(i in 1:length(consol$datapath)) {
      if(tools::file_ext(consol[[i,'datapath']])=="zip") {
        unzip(consol[[i,'datapath']], junkpaths = TRUE, exdir = "../temp/")
      } else {
        file.copy(consol[[i,'datapath']], paste0("../temp/", consol[[i,'name']]))
      }
    }
    
    #Extraction des informations à partir des fichiers chargés NB: ICI INTRODUIRE DES VERIFICATIONS SUR LES FICHIERS CHARGES !!!
    infofiles <<- tibble::enframe(name=NULL, list.files("../temp/")) %>% dplyr::rename(name=value) %>% separate(., name, into = c("finess", "annee", "mois"), sep = "\\.", remove = FALSE) %>% dplyr::mutate(ext=tools::file_ext(name))
    nsites <<- n_distinct(infofiles$finess)
    hopitaux <<- unique(infofiles$finess)
    annee <<- max(as.numeric(infofiles$annee))
    mois <<- min(as.numeric(infofiles$mois))
    
    #Ne copier le fichier structure que à la fin, pour avoir l'objet infofiles correct
    file.copy(struct[[1,'datapath']], paste0("../temp/", struct[[1,'name']]))
    #Formatage du fichier structure
    # fichier_structure <<- readxl::read_excel(paste0("../temp/", input$struct[[1,'name']]), col_types = c( "text" , "text" , "text" , "text" ,"text" , "text" , "text" , "text" , "text" ), col_names = c('nofiness', 'hopital', 'cdurm', 'uma_locale', 'uma_locale2',  'libelle_um', 'service', 'regroupement1', 'pole'), skip = 1  )

    print(infofiles)
    
    #Créer les fichiers manquants pour pie/porg/diap seulement:
    for(n in infofiles[infofiles$ext=="rsa",'name']) {
      if(nrow(dplyr::filter(infofiles, name==stringr::str_replace(n, ".rsa", ".pie"))) == 0) { addpie <<- stringr::str_replace(n, ".rsa", ".pie"); file.create(paste0("../temp/", addpie)) ; print("fichier(s) .pie créé")}
      if(nrow(dplyr::filter(infofiles, name==stringr::str_replace(n, ".rsa", ".porg"))) == 0) { addporg <<- stringr::str_replace(n, ".rsa", ".porg"); file.create(paste0("../temp/", addporg)) ; print("fichier(s) .porg créé") }
      if(nrow(dplyr::filter(infofiles, name==stringr::str_replace(n, ".rsa", ".diap"))) == 0) { adddiap <<- stringr::str_replace(n, ".rsa", ".diap"); file.create(paste0("../temp/", adddiap)) ; print("fichier(s) .diap créé") }
    }

    check$ok <- 0
    
  } )  
  
  output$readme2 <- renderDT( {
    courant <- input$courant ; consol <- input$consol ; struct <- input$struct
    if (is.null(courant) | is.null(consol) | is.null(struct)) {
      return(NULL)
    } else {
      withProgress( message = "Chargement", value = 0, {
        incProgress(0.3, detail = paste("copie des fichiers"))
        load()
        incProgress(0.6, detail = paste("production du tableau d'import"))
        tibble::enframe(name=NULL, list.files("../temp/")) %>% dplyr::rename(name=value) %>% dplyr::select(name) %>% dplyr::mutate(miss=ifelse(name %in% c(addpie, addporg, adddiap), "manquant: fichier factice créé", "présent: fichier régulièrement importé")) %>% dplyr::arrange(miss) %>% dplyr::rename("fichiers manquants"=miss)
      })
    }
  }, caption="Fichiers chargés", rownames=FALSE, options = list(autoWidth = TRUE)
  )
  
  
  observeEvent( input$launch, {
    
    courant <- input$courant ; consol <- input$consol ; struct <- input$struct
    if (is.null(courant) | is.null(consol) | is.null(struct)) {
      return(NULL)
    } else {
      
    withProgress( message = "Chargement", value = 0, {

      incProgress(0.2, detail = paste("calcul des valorisations"))
      
      rsa_full <- NULL
      rsa_v_full <- NULL
      rum_full <- NULL
      rum_v_full <- NULL
      vano_full <- NULL
      tra_full <- NULL
      pmctmono_full <- NULL
      pie_full <- NULL
      diap_full <- NULL
      porg_full <- NULL
      diags_full <- NULL
      
      for(f in hopitaux) {
        
      p <- noyau_pmeasyr(finess = f,  annee = annee, mois = mois, path   = '../temp/')
      
      pmsi <- imco(p = p, save = FALSE, persist = TRUE, tarifsante = FALSE)
      
      rsa_full = bind_rows(pmsi$rsa,rsa_full)
      rsa_v_full = bind_rows(pmsi$rsa_v,rsa_v_full)
      rum_full = bind_rows(pmsi$rum$rum,rum_full)
      diags_full = bind_rows(pmsi$rum$diags,diags_full)
      rum_v_full = bind_rows(pmsi$rum_v,rum_v_full)
      vano_full = bind_rows(pmsi$vano,vano_full)
      tra_full = bind_rows(pmsi$tra,tra_full)
      pmctmono_full = bind_rows(pmsi$pmctmono,pmctmono_full)
      pie_full = bind_rows(pmsi$pie,pie_full)
      diap_full = bind_rows(pmsi$diap,diap_full)
      porg_full = bind_rows(pmsi$porg,porg_full)

      }
      
      # suf <- paste0("_", annee, formatC(mois, width = 2, format = "d", flag = "0"))
      
      fichier_structure <- readxl::read_excel(paste0("../temp/", input$struct[[1,'name']]), col_types = c( "text" , "text" , "text" , "text" ,"text" , "text" , "text" , "text" , "text" ), col_names = c('nofiness', 'hopital', 'cdurm', 'uma_locale', 'uma_locale2',  'libelle_um', 'service', 'regroupement1', 'pole'), skip = 1  )
      
      rum <- dplyr::left_join(rum_full, fichier_structure)
      rum_v <- rum_v_full
      rsa <- rsa_full
      rsa_v <- rsa_v_full
      pmctmono <- pmctmono_full
      diags <- diags_full
      
      pmsidata <<- list('rum' = rum, 'rum_v' = rum_v, 'rsa' = rsa, 'rsa_v'= rsa_v, 'pmctmono' = pmctmono, 'diagnostics' = diags)
      
      if(input$tarifsant == TRUE) {
        
      incProgress(0.4, detail = paste("calcul avec tarifs antérieurs"))
      
      rsa_v_ant <- NULL
      rum_v_ant <- NULL
      
      for(f in hopitaux) {
        
        p <- noyau_pmeasyr(finess = f,  annee = annee, mois = mois, path   = '../temp/')
        
        pmsi <- imco(p = p, save = FALSE, persist = TRUE, tarifsante = TRUE)
        
        rsa_v_ant <- bind_rows(pmsi$rsa_v,rsa_v_ant)
        rum_v_ant <- bind_rows(pmsi$rum_v,rum_v_ant)
        
      }
      
      pmsidataant <<- list('rum_v_tarifs_anterieurs' = rum_v_ant, 'rsa_v_tarifs_anterieurs' = rsa_v_ant)
      
      }
      
      incProgress(0.4, detail = paste("élimination des fichiers"))
      
      do.call(file.remove, list(list.files("../temp/", full.names = TRUE)))
      
      check$ok <- 1
     } )
    }
  } )

  output$readme3 <- renderText( {
    courant <- input$courant ; consol <- input$consol ; struct <- input$struct ; ok <- check$ok
    if ((is.null(courant) | is.null(consol) | is.null(struct)) & is.null(ok)) {
      return(paste("Uploader les fichiers et attendre la fin de la copie des fichiers", sep="\n"))
      } 
    if (!is.null(courant) & !is.null(consol) & !is.null(struct) & ok == 0) {
      return(paste("Cliquer sur -Import et calculs-, attendre la fin du chargement puis cliquez sur -Téléchargement-", sep="\n"))
      }
    if (!is.null(courant) & !is.null(consol) & !is.null(struct) & ok == 1) {
          return(paste(paste0("Données produites pour ", nsites, " sites, à M", mois, " de ", annee,"."), " Vous pouvez maintenant télécharger l'objet R", sep="\n"))
      }
  })

  output$rendu1 <- downloadHandler(
    filename = function() { paste0("pmsi_", annee, "_M", mois, "_", Sys.Date(), ".RData") },
    content = function(file) {
      withProgress( message = "Compression des données", value = 0.3, { save(pmsidata, file = file) } )
                    } )

  output$rendu2 <- downloadHandler(
    filename = function() { paste0("pmsi_tarifs_anterieurs_", annee, "_M", mois, "_", Sys.Date(), ".RData") },
    content = function(file) {
      withProgress( message = "Compression des données", value = 0.3, { save(pmsidataant, file = file) } )
                    } )
  
}
