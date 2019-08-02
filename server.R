
options(shiny.maxRequestSize=10000*1024^2) 
nsites <<- NULL
hopitaux <<- NULL
annee <<- NULL
mois <<- NULL
ok <<- 0
pmsidata <<- NULL
pmsidataant <<- NULL 
  
function(input, output, session) { 
  
  url <- a("à cette page", href = "http://sls-dbim02.sls.aphp.fr:3838/4115983/servyce/")
  output$urlservyce <- renderUI({tagList("Une interface d'analyse des données par service est disponible ", url)})
  
  output$readme1 <- renderUI({HTML(paste("", "", "Le chargement des données doit être effectué ainsi:", "1) charger les bons fichiers genrsa directement dans l'interface de gauche (differents finess possibles),", "2) cliquer sur -Imports et calculs- puis attendre la fin des traitements,", "3) lorsque le traitement est fini, cliquer sur telechargement pour sauvegarder les fichiers au format RData", "", "", "", "", sep="<br/>"))})
  
  load <- reactive( {

    courant <- input$courant ; consol <- input$consol ; struct <- input$struct
    if (is.null(courant) | is.null(consol) | is.null(struct)) {return(NULL)}
    
    #Copier les fichier avec le bon nom
    for(i in 1:length(courant$datapath)) {
      file.copy(courant[[i,'datapath']], paste0("temp/", courant[[i,'name']]))
    }
    for(i in 1:length(consol$datapath)) {
      file.copy(consol[[i,'datapath']], paste0("temp/", consol[[i,'name']]))
    }
    file.copy(struct[[1,'datapath']], paste0("temp/", struct[[1,'name']]))

    #Formatage du fichier structure
    # fichier_structure <<- readxl::read_excel(paste0("temp/", input$struct[[1,'name']]), col_types = c( "text" , "text" , "text" , "text" ,"text" , "text" , "text" , "text" , "text" ), col_names = c('nofiness', 'hopital', 'cdurm', 'uma_locale', 'uma_locale2',  'libelle_um', 'service', 'regroupement1', 'pole'), skip = 1  )
    
    #Extraction des informations à partir des fichiers chargés NB: ICI INTRODUIRE DES VERIFICATIONS SUR LES FICHIERS CHARGES !!!
    infocourant <- as_tibble(courant) %>% separate(., name, into = c("finess", "annee", "mois"), sep = "\\.", remove = FALSE)
    infoconsol <- as_tibble(consol) %>% separate(., name, into = c("finess", "annee", "mois"), sep = "\\.", remove = FALSE)
    nsites <<- n_distinct(infocourant$finess)
    hopitaux <<- unique(infocourant$finess)
    annee <<- as.numeric(unique(infocourant$annee))
    mois <<- as.numeric(unique(infocourant$mois))
    ok <<- 0
   
  } )  
  
  observeEvent( input$launch, {
    
    withProgress( message = "Chargement", value = 0, {

      incProgress(0.1, detail = paste("copie des fichiers"))
      
      load()
      
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
        
      p <- noyau_pmeasyr(finess = f,  annee = annee, mois = mois, path   = 'temp/')
      
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
      
      fichier_structure <- readxl::read_excel(paste0("temp/", input$struct[[1,'name']]), col_types = c( "text" , "text" , "text" , "text" ,"text" , "text" , "text" , "text" , "text" ), col_names = c('nofiness', 'hopital', 'cdurm', 'uma_locale', 'uma_locale2',  'libelle_um', 'service', 'regroupement1', 'pole'), skip = 1  )
      
      rum <- dplyr::left_join(rum_full, fichier_structure)
      rum_v <- rum_v_full
      rsa <- rsa_full
      rsa_v <- rsa_v_full
      pmctmono <- pmctmono_full
      diags <- diags_full
      
      pmsidata <<- list('rum' = rum, 'rum_v' = rum_v, 'rsa' = rsa, 'rsa_v'= rsa_v, 'pmctmono' = pmctmono, 'diagnostics' = diags)
      
      incProgress(0.4, detail = paste("calcul avec tarifs antérieurs"))
      
      rsa_v_ant <- NULL
      rum_v_ant <- NULL
      
      for(f in hopitaux) {
        
        p <- noyau_pmeasyr(finess = f,  annee = annee, mois = mois, path   = 'temp/')
        
        pmsi <- imco(p = p, save = FALSE, persist = TRUE, tarifsante = TRUE)
        
        rsa_v_ant <- bind_rows(pmsi$rsa_v,rsa_v_ant)
        rum_v_ant <- bind_rows(pmsi$rum_v,rum_v_ant)
        
      }
      
      pmsidataant <<- list('rum_v_tarifs_anterieurs' = rum_v_ant, 'rsa_v_tarifs_anterieurs' = rsa_v_ant)
      
      incProgress(0.4, detail = paste("élimination des fichiers"))
      
      do.call(file.remove, list(list.files("temp/", full.names = TRUE)))
      
      ok <<- 1
      
    } )
  } )

  output$readme2 <- renderText( {
    courant <- input$courant ; consol <- input$consol ; struct <- input$struct ; ok <- ok
    if (is.null(courant) | is.null(consol) | is.null(struct)) {
      return(paste("Attendre la fin de tous les uploads puis cliquer sur -Import et calculs-", sep="\n"))
      } else {
        if(ok != 1){
          return(paste("Cliquer sur -Import et calculs-, attendre la fin du chargement puis cliquez sur -Téléchargement-", sep="\n"))
        } else {
          return(paste(paste0("Données produites pour ", nsites, " sites, à M", mois, " de ", annee,"."), " Vous pouvez maintenant télécharger l'objet R", sep="\n"))
        }
      }
  })

  output$rendu1 <- downloadHandler(
    filename = function() { paste0("pmsi_", annee, "_M", mois, "_", Sys.Date(), ".RData") },
    content = function(file) {
      save(pmsidata, file = file, compress = "xz", compression_level = 9 ) } )

  output$rendu2 <- downloadHandler(
    filename = function() { paste0("pmsi_tarifs_anterieurs_", annee, "_M", mois, "_", Sys.Date(), ".RData") },
    content = function(file) {
      save(pmsidataant, file = file, compress = "xz", compression_level = 9 ) } )
  
}
