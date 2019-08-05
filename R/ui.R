
dashboardPage(
  dashboardHeader(title="Imports pmsiR"),
  dashboardSidebar(
    # selectInput("anno", "Selection de l'année", choices = list("Année 2017" = 2017, "Année 2018" = 2018, "Année 2019" = 2019), selected = 2019),
    fileInput("courant", "Fichiers du mois d'analyse année en cours", multiple = TRUE), 
    fileInput("consol", "Fichiers M12 de  l'année précédente", multiple = TRUE),
    fileInput("struct", "Fichiers structure", multiple = FALSE),
    checkboxInput("tarifsant", label = "Ajouter calcul aux tarifs de l'année précédente"),
    actionButton("launch", icon("refresh"), label = "Import et calculs"),
    # actionButton("launchtarifsant", icon("refresh"), label = "Valorisation aux tarifs antérieurs"),
    # actionButton("remove", icon("refresh"), label = "Elimination des fichiers"),
    sidebarMenu(
      id="menuchoice",
      menuItem("Readme", tabName="readme"),
      # menuItem("Fichiers disponibles", tabName="dispo"),
      # menuItem("Selection des imports", tabName="select"),
      menuItem("Téléchargement", tabName="rendu")
    ) ),
  dashboardBody(
    fluidRow(uiOutput("urlservyce"),
             tabItems(
               tabItem(tabName="readme", h2("Import des données PMSI avec calcul des valorisations ventilées"),
                       fluidRow(
                       shinydashboard::box(htmlOutput("readme1"), width = 6, solidHeader = TRUE, collapsible = FALSE, collapsed =FALSE, title = "Notes", status = "primary"),
                       shinydashboard::box(DTOutput(outputId="readme2"), width = 6, solidHeader = TRUE, collapsible = FALSE, collapsed =FALSE, title = "Fichiers uploadés", status = "primary"),
                       shinydashboard::box(textOutput(outputId="readme3"), width = 12, solidHeader = TRUE, collapsible = FALSE, collapsed =FALSE, title = "Actions", status = "primary")
                       )
               ),
               tabItem(tabName="rendu", h2("Import des données PMSI avec calcul des valorisations ventilées"),
                             downloadButton(outputId="rendu1", "Téléchargement des données aux tarifs de l'année", class="dlButton"),
                             downloadButton(outputId="rendu2", "Téléchargement des données aux tarifs de l'année précédente", class="dlButton")
        )
      )
    )
  )
) 