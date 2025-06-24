# Charger les bibliothèques nécessaires pour le traitement et l'affichage des données
library(tidyr)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(sf)
library(readr)
library(DT)
library(ggplot2)
library(shinydashboard)
















# Charger les données de consommation à partir d'un fichier CSV
donnees_conso <- read_delim("C://Users//maria//Downloads//consoelecgaz2024.csv", delim = "\t")








# Remplacer les virgules par des points dans les colonnes concernées
donnees_conso$`Conso totale (MWh)` <- gsub(",", ".", donnees_conso$`Conso totale (MWh)`)
donnees_conso$`Conso moyenne (MWh)` <- gsub(",", ".", donnees_conso$`Conso moyenne (MWh)`)








# Convertir les colonnes nécessaires en format numérique (si ce n'est pas déjà le cas)
donnees_conso$`Conso totale (MWh)` <- as.numeric(donnees_conso$`Conso totale (MWh)`)
donnees_conso$`Conso moyenne (MWh)` <- as.numeric(donnees_conso$`Conso moyenne (MWh)`)
















# Renommer la colonne 'Code Département' pour correspondre à la colonne 'code' dans le shapefile
donnees_conso <- donnees_conso %>% rename(code = `Code Département`)
















# Ajouter un zéro devant les départements à un seul chiffre (par exemple, 7 devient 07)
donnees_conso$code <- ifelse(nchar(as.character(donnees_conso$code)) == 1,
                             paste0("0", donnees_conso$code),
                             as.character(donnees_conso$code))
















# Liste des fichiers GeoJSON pour les départements, incluant la France Métropolitaine et les départements d'Outre-mer
departments_files <- list(
  "Guadeloupe" = "https://github.com/marie-smk/tp/raw/main/departement-971-guadeloupe.geojson",
  "Martinique" = "https://github.com/marie-smk/tp/raw/main/departement-972-martinique.geojson",
  "Guyane" = "https://github.com/marie-smk/tp/raw/main/departement-973-guyane.geojson",
  "La Réunion" = "https://github.com/marie-smk/tp/raw/main/departement-974-la-reunion.geojson",
  "Mayotte" = "https://github.com/marie-smk/tp/raw/main/departement-976-mayotte.geojson",
  "France Métropolitaine" = "https://github.com/marie-smk/tp/raw/main/contour-des-departements.geojson"
)
















# Interface utilisateur avec shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Consommation Énergétique"),  # Titre de l'application
  dashboardSidebar(
    sidebarMenu(
      # Menu principal avec deux sections : Carte Choroplèthe et Visualisation de Tendance
      menuItem("Carte Choroplèthe", tabName = "carte_tab", icon = icon("map")),
      menuItem("Visualisation de Tendance", tabName = "tendance_tab", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Première section pour afficher la carte choroplèthe
      tabItem(tabName = "carte_tab",
              fluidRow(
                # Premier panneau pour les filtres de sélection
                column(4,
                       # Sélection des années
                       pickerInput("annee", "Sélectionnez une ou plusieurs années :",
                                   choices = unique(donnees_conso$Année),
                                   selected = unique(donnees_conso$Année),
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       
                       # Sélection des colonnes à afficher dans le tableau
                       pickerInput(
                         inputId = "colonnes",
                         label = "Sélectionnez les colonnes à afficher dans le tableau :",
                         choices = c(
                           "OPERATEUR" = "OPERATEUR",
                           "FILIERE" = "FILIERE",
                           "Année" = "Année",
                           "Code Département" = "Code Département",
                           "Nom Département" = "Nom Département",
                           "Code Région" = "Code Région",
                           "Nom Région" = "Nom Région",
                           "CODE CATEGORIE CONSOMMATION" = "CODE CATEGORIE CONSOMMATION",
                           "CODE GRAND SECTEUR" = "CODE GRAND SECTEUR",
                           "Nb sites" = "Nb sites",
                           "Conso totale (MWh)" = "Conso totale (MWh)",
                           "Conso moyenne (MWh)" = "Conso moyenne (MWh)"
                         ),
                         selected = c("Nom Département","OPERATEUR", "Nb sites", "Conso totale (MWh)", "Conso moyenne (MWh)"),
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)
                       ),
                       
                       # Sélection du secteur d'activité
                       radioButtons("secteur", "Sélectionnez un secteur d'activité :",
                                    choices = c("TERTIAIRE", "INDUSTRIE", "RESIDENTIEL", "AGRICULTURE", "INCONNU")),
                       
                       # Sélection de France Métropolitaine ou Outre-mer
                       selectInput("type_departements", "Sélectionnez les départements :",
                                   choices = c("France Métropolitaine", "Outre-mer")),
                       
                       # UI dynamique pour afficher les départements en fonction du choix
                       uiOutput("departement_ui"),  # Affiche le menu dynamique des départements
                       
                       # Sélection du type de consommation à afficher
                       radioButtons("conso_type", "Sélectionner le type de consommation :",
                                    choices = c("Consommation Totale" = "Conso totale (MWh)",
                                                "Consommation Moyenne" = "Conso moyenne (MWh)"),
                                    selected = "Conso totale (MWh)")
                ),
                # Deuxième panneau pour la carte et le tableau des données
                column(8,
                       # Affichage de la carte
                       leafletOutput("map", height = 600),
                       
                       # Affichage du tableau des données filtrées
                       dataTableOutput("table")
                )
              )
      ),
      
      # Deuxième section pour la visualisation de tendance des données
      tabItem(tabName = "tendance_tab",
              fluidRow(
                column(4,
                       # Sélection du secteur pour la tendance
                       selectInput("secteur_tendance", "Sélectionnez un secteur d'activité :",
                                   choices = c("TERTIAIRE", "INDUSTRIE", "RESIDENTIEL", "AGRICULTURE", "INCONNU"),
                                   selected = "TERTIAIRE"),
                       
                       # Sélection dynamique du département pour la tendance
                       pickerInput(
                         inputId = "departement_tendance",
                         label = "Sélectionnez un département :",
                         choices = c("Tous les départements" = "all", unique(donnees_conso$`Nom Département`)),
                         options = list(
                           liveSearch = TRUE,
                           liveSearchPlaceholder = "Tapez pour rechercher un département...",
                           size = 10
                         ),
                         selected = "all"
                       ),
                       
                       # Sélection de la mesure à afficher (totale ou moyenne)
                       selectInput("mesure_tendance", "Mesure à afficher :",
                                   choices = c("Consommation totale (MWh)" = "Conso totale (MWh)",
                                               "Consommation moyenne par site (MWh)" = "Conso moyenne (MWh)")),
                       
                       # Choix du type de graphique (ligne ou barres)
                       radioButtons("graph_type", "Choisissez le type de graphique :",
                                    choices = c("Ligne" = "line", "Barres" = "bar"),
                                    selected = "line")
                ),
                
                column(8,
                       # Affichage du graphique de tendance
                       plotOutput("trend_plot", height = 500)
                )
              )
      )
    )
  )
)
















# Serveur
server <- function(input, output, session) {
  
  # Observer pour mettre à jour les départements selon le type sélectionné (France Métropolitaine ou Outre-mer)
  observe({
    if(input$type_departements == "France Métropolitaine") {
      updatePickerInput(session, "departement",
                        choices = c("Tous les départements" = "all", unique(donnees_conso$`Nom Département`)))
    } else {
      # Ne pas inclure "Tous les départements" pour Outre-mer
      updatePickerInput(session, "departement",
                        choices = c(names(departments_files)[1:5]))
    }
  })
  
  # Créer l'UI dynamique pour le menu déroulant des départements en fonction du type sélectionné
  output$departement_ui <- renderUI({
    if(input$type_departements == "France Métropolitaine") {
      pickerInput("departement", "Sélectionnez un département :",
                  choices = c("Tous les départements" = "all", unique(donnees_conso$`Nom Département`)),
                  selected = "all")
    } else {
      pickerInput("departement", "Sélectionnez un département d'Outre-mer :",
                  choices = c(names(departments_files)[1:5]),  # Liste des départements d'Outre-mer
                  selected = "all")
    }
  })
  
  # Filtrage des données en fonction des sélections de l'utilisateur (année, secteur, département)
  filtered_data <- reactive({
    df <- donnees_conso
    if (!is.null(input$annee) && length(input$annee) > 0) {
      df <- df %>% filter(Année %in% input$annee)
    }
    if (!is.null(input$secteur)) {
      df <- df %>% filter(`CODE GRAND SECTEUR` == input$secteur)
    }
    if (input$departement != "all") {
      df <- df %>% filter(`Nom Département` == input$departement)
    }
    df
  })
  
  # Générer la carte avec les données agrégées par département
  map_data <- reactive({
    department_file <- departments_files[["France Métropolitaine"]]
    if(input$type_departements == "Outre-mer") {
      department_file <- departments_files[input$departement]
    }
    
    df_geo <- st_read(department_file, quiet = TRUE)
    df <- filtered_data()
    df_aggregated <- df %>%
      group_by(code, `Nom Département`) %>%
      summarize(
        Mesure = if (input$conso_type == "Conso totale (MWh)") {
          sum(`Conso totale (MWh)`, na.rm = TRUE)
        } else {
          mean(`Conso moyenne (MWh)`, na.rm = TRUE)
        },
        .groups = "drop"
      )
    df_geo <- left_join(df_geo, df_aggregated, by = "code")
    df_geo
  })
  
  # Affichage de la carte avec les départements et les données de consommation
  output$map <- renderLeaflet({
    df_geo <- map_data()
    palette <- colorNumeric("YlOrRd", domain = df_geo$Mesure, na.color = "transparent")
    
    leaflet(df_geo) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~palette(Mesure),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 5, color = "#666"),
        label = ~paste(`Nom Département`, ": ", round(Mesure, 2), " MWh"),
        layerId = ~`Nom Département`,
        popup = ~paste("Nom Département: ", `Nom Département`, "<br>Consommation: ", round(Mesure, 2), " MWh")
      ) %>%
      addLegend(pal = palette, values = ~Mesure, title = input$mesure)
  })
  
  # Mettre à jour l'input de département lorsque l'on clique sur un département dans la carte
  observeEvent(input$map_shape_click, {
    clicked_dept <- input$map_shape_click$id
    updatePickerInput(session, "departement", selected = clicked_dept)
  })
  
  # Affichage des données sous forme de tableau, avec sélection dynamique des colonnes
  output$table <- renderDataTable({
    df <- filtered_data()
    
    if (is.null(input$colonnes) || length(input$colonnes) == 0 || "all" %in% input$colonnes) {
      df_selected <- df
    } else {
      df_selected <- df[, input$colonnes, drop = FALSE]
    }
    
    datatable(df_selected, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Fonction pour calculer les tendances en fonction des années et des départements
  trend_data <- reactive({
    df <- donnees_conso
    if (!is.null(input$secteur_tendance)) {
      df <- df %>% filter(`CODE GRAND SECTEUR` == input$secteur_tendance)
    }
    if (input$departement_tendance != "all") {
      df <- df %>% filter(`Nom Département` == input$departement_tendance)
    }
    all_years <- seq(min(donnees_conso$Année, na.rm = TRUE), max(donnees_conso$Année, na.rm = TRUE))
    df_trend <- df %>%
      group_by(Année) %>%
      summarize(
        Mesure = if (input$mesure_tendance == "Conso totale (MWh)") {
          sum(`Conso totale (MWh)`, na.rm = TRUE)
        } else {
          mean(`Conso moyenne (MWh)`, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      complete(Année = all_years, fill = list(Mesure = NA))
    df_trend
  })
  
  # Affichage de la tendance sous forme de graphique (ligne ou barres)
  output$trend_plot <- renderPlot({
    df_trend <- trend_data()
    if (input$graph_type == "line") {
      ggplot(df_trend, aes(x = Année, y = Mesure, group = 1)) +
        geom_line(color = "blue") +
        labs(
          title = paste("Tendance de la", input$mesure_tendance),
          x = "Année",
          y = input$mesure_tendance
        ) +
        scale_x_continuous(breaks = seq(min(df_trend$Année), max(df_trend$Année), by = 1)) +
        theme_minimal()
    } else {
      ggplot(df_trend, aes(x = Année, y = Mesure, fill = as.factor(Année))) +
        geom_bar(stat = "identity") +
        labs(
          title = paste("Tendance de la", input$mesure_tendance),
          x = "Année",
          y = input$mesure_tendance
        ) +
        scale_x_continuous(breaks = seq(min(df_trend$Année), max(df_trend$Année), by = 1)) +
        scale_fill_brewer(palette = "Set3") +
        theme_minimal()
    }
  })
}
















# Lancement de l'application
shinyApp(ui = ui, server = server)































































