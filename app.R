library(shiny)
library(DBI)
library(RPostgres)
library(DT)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(shinycssloaders)
library(lubridate)
library(stringr)
library(forcats)

# Liste des colonnes disponibles
colonnes_possibles <- c(
  "idconvention", "civilite", "nom_etu", "prenom_etu", "num_etu", "mail_etu",
  "tel_etu", "adresse_etu", "ville_etu", "code_postal_etu", "nom_tuteur_univ",
  "prenom_tuteur_univ", "mail_tuteur_univ", "tel_tuteur_univ", "service",
  "taille_entreprise", "nom_rue_entreprise", "code_postal_entreprise", "ville",
  "nom_tuteur_entreprise", "prenom_tuteur_entreprise", "fonction_tuteur_entreprise",
  "mail_tuteur_entreprise", "tel_tuteur_entreprise", "numero_siret", "activite_entreprise",
  "pays_entreprise", "sujet_stage", "competence", "fonction_tache", "date_debut",
  "date_fin", "interruptionstage", "horaire_regulier", "nombre_conges",
  "duree_exceptionnelle", "gratification", "montantgratification", "nombre_heure_j",
  "provenance_stage"
)

# Connexion à PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "GEStage_Project",
  host = "localhost",
  port = 5433,
  user = "postgres",
  password = "Mariama2004"
)

get_data <- function() {
  df <- dbReadTable(con, "donnees_brutes")
  df <- mutate(df,
               BUT = case_when(
                 grepl("^22", num_etu) ~ "BUT2",
                 grepl("^21", num_etu) ~ "BUT3",
                 TRUE ~ "Autre"
               ),
               date_debut = as.Date(date_debut),
               date_fin = as.Date(date_fin),
               departement_entreprise = str_sub(code_postal_entreprise, 1, 2)
  )
  return(df)
}

get_tuteurs_pedagogiques <- function() {
  df <- dbReadTable(con, "tuteur_pedagogique")
  df <- arrange(df, nom_etudiant, prenom_etudiant)
  return(df)
}

ui <- navbarPage(
  title = div(
    img(src = "OIG1.jpg", height = "40px", style = "margin-right:10px; border-radius:5px;"),
    "GEStage - ML"
  ),
  theme = shinytheme("cerulean"),
  windowTitle = "GEStage Dashboard",
  
  tabPanel(
    "Fichiers",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        wellPanel(
          h4("Filtres", icon("filter")),
          selectInput("but_filtre_fichiers", "Filtrer par BUT", 
                      choices = c("Tous", "BUT2", "BUT3"),
                      selected = "Tous"),
          pickerInput(
            inputId = "colonnes",
            label = "Colonnes à afficher :",
            choices = colonnes_possibles,
            selected = c("nom_etu", "prenom_etu", "nom_tuteur_univ", "prenom_tuteur_univ", "mail_tuteur_univ"),
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ),
          colourInput("color1", "Couleur 1", value = "#3498db"),
          colourInput("color2", "Couleur 2", value = "#2ecc71"),
          colourInput("color3", "Couleur 3", value = "#9b59b6")
        ),
        wellPanel(
          h4("Ajouter un tuteur pédagogique", icon("user-graduate")),
          selectizeInput("nom_etu", "Nom de l'étudiant*", choices = NULL, options = list(placeholder = 'Sélectionnez un étudiant')),
          selectizeInput("prenom_etu", "Prénom de l'étudiant*", choices = NULL, options = list(placeholder = 'Sélectionnez un étudiant')),
          textInput("nom_tuteur", "Nom du tuteur*", placeholder = "Nom"),
          textInput("prenom_tuteur", "Prénom du tuteur*", placeholder = "Prénom"),
          textInput("fonction_tuteur", "Fonction du tuteur", placeholder = "Fonction"),
          textInput("mail_tuteur", "Email du tuteur*", placeholder = "email@universite.fr"),
          textInput("tel_tuteur", "Téléphone du tuteur", placeholder = "01 23 45 67 89"),
          helpText("* Champs obligatoires"),
          actionBttn("submit_tuteur", "Enregistrer le tuteur", 
                     style = "gradient",
                     color = "primary")
        )
      ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Tableau des stages",
            icon = icon("table"),
            div(
              style = "margin-top: 20px;",
              withSpinner(DTOutput("table_fichiers"), type = 6, color = "#3498db")
            )
          ),
          tabPanel(
            "Tuteurs pédagogiques",
            icon = icon("chalkboard-teacher"),
            div(
              style = "margin-top: 20px;",
              withSpinner(DTOutput("table_tuteurs_pedagogiques"), type = 6, color = "#3498db")
            )
          ),
          tabPanel(
            "Résumé",
            icon = icon("chart-bar"),
            fluidRow(
              valueBoxOutput("nb_etudiants_avec_tuteur", width = 6),
              valueBoxOutput("nb_etudiants_sans_tuteur", width = 6)
            ),
            fluidRow(
              box(
                title = "Étudiants sans tuteur universitaire",
                status = "danger",
                solidHeader = TRUE,
                width = 12,
                withSpinner(DTOutput("table_sans_tuteur"), type = 6, color = "#dc3545")
              )
            ),
            fluidRow(
              valueBoxOutput("nb_stages", width = 4),
              valueBoxOutput("nb_entreprises", width = 4),
              valueBoxOutput("nb_tuteurs", width = 4)
            ),
            fluidRow(
              box(
                title = "Répartition par BUT", 
                status = "primary",
                solidHeader = TRUE,
                plotOutput("repartition_but", height = 250)
              ),
              box(
                title = "Top 5 entreprises", 
                status = "success",
                solidHeader = TRUE,
                plotOutput("top_entreprises", height = 250)
              )
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Statistiques",
    icon = icon("chart-line"),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("Filtres statistiques", icon("sliders-h")),
          selectInput("but_filtre_stats", "Filtrer par BUT", 
                      choices = c("Tous", "BUT2", "BUT3"),
                      selected = "Tous"),
          dateRangeInput("date_range", "Période:",
                         start = Sys.Date() - 365,
                         end = Sys.Date())
        )
      ),
      column(
        width = 9,
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Services de l'entreprise",
            fluidRow(
              box(
                title = "Nombre de stages par service", 
                width = 12,
                status = "info",
                solidHeader = TRUE,
                withSpinner(plotOutput("evolution_stages_entreprise"), type = 6, color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Taille des entreprises",
            fluidRow(
              box(
                title = "Répartition par taille", 
                width = 12,
                status = "success",
                solidHeader = TRUE,
                withSpinner(plotOutput("taille_entreprise", height = 500), type = 6, color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Localisation & Recherche",
            fluidRow(
              box(
                title = "Départements des entreprises", 
                width = 6,
                status = "info",
                solidHeader = TRUE,
                withSpinner(plotOutput("departement_entreprises", height = 400), type = 6, color = "#3498db")
              ),
              box(
                title = "Provenance du Stage", 
                width = 6,
                status = "success",
                solidHeader = TRUE,
                withSpinner(plotOutput("moyen_recherche", height = 400), type = 6, color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Répartition par BUT",
            fluidRow(
              box(
                title = "Répartition par BUT", 
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                withSpinner(plotOutput("repartition_but_stats", height = 500), type = 6, color = "#3498db")
              )
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Date de Soutenance",
    icon = icon("calendar-check"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        wellPanel(
          h4("Paramètres des soutenances", icon("cog")),
          numericInput("jours_apres_stage", "Jours après ou avant la fin du stage:", 
                       value = 14, min = -60, max = 60),
          selectInput("but_filtre_soutenance", "Filtrer par BUT", 
                      choices = c("Tous", "BUT2", "BUT3"),
                      selected = "Tous"),
          dateRangeInput("periode_soutenance", "Période de soutenance:",
                         start = Sys.Date(),
                         end = Sys.Date() + 60),
          actionButton("calculer_dates", "Calculer les dates", 
                       icon = icon("calculator"),
                       class = "btn-primary")
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(
            "Planning des soutenances",
            withSpinner(DTOutput("table_soutenances"), type = 6, color = "#3498db")
          ),
          tabPanel(
            "Visualisation",
            withSpinner(plotOutput("graph_soutenances", height = "600px"), type = 6, color = "#3498db")
          ),
          tabPanel(
            "Statistiques",
            fluidRow(
              valueBoxOutput("nb_soutenances", width = 4),
              valueBoxOutput("prochaines_soutenances", width = 4),
              valueBoxOutput("moyenne_jours", width = 4)
            ),
            fluidRow(
              box(
                title = "Répartition par semaine", 
                width = 12,
                status = "info",
                solidHeader = TRUE,
                plotOutput("repartition_semaine", height = "400px")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Charger données
  data_all <- reactiveVal(get_data())
  tuteurs_pedagogiques <- reactiveVal(get_tuteurs_pedagogiques())
  
  # Mise à jour des listes déroulantes pour les étudiants
  observe({
    df <- data_all()
    updateSelectizeInput(session, "nom_etu", choices = unique(df$nom_etu), server = TRUE)
    updateSelectizeInput(session, "prenom_etu", choices = unique(df$prenom_etu), server = TRUE)
  })
  
  # Calculer les dates de soutenance
  soutenances_data <- eventReactive(input$calculer_dates, {
    df <- data_all()
    df <- mutate(df,
                 date_soutenance = date_fin + days(input$jours_apres_stage),
                 jours_restants = as.numeric(date_soutenance - Sys.Date()),
                 semaine_soutenance = paste0("S", isoweek(date_soutenance), "-", year(date_soutenance)),
                 statut = case_when(
                   date_soutenance < Sys.Date() ~ "Passée",
                   jours_restants <= 7 ~ "Cette semaine",
                   jours_restants <= 14 ~ "Semaine prochaine",
                   TRUE ~ "À venir"
                 )
    )
    
    if(input$but_filtre_soutenance != "Tous") {
      df <- filter(df, BUT == input$but_filtre_soutenance)
    }
    
    df <- filter(df,
                 date_soutenance >= input$periode_soutenance[1],
                 date_soutenance <= input$periode_soutenance[2]
    )
    
    return(df)
  })
  
  # Fonction pour récupérer les tuteurs uniques
  tuteurs_data <- reactive({
    df <- data_all()
    df <- select(df, nom_tuteur_univ, prenom_tuteur_univ, mail_tuteur_univ, tel_tuteur_univ)
    df <- distinct(df)
    df <- filter(df, !is.na(nom_tuteur_univ))
    return(df)
  })
  
  # Observer le bouton d'enregistrement du tuteur pédagogique
  observeEvent(input$submit_tuteur, {
    req(input$nom_etu, input$prenom_etu, input$nom_tuteur, input$prenom_tuteur, input$mail_tuteur)
    
    if(!grepl("^[^@]+@[^@]+\\.[^@]+", input$mail_tuteur)) {
      showNotification("Veuillez entrer une adresse email valide", type = "error")
      return()
    }
    
    new_tuteur <- data.frame(
      nom_etudiant = input$nom_etu,
      prenom_etudiant = input$prenom_etu,
      nom_tuteur = input$nom_tuteur,
      prenom_tuteur = input$prenom_tuteur,
      fonction = ifelse(is.null(input$fonction_tuteur) || input$fonction_tuteur == "", NA, input$fonction_tuteur),
      mail = input$mail_tuteur,
      tel = ifelse(is.null(input$tel_tuteur) || input$tel_tuteur == "", NA, input$tel_tuteur),
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      dbWriteTable(con, "tuteur_pedagogique", new_tuteur, append = TRUE, row.names = FALSE)
      tuteurs_pedagogiques(get_tuteurs_pedagogiques())
      showNotification("Tuteur pédagogique ajouté avec succès!", type = "message")
      
      updateSelectizeInput(session, "nom_etu", selected = "")
      updateSelectizeInput(session, "prenom_etu", selected = "")
      updateTextInput(session, "nom_tuteur", value = "")
      updateTextInput(session, "prenom_tuteur", value = "")
      updateTextInput(session, "fonction_tuteur", value = "")
      updateTextInput(session, "mail_tuteur", value = "")
      updateTextInput(session, "tel_tuteur", value = "")
    }, error = function(e) {
      showNotification(paste0("Erreur lors de l'ajout du tuteur: ", e$message), type = "error")
    })
  })
  
  # Tableau des fichiers
  output$table_fichiers <- renderDT({
    df <- data_all()
    
    if(input$but_filtre_fichiers != "Tous") {
      df <- filter(df, BUT == input$but_filtre_fichiers)
    }
    
    cols_ok <- input$colonnes[input$colonnes %in% colnames(df)]
    df <- select(df, all_of(cols_ok))
    
    dt <- datatable(
      df,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'excel', text = 'Excel', 
               filename = 'export_stages', 
               title = "Export des stages")
        ),
        scrollX = TRUE,
        scrollY = "600px",
        scroller = TRUE
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'display nowrap cell-border stripe hover'
    )
    dt <- formatStyle(dt, columns = names(df), fontSize = '85%')
    return(dt)
  })
  
  # Tableau des tuteurs pédagogiques
  output$table_tuteurs_pedagogiques <- renderDT({
    df <- tuteurs_pedagogiques()
    
    dt <- datatable(
      df,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'excel', text = 'Excel', 
               filename = 'export_tuteurs_pedagogiques', 
               title = "Export des tuteurs pédagogiques")
        ),
        scrollX = TRUE,
        scrollY = "600px",
        scroller = TRUE
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'display nowrap cell-border stripe hover'
    )
    dt <- formatStyle(dt, columns = names(df), fontSize = '85%')
    return(dt)
  })
  
  # Tableau des soutenances
  output$table_soutenances <- renderDT({
    req(soutenances_data())
    
    df <- soutenances_data()
    df <- select(df, nom_etu, prenom_etu, BUT, date_fin, date_soutenance, jours_restants, statut, nom_tuteur_univ)
    
    dt <- datatable(
      df,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'excel', text = 'Excel', 
               filename = 'export_soutenances', 
               title = "Export des soutenances")
        ),
        scrollX = TRUE,
        scrollY = "500px",
        scroller = TRUE
      ),
      rownames = FALSE,
      colnames = c("Nom", "Prénom", "BUT", "Fin stage", "Date soutenance", "Jours restants", "Statut", "Tuteur"),
      class = 'display nowrap cell-border stripe hover'
    )
    dt <- formatStyle(dt,
                      'statut',
                      target = 'row',
                      backgroundColor = styleEqual(
                        c("Passée", "Cette semaine", "Semaine prochaine", "À venir"),
                        c("#f8d7da", "#fff3cd", "#d1ecf1", "#d4edda")
                      )
    )
    return(dt)
  })
  
  # Graphique des soutenances
  output$graph_soutenances <- renderPlot({
    req(soutenances_data())
    
    df <- soutenances_data()
    df <- count(df, date_soutenance, statut)
    
    p <- ggplot(df, aes(x = date_soutenance, y = n, fill = statut)) +
      geom_col() +
      scale_fill_manual(values = c("Passée" = "#dc3545", 
                                   "Cette semaine" = "#ffc107", 
                                   "Semaine prochaine" = "#17a2b8", 
                                   "À venir" = "#28a745")) +
      labs(title = "Planning des soutenances",
           x = "Date de soutenance",
           y = "Nombre de soutenances",
           fill = "Statut") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  })
  
  # Indicateurs clés
  output$nb_stages <- renderValueBox({
    valueBox(
      nrow(data_all()),
      "Nombre de stages",
      icon = icon("briefcase"),
      color = "aqua"
    )
  })
  
  output$nb_entreprises <- renderValueBox({
    valueBox(
      n_distinct(data_all()$nom_tuteur_entreprise),
      "Entreprises différentes",
      icon = icon("building"),
      color = "green"
    )
  })
  
  output$nb_tuteurs <- renderValueBox({
    valueBox(
      nrow(tuteurs_data()),
      "Tuteurs universitaires",
      icon = icon("user-graduate"),
      color = "light-blue"
    )
  })
  
  output$nb_soutenances <- renderValueBox({
    req(soutenances_data())
    nb <- nrow(soutenances_data())
    valueBox(
      ifelse(is.na(nb) || is.null(nb), 0, nb),
      "Soutenances programmées",
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$prochaines_soutenances <- renderValueBox({
    req(soutenances_data())
    nb <- tryCatch({
      df <- soutenances_data()
      df <- filter(df, date_soutenance >= Sys.Date(), date_soutenance <= Sys.Date() + 7)
      nrow(df)
    }, error = function(e) 0)
    
    valueBox(
      nb,
      "Soutenances cette semaine",
      icon = icon("hourglass-half"),
      color = ifelse(nb > 5, "red", ifelse(nb > 0, "yellow", "green"))
    )
  })
  
  output$moyenne_jours <- renderValueBox({
    req(soutenances_data())
    moy <- tryCatch({
      round(mean(soutenances_data()$jours_restants, na.rm = TRUE), 1)
    }, error = function(e) 0)
    
    valueBox(
      moy,
      "Jours moyens avant soutenance",
      icon = icon("clock"),
      color = ifelse(moy < 10, "red", "green")
    )
  })
  
  output$repartition_semaine <- renderPlot({
    req(soutenances_data())
    
    df <- tryCatch({
      count(soutenances_data(), semaine_soutenance)
    }, error = function(e) {
      data.frame(semaine_soutenance = character(), n = integer())
    })
    
    if(nrow(df) == 0) {
      p <- ggplot() + 
        annotate("text", x = 1, y = 1, label = "Aucune soutenance programmée", size = 6) + 
        theme_void()
    } else {
      p <- ggplot(df, aes(x = semaine_soutenance, y = n)) +
        geom_col(fill = "#3498db") +
        labs(title = "Répartition des soutenances par semaine",
             x = "Semaine",
             y = "Nombre de soutenances") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(p)
  })
  
  output$repartition_but <- renderPlot({
    df <- data_all()
    p <- ggplot(df, aes(x = BUT, fill = BUT)) +
      geom_bar() +
      scale_fill_manual(values = c("BUT2" = input$color1, 
                                   "BUT3" = input$color2, 
                                   "Autre" = input$color3)) +
      labs(x = "BUT", y = "Nombre de stages") +
      theme_minimal() +
      theme(legend.position = "none")
    return(p)
  })
  
  output$repartition_but_stats <- renderPlot({
    df <- data_all()
    
    if(input$but_filtre_stats != "Tous") {
      df <- filter(df, BUT == input$but_filtre_stats)
    }
    
    p <- ggplot(df, aes(x = BUT, fill = BUT)) +
      geom_bar() +
      scale_fill_manual(values = c("BUT2" = "#3498db", 
                                   "BUT3" = "#2ecc71", 
                                   "Autre" = "#9b59b6")) +
      labs(title = "Répartition des stages par BUT",
           x = "BUT",
           y = "Nombre de stages") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    return(p)
  })
  
  output$top_entreprises <- renderPlot({
    df <- data_all()
    df <- count(df, nom_tuteur_entreprise, sort = TRUE)
    df <- head(df, 5)
    
    p <- ggplot(df, aes(x = reorder(nom_tuteur_entreprise, n), y = n, fill = nom_tuteur_entreprise)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = colorRampPalette(c(input$color1, input$color2, input$color3))(nrow(df))) +
      labs(x = "Entreprise", y = "Nombre de stages") +
      theme_minimal() +
      theme(legend.position = "none")
    return(p)
  })
  
  output$evolution_stages_entreprise <- renderPlot({
    df <- data_all()
    df <- count(df, activite_entreprise)
    df <- filter(df, n > 0)
    df <- mutate(df, activite_entreprise = ifelse(is.na(activite_entreprise), "Inconnue", activite_entreprise))
    
    p <- ggplot(df, aes(x = reorder(activite_entreprise, n), y = n)) +
      geom_col(fill = input$color1) +
      labs(x = "Activité entreprise", y = "Nombre", title = "Nombre de stages par activité d'entreprise") +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))
    return(p)
  })
  
  output$taille_entreprise <- renderPlot({
    df <- data_all()
    
    p <- ggplot(df, aes(x = taille_entreprise, fill = taille_entreprise)) +
      geom_bar() +
      scale_fill_manual(values = colorRampPalette(c(input$color1, input$color2, input$color3))(length(unique(df$taille_entreprise)))) +
      labs(x = "Taille", y = "Nombre", title = "Répartition des stages selon la taille des entreprises") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
  })
  
  output$departement_entreprises <- renderPlot({
    df <- data_all()
    df <- filter(df, !is.na(departement_entreprise))
    df <- count(df, departement_entreprise)
    df <- arrange(df, desc(n))
    df <- mutate(df, departement_entreprise = factor(departement_entreprise, levels = departement_entreprise))
    
    p <- ggplot(df, aes(x = departement_entreprise, y = n, fill = departement_entreprise)) +
      geom_col() +
      scale_fill_viridis_d() +
      labs(title = "Localisation des entreprises par département",
           x = "Département",
           y = "Nombre de stages",
           fill = "Département") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    return(p)
  })
  
  output$moyen_recherche <- renderPlot({
    df <- data_all()
    df <- filter(df, !is.na(provenance_stage))
    df <- count(df, provenance_stage)
    df <- mutate(df, provenance_stage = fct_reorder(provenance_stage, n))
    
    p <- ggplot(df, aes(x = provenance_stage, y = n, fill = provenance_stage)) +
      geom_col() +
      coord_flip() +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Provenance du stage",
           x = "Source",
           y = "Nombre",
           fill = "Source") +
      theme_minimal() +
      theme(legend.position = "none")
    return(p)
  })
  
  output$stagiaires_par_tuteur <- renderPlot({
    df <- data_all()
    df <- filter(df, !is.na(nom_tuteur_univ))
    df <- mutate(df, tuteur_complet = paste(prenom_tuteur_univ, nom_tuteur_univ))
    df <- count(df, tuteur_complet, sort = TRUE)
    df <- head(df, 20)
    df <- mutate(df, tuteur_complet = fct_reorder(tuteur_complet, n))
    
    p <- ggplot(df, aes(x = tuteur_complet, y = n, fill = tuteur_complet)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d() +
      labs(title = "Nombre de stagiaires suivis par tuteur (top 20)",
           x = "Tuteur universitaire",
           y = "Nombre de stagiaires",
           fill = "Tuteur") +
      theme_minimal() +
      theme(legend.position = "none")
    return(p)
  })
  
  output$nb_etudiants_avec_tuteur <- renderValueBox({
    df <- data_all()
    n <- nrow(filter(df, !is.na(nom_tuteur_univ) & nom_tuteur_univ != ""))
    valueBox(
      n, "Étudiants avec tuteur universitaire",
      icon = icon("user-check"),
      color = "green"
    )
  })
  
  output$nb_etudiants_sans_tuteur <- renderValueBox({
    df <- data_all()
    n <- nrow(filter(df, is.na(nom_tuteur_univ) | nom_tuteur_univ == ""))
    valueBox(
      n, "Étudiants sans tuteur universitaire",
      icon = icon("user-times"),
      color = "red"
    )
  })
  
  output$table_sans_tuteur <- renderDT({
    df <- data_all()
    df <- filter(df, is.na(nom_tuteur_univ) | nom_tuteur_univ == "")
    df <- select(df, nom_etu, prenom_etu, mail_etu, BUT)
    
    dt <- datatable(
      df,
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Nom", "Prénom", "Email", "BUT")
    )
    return(dt)
  })
}

shinyApp(ui, server)