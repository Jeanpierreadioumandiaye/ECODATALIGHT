library(shiny)
library(ggplot2)
library(readxl)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(openxlsx)  # Pour la fonction de téléchargement Excel

# Charger les données depuis Excel
base_commune <- read_excel("base_commune.xlsx")
base_region <- read_excel("base_region.xlsx")
base_departement <- read_excel("base_departement.xlsx")
base_region2 <- read_excel("base_region2.xlsx")
base_region3 <- read_excel("base_region3.xlsx") # Contient les données PIB
base_region4 <- read_excel("base_region4.xlsx")
base_previsions_2012_2025 <- read_excel("base_previsions_2012_2025.xlsx")
base_intensite_lumineuses <- read_excel("base_intensite_lumineuses.xlsx")

# Interface utilisateur
ui <- dashboardPage(
  
  dashboardHeader(title = "ECODATALIGHT"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Luminosité", tabName = "luminosite", icon = icon("globe-africa")),
      menuItem("Statistiques Régionales", tabName = "statistiques", icon = icon("bar-chart"),
               menuSubItem("PIB Estimé", tabName = "pib_estime"),
               menuSubItem("Taux de Chômage Estimé", tabName = "chomage_estime")),
      
      menuItem("Calculs des PIB", tabName = "statistiques", icon = icon("bar-chart"),
               menuSubItem("Estimations des PIB régionaux", tabName = "estimations_pib"),
               menuSubItem("Estimations des PIB départementaux", tabName = "estimations_pib_dep"),
               menuSubItem("Estimations des PIB communaux", tabName = "estimations_pib_com")),
      menuItem("Prévisions des PIB régionnaux", tabName = "previsions_pib", icon = icon("globe")), 
      menuItem("Equipe", tabName = "equipe", icon = icon("users"))  # Onglet "Equipe"
    )
  ),
  dashboardBody(
    tabItems(
      
      
      
      
      # Onglet Accueil
      tabItem(tabName = "accueil",
              # Ajoutez le logo dans l'onglet d'accueil
              fluidRow(
                column(12,
                       # Affichage du logo avec imageOutput
                       imageOutput("image_logo", height = "100px")
                )
              ),
              
              fluidRow(
                # Phrase en bleu, hors d'un box
                div(style = "font-size: 28px; 
                       font-weight: bold; 
                       text-align: center; 
                       margin: 20px 0; 
                       font-family: 'Arial, sans-serif'; 
                       color: #1e90ff; 
                       background: -webkit-linear-gradient(45deg, #1e90ff, #87cefa); 
                       -webkit-background-clip: text; 
                       -webkit-text-fill-color: transparent;",
                    "Visualiser l'économie, éclairer les décisions."
                ),
                box(title = "Bienvenue sur EcoDataLight", width = 12, solidHeader = TRUE, status = "primary",
                    p("EcoDataLight est une plateforme interactive conçue pour offrir des visualisations dynamiques et des analyses des indicateurs macroéconomiques du Sénégal. À l'aide de données sur la luminosité nocturne, les performances économiques régionales, les taux de chômage et d'autres indicateurs clés, EcoDataLight vous permet d'explorer les disparités régionales et d'identifier les tendances économiques et sociales.")
                ),
                # Texte sous forme de nuage (détaché)
                div(style = "font-size: 18px; 
                       font-weight: bold; 
                       text-align: center; 
                       margin: 200px auto 50px auto;  /* Crée un espace haut et bas de 50px */
                       max-width: 800px;            /* Limite la largeur pour centrer le contenu */
                       line-height: 1.6; 
                       font-family: 'Arial, sans-serif'; 
                       color: #555555; 
                       background-color: #e6f7ff; 
                       border-radius: 50px; 
                       padding: 20px; 
                       box-shadow: 0px 4px 10px rgba(0,0,0,0.15); 
                       border: 2px solid #b3e5fc;",
                    "Explorez nos sections pour découvrir des insights uniques  révélateurs de l’activité économique. "
                ),
                
                
                
                box(title = "Indicateurs clés", width = 12, status = "info", solidHeader = TRUE,
                    fluidRow(
                      valueBoxOutput("cadran_senegal", width = 4),
                      valueBoxOutput("cadran_pib", width = 4),
                      valueBoxOutput("cadran_region_non_eclairee", width = 4)
                    )
                ),
                box(title = "Explorer les Graphiques", width = 12, solidHeader = TRUE,
                    selectInput("choix_graphique", "Choisissez un graphique :", 
                                choices = c("Distribution des intensités lumineuses par région en 2020" = "distribution",
                                            "Top 10 des départements les plus éclairés en 2020" = "top_departement",
                                            "Top 10 des communes les plus éclairées en 2020" = "top_commune")),
                    plotlyOutput("graphique_accueil")
                )
              )
      ),
      
      # Onglet Statistiques Régionales
      tabItem(tabName = "statistiques",
              fluidRow(
                box(title = "Statistiques des Régions", width = 12, solidHeader = TRUE,
                    DT::dataTableOutput("region_stats"),
                    downloadButton("download_region_stats", "Télécharger les statistiques")
                )
              )
      ),
      # Sous-onglet PIB Estimé
      tabItem(tabName = "pib_estime",
              fluidRow(
                box(title = "Tableau : PIB Estimé par Région en 2020", width = 12, solidHeader = TRUE,
                    DT::dataTableOutput("table_pib")),
                box(title = "Graphiques : Répartition du PIB Estimé de 2012 à 2020", width = 12, solidHeader = TRUE,
                    selectInput("choix_graphique_pib", "Choisissez une région :", 
                                choices = c("Dakar", "Diourbel", "Fatick", "Kaffrine", "Kaolack",
                                            "Kédougou", "Kolda", "Louga", "Matam", "Saint Louis",
                                            "Sédhiou", "Tambacounda", "Thiès", "Ziguinchor", "Sénégal")),
                    plotlyOutput("graphique_pib"))
              )
      ),
      # Sous-onglet Taux de Chômage Estimé
      tabItem(tabName = "chomage_estime",
              fluidRow(
                box(title = "Table des Taux de Chômage en 2020", width = 12, solidHeader = TRUE,
                    DT::dataTableOutput("table_chomage")),
                box(title = "Répartition du Taux de Chômage en 2020", width = 12, solidHeader = TRUE,
                    plotlyOutput("barplot_chomage"))
              )
      ),
      # Onglet Luminosité
      tabItem(tabName = "luminosite",
              fluidRow(
                box(title = "Carte des intensités lumineuses par région (Satellite) en 2013", width = 6, solidHeader = TRUE, status = "primary",
                    imageOutput("image_satellite"),
                    p("Source : Visible Infrared Imaging Radiometer Suit, AIDDATA", style = "font-size: 12px; color: gray;")
                ),
                box(title = "Carte des intensités lumineuses par région (SIG) en 2020", width = 6, solidHeader = TRUE, status = "success",
                    imageOutput("image_SIG"),
                    p("Source : Visible Infrared Imaging Radiometer Suit, AIDDATA", style = "font-size: 12px; color: gray;")),
                
                box(title = "Tableau : Intensités lumineuses par région en 2020", width = 12, solidHeader = TRUE,
                    DT::dataTableOutput("table_il")
                )
              )
      ),
      
      # Onglet Estimations des PIB 
      tabItem(tabName = "estimations_pib",
              fluidRow(
                box(title = "Calcul des PIB régionaux", width = 12, solidHeader = TRUE, status = "primary",
                    numericInput("pib_national", "Saisir le PIB national :", value = 1000000, min = 0, step = 1000),
                    fileInput("file_input", "Charger un fichier Excel avec colonnes 'region' et 'NLTreg'", 
                              accept = c(".xlsx")),
                    p("Assurez-vous que les colonnes du fichier sont correctement nommées : 'region' pour les régions et 'NLTreg' pour les intensités lumineuses."),
                    actionButton("calculate_pib", "Calculer les PIB régionaux")
                ),
                box(title = "Résultats", width = 12, solidHeader = TRUE, status = "success",
                    DT::dataTableOutput("pib_table"))
              )
      ),
      
      # Onglet Estimations des PIB départementaux
      tabItem(tabName = "estimations_pib_dep",
              fluidRow(
                box(title = "Calcul des PIB départementaux", width = 12, solidHeader = TRUE, status = "primary",
                    numericInput("pib_regional", "Saisir le PIB régional :", value = 100000, min = 0, step = 1000),
                    fileInput("file_input_dep", "Charger un fichier Excel avec colonnes 'departement' et 'NLTdep'", 
                              accept = c(".xlsx")),
                    p("Assurez-vous que les colonnes du fichier sont correctement nommées : 'departement' pour les départements et 'NLTdep' pour les intensités lumineuses."),
                    actionButton("calculate_pib_dep", "Calculer les PIB départementaux")
                ),
                box(title = "Résultats", width = 12, solidHeader = TRUE, status = "success",
                    DT::dataTableOutput("pib_dep_table"))
              )
      ),
      
      # Onglet Estimations des PIB communaux
      tabItem(tabName = "estimations_pib_com",
              fluidRow(
                box(title = "Calcul des PIB communaux", width = 12, solidHeader = TRUE, status = "primary",
                    numericInput("pib_departemental", "Saisir le PIB départemental :", value = 50000, min = 0, step = 1000),
                    fileInput("file_input_com", "Charger un fichier Excel avec colonnes 'communes' et 'NLTcom'", 
                              accept = c(".xlsx")),
                    p("Assurez-vous que les colonnes du fichier sont correctement nommées : 'communes' pour les communes et 'NLTcom' pour les intensités lumineuses."),
                    actionButton("calculate_pib_com", "Calculer les PIB communaux")
                ),
                box(title = "Résultats", width = 12, solidHeader = TRUE, status = "success",
                    DT::dataTableOutput("pib_com_table"))
              )
      ),
      
      # Onglet Previsions des PIB Regionnaux
      
      tabItem(tabName = "previsions_pib",
              fluidRow(
                box(title = "Graphiques : Prévisions des PIB Estimés par Région", width = 12, solidHeader = TRUE,
                    selectInput("choix_graphique_pib", "Choisissez une région :", 
                                choices = c("Dakar", "Diourbel", "Fatick", "Kaffrine", "Kaolack",
                                            "Kédougou", "Kolda", "Louga", "Matam", "Saint Louis",
                                            "Sédhiou", "Tambacounda", "Thiès", "Ziguinchor", "Sénégal")),
                    plotlyOutput("previsions_pib")
                )
              )
      ),
      
      # Onglet Equipe
      tabItem(tabName = "equipe",
              fluidRow(
                box(title = "L'équipe de Concepteurs", width = 12, solidHeader = TRUE, status = "primary",
                    p("Nous vousprésentons l' ")
                ),
                # Biographie du concepteur 1
                box(title = "Ambre Geraut", width = 4, solidHeader = TRUE,
                    img(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\ambre.jpg", height = "150px", width = "150px"),
                    p("Étudiante dans le master aide à la décision et évaluation des politiques publiques de l’ENSAE
                      en partenariat avec l’Université Paris Dauphine. Elle a travaillé comme économiste statisticienne à la 
                      Banque de france en parallèle de son Master d'Analyse et Politique Économique à l'université Panthéon-Assas."))
                ,
                # Biographie du concepteur 2
                box(title = "Jean Pierre Adiouma Ndiaye", width = 4, solidHeader = TRUE,
                    img(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\karel.jpg", height = "150px", width = "150px"),
                    p("Ingénieur statisticien économiste, il est étudiant en Master ADEPP de l'ENSAE 
                    et il a travaillé en tant que stagiaire à la BCEAO. Il maitrise les logiciels de traitement de données et a une passion
                    pour l’utilisation du Machine Learning pour améliorer la qualité des indicateurs."))
                ,
                # Biographie du concepteur 3
                box(title = "Karel Sodijinouti", width = 4, solidHeader = TRUE,
                    img(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\jp.jpg", height = "150px", width = "150px"),
                    p("Élève dans la formation ingénieur statisticien économiste de l'ENSAE. Il maîtrise le langage C, 
                    Latex et s’intéresse à la macroéconomie ainsi qu’à l’utilisation des nouvelles sources de données dans la statistique. "))
              )
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Affichage du logo en utilisant renderImage
  output$image_logo <- renderImage({
    list(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\image_logo.JPG", contentType = "image/jpg", width = "14%", height = "auto")
  }, deleteFile = FALSE)
  
  # Cadrans
  output$cadran_senegal <- renderValueBox({
    valueBox("Sénégal", "Pays", icon = icon("flag"), color = "yellow")
  })
  
  output$cadran_pib <- renderValueBox({
    valueBox("20,3K Milliards FCFA", "PIB estimé en 2020", icon = icon("money-bill-wave"), color = "green")
  })
  
  output$cadran_region_eclairee <- renderValueBox({
    valueBox("Dakar", "Région la plus éclairée en 2020", icon = icon("sun"), color = "blue")
  })
  
  output$cadran_region_non_eclairee <- renderValueBox({
    valueBox("Tambacounda", "Région la moins éclairée en 2020", icon = icon("sun"), color = "light-blue")
  })
  
  # Graphiques dynamiques pour l'onglet Accueil
  output$graphique_accueil <- renderPlotly({
    if (input$choix_graphique == "distribution") {
      region_data <- base_region %>%
        group_by(region) %>%
        summarise(intensite_moyenne = mean(intensite, na.rm = TRUE))
      
      plot_ly(region_data, x = ~region, y = ~intensite_moyenne, type = 'bar') %>%
        layout(title = "Distribution des Intensités Lumineuses par Région en 2020",
               xaxis = list(title = "Région"), yaxis = list(title = "Intensité Moyenne"))
    } else if (input$choix_graphique == "top_departement") {
      top_depts <- base_departement %>%
        group_by(departement) %>%
        summarise(intensite_totale = sum(intensite, na.rm = TRUE)) %>%
        arrange(desc(intensite_totale)) %>%
        slice(1:10)
      
      plot_ly(top_depts, x = ~departement, y = ~intensite_totale, type = 'bar') %>%
        layout(title = "Top 5 Départements les Plus Éclairés en 2020",
               xaxis = list(title = "Département"), yaxis = list(title = "Intensité Totale"))
    } else if (input$choix_graphique == "top_commune") {
      top_communes <- base_commune %>%
        group_by(commune) %>%
        summarise(intensite_totale = sum(intensite, na.rm = TRUE)) %>%
        arrange(desc(intensite_totale)) %>%
        slice(1:5)
      
      plot_ly(top_communes, x = ~commune, y = ~intensite_totale, type = 'bar') %>%
        layout(title = "Top 5 Communes les Plus Éclairées en 2020",
               xaxis = list(title = "Commune"), yaxis = list(title = "Intensité Totale"))
    }
  })
  
  
  # Graphiques PIB
  output$table_pib <- DT::renderDataTable({ base_region2 })
  
  output$graphique_pib <- renderPlotly({
    region_data <- base_region3 %>%
      select(annee, all_of(input$choix_graphique_pib)) %>%
      rename(PIB = all_of(input$choix_graphique_pib))
    plot_ly(region_data, x = ~annee, y = ~PIB, type = 'scatter', mode = 'lines+markers')
  })
  # Taux de Chômage Estimé
  output$table_chomage <- DT::renderDataTable({
    base_region4
  })
  
  output$barplot_chomage <- renderPlotly({
    plot_ly(base_region4, x = ~region, y = ~taux_chomage, type = 'bar') %>%
      layout(title = "Répartition du Taux de Chômage par Région",
             xaxis = list(title = "Région"), yaxis = list(title = "Taux de Chômage (%)"))
  })
  
  
  # Images
  output$image_satellite <- renderImage({
    list(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\image_satellite.JPG", contentType = "image/jpg", width = "100%")
  }, deleteFile = FALSE)
  
  output$image_SIG <- renderImage({
    list(src = "C:\\Users\\HP\\Documents\\NDIAYE_JEAN_PIERRE_ADIOUMA\\ECODATALIGHT\\www\\image_SIG.JPG", contentType = "image/jpg", width = "100%")
  }, deleteFile = FALSE)
  
  output$table_il <- DT::renderDataTable({ base_intensite_lumineuses })
  
  
  # Calcul des PIB régionaux
  observeEvent(input$calculate_pib, {
    req(input$file_input)
    
    pib_data <- read_excel(input$file_input$datapath)
    
    validate(
      need(all(c("region", "NLTreg") %in% colnames(pib_data)), 
           "Le fichier doit contenir les colonnes 'region' et 'NLTreg'.")
    )
    
    NLTnat <- sum(pib_data$NLTreg, na.rm = TRUE)
    pib_data <- pib_data %>%
      mutate(PIB_reg = input$pib_national * (NLTreg / NLTnat))
    
    output$pib_table <- DT::renderDataTable({
      pib_data %>% select(region, PIB_reg) %>% rename(`PIB régionaux estimés` = PIB_reg)
    })
  })
  
  # Calcul des PIB départementaux
  observeEvent(input$calculate_pib_dep, {
    req(input$file_input_dep)
    
    dep_data <- read_excel(input$file_input_dep$datapath)
    
    validate(
      need(all(c("departement", "NLTdep") %in% colnames(dep_data)), 
           "Le fichier doit contenir les colonnes 'departement' et 'NLTdep'.")
    )
    
    NLTreg <- sum(dep_data$NLTdep, na.rm = TRUE)
    dep_data <- dep_data %>%
      mutate(PIB_dep = input$pib_regional * (NLTdep / NLTreg))
    
    output$pib_dep_table <- DT::renderDataTable({
      dep_data %>% select(departement, PIB_dep) %>% rename(`PIB départementaux estimés` = PIB_dep)
    })
  })
  
  # Calcul des PIB communaux
  observeEvent(input$calculate_pib_com, {
    req(input$file_input_com)
    
    com_data <- read_excel(input$file_input_com$datapath)
    
    validate(
      need(all(c("communes", "NLTcom") %in% colnames(com_data)), 
           "Le fichier doit contenir les colonnes 'communes' et 'NLTcom'.")
    )
    
    NLTdep <- sum(com_data$NLTcom, na.rm = TRUE)
    com_data <- com_data %>%
      mutate(PIB_com = input$pib_departemental * (NLTcom / NLTdep))
    
    output$pib_com_table <- DT::renderDataTable({
      com_data %>% select(communes, PIB_com) %>% rename(`PIB communaux estimés` = PIB_com)
    })
  })
  
  # Previsions des PIB regionaux
  output$previsions_pib <- renderPlotly({
    # Sélectionner les données de la région choisie
    region_data <- base_previsions_2012_2025 %>%
      select(Année, all_of(input$choix_graphique_pib)) %>%
      rename(PIB = all_of(input$choix_graphique_pib))
    
    # Séparer les données avant et après 2020
    data_historique <- region_data %>% filter(Année <= 2021)
    data_previsions <- region_data %>% filter(Année > 2020)
    
    # Créer le graphique
    p <- plot_ly() %>%
      # Ajouter les données historiques (2012-2020)
      add_trace(data = data_historique, x = ~Année, y = ~PIB, type = 'scatter', mode = 'lines+markers',
                line = list(color = 'blue'), marker = list(size = 6), name = 'Estimations') %>%
      # Ajouter les prévisions (2021-2025) en rouge
      add_trace(data = data_previsions, x = ~Année, y = ~PIB, type = 'scatter', mode = 'lines+markers',
                line = list(color = 'red'), marker = list(size = 6), name = 'Prévisions') 
    
    p  # Retourner le graphique
  })
  
  # Équipe
  
}

# Lancer l'application
shinyApp(ui = ui, server = server)
