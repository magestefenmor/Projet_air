library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readr)
library(jsonlite)
library(leaflet.extras)
library(highcharter)
library(purrr)
library(plotly)
library(tibble)
library(dendextend)
library(ggplot2)
library(cluster)
library(FactoMineR)
library(factoextra)
library(DT)
# Chargement des datasets
annual <- read_csv("E:/Projet_Tutoré/Donnees projet/bannualll.csv")
df <- read_csv("E:/Projet_Tutoré/Donnees projet/datAna_capi.csv")
data <- read_csv("E:/Projet_Tutoré/Donnees projet/dataset.csv")
taf <- read_csv("E:/Semestre 2/Projet_Tutoré/projet cheffe/datalyse.csv")


dflong <- taf %>%
  column_to_rownames(var = "Country")

# Prétraiter les données
dfscale <- scale(dflong)
print("Data scaled successfully")

# Effectuer le clustering hiérarchique
dist_matrix <- dist(dfscale)
hc <- hclust(dist_matrix, method = "ward.D2")
print("Hierarchical clustering performed successfully")

# Convertir en hclust
temp2 <- as.hclust(hc)


# Définir les seuils pour chaque indicateur
thresholds <- list(
  PM25 = list(
    "Bonne" = c(0, 10),
    "Moyenne" = c(10.1, 15),
    "Dégradée" = c(15.1, 25),
    "Mauvaise" = c(25.1, 35),
    "Très mauvaise" = c(35.1, 50),
    "Extrêmement mauvaise" = c(50.1, Inf)
  ),
  PM10 = list(
    "Bonne" = c(0, 20),
    "Moyenne" = c(20.1, 30),
    "Dégradée" = c(30.1, 40),
    "Mauvaise" = c(40.1, 50),
    "Très mauvaise" = c(50.1, 70),
    "Extrêmement mauvaise" = c(70.1, Inf)
  ),
  NO2 = list(
    "Bonne" = c(0, 10),
    "Moyenne" = c(10.1, 20),
    "Dégradée" = c(20.1, 30),
    "Mauvaise" = c(30.1, 40),
    "Très mauvaise" = c(40.1, 50),
    "Extrêmement mauvaise" = c(50.1, Inf)
  ),
  
  IQA = list(
    "Bonne" = c(0, 3),
    "Moyenne" = c(3.1, 4),
    "Dégradée" = c(4.1, 6),
    "Mauvaise" = c(6.1, 8),
    "Très mauvaise" = c(8.1, 10),
    "Extrêmement mauvaise" = c(10.1, Inf)
  ),
  O3 = list(
    "Bonne" = c(0, 60),
    "Moyenne" = c(60.1, 100),
    "Dégradée" = c(100.1, 120),
    "Mauvaise" = c(120.1, 160),
    "Très mauvaise" = c(160.1, 200),
    "Extrêmement mauvaise" = c(200.1, Inf)
  )
)

# Fonction pour définir la couleur en fonction de la valeur de l'indicateur et des seuils
get_color2 <- function(value, indicator) {
  if (is.na(value)) {
    return(NA)
  }
  indicator_thresholds <- thresholds[[indicator]]
  for (category in names(indicator_thresholds)) {
    if (value >= indicator_thresholds[[category]][1] && value <= indicator_thresholds[[category]][2]) {
      return(case_when(
        category == "Bonne" ~ "lightblue",
        category == "Moyenne" ~ "lightgreen",
        category == "Dégradée" ~ "yellow",
        category == "Mauvaise" ~ "orange",
        category == "Très mauvaise" ~ "red",
        category == "Extrêmement mauvaise" ~ "brown"
      ))
    }
  }
}

# Fonction pour définir la couleur en fonction de la valeur de PM2.5
get_color <- function(PM25) {
  if (PM25 <= 25) {
    return("lightblue")
  } else if (PM25 <= 50) {
    return("lightgreen")
  } else if (PM25 <= 75) {
    return("yellow")
  } else if (PM25 <= 100) {
    return("orange")
  } else if (PM25 <= 125) {
    return("red")
  } else if (PM25 <= 150) {
    return("purple")
  } else {
    return("brown")
  }
}

get_color1 <- function(IQAM) {
  if (IQAM <= 1) {
    return("lightblue")
  } else if (IQAM <= 3) {
    return("lightgreen")
  } else if (IQAM <= 4) {
    return("yellow")
  } else if (IQAM <= 6) {
    return("orange")
  } else if (IQAM <= 8) {
    return("red")
  } else if (IQAM <= 10) {
    return("purple")
  } else {
    return("brown")
  }
}


# Ajouter une colonne couleur au dataset
data <- data %>%
  mutate(color = sapply(PM25, get_color))

data <- data %>%
  mutate(color1 = sapply(IQAM, get_color1))

#### bloom graphe groupé
base_chart_data <- data %>%
  group_by(Continent) %>%
  summarise(
    Iqa = round(mean(IQAM, na.rm = TRUE),2)
  ) %>%
  arrange(desc(Iqa))

# Calcul des données pour le drilldown
drilldown_chart_data <- data %>%
  group_by(Continent, Country) %>%
  summarise(
    avg = round(mean(IQAM, na.rm = TRUE),2) 
  ) %>%
  group_nest(Continent) %>%
  mutate(
    id = Continent,
    type = "column",
    data = map(data, ~ .x %>% mutate(name = Country, y = avg)),
    data = map(data, list_parse)
  )

# Calcul des données pour le graphique de base
base_chart_data1 <- df %>%
  filter(Specie == "pm25") %>%
  group_by(Continent) %>%
  summarise(
    Pm =round(mean(median, na.rm = TRUE))  
  ) %>%
  arrange(desc(Pm))

# Calcul des données pour le drilldown
drilldown_chart_data1 <- df %>%
  filter(Specie == "pm25") %>%
  group_by(Continent, Country) %>%
  summarise(
    avg = round(mean(median, na.rm = TRUE))  
  ) %>%
  group_nest(Continent) %>%
  mutate(
    id = Continent,
    type = "column",
    data = map(data, ~ .x %>% mutate(name = Country, y = avg)),
    data = map(data, list_parse)
  )

# Filtrer les données pour exclure l'espèce "iqa" et préparer les moyennes annuelles par espèce
annual_median_avg <- df %>%
  filter(Specie != "aqi") %>%
  group_by(years, Specie) %>%
  summarise(annual_median_average = mean(median, na.rm = TRUE))

# Sélection des colonnes nécessaires.
data1 <- df %>% select(Date, Specie, median)


# Fonction pour définir l'icône en fonction de la valeur de l'indicateur et des seuils
get_icon <- function(value, indicator) {
  if (is.na(value)) {
    return(NA)
  }
  indicator_thresholds <- thresholds[[indicator]]
  for (category in names(indicator_thresholds)) {
    if (value >= indicator_thresholds[[category]][1] && value <= indicator_thresholds[[category]][2]) {
      return(case_when(
        category == "Bonne" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/50.18.png",
        category == "Moyenne" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/100.18.png",
        category == "Dégradée" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/150.18.png",
        category == "Mauvaise" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/200.18.png",
        category == "Très mauvaise" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/300.18.png",
        category == "Extrêmement mauvaise" ~ "E:/Projet_Tutoré/Donnees projet/panneaux/500.18.png"
      ))
    }
  }
}



# Définir la couleur de la barre latérale via CSS
sidebar_color_css <- "
.skin-blue .main-sidebar {
  background-color: #FFA500; /* Changer cette couleur selon vos besoins */
}
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
  background-color: #104E8B; /* Couleur de l'élément actif */
}
.skin-blue .main-sidebar .sidebar .sidebar-menu a {
  color: #FFFFFF; /* Couleur du texte des éléments de menu */
}
.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
  background-color: #4682B4; /* Couleur de survol */
}
"

# CSS personnalisé pour réduire la taille du selectInput et du sliderInput
custom_css <- "
.select-input-small .selectize-input {
  width: 150px;  /* Ajustez cette valeur pour modifier la largeur */
  font-size: 12px;  /* Ajustez cette valeur pour modifier la taille du texte */
}
.slider-input-small {
  width: 170px;  /* Ajustez cette valeur pour modifier la largeur */
  font-size: 10px;
}
"

ui <- dashboardPage(
  dashboardHeader(title = "Qualité de l'air"),
  dashboardSidebar(
    tags$head(tags$style(HTML(sidebar_color_css))), # Inclure le CSS personnalisé pour la barre latérale
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Statistique", tabName = "serie", icon = icon("chart-simple")),
      menuItem("IQA", tabName = "iqa", icon = icon("chart-bar")),
      #menuItem("Global", tabName = "global", icon = icon("chart")),
      menuItem("Classement Par Pays", tabName = "Classement_Pays", icon=icon("chart-line")),
      menuItem("Classification", tabName = "classification", icon = icon("dna")),
      menuItem("Analyse", tabName = "analyse", icon = icon("map"))
      
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))), # Inclure le CSS personnalisé pour le selectInput et le sliderInput
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Présentation des données sur carte", status = "primary", solidHeader = TRUE,
                  width = 12,
                  leafletOutput("lineageMap", height = 450),
                  fluidRow(
                    column(width = 6, div(class = "select-input-small",  # Appliquer la classe CSS personnalisée
                                          selectInput("indicator", "Choisissez un indicateur:", 
                                                      choices = c("IQA"="IQA","PM2.5" = "PM25", "PM10" = "PM10", "NO2" = "NO2", "O3" = "O3")))),
                    column(width = 4, offset = 2, div(class = "slider-input-small",  # Appliquer la classe CSS personnalisée
                                                      sliderInput("selected_year", "Sélectionnez l'année :", 
                                                                  min = min(annual$Year), max = max(annual$Year), value = min(annual$Year), sep = "")))
                  ),
                  HTML('
                      <div style="text-align: center; margin-top: 10px;">
                        <table style="width:100%; margin: auto;">
                          <tr>
                            <td style="background-color:#099D6E;color:white;text-align:center;padding:5px;">Bonne</td>
                            <td style="background-color:#FBDF3D;color:black;text-align:center;padding:5px;">Moyenne</td>
                            <td style="background-color:#FB9D3D;color:white;text-align:center;padding:5px;">Dégradée</td>
                            <td style="background-color:#CB0C3D;color:white;text-align:center;padding:5px;">Mauvaise</td>
                            <td style="background-color:#9400D3;color:white;text-align:center;padding:5px;">Très mauvaise</td>
                            <td style="background-color:#810C2E;color:white;text-align:center;padding:5px;">Extrêmement mauvaise</td>
                          </tr>
                        </table>
                      </div>')
                )
              )
      ),
      tabItem(tabName = "serie",
              fluidRow(
                column(12, navbarPage(title = "Nos Données dans le temps",
                                      tabPanel(h4("Variation annuelle des indicateurs"),
                                               plotlyOutput("timeSeriesPlot")),
                                      tabPanel(h4("Jour pour Jour"),
                                               sidebarPanel(
                                                 # Réglage de l'intervalle de date.
                                                 dateRangeInput(
                                                   "date_range",
                                                   label = "Intervalle de date",
                                                   start = "2020-04-01",
                                                   end = "2020-12-31",
                                                   min = "2019-01-01",
                                                   max = "2023-12-31",
                                                   separator = "-"
                                                 ),
                                                 # Réglage des checkbox.
                                                 checkboxGroupInput("pollutants",
                                                                    label = "Sélectionner un ou plusieurs polluants",
                                                                    choices = c("pm25", "no2", "pm10", "co", "so2", "o3", "humidity", "temperature"),
                                                                    selected = c("pm25", "no2", "pm10")
                                                 )
                                               ),
                                               plotOutput("air_quality_plot"))
                                      ))
                
              )),
      
      tabItem(tabName = "iqa",
              fluidRow(
                highchartOutput("hchart")
              )
      ),
      
      tabItem(tabName = "global",
              fluidRow(
                highchartOutput("hchart1")
              )
      ),
      
      tabItem((tabName = "Classement_Pays"),
              fluidRow(
                column(12,  navbarPage(title = "Classement en fonction du IQA durant la période de notre analyse",
                                       
                                       tabPanel(h4("Classement"),
                                                plotOutput("barPlot")
                                       ),
                                    
                                       tabPanel(h4("Africa"),
                                                tabsetPanel(
                                                  
                                                  box(title = "Afrique", status = "primary", solidHeader = TRUE,
                                                      leafletOutput("africa_map")),
                                                  box(title = "classement Afrique", status = "primary", solidHeader = TRUE,
                                                      plotOutput("africa_barPlot"))
                                                )),
                                       tabPanel(h4("Europe"),
                                                tabsetPanel(
                                                  
                                                  box(title = "Europe", status = "primary", solidHeader = TRUE,
                                                      leafletOutput("europe_map")),
                                                  box(title = "classement Europe", status = "primary", solidHeader = TRUE,
                                                      plotOutput("europe_barPlot"))
                                                  
                                                )),
                                       tabPanel(h4("America"),
                                                tabsetPanel(
                                                  
                                                  box(title = "Amerique", status = "primary", solidHeader = TRUE,
                                                      leafletOutput("amerique_map")),
                                                  box(title = "classement Amerique", status = "primary", solidHeader = TRUE,
                                                      plotOutput("amerique_barPlot"))
                                                )),
                                       tabPanel(h4("Asia"),
                                                tabsetPanel(
                                                 
                                                  box(title = "Asie", status = "primary", solidHeader = TRUE,
                                                      leafletOutput("asie_map")),
                                                  box(title = "classement Asie", status = "primary", solidHeader = TRUE,
                                                      plotOutput("asie_barPlot"))
                                                  
                                                )),
                                       tabPanel(h4("Oceania"),
                                                tabsetPanel(
                                                
                                                  box(title = "Oceanie", status = "primary", solidHeader = TRUE,
                                                      leafletOutput("oceanie_map")),
                                                  box(title = "classement Oceanie", status = "primary", solidHeader = TRUE,
                                                      plotOutput("oceanie_barPlot"))
                                                  
                                                ))
                ))
                
                
              )
              
      ),
      tabItem(tabName = "analyse",
              fluidRow(
                plotOutput("barPlot1"),
                #box(title = "Focus sur PM2.5", status = "primary", solidHeader = TRUE,
                    #"Content for Variant Maps")
              )
      ),
      tabItem(tabName = "classification",
              fluidRow(
                column(12,  
                       navbarPage(title = "Regroupement des individus",
                                  tabPanel(h4("Dendrogram"),
                                           plotOutput("histogram"),
                                           plotOutput("dendrogram"),
                                           numericInput("clusters", "Nombre de clusters:", 5, min = 1, max = 10),
                                           actionButton("run", "Run")
                                  ),
                                  tabPanel(h4("K-means"),
                                           plotOutput("clusterplot"),
                                           tableOutput("quanti_var_table"),
                                           uiOutput("quanti_stats"),
                                           uiOutput("cluster_elements")  # Sortie dynamique pour afficher les éléments de chaque cluster
                                  )
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Fonction pour filtrer les données en fonction des sélections de l'utilisateur
  filtered_data <- reactive({
    filter(annual, Year == input$selected_year & !is.na(get(input$indicator)))
  })
  
 
  # Placeholder plots, replace with actual plot functions
  output$totalGenomesPlot <- renderPlot({
    plot(cars)  # Placeholder plot function, replace with actual logic
  })
  
  output$newGenomesPlot <- renderPlot({
    plot(pressure)  # Placeholder plot function, replace with actual logic
  })
  output$hchart <- renderHighchart({
    # Création du graphique de base avec highcharter
    base_chart <- hchart(
      base_chart_data,
      "column",
      hcaes(x = Continent, y = Iqa, drilldown = Continent),
      name = "IQA",
      colorByPoint = TRUE
    ) 
    
    # Ajout de la prise en charge du forage (drilldown)
    base_chart |>
      hc_title(text = "Classement par Continent et leurs subdivisions") |>
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list_parse(drilldown_chart_data)
      )
  })
  
  
  output$hchart1 <- renderHighchart({
    # Création du graphique de base avec highcharter
    base_chart1 <- hchart(
      base_chart_data1,
      "column",
      hcaes(x = Continent, y = Pm, drilldown = Continent),
      name = "Pm2.5",
      colorByPoint = TRUE
    ) 
    
    # Ajout de la prise en charge du forage (drilldown)
    base_chart1 |>
      hc_title(text = "Classement par Continent et leur subdivision") |>
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = list_parse(drilldown_chart_data1)
      )
  })
  output$barPlot1 <- renderPlot({
    ggplot(data, aes(x = reorder(Country, PM25), y = PM25, fill = color)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "PM2.5", title = "Classement des pays selon le PM2.5") +
      theme_minimal()
  })
  
  output$barPlot <- renderPlot({
    ggplot(data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays selon le IQA") +
      theme_minimal()
  })
  
  africa_data <- data %>% filter(Continent == "Africa")
  
  output$africa_barPlot <- renderPlot({
    ggplot(africa_data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays africains selon le IQA") +
      theme_minimal()
  })
  
  output$africa_map <- renderLeaflet({
    leaflet(africa_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        weight = 1, radius = ~Population / 1000,
        popup = ~paste(Country, ": IQA =", IQAM, "<br>Population =", Population),
        color = ~color1
      )
  })
  
  europe_data <- data %>% filter(Continent == "Europe")
  
  output$europe_barPlot <- renderPlot({
    ggplot(europe_data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays Européens selon le IQA") +
      theme_minimal()
  })
  
  output$europe_map <- renderLeaflet({
    leaflet(europe_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        weight = 1, radius = ~Population / 1000,
        popup = ~paste(Country, ": IQA =", IQAM, "<br>Population =", Population),
        color = ~color1
      )
  })
  
  amerique_data <- data %>% filter(Continent == "America")
  
  output$amerique_barPlot <- renderPlot({
    ggplot(amerique_data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays americains selon le IQA") +
      theme_minimal()
  })
  
  output$amerique_map <- renderLeaflet({
    leaflet(amerique_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        weight = 1, radius = ~Population / 1000,
        popup = ~paste(Country, ": IQA =", IQAM, "<br>Population =", Population),
        color = ~color1
      )
  })
  
  asie_data <- data %>% filter(Continent == "Asia")
  
  output$asie_barPlot <- renderPlot({
    ggplot(asie_data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays d'asia selon le IQA") +
      theme_minimal()
  })
  
  output$asie_map <- renderLeaflet({
    leaflet(asie_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        weight = 1, radius = ~Population / 1000,
        popup = ~paste(Country, ": IQA =", IQAM, "<br>Population =", Population),
        color = ~color1
      )
  })
  
  oceanie_data <- data %>% filter(Continent == "Oceania")
  
  output$oceanie_barPlot <- renderPlot({
    ggplot(oceanie_data, aes(x = reorder(Country, IQAM), y = IQAM, fill = color1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_identity() +
      labs(x = "Pays", y = "IQA", title = "Classement des pays de l'oceanie selon le IQA") +
      theme_minimal()
  })
  
  output$oceanie_map <- renderLeaflet({
    leaflet(oceanie_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        weight = 1, radius = ~Population / 1000,
        popup = ~paste(Country, ": IQA =", IQAM, "<br>Population =", Population),
        color = ~color1
      )
  })
  #serie temporelle
  output$timeSeriesPlot <- renderPlotly({
    # Créer le plot avec ggplot
    p <- ggplot(annual_median_avg, aes(x = years, y = annual_median_average, color = Specie, group = Specie)) +
      geom_line() +
      geom_point() +
      labs(title = "Variation annuelle des indicateurs",
           x = "Année",
           y = "Moyenne annuelle") +
      theme_minimal()
    
    # Convertir en plotly pour interaction
    ggplotly(p)
  })
  
  # Configuration de l'intervalle de date.
  filtered_data1 <- reactive({
    data1 %>% filter(
      Date >= input$date_range[1],
      Date <= input$date_range[2],
      Specie %in% input$pollutants
    )
  })
  
  # Quelques couleurs pour chaque polluant.
  color_values <- reactive({
    colors <- c("pm25" = "blue", "pm10" = "red", "o3" = "green", 
                "no2" = "orange", "humidity" = "cyan", "temperature" = "brown", "co" = "purple", "so2" = "pink")
    colors[input$pollutants]
  })
  
  # Rendu des courbes du graphique.
  output$air_quality_plot <- renderPlot({
    gg <- ggplot(filtered_data1(), aes(x = Date, y = median, color = Specie)) +
      geom_line() +
      labs(
        x = "Date", y = "Valeur du polluant en µg/m³",
        title = "Qualité de l'air",
        color = "Légende"
      ) +
      scale_color_manual(values = color_values()) +
      theme_minimal() +
      theme(
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.margin = margin(10, 10, 10, 10)
      )
    gg
  })
  
  # Filtrer les données en fonction de l'année sélectionnée
  filtered_data2 <- reactive({
    annual %>%
      filter(Year == input$selected_year)
  })
  
  # Ajouter une colonne 'label' et 'icon' en utilisant case_when pour gérer les valeurs NA
  labeled_data2 <- reactive({
    filtered_data2() %>%
      mutate(
        label = case_when(
          !is.na(get(input$indicator)) ~ paste0("City: ", City, ", ", input$indicator, ": ", round(get(input$indicator), 2)),
          TRUE ~ NA_character_
        ),
        icon = sapply(get(input$indicator), get_icon, indicator = input$indicator)
      )
  })
  
  # Filtrer les données pour exclure les valeurs NA pour l'indicateur sélectionné
  filtered_labeled_data2 <- reactive({
    labeled_data2() %>%
      filter(!is.na(icon))
  })
  
  # Fonction pour mettre à jour la carte Leaflet
  output$lineageMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        data = filtered_labeled_data2(), 
        ~lng, ~lat,
        icon = ~icons(iconUrl = icon, iconWidth = 12, iconHeight = 20),
        label = ~label
      )
  })
  
   
  # Afficher l'histogramme des valeurs propres
  output$histogram <- renderPlot({
    barplot(rev(temp2$height), type = "h", ylab = "Hauteurs", main = "Histogramme des valeurs propres")
  })
  
  observeEvent(input$run, {
    # Effectuer le clustering K-means
    set.seed(123)
    km_model <- kmeans(dfscale, centers = input$clusters, nstart = 20)
    
    # Créer un plot des clusters
    output$clusterplot <- renderPlot({
      fviz_cluster(km_model, dfscale, ellipse.type = "norm") +
        labs(title = "K-means Clustering")
    })
    
    # Ajouter les clusters aux données
    dfcomp <- cbind.data.frame(dfscale, classe = factor(km_model$cluster))
    
    # Effectuer l'analyse CATDES
    catdes_result <- catdes(dfcomp, num.var = ncol(dfcomp))
    
    # Afficher les éléments de chaque cluster dans une table interactive DT
    output$cluster_elements <- renderUI({
      cluster_outputs <- lapply(1:input$clusters, function(cluster_index) {
        cluster_info <- catdes_result$quanti[[cluster_index]]
        datatable(cluster_info, options = list(pageLength = 10, searching = FALSE))  # Afficher les données sans barre de recherche
      })
      tagList(cluster_outputs)
    })
    
    # Afficher le dendrogramme
    output$dendrogram <- renderPlot({
      plot(hc, main = "Dendrogram", xlab = "", sub = "", cex = 0.9)
      rect.hclust(hc, k = input$clusters, border = 2:5)
    })
    
  })
  # Lancer l'application Shiny
  shinyApp(ui = ui, server = server)
  
  
}
  

shinyApp(ui, server)
