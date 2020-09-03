setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(dplyr)
library(shinythemes)
library(hrbrthemes)
library(waffle)


if(!exists("all_obs")){source("Manipulations_rcoleo.R")}
#source("Manipulations_rcoleo.R")

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = shinytheme("slate"),
  fluidRow(
    column(4,
           selectInput("site",
                       "Code du site",
                       c("tous", sort(unique(all_obs$site_code))))),
    column(4,
           selectInput("hab",
                       "Type d'échantillonnage",
                       c("tous", sort(unique(all_sites$type))))
    )
  ),
  fluidRow(
    column(5,
           "Description site",
           fluidRow(
             column(12,
                    "Conditions abiotiques",
                    plotOutput("condAb"))
           )
    )
    ,
    column(7,
           leafletOutput("map"))
    
    
  ),
  fluidRow(
    column(6,
           "Répartition type espèce",
           plotOutput("waff")),
    column(6,
           dataTableOutput("donnees"))
    
  ),
  fluidRow(
    downloadButton("DL_data", "Télécharger")
  )
  
)

# ---------------------------------------------------- #
# ---------------------------------------------------- #

server <- function(input, output, session) {
  
  # -------- # 
  # Map output 
  output$map <- renderLeaflet({
    if(input$hab == "tous"){
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = all_sites$long_site, # Positionnement des sites avec les coordonnées long/lat
                         lat = all_sites$lat_site,
                         radius = 8, # taille du cercle
                         popup = all_sites$popup_info, # Ajout de fenêtres pop-up
                         color = all_sites$col)
    }else{
      
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = all_sites$long_site[all_sites$type == input$hab], # Positionnement des sites avec les coordonnées long/lat
                         lat = all_sites$lat_site[all_sites$type == input$hab],
                         radius = 8, # taille du cercle
                         popup = all_sites$popup_info[all_sites$type == input$hab], # Ajout de fenêtres pop-up
                         color = unique(all_sites$col[all_sites$type == input$hab]))
      
    }
  })
  
  # -------- # 
  output$condAb <- renderPlot(
    plot(iris$Sepal.Length, type = "l")
  )
  
  
  # -------- # 
  # Espèces par site #
  # ------- #
  
  # Waffle plot 
  
  xdf <- reactive({
    if(input$site == "tous"){
      all_obs %>%
        count(type)
    } else {
      all_obs %>%
        filter(site_code == input$site) %>%
        count(type)
    }
  })   
  n_row_2 <- reactive({
    if(input$site == "tous"){
      60
    } else {
      6
    }
  })
  
  output$waff <- renderPlot({
    ggplot(xdf(), aes(fill = type, values = n)) +
      geom_waffle(color = "white", size = 1.125, n_rows = n_row_2()) +
      coord_equal() +
      #labs(
      #title = paste("Site", input$site, sep = " ")
      #) +
      theme_ipsum_rc(grid="") +
      theme_enhance_waffle()
  })
  
  
  # Tableau de données #  
  
  DF_SP <- reactive({
    if(input$site == "tous"){
      DF_SP <- all_obs %>% arrange(obs_species.taxa_name) %>%
        select(obs_species.taxa_name, type) %>% arrange(obs_species.taxa_name)
    } else {
      DF_SP <- all_obs %>% filter(site_code == input$site) %>% 
        select(obs_species.taxa_name, type) %>% arrange(obs_species.taxa_name)
    }
  })
  output$donnees <- renderDataTable(
    
    DF_SP()[!duplicated(DF_SP()$obs_species.taxa_name),],
    options = list(pageLength = 10)
    
  )
  # -------- #  
  output$DL_data <- downloadHandler(
    filename = function() {
      paste(input$site, paste("_", Sys.Date(), sep = ""), '.csv', sep="")
    },
    content = function(file) {
      write.csv(DF_SP()[!duplicated(DF_SP()$obs_species.taxa_name),], file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
