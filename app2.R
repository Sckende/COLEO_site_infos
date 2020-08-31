setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/Fichiers_tests")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(dplyr)
library(shinythemes)

source("Manipulations_rcoleo.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
  fluidRow(
    column(4,
           # selectInput("site",
           #             "Code du site",
           #             c("all", j$site_code)
#
#            ),
           selectInput("hab",
                       "Type d'habitat",
                       j$type
                       #c("all", j$type)
                       )

    ),
    column(8,
           leafletOutput("map"))


)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    # if(input$hab == "all"){
    #   leaflet() %>%
    #     addTiles() %>% # Affichage du fond de carte
    #     addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat
    #                      lat = j$lat_site,
    #                      radius = 8, # taille du cercle
    #                      popup = j$popup_info, # Ajout de fenêtres pop-up
    #                      color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])
    # }else{
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = j$long_site[j$type == input$hab], # Positionnement des sites avec les coordonnées long/lat
                         lat = j$lat_site[j$type == input$hab],
                         radius = 8, # taille du cercle
                         popup = j$popup_info, # Ajout de fenêtres pop-up
                         color = c("#66CC00", "#000066", "#666600", "#003333", "#0066CC", "#FF9900", "#660000")[as.integer(as.factor(j$type))])
   # }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
