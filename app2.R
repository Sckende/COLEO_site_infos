setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(dplyr)
library(shinythemes)
#df = data.frame(Lat = -9:10, Long = rnorm(20), hab = sample(paste("hab", 1:5), 20, replace = T))

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
                                     #sort(unique(df$hab)),
                                     c("all", sort(unique(j$type)))
                         )
                         
                  ),
                  column(8,
                         leafletOutput("map"))
                  
                  
                )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    if(input$hab == "all"){
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = j$long_site, # Positionnement des sites avec les coordonnées long/lat
                         lat = j$lat_site,
                         radius = 8, # taille du cercle
                         popup = j$popup_info, # Ajout de fenêtres pop-up
                         color = j$col)
    }else{
      
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = j$long_site[j$type == input$hab], # Positionnement des sites avec les coordonnées long/lat
                         lat = j$lat_site[j$type == input$hab],
                         radius = 8, # taille du cercle
                         popup = j$popup_info[j$type == input$hab], # Ajout de fenêtres pop-up
                         color = unique(j$col[j$type == input$hab]))
      
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
