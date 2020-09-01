setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(dplyr)
library(shinythemes)


if(!exists("all_obs")){source("Manipulations_rcoleo.R")}
#source("Manipulations_rcoleo.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
                fluidRow(
                  column(4,
                         selectInput("site",
                                     "Code du site",
                                     c("all", sort(unique(all_obs$site_code)))),
                         selectInput("hab",
                                     "Type d'habitat",
                                     c("all", sort(unique(all_sites$type)))
                         )
                         
                  ),
                  column(8,
                         leafletOutput("map"))
                  
                  
                ),
                fluidRow(
                dataTableOutput("donnees"))
)

# ---------------------------------------------------- #
# ---------------------------------------------------- #

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    if(input$hab == "all"){
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
  output$donnees <- renderDataTable(all_obs[all_obs$site_code == input$site,],                                     options = list(pageLength = 50))

}

# Run the application
shinyApp(ui = ui, server = server)
