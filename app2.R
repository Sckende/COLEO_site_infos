setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(dplyr)
library(shinythemes)


#if(!exists("all_obs_sp_df")){source("Manipulations_rcoleo.R")}

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
                  
                  
                ),
                fluidRow(#dataTableOutput("donnees"))
                  tableOutput("donnees"))
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
  output$donnees <- renderTable({
#    if(input$hab == "all"){
    renderTable(head(all_obs_sp_df))
#      renderDataTable(all_obs_sp_df, options = list(pageLength = 5))
 #   } else {
    #   renderTable(all_sp_df[all_obs_sp_df$type == input$hab], options = list(pageLength = 5))
    # }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
