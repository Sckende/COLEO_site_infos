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
ui <- fluidPage(theme = shinytheme("slate"),
                fluidRow(
                  column(4,
                         selectInput("site",
                                     "Code du site",
                                     c("all", sort(unique(all_obs$site_code))))),
                  column(4,
                         selectInput("hab",
                                     "Type d'habitat",
                                     c("all", sort(unique(all_sites$type))))
                         )
                ),
                fluidRow(
                  column(6,
                         plotOutput("waff"))
                  ,
                  column(6,
                         leafletOutput("map"))
                  
                  
                ),
                fluidRow(
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
  
  
  # Map output 
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
  
  
  output$waff <- renderPlot({
   xdf <- all_obs %>%  filter(site_code == input$site) %>% count(type)
    
    ggplot(xdf, aes(fill=type, values=n)) +
      geom_waffle(color = "white", size=1.125, n_rows = 6) +
      coord_equal() +
      labs(
        title = paste("Répartition du type d'espèces sur le site", input$site, sep = " ")
      ) +
      theme_ipsum_rc(grid="") +
      theme_enhance_waffle()
  })
  
  output$donnees <- renderDataTable(all_obs[all_obs$site_code == input$site,],                                     options = list(pageLength = 20))
  
  output$DL_data <- downloadHandler(
    filename = function() {
          paste(input$site, paste("_", Sys.Date(), sep = ""), '.csv', sep="")
        },
        content = function(file) {
          write.csv(all_obs[all_obs$site_code == input$site,], file)
        }
  )

}

# Run the application
shinyApp(ui = ui, server = server)
