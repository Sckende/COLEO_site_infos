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
    column(3,
           selectInput("an",
                       "Année d'échantillonnage des sites",
                       #c("toutes", unique(sort(all_obs$Y_creation))))
                        unique(sort(all_obs$Y_creation)))),
    column(3,
           uiOutput("echan"))
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
           #textOutput("donnees"))
           #tableOutput("donnees"))
    
  ),
  fluidRow(
    downloadButton("DL_data", "Télécharger")
  )
  
)

# ---------------------------------------------------- #
# ---------------------------------------------------- #

server <- function(input, output, session) {
  
  # -------- #
  # Listes déroulantes avec années, puis les types d'échantillonnage correspondants - Listes reliées
  
  obs_an = reactive({
    all_obs[all_obs$Y_creation == input$an,]
  })
  output$echan <- renderUI({
    selectInput("type_ech", "Type d'échantill", c("Tous", sort(unique(obs_an()$type_ech))))
  })
  
  # -------- # 
  # Map output 
  output$map <- renderLeaflet({
    if(input$type_ech == "Tous"){
      
      #data_obs <- all_obs %>% filter(Y_creation == obs_an()$Y_creation)
      
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = obs_an()$long_site, # Positionnement des sites avec les coordonnées long/lat
                         lat = obs_an()$lat_site,
                         radius = 8, # taille du cercle
                         popup = obs_an()$popup_info, # Ajout de fenêtres pop-up
                         color = obs_an()$col,
                         layerId = obs_an()$site_code)



      }
   # else{
    #   
    #   data_obs <- all_sites %>% filter(Y_creation == obs_an()$Y_creation, type == input$type_ech)
    #   
    #   leaflet() %>%
    #     addTiles() %>% # Affichage du fond de carte
    #     addCircleMarkers(lng = data_obs$long_site, # Positionnement des sites avec les coordonnées long/lat
    #                      lat = data_obs$lat_site,
    #                      radius = 8, # taille du cercle
    #                      popup = data_obs$popup_info, # Ajout de fenêtres pop-up
    #                      color = unique(data_obs$col))
    #   
    # }
        
      })
  
      # Click on a map marker
  observe({ 
    
    event <- input$map_marker_click

   #message <- head(all_obs[all_obs$site_code == event$id,])
   message <- all_obs[all_obs$site_code == event$id, c("site_code", "opened_at", "obs_species.taxa_name")]
    #message <- head(all_obs[all_obs$site_code == event$id, c("site_code", "opened_at", "obs_species.taxa_name")])
    #output$donnees <- renderTable(message)
    output$donnees <- renderDataTable(message,
                                      options = list(pageLength = 10))

    
    
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
  
  # DF_SP <- reactive({
  #   if(input$site == "tous"){
  #     DF_SP <- all_obs %>% arrange(obs_species.taxa_name) %>%
  #       select(obs_species.taxa_name, type) %>% arrange(obs_species.taxa_name)
  #   } else {
  #     DF_SP <- all_obs %>% filter(site_code == input$site) %>% 
  #       select(obs_species.taxa_name, type) %>% arrange(obs_species.taxa_name)
  #   }
  # })
  # output$donnees <- renderDataTable(
  #   
  #   DF_SP()[!duplicated(DF_SP()$obs_species.taxa_name),],
  #   options = list(pageLength = 10)
  #   
  # )
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
