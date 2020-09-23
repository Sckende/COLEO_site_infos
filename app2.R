setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
library(shiny)
library(tidyverse)
library(rcoleo)
library(leaflet)
library(plyr)
library(dplyr)
library(shinythemes)
library(hrbrthemes)
library(waffle)
library(shinydashboard)


if(!exists("all_obs")){source("Manipulations_rcoleo.R")}
#source("Manipulations_rcoleo.R")

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = shinytheme("slate"),
  fluidRow(
  tags$style(type = "text/css", "#map {height: calc(100vh) !important;}", HTML("#controls {background-color: rgba(0,0,0,0.2); font-color: rgba(1,1,1,0)}")),
    # tags$style(type = "text/css", "#map {height: calc(100vh) !important;}", HTML("#controls {background-color: rgba(0,0,0); opacity: 0.2}") ),
  leafletOutput("map"),
  absolutePanel(id = "controls",
                class = "panel panel-default",
               fixed = FALSE,
                draggable = FALSE,
                top = 0,
                left = "auto",
                right = 20,
                bottom = "auto",
                width = 330,
                #height = "auto",
                height = 665,
    selectInput("an",
                "Année d'échantillonnage des sites",
                unique(sort(all_obs$Y_obs))),
    uiOutput("echan"),
    h3("Conditions abiotiques du site sélectionné"),
    plotOutput("FakeTemp", height = 230),
    plotOutput("FakePrec", height = 250))),
  fluidRow(
      column(6,
             "Répartition type d'échantillonnage",
             plotOutput("waff")),
      #textOutput("waff")),

      column(6,
             dataTableOutput("donnees"))

    ),
  fluidRow(
    downloadButton("DL_data", "Télécharger"))
  )

# -------------------------------------------------------- #
# ------------ When the magic happens -------------------- #
# -------------------------------------------------------- #

server <- function(input, output, session) {
  
  # -------- #
  # Listes déroulantes avec années, puis les types d'échantillonnage correspondants - Listes reliées
  
  obs_an = reactive({
    all_obs[all_obs$Y_obs == input$an,]
  })
  output$echan <- renderUI({
    selectInput("type_ech", "Type d'habitat", c("Tous", sort(unique(obs_an()$type_ech))))
  })
  
  # -------- # 
  # Map output 
  output$map <- renderLeaflet({width = "100%"
                               height = 400
    if(input$type_ech == "Tous"){
      
      
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = obs_an()$long_site, # Positionnement des sites avec les coordonnées long/lat
                         lat = obs_an()$lat_site,
                         radius = 8, # taille du cercle
                         popup = obs_an()$popup_info, # Ajout de fenêtres pop-up
                         color = obs_an()$col,
                         layerId = obs_an()$site_code)
      
      
      
    } else {
      
      leaflet() %>%
        addTiles() %>% # Affichage du fond de carte
        addCircleMarkers(lng = obs_an()$long_site[obs_an()$type_ech == input$type_ech], # Positionnement des sites avec les coordonnées long/lat
                         lat = obs_an()$lat_site[obs_an()$type_ech == input$type_ech],
                         radius = 8, # taille du cercle
                         popup = obs_an()$popup_info[obs_an()$type_ech == input$type_ech], # Ajout de fenêtres pop-up
                         color = unique(obs_an()$col[obs_an()$type_ech == input$type_ech]),
                         layerId = obs_an()$site_code[obs_an()$type_ech == input$type_ech])
      
    }
    
  })
  # ---------------------- #
  # Click on a map marker #
  # --------------------- #
  
  # 
  observe({ 
    
    event <- input$map_marker_click
    
    # Obtention de la description du site et des conditions météorologiques

    output$FakeTemp <- renderPlot({

       if (is.null(event))
         return(NULL)

      par(bg = "transparent")
      cell <- unique(obs_an()$cell_id[obs_an()$site_code == event$id])
      
      plot(FakeTemp$Temp[FakeTemp$cell_id == cell],
           type = "l",
           bty = "n",
           xlim = c(1,108),
           xlab = "Time",
           ylab = "Températures")
    })
    
    output$FakePrec <- renderPlot({
      
      
      if (is.null(event))
        return(NULL)
      
      par(bg = "transparent")

      cell <- unique(obs_an()$cell_id[obs_an()$site_code == event$id])

      barplot(FakePrec$Prec[FakePrec$cell_id == cell],
              xlab = "Time",
              ylab = "Précipitations cumulées")
    })
    
    # Obtention de la liste des espèces observées lors de l'échantillonnage TOUTES CAMPAGNES CONFONDUES
    #----------------------------------------------------------------------
    
    mess <- obs_an()[obs_an()$site_code == event$id, "obs_species.taxa_name"]
    #message <- message %>% arrange(obs_species.taxa_name)
    
    # message <- obs_an()[obs_an()$site_code == event$id, "obs_species.taxa_name"]
    message <- data.frame(species = sort(unique(mess)))
    
    
    # output$donnees <- renderDataTable(message[!duplicated(message$obs_species.taxa_name),],
    #                                   options = list(pageLength = 10))
    output$donnees <- renderDataTable(message,
                                      options = list(pageLength = 10))
    
    # Obtention du waffle plot pour la répartition du type d'espèces observées TOUTES CAMPAGNES CONFONDUES
    # ------------------------------------------------------------------------
    
    wa <- plyr::count(obs_an()$type[obs_an()$site_code == event$id])
    
    output$waff <- renderPlot({
      ggplot(wa, aes(fill = x, values = freq)) +
        geom_waffle(color = "white", size = 1.125, n_rows = 6) +
        coord_equal() +
        #labs(
        #title = paste("Site", input$site, sep = " ")
        #) +
        theme_ipsum_rc(grid="") +
        theme_enhance_waffle()
    })
    
    # Obtention des données à télécharger
    # -----------------------------------
    
    output$DL_data <- downloadHandler(
      filename = function() {
        paste(event$id, paste("_", unique(obs_an()$Y_obs[obs_an()$site_code == event$id]), sep = ""), '.csv', sep="")
      },
      content = function(file) {
        write.csv(message, file)
      }
    )
  })
  
  
  # n_row_2 <- reactive({
  #   if(input$site == "tous"){
  #     60
  #   } else {
  #     6
  #   }
  # })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
