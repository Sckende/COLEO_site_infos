setwd("/home/claire/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
rm(list = ls())
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
source("waffle_functions.R")
source("multiplot_functions.R")
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
                unique(sort(all_obs$obs_year))),
    uiOutput("echan"),
    h3("Conditions abiotiques du site sélectionné"),
    plotOutput("FakeTemp", height = 230),
    plotOutput("FakePrec", height = 250))),
  fluidRow(
      column(6,
             "",
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
    all_obs[all_obs$obs_year == input$an,]
  })
  output$echan <- renderUI({
    selectInput("hab_type", "Type d'habitat", c("Tous", sort(unique(obs_an()$hab_type))))
  })
  
  # -------- # 
  # Map output 
  output$map <- renderLeaflet({width = "100%"
                               height = 400
    if(input$hab_type == "Tous"){
      
      
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
        addCircleMarkers(lng = obs_an()$long_site[obs_an()$hab_type == input$hab_type], # Positionnement des sites avec les coordonnées long/lat
                         lat = obs_an()$lat_site[obs_an()$hab_type == input$hab_type],
                         radius = 8, # taille du cercle
                         popup = obs_an()$popup_info[obs_an()$hab_type == input$hab_type], # Ajout de fenêtres pop-up
                         color = unique(obs_an()$col[obs_an()$hab_type == input$hab_type]),
                         layerId = obs_an()$site_code[obs_an()$hab_type == input$hab_type])
      
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
    
    mess <- obs_an()[obs_an()$site_code == event$id, "name"]
    #message <- message %>% arrange(obs_species.taxa_name)
    
    # message <- obs_an()[obs_an()$site_code == event$id, "obs_species.taxa_name"]
    message <- data.frame(species = sort(unique(mess)))
    
    
    # output$donnees <- renderDataTable(message[!duplicated(message$obs_species.taxa_name),],
    #                                   options = list(pageLength = 10))
    output$donnees <- renderDataTable(message,
                                      options = list(pageLength = 10))
    
    # Obtention du waffle plot pour la répartition du type d'espèces observées TOUTES CAMPAGNES CONFONDUES
    # ------------------------------------------------------------------------

    site_count <- na.omit(plyr::count(obs_an()$category[obs_an()$site_code == event$id]))
    names(site_count)[1] <- "category"
    site_count <- dplyr::left_join(site_count, indic_count, by = "category")
    names(site_count)[1:4] <- c("category", "freq_site", "freq_tot", "prop_tot")
    site_count <- as.data.frame(rbind(as.matrix(site_count[, c(1, 2,5)]), as.matrix(site_count[, c(1, 3, 5)])))
    names(site_count)[2] <- "freq"
    site_count$data <- c(rep("site", length(unique(site_count$category))), rep("totale", length(unique(site_count$category))))
    site_count$freq <- as.numeric(as.character(site_count$freq))
    
    output$waff <- renderPlot({
      if(is.null(event$id)){
        
        waffle_chart(data = indic_count,
                     fill = "category",
                     value = "freq",
                     base_size = 20,
                     plot.title = "Proportion globale des indicateurs",
                     fill_title = "Indicateurs",
                     fill_colors = as.character(indic_count$cat_coul),
                     legend.position = "right")
        
      } else {
        if (length(unique(site_count$category)) == 1){
          waffle_chart(data = site_count,
                       fill = "data",
                       value = "freq",
                       fill_colors = c(as.character(unique(site_count$cat_coul)), "grey"))
        } else {
        
        plot_list <- list()
        
        for (i in 1: length(unique(site_count$category))){
          d <- site_count[site_count$category == unique(site_count$category)[i],]
          plot_list[[i]] <- waffle_chart(data = d,
                                         fill = "data",
                                         facet = "category",
                                         value = "freq",
                                         composition = FALSE,
                                         max_value = sum(site_count$freq[site_count$data == "totale"]),
                                         base_size = 20,
                                         fill_colors = c(as.character(unique(d$cat_coul)), "grey"))
        }
        

        
        multiplot(plotlist = plot_list, cols =  ceiling(length(plot_list)/2))
        
        }
      }
    })
    
    # Obtention des données à télécharger
    # -----------------------------------
    
    output$DL_data <- downloadHandler(
      filename = function() {
        paste(event$id, paste("_", unique(obs_an()$obs_year[obs_an()$site_code == event$id]), sep = ""), '.csv', sep="")
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
