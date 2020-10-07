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
library(plotly)


if(!exists("all_obs")){source("Manipulations_rcoleo.R")}
#source("multiplot_functions.R")
#source("regions_Ouranos_vs_cellules_COLEO.R")

# Define UI for application that draws a histogram
ui <- fluidPage(#theme = shinytheme("slate"),
    fluidRow(
        tags$style(type = "text/css", "#map {height: calc(100vh) !important;}", HTML("#controls {background-color: rgba(255,255,255,0.5); font-color: rgba(1,1,1,0)}")),
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
                      #width = 330,
                      width = "auto",
                      height = "auto",
                      #height = 842,
                      selectInput("an",
                                  "Année d'échantillonnage des sites",
                                  unique(sort(all_obs$obs_year))),
                      uiOutput("echan"),
                      h3("Conditions météorologiques du site"),

                      plotlyOutput("TempPrec")),
    absolutePanel(id = "controls",
                  class = "panel panel-default",
                  fixed = FALSE,
                  draggable = FALSE,
                  top = 0,
                  left = 20,
                  right = "auto",
                  bottom = "auto",
                  #width = 330,
                  width = "auto",
                  height = "auto",
                  #height = 842,
                  h3("Scénarios changements climatiques pour la région"),
                  plotlyOutput("scenarios_temp"),
                  plotlyOutput("scenarios_prec")
    ),
    fluidRow(
        column(6,
               "",
               plotlyOutput("waff")),
        
        column(6,
               dataTableOutput("donnees"))
        
    ),
    fluidRow(
        downloadButton("DL_data", "Télécharger"))
)
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
        
        output$TempPrec <- renderPlotly({
            
            if (is.null(event))
                return(NULL)
            
            cell <- unique(obs_an()$cell_id[obs_an()$site_code == event$id])
            temp <- meteoCELLSdf[meteoCELLSdf$cell_id == cell & meteoCELLSdf$indic_meteo == "Temp",]
            temp$Month <- factor(temp$Month, temp$Month)
            prec <- meteoCELLSdf[meteoCELLSdf$cell_id == cell & meteoCELLSdf$indic_meteo == "Prec",]
            prec$Month <- factor(prec$Month, prec$Month)

            # ---- #
            
            ay <- list(
                tickfont = list(color = "green"),
                overlaying = "y",
                side = "right",
                title = "Précipitations cumulées (mm)",
                showgrid = F)
            
            over <- plot_ly()
            over <- over %>% add_lines(x = temp$Month,
                                       y = temp$Value,
                                       type = "scatter",
                                       mode = "lines+markers",
                                       marker = list(symbol = "circle",
                                                     color = 'darkorange'),
                                       line = list(color = "darkorange"),
                                       fill = "tozeroy",
                                       fillcolor = "rgba(255,126,0,0.4)",
                                       name = "Températures")
            over <- over %>% add_trace(x = prec$Month,
                                       y = prec$Value,
                                       name = "Précipitations",
                                       type = "bar",
                                       opacity = 0.5,
                                       orientation = "v",
                                       marker = list(color = "green"),
                                       yaxis = "y2")
            over <- over %>%  layout(title = event$id,
                                     yaxis = list(title = "Températures moyennes (C)",
                                                  tickfont = list(color = "darkorange"),
                                                  showgrid = F),
                                     yaxis2 = ay,
                                     xaxis = list(title = "Mois",
                                                  showgrid = F),
                                     showlegend = TRUE,
                                     legend = list(orientation = 'v', # show entries horizontally
                                                   xanchor = "center", # use center of legend as anchor
                                                   x = 0.5,
                                                   y = -1), #put legend in center of x-axis
                                     margin = list(I = 80,
                                                   r = 80,
                                                   t = 100,
                                                   b = 100,
                                                   autoexpand = TRUE)) %>%
                config(displayModeBar = FALSE) %>% 
                layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
                layout(paper_bgcolor = "rgba(254, 247, 234, 0)")
            over
        })
        
        output$scenarios_temp <- renderPlotly({
            
            
            if (is.null(event))
                return(NULL)
            
            #---#
            reg <- unique(obs_an()$Region[obs_an()$site_code == event$id])
            scenar <- scenario_meteo[scenario_meteo$Region == reg,]
            
            #---#
            figTemp <- plot_ly(x = scenar$Annee[scenar$param_met == "temp"],
                               y = scenar$Obs[scenar$param_met == "temp"],
                               type = "scatter",
                               mode = "lines",
                               line = list(color = 'darkorange'),
                               name = "hist_temp",
                               showlegend = TRUE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$Hist.Max[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "hist_max",
                          showlegend = FALSE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$Hist.Min[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "hist_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>% add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                                             y = scenar$rcp45.Avg[scenar$param_met == "temp"],
                                             type = "scatter",
                                             mode = "lines",
                                             line = list(color = 'rgba(153,102,0,1)'),
                                             name = "emission_moderees",
                                             showlegend = TRUE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$rcp45.Max[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "emission_moderee_max",
                          showlegend = FALSE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$rcp45.Min[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(153,102,0,0.2)',
                          line = list(color = 'transparent'),
                          name = "emission_moderee_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>% add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                                             y = scenar$rcp85.Avg[scenar$param_met == "temp"],
                                             type = "scatter",
                                             mode = "lines",
                                             line = list(color = "rgba(255,51,51,1)"),
                                             name = "emission_fortes",
                                             showlegend = TRUE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$rcp85.Max[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor='rgba(255,51,51,0.2)',
                          line = list(color = 'transparent'),
                          name = "emission_forte_max",
                          showlegend = FALSE) %>% 
                add_trace(x = scenar$Annee[scenar$param_met == "temp"],
                          y = scenar$rcp85.Min[scenar$param_met == "temp"],
                          type = "scatter",
                          mode = "lines",
                          fill = "tonexty",
                          fillcolor= "rgba(255,51,51,0.2)",
                          line = list(color = 'transparent'),
                          name = "emission_forte_min",
                          showlegend = FALSE)
            figTemp <- figTemp %>% 
                layout(title = reg,
                       yaxis = list(title = "Températures moyennes (C)",
                                    #tickfont = list(color = "darkorange"),
                                    showgrid = F),
                       xaxis = list(title = "Année",
                                    showgrid = F),
                       showlegend = TRUE,
                       legend = list(orientation = 'h', # show entries horizontally
                                     xanchor = "center", # use center of legend as anchor
                                     x = 0.5,
                                     y = -0.5), #put legend in center of x-axis
                       margin = list(I = 80,
                                     r = 80,
                                     t = 100,
                                     b = 100,
                                     autoexpand = TRUE)) %>%
                layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
                layout(paper_bgcolor = "rgba(254, 247, 234, 0)") %>%
                config(displayModeBar = FALSE)
            })

        #---#
        output$scenarios_prec <- renderPlotly({
            
            
            if (is.null(event))
                return(NULL)
            #---#
            reg <- unique(obs_an()$Region[obs_an()$site_code == event$id])
            scenar <- scenario_meteo[scenario_meteo$Region == reg,]
            #---#
        figPrec <- plot_ly(x = scenar$Annee[scenar$param_met == "prec"],
                           y = scenar$Obs[scenar$param_met == "prec"],
                           type = "scatter",
                           mode = "lines",
                           #line = list(color = 'rgb(59,122,128'),
                           line = list(color = "green"),
                           name = "hist_prec",
                           showlegend = TRUE)
        figPrec <- figPrec %>% add_trace(x = scenar$Annee[scenar$param_met == "prec"],
                                         y = scenar$rcp45.Avg[scenar$param_met == "prec"],
                                         type = "scatter",
                                         mode = "lines",
                                         line = list(color = 'rgba(153,102,0,1)'),
                                         name = "emission_moderees",
                                         showlegend = TRUE)
        figPrec <- figPrec %>% add_trace(x = scenar$Annee[scenar$param_met == "prec"],
                                         y = scenar$rcp85.Avg[scenar$param_met == "prec"],
                                         type = "scatter",
                                         mode = "lines",
                                         line = list(color = "rgba(255,51,51,1)"),
                                         name = "emission_fortes",
                                         showlegend = TRUE)
        figPrec <- figPrec %>% 
            layout(yaxis = list(title = "Précipitations cumulées",
                                showgrid = F),
                   xaxis = list(title = "Année",
                                showgrid = F),
                   showlegend = TRUE,
                   legend = list(orientation = 'h', # show entries horizontally
                                 xanchor = "center", # use center of legend as anchor
                                 x = 0.5,
                                 y = -0.5), #put legend in center of x-axis
                   margin = list(I = 80,
                                 r = 80,
                                 t = 20,
                                 b = 100,
                                 autoexpand = TRUE)) %>%
            layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>%
            layout(paper_bgcolor = "rgba(254, 247, 234, 0)") %>%
            config(displayModeBar = FALSE)
        #---#
        # scen_fig <- subplot(figTemp,
        #                     figPrec,
        #                     nrows = 2,
        #                     shareX = TRUE)
        # scen_fig <- scen_fig %>% 
        #     layout(title = reg,
        #            yaxis = list(title = "Températures moyennes (C)",
        #                         tickfont = list(color = "darkorange"),
        #                         showgrid = F),
        #            yaxis2 = list(title = "Précipitations cumulées",
        #                          tickfont = list(color = "green"),
        #                          showgrid = F),
        #            xaxis = list(title = "Mois",
        #                         showgrid = F),
        #            showlegend = TRUE,
        #            legend = list(orientation = 'v', # show entries horizontally
        #                          xanchor = "center", # use center of legend as anchor
        #                          x = 0.5,
        #                          y = -1), #put legend in center of x-axis
        #            margin = list(I = 80,
        #                          r = 80,
        #                          t = 100,
        #                          b = 100,
        #                          autoexpand = TRUE)) %>% 
        #     layout(plot_bgcolor = "rgba(254, 247, 234, 0)") %>% 
        #     layout(paper_bgcolor = "rgba(254, 247, 234, 0)") %>% 
        #     config(displayModeBar = FALSE)
        # scen_fig

    })
        # Obtention de la liste des espèces observées lors de l'échantillonnage TOUTES CAMPAGNES CONFONDUES
        #----------------------------------------------------------------------
        
        mess <- obs_an()[obs_an()$site_code == event$id, "name"]
        message <- data.frame(species = sort(unique(mess)))
        
        if(is.null(event$id)){
            output$donnees <- renderDataTable(data.frame(espèce = sort(unique(all_obs$name))),
                                              options = list(pageLength = 10))
        } else {
            output$donnees <- renderDataTable(message,
                                              options = list(pageLength = 10))
        }


        
        # Obtention du bar plot pour la répartition du type d'espèces observées TOUTES CAMPAGNES CONFONDUES
        # ------------------------------------------------------------------------
        
        site_count <- na.omit(plyr::count(obs_an()$category[obs_an()$site_code == event$id]))
        names(site_count)[1] <- "category"
        site_count <- dplyr::left_join(site_count, indic_count, by = "category")
        names(site_count)[1:4] <- c("category", "freq_site", "freq_tot", "prop_tot")
        
        if(length(site_count$category) != length(indic_count$category)){
            for (i in setdiff(indic_count$category, site_count$category)){
                newline <- c(i, 0, indic_count$freq[indic_count$category == i], indic_count$prop[indic_count$category == i])
                site_count <- rbind(site_count, newline)
            }
        }
        


        output$waff <- renderPlotly({
            if(is.null(event$id)){
                
                figure <- plot_ly(data = indic_count,
                                  x = ~freq,
                                  y = ~category,
                                  type = "bar",
                                  orientation = "h",
                                  color = ~category,
                                  colors = "Dark2",
                                  opacity = 1,
                                  name = "Total") %>%
                    layout(xaxis = list(title = "Occurence"),
                           yaxis = list(title = ""),
                           showlegend = F) %>% 
                    config(displayModeBar = FALSE)
                figure

            } else {

                figure <- plot_ly(
                    #data = site_count,
                                  x = as.numeric(site_count$freq_tot),
                                  y = site_count$category,
                                  type = "bar",
                                  orientation = "h",
                                  color = site_count$category,
                                  colors = "Dark2",
                                  opacity = 0.1,
                                  name = "Total") %>%
                    add_trace(x = as.numeric(site_count$freq_site),
                              y = site_count$category,
                              type = "bar",
                              orientation = "h",
                              color = site_count$category,
                              colors = "Dark2",
                              opacity = 1,
                              name = "Site") %>%
                    layout(barmode = "overlay",
                           xaxis = list(title = "Occurence"),
                           yaxis = list(title = ""),
                           showlegend = F) %>% 
                    config(displayModeBar = FALSE)
                figure
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
    
}

# Run the application
shinyApp(ui = ui, server = server)
