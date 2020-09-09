# setwd("C:/Users/HP_9470m/Desktop/PostDoc_COLEO/shiny_site_info/SITES_INFOS_tests/COLEO_site_infos")
# # selection of sites by shiny
# 
# library(shiny)
# library(rnaturalearth)
# library(sf)
# library(tidyverse)
# library(rcoleo)
# library(ggiraph)
# source("functions.R")
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("plot selection"),
# 
#     # Sidebar with a slider input for number of bins
#     fluidRow(
#         column(3,
#                verbatimTextOutput("site_name")
#         ),
# 
#         # Show a plot of the generated distribution
#         column(9,
#                girafeOutput("map_plot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     ne_can <- get_can_ne()
#     #obs_df <- get_obs_df()
#     cells <- rcoleo::sf_cells()
#     # obs_cells <- get_observed_cells(cells, obs_df)
#     obs_cells <- filter_cells_are_observed(cells)
#     cells_on_map <- plot_map_cells(ne_can, obs_cells, zoom = "montreal")
# 
#     output$map_plot <- renderGirafe({
#         # generate bins based on input$bins from ui.R
#         girafe(ggobj = cells_on_map,
#                options = list(opts_selection(type = "single",
#                                              only_shiny = TRUE,
#                                              selected = "135_104")) )
#     })
# 
#     output$site_name <- renderText({
# 
#         # browser()
# 
#         obs_cells$name
#         obs_cells %>%
#             filter(cell_code == input$map_plot_selected) %>%
#             pull("name")
# 
#         })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)



#### TESTS - HTML WIDGETS ####

# library(shiny)
# library(sigma)
# 
# gexf <- system.file("examples/ediaspora.gexf.xml", package = "sigma")
# 
# ui = shinyUI(fluidPage(
#   checkboxInput("drawEdges", "Draw Edges", value = TRUE),
#   checkboxInput("drawNodes", "Draw Nodes", value = TRUE),
#   sigmaOutput('sigma')
# ))
# 
# server = function(input, output) {
#   output$sigma <- renderSigma(
#     sigma(gexf, 
#           drawEdges = input$drawEdges, 
#           drawNodes = input$drawNodes)
#   )
# }
# 
# shinyApp(ui = ui, server = server)

#### Multiple selectInput choices ####

# runApp(list(
#   ui = bootstrapPage(
#     selectInput('an', 'année création', unique(sort(all_sites$Y_creation))),
#     uiOutput('echan')
#   ),
#   server = function(input, output, session){
#     site_an = reactive({
#       all_sites[all_sites$Y_creation == input$an,]
#     })
#     output$echan <- renderUI({
#       selectInput("type_ech", "Type d'échantill", sort(unique(site_an()$type)))
#     })
#       }
#   
# ))


# library(shiny)
# runApp(list(
#   ui = bootstrapPage(
#     textInput("text", "Enter Formula", "a=b+c"),
#     uiOutput('variables')
#   ),
#   server = function(input, output){
#     outVar <- reactive({
#       vars <- all.vars(parse(text = input$text))
#       vars <- as.list(vars)
#       return(vars)
#     })
#     
#     output$variables = renderUI({
#       selectInput('variables2', 'Variables', outVar())
#     })
#   }
# ))

#### Selecting data when clicking on a point - LEAFLET ####

# library(dplyr)
# library(shiny)
# library(leaflet)
# library(sf)
# 
# # NC counties - a shapefile shipped with the sf package
# shape <- st_read(system.file("shape/nc.shp", package ="sf")) %>% 
#   st_transform(shape, crs = 4326) %>% 
#   mutate(widgets = 300) %>% # a column of fake data
#   group_by(widgets) %>% 
#   summarize()
# 
# 
# # three cities - note the x and y coordinates
# points <- data.frame(name = c("Raleigh", "Greensboro", "Wilmington"),
#                      x = c(-78.633333, -79.819444, -77.912222),
#                      y = c(35.766667, 36.08, 34.223333),
#                      widgets = c(10, 20, 30)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326)
# 
# 
# # create unique ids for both data sets
# 
# shape$uid <- "P1"
# points$uid <- paste0("M", 1:3)
# 
# 
# # Define UI 
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Go Tar Heels!"),
#   
#   
#   verticalLayout(
#     # Top panel with widgets sold
#     wellPanel(
#       textOutput("widgets")
#     ),
#     
#     # the map itself
#     mainPanel(
#       leafletOutput("map")
#     )
#   )
# )
# 
# # Define server logic       
# server <- function(input, output) {
#   
#   output$map <- renderLeaflet({
#     
#     leaflet() %>% 
#       addProviderTiles("Stamen.Toner") %>% 
#       # addPolygons(data = shape, 
#       #             fillColor = "aliceblue", 
#       #             color = "grey",
#       #             layerId = ~uid) %>%  # unique id for polygons
#       addCircleMarkers(data = points, 
#                        fillColor = "red", 
#                        color = NA,
#                        radius = 10,
#                        fillOpacity = .75,
#                        layerId = ~uid)  # unique id for points
#   })
#   
#   # click on polygon
#   # observe({ 
#   #   
#   #   event <- input$map_shape_click
#   #   
#   #   message <- paste("widgets sold in North Carolina:", shape$widgets[shape$uid == event$id])
#   #   
#   #   output$widgets <- renderText(message)
#   #   
#   #   
#   #   
#   #   
#   # })
#   # click on a marker
#   observe({ 
#     
#     event <- input$map_marker_click
#     
#     message <- paste("widgets sold in", points$name[points$uid == event$id],":", points$widgets[points$uid == event$id]) 
#     
#     output$widgets <- renderText(message)
#     
#     
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)  
