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

runApp(list(
  ui = bootstrapPage(
    selectInput('an', 'année création', unique(sort(all_sites$Y_creation))),
    uiOutput('echan')
  ),
  server = function(input, output, session){
    site_an = reactive({
      all_sites[all_sites$Y_creation == input$an,]
    })
    output$echan <- renderUI({
      selectInput("type_ech", "Type d'échantill", sort(unique(site_an()$type)))
    })
      }
  
))


library(shiny)
runApp(list(
  ui = bootstrapPage(
    textInput("text", "Enter Formula", "a=b+c"),
    uiOutput('variables')
  ),
  server = function(input, output){
    outVar <- reactive({
      vars <- all.vars(parse(text = input$text))
      vars <- as.list(vars)
      return(vars)
    })
    
    output$variables = renderUI({
      selectInput('variables2', 'Variables', outVar())
    })
  }
))
