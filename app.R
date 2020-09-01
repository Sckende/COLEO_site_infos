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



#### TESTS ####

ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
