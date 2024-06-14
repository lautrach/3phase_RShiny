#app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggmap)
library(RColorBrewer)

# Stadia Maps API 
ggmap::register_stadiamaps(key = 'api_key') #probably need to figure something out for this...

# UI
ui <- dashboardPage(
  dashboardHeader(title = "3 Phase"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Phase 1", tabName = "phase1"),
      menuItem("Phase 2", tabName = "phase2"),
      menuItem("Phase 3", tabName = "phase3")
    )
  ),
  dashboardBody(
    tabItems(
      # HOME PAGE
      tabItem(tabName = "home",
              h2("3 Phase application (change name/title later)"),
              div(
                h3("Purpose"),
                p("Purpose of the application will go here..."),
                h3("Usage"),
                p("A brief outline of how to use the application will go here...")
              )
      ),
      # PHASE 1
      tabItem(tabName = "phase1",
              h2("Phase 1"),
              fluidRow(
                column(4,
                       fileInput("file1", "Choose RDS File", accept = ".rds"),  # File input for uploading RDS files
                       actionButton("load", "Load Data")
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Map", plotOutput("mapPlot")),
                         tabPanel("Data", tableOutput("dataView")),
                         tabPanel("Analysis Results", tableOutput("analysisResults"))
                       )
                )
              )
      ),
      # PHASE 2
      tabItem(tabName = "phase2",
              h2("Phase 2"),
              p("Content for Phase 2...")
      ),
      # PHASE 3
      tabItem(tabName = "phase3",
              h2("Phase 3"),
              p("Content for Phase 3...")
      )
    )
  )
)

# SERVER LOGIC
server <- function(input, output) {
  trap_results <- reactive({
    req(input$file1)  # Require that the file input is not NULL
    readRDS(input$file1$datapath)
  })
  
  observeEvent(input$load, {
    trap_results_sens <- trap_results()  # Use the uploaded data as trap_results_sens
  })
  
  output$loadedData <- renderTable({
    req(trap_results())
    head(trap_results())
  })
  
  generateMapPlot <- function(data) {
    map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
    coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
    coords.map <- ggmap(coords.map, extent = "device", legend = "none")
    (coords.map <- coords.map + geom_point(data = data,  
                                           aes(x = long, y = lat), 
                                           fill = "green", shape = 23,
                                           alpha = 0.8, size = 1))
    return(coords.map)
  }
  
  output$mapPlot <- renderPlot({
    req(trap_results())
    generateMapPlot(trap_results())
  })
  
  output$dataView <- renderTable({
    req(trap_results())
    head(trap_results())
  })
  
  output$analysisResults <- renderTable({
    req(trap_results())
    trap_results()  # Display the uploaded data in the 'Analysis Results' tab
  })
}

shinyApp(ui = ui, server = server)
