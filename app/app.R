#app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggmap)
library(RColorBrewer)

# Stadia Maps API 
ggmap::register_stadiamaps(key = '170f7302-0a84-41f9-b560-ff13b8e1c647') #probably need to figure something out for this...

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
                       fileInput("file1", "Choose RDS File", accept = ".rds"),  
                       actionButton("load", "Load Data")
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Map", plotOutput("mapPlot")),
                         tabPanel("Histogram", plotOutput("histogramPlot")),
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
    trap_results_sens <- trap_results()  
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

  generateHistogram <- function(data) {
    specihist <- ggplot(data = data, aes(x = avg_specificity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept = mean(avg_specificity)),
                 color = "blue", linetype = "dashed", size = 1) +
      ggtitle("Histogram of Specificity values for Model 1")
    return(specihist)
  }
  
  # generateMapPlot <- function(data){ 
  # map_bounds <- c(left = -88.8, bottom = 41.3, right = -87.3, top = 42.4)
  # coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
  # coords.map <- ggmap(coords.map, extent="device", legend="none")
  # coords.map <- coords.map %+% trap_results_sens + aes(x = long,y = lat,z = score) +
  #   stat_summary_2d(fun = median,geom = "tile", 
  #                   binwidth = c(0.028, 0.028),
  #                   alpha = 0.80)+
  #   scale_fill_gradientn(colours=(brewer.pal(7, "RdYlGn")))
  # coords.map <- coords.map + theme_bw()
  # return(coords.map)
  # }
  # 
  output$mapPlot <- renderPlot({
    req(trap_results())
    generateMapPlot(trap_results())
  })

  output$histogramPlot <- renderPlot({
    req(trap_results())
    generateHistogram(trap_results())
  })
  
  output$dataView <- renderTable({
    req(trap_results())
    head(trap_results())
  })
  
  output$analysisResults <- renderTable({
    req(trap_results())
    trap_results()
  })
}

shinyApp(ui = ui, server = server)
