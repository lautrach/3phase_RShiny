#app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggmap)
library(RColorBrewer)

trap_results = readRDS("/Users/laura/Desktop/3phase_RShiny/trapAug2022.rds")
trap_results_sens = readRDS("/Users/laura/Desktop/3phase_RShiny/trapsensAug2022.rds")

#Stadia Maps API 
ggmap::register_stadiamaps(key = 'API_Key')

#UI
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
      #HOME PAGE
      tabItem(tabName = "home",
              h2("3 Phase application (change name/title later)"),
              div(
                h3("Purpose"),
                p("Purpose of the application will go here..."),
                h3("Usage"),
                p("A brief outline of how to use the application will go here...")
              )
      ),
      #PHASE 1
      #      tabItem(tabName = "phase1",
      #h2("Phase 1"),
      #p("Content for Phase 1..."),
      #selectInput("selection", "Choose an option:",
                  #choices = c("Specificity Values(1062 traps) " = "specificity1",
      #For our analysis, we consider a total of 1062 traps
                              #"Sensitivity Values(326 traps)" = "sensitivity",
                              #"Specificity Values(326 traps)" = "specificity2"))
    #),
      tabItem(tabName = "phase1",
              h2("Phase 1"),
              fluidRow(
                column(4,
                       sliderInput(inputId = "yearRange", label = "Select Year Range:",
                                   min = 2004, max = 2018, value = c(2004, 2018), step = 1),
                       actionButton(inputId = "submitPhase1", label = "Submit")
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
      #PHASE 2
      tabItem(tabName = "phase2",
              h2("Phase 2"),
              p("Content for Phase 2...")
      ),
      #PHASE 3
      tabItem(tabName = "phase3",
              h2("Phase 3"),
              p("Content for Phase 3...")
      )
    )
  )
)

#SERVER LOGIC
server <- function(input, output) {
  
  #   processedDataPhase1 <- eventReactive(input$submitPhase1, {
  #   yearRange <- str_split(input$yearRange, ",", simplify = TRUE)
  #   yearRange <- as.numeric(yearRange)
  #   dataList <- lapply(yearRange, preprocess_data)
  #   do.call(rbind, dataList)
  # })
    processedDataPhase1 <- trap_results
    
    generateMapPlot <- function(data) {
      map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
      coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
      coords.map <- ggmap(coords.map, extent="device", legend="none")
      (coords.map <- coords.map + geom_point(data=trap_results,  
                                             aes(x=long, y=lat), 
                                             fill="green", shape=23,
                                             alpha=0.8,size = 1) ) 
      return(coords.map)
    }
    
  output$mapPlot <- renderPlot({
    
    generateMapPlot(processedDataPhase1)  
  })
  
  output$dataView <- renderTable({
    
    head(processedDataPhase1)
  })
  
  output$analysisResults <- renderTable({
 
    # analysisResults <- performAnalysis(processedDataPhase1) 
    analysisResults <- processedDataPhase1
  })
  
}


shinyApp(ui = ui, server = server)

