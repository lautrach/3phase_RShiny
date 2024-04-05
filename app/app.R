#app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)

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
    processedDataPhase1 <- eventReactive(input$submitPhase1, {
    yearRange <- str_split(input$yearRange, ",", simplify = TRUE)
    yearRange <- as.numeric(yearRange)
    dataList <- lapply(yearRange, preprocess_data)
    do.call(rbind, dataList)
  })
  #NEED TO WRITE THIS METHOD FIRST - MAP CREATION
  output$mapPlot <- renderPlot({
    data <- processedDataPhase1()
    generateMapPlot(data)  
  })
  
  output$dataView <- renderTable({
    data <- processedDataPhase1()
    head(data)
  })
  
  output$analysisResults <- renderTable({
    data <- processedDataPhase1()
    analysisResults <- performAnalysis(data) 
    analysisResults
  })
  
}


shinyApp(ui = ui, server = server)

