# app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggmap)
library(RColorBrewer)
library(DT)

# Stadia Maps API 
ggmap::register_stadiamaps(key = 'API_KEY') # we can look into leaflet -- no api needed look at commented code below

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
    tags$head(
      tags$style(HTML("
        table.data {
          width: 100% !important;
          overflow-x: auto !important;
        }
      "))
    ),
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
                         tabPanel("Plots", 
                                  plotOutput("mapPlot"),
                                  plotOutput("histogramPlot1"),
                                  plotOutput("histogramPlot2"),
                                  plotOutput("histogramPlot3")),
                         tabPanel("Data", dataTableOutput("dataView")),
                         tabPanel("Analysis Results", dataTableOutput("analysisResults"))
                       )
                )
              )
      ),
      # PHASE 2
      tabItem(tabName = "phase2",
              h2("Phase 2"),
              fluidRow(
                column(4,
                       fileInput("file2", "Choose CSV File", accept = ".csv"),
                       downloadButton("downloadData", "Download Processed Data")
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Map", 
                                  plotOutput("mapPhase2"),
                                  plotOutput("histogramScore")),
                         tabPanel("Data", dataTableOutput("dataPhase2"))
                       )
                )
              )
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
  # PHASE 1 SERVER
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
    coords.map <- coords.map + geom_point(data = data,
                                          aes(x = long, y = lat),
                                          fill = "green", shape = 23,
                                          alpha = 0.8, size = 1)
    return(coords.map)
  }
  
  #Specificity atleast one case
  generateHistogram1 <- function(data) {
    specihist <- ggplot(data = data, aes(x = avg_specificity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept = mean(avg_specificity)),
                 color = "blue", linetype = "dashed", size = 1) +
      ggtitle("Histogram of Specificity values for Model 1")
    return(specihist)
  }
  
  #Specificity atleast one case
  generateHistogram2 <- function(data){
    specihist2 <- ggplot(data = trap_results_sens, aes(x= avg_specificity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept=mean(avg_specificity)),
                 color="blue", linetype="dashed", size=1) +
      ggtitle("Histogram of Specificity values for traps with atleast one case Model 1") 
    return(specihist2)
  }
  
  #Sensitivity atleast one case
  generateHistogram3 <- function(data){
    sensihist <- ggplot(data = trap_results_sens, aes(x= avg_sensitivity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept=mean(avg_specificity)),
                 color="blue", linetype="dashed", size=1) +
      ggtitle("Histogram of Sensitivity values for traps with atleast one case Model 1")
    return(sensihist)
  }
  
  output$mapPlot <- renderPlot({
    req(trap_results())
    generateMapPlot(trap_results())
  })
  
  output$histogramPlot1 <- renderPlot({
    req(trap_results())
    generateHistogram1(trap_results())
  })
  
  output$histogramPlot2 <- renderPlot({
    req(trap_results())
    generateHistogram2(trap_results())
  })
  
  output$histogramPlot3 <- renderPlot({
    req(trap_results())
    generateHistogram3(trap_results())
  })
  
  output$dataView <- renderDataTable({
    req(trap_results())
    datatable(trap_results(), options = list(scrollX = TRUE))
  })
  
  output$analysisResults <- renderDataTable({
    req(trap_results())
    datatable(trap_results(), options = list(scrollX = TRUE))
  })
  
  # PHASE 2 SERVER 
  rds_data <- reactive({
    readRDS("/Users/ram/Desktop/3phase_RShiny/trapAug2022.rds")
  })
  results_phase2 <- reactive({
    req(input$file2)
    csv_data <- read.csv(input$file2$datapath)
    rds_data <- rds_data()
    merged_data <- merge(csv_data, rds_data[, c("ID", "lat", "long")], by = "ID")
    merged_data <- merged_data %>%
      mutate(across(contains("specificity"), ~replace_na(.x, 0))) %>%
      mutate(across(contains("sensitivity"), ~replace_na(.x, 0))) %>%
      mutate(
        avg_specificity = rowMeans(select(., contains("specificity")), na.rm = TRUE),
        avg_sensitivity = rowMeans(select(., contains("sensitivity")), na.rm = TRUE),
        score = if_else(is.na(avg_specificity) | is.na(avg_sensitivity), NA_real_,
                        (0.9 * avg_specificity + avg_sensitivity) / 1.9)
      )
    
    merged_data
  })
  
  output$mapPhase2 <- renderPlot({
    req(results_phase2())
    data <- results_phase2()
    map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
    map_data <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
    base_map <- ggmap(map_data)
    base_map +
      geom_point(data = data, aes(x = long, y = lat, fill = score), shape = 21, size = 3, color = "black") +
      scale_fill_gradient(low = "green", high = "red", na.value = "gray") +
      labs(fill = "Score")
  })
  
  output$dataPhase2 <- renderDataTable({
    req(results_phase2())
    datatable(results_phase2(), options = list(scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        req(results_phase2())
        write.csv(results_phase2(), file, row.names = FALSE)
      }, error = function(e) {
        message("An error occurred: ", e$message)
        write.csv(data.frame(Error = "An error occurred while generating the file"), file)
      })
    }
  )
}

shinyApp(ui = ui, server = server)

# LEAFLET FOR PHASE 2 -- WE COULD USE THIS TO AVOID API KEY ISSUES WITH GGMAP 

# # app.R
# library(shiny)
# library(shinydashboard)
# library(data.table)
# library(tidyverse)
# library(ggmap)
# library(RColorBrewer)
# library(leaflet)
# 
# # Stadia Maps API 
# ggmap::register_stadiamaps(key = 'API_KEY')
# 
# # UI
# ui <- dashboardPage(
#   dashboardHeader(title = "3 Phase"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Home", tabName = "home", icon = icon("home")),
#       menuItem("Phase 1", tabName = "phase1"),
#       menuItem("Phase 2", tabName = "phase2"),
#       menuItem("Phase 3", tabName = "phase3")
#     )
#   ),
#   dashboardBody(
#     tags$head(
#       tags$style(HTML("
#         table.data {
#           width: 100% !important;
#           overflow-x: auto !important;
#         }
#       "))
#     ),
#     tabItems(
#       # HOME PAGE
#       tabItem(tabName = "home",
#               h2("3 Phase application (change name/title later)"),
#               div(
#                 h3("Purpose"),
#                 p("Purpose of the application will go here..."),
#                 h3("Usage"),
#                 p("A brief outline of how to use the application will go here...")
#               )
#       ),
#       # PHASE 1
#       tabItem(tabName = "phase1",
#               h2("Phase 1"),
#               fluidRow(
#                 column(4,
#                        fileInput("file1", "Choose RDS File", accept = ".rds"),  
#                        actionButton("load", "Load Data")
#                 ),
#                 column(8,
#                        tabsetPanel(
#                          tabPanel("Plots", 
#                                   plotOutput("mapPlot"),
#                                   plotOutput("histogramPlot1"),
#                                   plotOutput("histogramPlot2"),
#                                   plotOutput("histogramPlot3")),
#                          tabPanel("Data", tableOutput("dataView")),
#                          tabPanel("Analysis Results", tableOutput("analysisResults"))
#                        )
#                 )
#               )
#       ),
#       # PHASE 2
#       tabItem(tabName = "phase2",
#               h2("Phase 2"),
#               fluidRow(
#                 column(4,
#                        fileInput("file2", "Choose CSV File", accept = ".csv"),
#                        downloadButton("downloadData", "Download Processed Data")
#                 ),
#                 column(8,
#                        tabsetPanel(
#                          tabPanel("Map", leafletOutput("mapPhase2")),
#                          tabPanel("Data", tableOutput("dataPhase2"))
#                        )
#                 )
#               )
#       ),
#       # PHASE 3
#       tabItem(tabName = "phase3",
#               h2("Phase 3"),
#               p("Content for Phase 3...")
#       )
#     )
#   )
# )
# 
# # SERVER LOGIC
# server <- function(input, output) {
#   # Phase 1 logic
#   trap_results <- reactive({
#     req(input$file1)  # Require that the file input is not NULL
#     readRDS(input$file1$datapath)
#   })
#   
#   observeEvent(input$load, {
#     trap_results_sens <- trap_results()  
#   })
#   
#   output$loadedData <- renderTable({
#     req(trap_results())
#     head(trap_results())
#   })
#   
#   generateMapPlot <- function(data) {
#     map_bounds <- c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
#     coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
#     coords.map <- ggmap(coords.map, extent = "device", legend = "none")
#     coords.map <- coords.map + geom_point(data = data,
#                                           aes(x = long, y = lat),
#                                           fill = "green", shape = 23,
#                                           alpha = 0.8, size = 1)
#     return(coords.map)
#   }
#   
#   #Specificity atleast one case
#   generateHistogram1 <- function(data) {
#     specihist <- ggplot(data = data, aes(x = avg_specificity)) +
#       geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
#       geom_vline(aes(xintercept = mean(avg_specificity)),
#                  color = "blue", linetype = "dashed", size = 1) +
#       ggtitle("Histogram of Specificity values for Model 1")
#     return(specihist)
#   }
#   
#   #Specificity atleast one case
#   generateHistogram2 <- function(data){
#     specihist2 <- ggplot(data = trap_results_sens, aes(x= avg_specificity)) +
#       geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
#       geom_vline(aes(xintercept=mean(avg_specificity)),
#                  color="blue", linetype="dashed", size=1) +
#       ggtitle("Histogram of Specificity values for traps with atleast one case Model 1") 
#     return(specihist2)
#   }
#   
#   #Sensitivity atleast one case
#   generateHistogram3 <- function(data){
#     sensihist <- ggplot(data = trap_results_sens, aes(x= avg_sensitivity)) +
#       geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
#       geom_vline(aes(xintercept=mean(avg_specificity)),
#                  color="blue", linetype="dashed", size=1) +
#       ggtitle("Histogram of Sensitivity values for traps with atleast one case Model 1")
#     return(sensihist)
#   }
#   
#   output$mapPlot <- renderPlot({
#     req(trap_results())
#     generateMapPlot(trap_results())
#   })
#   
#   output$histogramPlot1 <- renderPlot({
#     req(trap_results())
#     generateHistogram1(trap_results())
#   })
#   
#   output$histogramPlot2 <- renderPlot({
#     req(trap_results())
#     generateHistogram2(trap_results())
#   })
#   
#   output$histogramPlot3 <- renderPlot({
#     req(trap_results())
#     generateHistogram3(trap_results())
#   })
#   
#   output$dataView <- renderTable({
#     req(trap_results())
#     head(trap_results())
#   })
#   
#   output$analysisResults <- renderTable({
#     req(trap_results())
#     trap_results()
#   })
#   
#   # Phase 2 logic
#   rds_data <- reactive({
#     readRDS("/Users/ram/Desktop/3phase_RShiny/trapAug2022.rds")
#   })
#   
#   # Process the uploaded CSV file and merge with lat/long data from the RDS file
#   results_phase2 <- reactive({
#     req(input$file2)
#     csv_data <- read.csv(input$file2$datapath)
#     
#     # Merge lat/long from RDS data with the CSV data
#     rds_data <- rds_data()
#     merged_data <- merge(csv_data, rds_data[, c("ID", "lat", "long")], by = "ID")
#     
#     # Calculate score
#     merged_data <- merged_data %>%
#       mutate(across(contains("specificity"), ~replace_na(.x, 0))) %>%
#       mutate(across(contains("sensitivity"), ~replace_na(.x, 0))) %>%
#       mutate(
#         avg_specificity = rowMeans(select(., contains("specificity")), na.rm = TRUE),
#         avg_sensitivity = rowMeans(select(., contains("sensitivity")), na.rm = TRUE),
#         score = if_else(is.na(avg_specificity) | is.na(avg_sensitivity), NA_real_,
#                         (0.9 * avg_specificity + avg_sensitivity) / 1.9)
#       )
#     
#     merged_data
#   })
#   
#   # Render the leaflet map using the processed data
#   output$mapPhase2 <- renderLeaflet({
#     req(results_phase2())
#     data <- results_phase2()
#     
#     # Create color palette
#     pal <- colorNumeric(palette = "RdYlGn", domain = data$score, reverse = TRUE)
#     
#     leaflet(data) %>%
#       addTiles() %>%
#       addCircleMarkers(
#         ~long, ~lat,
#         radius = 5,
#         color = ~pal(score),
#         stroke = TRUE,
#         fillOpacity = 0.7,
#         popup = ~paste("Trap ID:", ID, "<br>Score:", score, "<br>Lat:", lat, "<br>Long:", long)
#       ) %>%
#       addLegend("bottomright", pal = pal, values = ~score,
#                 title = "Score",
#                 opacity = 1)
#   })
#   
#   # Render a table with the processed data
#   output$dataPhase2 <- renderTable({
#     req(results_phase2())
#     head(results_phase2(), 10)  # Display the first 10 rows of the data
#   }, class = "data")
#   
#   # Allow the user to download the processed data
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("processed_data_", Sys.Date(), ".csv", sep = "")
#     },
#     content = function(file) {
#       tryCatch({
#         req(results_phase2())
#         write.csv(results_phase2(), file, row.names = FALSE)
#       }, error = function(e) {
#         message("An error occurred: ", e$message)
#         write.csv(data.frame(Error = "An error occurred while generating the file"), file)
#       })
#     }
#   )
# }
# 
# shinyApp(ui = ui, server = server)