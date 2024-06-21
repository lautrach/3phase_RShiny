#app.R
library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(ggmap)
library(RColorBrewer)

# Stadia Maps API 
ggmap::register_stadiamaps(key = 'API_KEY') #probably need to figure something out for this...

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
                         tabPanel("Plots", 
                                  plotOutput("mapPlot"),
                                  plotOutput("histogramPlot1"),
                                  plotOutput("histogramPlot2"),
                                  plotOutput("histogramPlot3")),
                         tabPanel("Data", tableOutput("dataView")),
                         tabPanel("Analysis Results", tableOutput("analysisResults"))
                       )
                )
              )
      ),
      # PHASE 2
      tabItem(tabName = "phase2",
              h2("Phase 2"),
              tabPanel("Heatmap", 
                       plotOutput("heatmapscore"))
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
    specihist2 <- ggplot(data = trap_results_sens,aes(x= avg_specificity)) +
      geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
      geom_vline(aes(xintercept=mean(avg_specificity)),
                 color="blue", linetype="dashed", size=1) +
      ggtitle("Histogram of Specificity values for traps with atleast one case Model 1") 
    return(specihist2)
  }
  
  #Sensitivity atleast one case
  generateHistogram3 <- function(data){
    sensihist <-ggplot(data = trap_results_sens,aes(x= avg_sensitivity)) +
      geom_histogram(color = "darkgreen",fill = "lightgreen",alpha = 0.5,lwd = 1)+
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
  
  output$dataView <- renderTable({
    req(trap_results())
    head(trap_results())
  })
  
  output$analysisResults <- renderTable({
    req(trap_results())
    trap_results()
  })
#phase2 plots
  # heatmapscore <- function(data){
  #   heatmap <- plot_heatmap = function(df,z = "specificity",binwidth_h=0.1,
  #                                                        binwidth_v=0.1,fun=median,point_disp = F){
  #   dat = df[c("lat","long",z)]
  #   colnames(dat)[3] = "z" 
  #   map_bounds <-  c(left = -89.2, bottom = 41.3, right = -87.3, top = 42.7)
  #   coords.map <- get_stadiamap(map_bounds, zoom = 9, maptype = "stamen_toner_lite")
  #   coords.map <- ggmap(coords.map, extent="device", legend="none")
  #   coords.map <- coords.map %+% dat + aes(x = long,y = lat,z = z) +
  #     stat_summary_2d(fun = fun,geom = "tile", 
  #                     binwidth = c(binwidth_h, binwidth_v),
  #                     alpha = 0.5)+
  #     scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")))
  #   coords.map <- coords.map + theme_bw()
  #   if(point_disp == T){
  #     coords.map <- coords.map + geom_point(data=df,  
  #                                           aes(x=long, y=lat), 
  #                                           fill="red", shape=23,
  #                                           alpha=0.8,size = 0.5)  
  #   }
  #   coords.map
  #   return(heatmap)
  # }
  #   
  # }
  
  
}

shinyApp(ui = ui, server = server)
