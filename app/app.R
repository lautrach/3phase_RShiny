library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(DT)
library(mgcv)
# -- UI --
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo2.jpg", height = "48px", width = "44px", style = "margin-right: 10px;"),
      "3 Phase"
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
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
        .instructions {
          margin-top: 20px;
        }
      "))
    ),
    tabItems(
      # HOME PAGE (ABOUT PAGE)
      tabItem(tabName = "home",
              includeMarkdown("about.Rmd")
      ),
      # PHASE 1
      tabItem(tabName = "phase1",
              h2("Phase 1: Location of traps"),
              fluidRow(
                column(4,
                       fileInput("file1", "Choose RDS File", accept = ".rds"),
                       div(
                         class = "instructions",
                         tags$h4(tags$b("Usage Instructions")),
                         tags$ul(
                           tags$li("Choose an RDS file that contains the trap result. Please make sure that this RDS file has columns for the longitude and latitude of traps for plotting purposes. "),
                           tags$li("After the file is uploaded and processed by the application, an interactive map will be generated displaying the trap ID, longitude, and latitude. Additionally, histograms representing sensitivity and specificity values will be provided."),
                         )
                       )
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Map",
                                  leafletOutput("mapPlot"),
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
              h2("Phase 2: Score of traps"),
              fluidRow(
                column(4,
                       fileInput("file2", "Choose CSV File", accept = ".csv"),
                       downloadButton("downloadData", "Download Processed Data"),
                       div(
                         class = "instructions",
                         tags$h4(tags$b("Usage Instructions")),
                         tags$ul(
                           tags$li("Choose a CSV file containing the results. Please make sure that this CSV file has a column for the longitude and latitude of traps for plotting purposes."),
                           tags$li("Once the file is uploaded and read by the application, an interactive plot will be generated outputting the trap id, the longitude, the latitude and the calculated trap score."),
                           tags$li("A new CSV file with an added", tags$i("score"), "column is available for downloaded and can be downloaded though the", tags$i("Download Processed Data"), "button"),
                           tags$li("The", tags$i("Data"), "tab contains a snapshot of the first few rows of the downloadable CSV file.")
                         )
                       )
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Map",
                                  leafletOutput("mapPhase2"),
                                  plotOutput("histogramScore")),
                         tabPanel("Data", dataTableOutput("dataPhase2"))
                       )
                )
              )
      ),
      # PHASE 3
      tabItem(tabName = "phase3",
              h2("Phase 3: Advanced Causal Analysis"),
              fluidRow(
                column(4,
                       fileInput("file3", "Choose CSV File", accept = ".csv"),
                       uiOutput("treatmentVarSelector"),
                       actionButton("runModel", "Run Analysis", 
                                    class = "btn-primary",
                                    style = "margin-top: 10px"),
                       div(
                         class = "instructions",
                         tags$h4(tags$b("Usage Instructions")),
                         tags$ul(
                           tags$li("Upload a CSV file containing predictor variables and the 'score' column."),
                           tags$li("Select the predictor variables you want to include in the model."),
                           tags$li("Click 'Run Analysis' to fit the GAM model and view results."),
                           tags$li("Rows with NA values in the 'score' column will be excluded from the analysis.")
                         )
                       )
                ),
                column(8,
                       tabsetPanel(
                         tabPanel("Summary", 
                                  dataTableOutput("summaryTable")),
                         tabPanel("Model Output", 
                                  verbatimTextOutput("modelSummary")),
                         tabPanel("Plots", 
                                  plotOutput("gamPlots", height = "800px"))
                       )
                )
              )
      )
    )
  )
)

# -- SERVER LOGIC --
server <- function(input, output, session) {
  
  # PHASE 1 SERVER
  trap_results <- reactive({
    req(input$file1)
    readRDS(input$file1$datapath)
  })
  
  trap_results_sens <- reactive({
    trap_data <- trap_results()
    trap_data_sens <- trap_data[trap_data$avg_specificity > 0, ]
    return(trap_data_sens)
  })
  
  output$loadedData <- renderTable({
    req(trap_results())
    head(trap_results())
  })
  
  output$mapPlot <- renderLeaflet({
    req(trap_results())  
    data <- trap_results()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = 1,
        color = "green",
        stroke = TRUE,
        fillOpacity = 0.7,
        popup = ~paste("Trap ID:", ID, "<br>Lat:", lat, "<br>Long:", long)
      )
  })
  
  generateHistogram1 <- function(data) {
    specihist <- ggplot(data = data, aes(x = avg_specificity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept = mean(avg_specificity)),
                 color = "blue", linetype = "dashed", size = 1) +
      ggtitle("Histogram of Specificity values for Model 1")
    return(specihist)
  }
  
  generateHistogram2 <- function(data){
    specihist2 <- ggplot(data = data, aes(x = avg_specificity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept=mean(avg_specificity)),
                 color="blue", linetype="dashed", size=1) +
      ggtitle("Histogram of Specificity values for traps with atleast one case Model 1")
    return(specihist2)
  }
  
  generateHistogram3 <- function(data){
    sensihist <- ggplot(data = data, aes(x = avg_sensitivity)) +
      geom_histogram(color = "darkgreen", fill = "lightgreen", alpha = 0.5, lwd = 1) +
      geom_vline(aes(xintercept = mean(avg_specificity)),
                 color = "blue", linetype = "dashed", size = 1) +
      ggtitle("Histogram of Sensitivity values for traps with atleast one case Model 1")
    return(sensihist)
  }
  
  output$histogramPlot1 <- renderPlot({
    req(trap_results())
    generateHistogram1(trap_results())
  })
  
  output$histogramPlot2 <- renderPlot({
    req(trap_results_sens())
    generateHistogram2(trap_results_sens())
  })
  
  output$histogramPlot3 <- renderPlot({
    req(trap_results_sens())
    generateHistogram3(trap_results_sens())
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
  results_phase2 <- reactive({
    req(input$file2)
    csv_data <- read.csv(input$file2$datapath)
    merged_data <- csv_data %>%
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
  output$mapPhase2 <- renderLeaflet({
    req(results_phase2())
    data <- results_phase2()
    pal <- colorNumeric(palette = "RdYlGn", domain = data$score, reverse = TRUE)
    leaflet(data) %>%
      
      addTiles() %>%
      
      addCircleMarkers(
        lng = ~long, lat = ~lat,  
        radius = 3,
        color = ~pal(score),
        stroke = TRUE,
        fillOpacity = 1.0,
        popup = ~paste("Trap ID:", ID, "<br>Score:", score, "<br>Lat:", lat, "<br>Long:", long)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~score,
                title = "Score",
                opacity = 1)
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
  
  # PHASE 3 SERVER
  data_phase3 <- reactive({
    req(input$file3)
    data <- fread(input$file3$datapath)
    data <- data[!is.na(score)]
    return(data)
  })
  output$treatmentVarSelector <- renderUI({
    req(data_phase3())
    var_choices <- setdiff(names(data_phase3()), "score")
    
    selectizeInput(
      "selectedVars",
      "Select Treatment Variables",
      choices = var_choices,
      multiple = TRUE,
      options = list(
        plugins = list('remove_button'),
        placeholder = 'Select variables...'
      )
    )
  })
  gam_formula <- reactive({
    req(input$selectedVars)
    smooth_terms <- paste0("s(", input$selectedVars, ")", collapse = " + ")
    as.formula(paste("score ~", smooth_terms))
  })
  model_results <- reactiveVal(NULL)
  observeEvent(input$runModel, {
    req(data_phase3(), input$selectedVars, gam_formula())
    model <- tryCatch({
      gam(gam_formula(), data = data_phase3())
    }, error = function(e) {
      return(NULL)
    })
    
    model_results(model)
  })
  
  output$summaryTable <- renderDataTable({
    req(data_phase3())
    selected_cols <- c("score", input$selectedVars)
    datatable(
      head(data_phase3()[, ..selected_cols], 10), 
      options = list(scrollX = TRUE)
    )
  })
  
  output$modelSummary <- renderPrint({
    req(model_results())
    summary(model_results())
  })
  
  output$gamPlots <- renderPlot({
    req(model_results())
    n_vars <- length(input$selectedVars)
    n_rows <- ceiling(n_vars / 3) 
    par(mfrow = c(n_rows, min(3, n_vars)))
    plot(model_results(), pages = 1, shade = TRUE)
  })
}

shinyApp(ui = ui, server = server)




