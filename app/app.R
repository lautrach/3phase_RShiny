#app.R
library(shiny)
library(shinydashboard)
#TODO 1: Need to create individual RShiny files for each of the three phases 
#TODO 2: Connect the RShiny files for each of the three phases to the main file (app.R)

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
      tabItem(tabName = "phase1",
              h2("Phase 1"),
              p("Content for Phase 1...")
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
server <- function(input, output) { }

shinyApp(ui = ui, server = server)

