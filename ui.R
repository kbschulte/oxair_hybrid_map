ui <- fluidPage(
  
  # Application title
  theme = shinythemes::shinytheme("yeti"),
  titlePanel(h1("OxAir - Hybrid AQ Data Map", align = "center")),
  
  dashboardBody(
    fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE, style='padding:20px;',
                 leafletOutput(outputId = "oxmap", height = 400), 
             ),
             box(width = NULL, height = 500,
                 dygraphOutput("CGM1_pm2_5_timeseries")
             )),
      column(width = 3,
             box(width = NULL, status = "warning",
                 uiOutput("routeSelect"),
                 selectInput(inputId = "Selector",
                             label = "Select Example Journey",
                             choices = c("Please Select:",
                                         "CGM1",
                                         "CGM2"),
                             selected = "Please Select:"),
                 p(
                   class = "text-muted",
                   paste("Note: each 'Journey' represents a different OxAir Competency Group member's path through Oxford. Data was collected at different dates and times."
                   )
                 ),
             ),
      )
    ),
  ))
