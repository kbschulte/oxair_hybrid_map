#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




rsconnect::setAccountInfo(name='kayla-schulte',
                          token='77725AA794B8B3A4B500E5E1A7CB0E69',
                          secret='jy944RHS38cQCim414AONiz1dt7eXLCb2GMLnkhF')

rsconnect::deployApp("~/shinyapps/oxair_hybrid_map/", account = 'kayla-schulte')

library(shiny)
library(rsconnect)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(dygraphs)
library(dashboardthemes)

# UI

ui <- fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    titlePanel(h1("OxAir - Hybrid AQ Data Map", align = "center")),
    
    # Sidebar with a slider input for number of bins 
    
    dashboardBody(
        fluidRow(
            column(width = 9,
                   box(width = NULL, solidHeader = TRUE,
                       leafletOutput(outputId = "oxmap", height = 400), 
                   ),
                   box(width = NULL, height = 500,
                       dygraphOutput("CGM1_pm2_5_timeseries")
                   )),
            column(width = 3,
                   box(width = NULL, status = "warning",
                       uiOutput("routeSelect"),
                       checkboxGroupInput("directions", "Show",
                                          choices = c(
                                              Journey_1 = 4,
                                              Journey_2 = 1,
                                              Journey_3 = 2,
                                              Journey_4 = 3,
                                              Journey_5 = 5,
                                              Journey_6 = 6
                                          ),
                                          selected = c(1, 2, 3, 4, 5, 6)
                       ),
                       p(
                           class = "text-muted",
                           paste("Note: each 'Journey' represents a different OxAir Competency Group member's path through Oxford. Data was collected at different dates and times."
                           )
                       ),
                       actionButton("zoomButton", "Zoom out to see all")
                   ),
            )
        ),
        absolutePanel(top = 60, left = 20, 
                      checkboxInput("markers", "Depth", FALSE),
                      checkboxInput("heat", "Heatmap", FALSE)
        )))




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$oxmap <- renderLeaflet({
        
        icons <- awesomeIcons(
            icon = 'bicycle',
            markerColor = 'yellow',
            library = 'ion'
        )
        
        map <- leaflet() %>% setView(lng = -1.189, lat = 51.721, zoom = 11) %>% 
            addTiles() %>%
            addPolylines(data = CGM1_pm, lat = ~X, lng = ~Y) %>%
            addAwesomeMarkers(
                lng = -1.265375, lat = 51.77881,
                icon=icons,
                label = "TEST 1",
                labelOptions = labelOptions(noHide = F)) %>%
            addAwesomeMarkers(
                lng = -1.266362, lat = 51.77441,
                icon=icons,
                label = "TEST 2",
                labelOptions = labelOptions(noHide = F))
        
    })
    
    output$CGM1_pm2_5_timeseries <- renderDygraph({
        xts(x = CGM1_pm$pm2_5, order.by = CGM1_pm$datetime) %>%
            dygraph() %>%
            dyOptions( drawPoints = TRUE, pointSize = 4) %>%
            dyLimit(0, "Low", labelLoc="right", color="grey") %>%
            dyLimit(30, "Medium", labelLoc="right", color="grey") %>%
            dyLimit(70, "High", 
                    labelLoc="right", color="grey") %>%
            dyLimit(500, "Very High", labelLoc="right", color="grey") %>%    
            dyShading(0, 29, color="#ccff99", axis="y") %>%
            dyShading(30, 69, color="#ffffcc", axis="y") %>%
            dyShading(70, 499, color="#ffebcc", axis="y") %>%
            dyShading(500, 1000, color="#ffcccc", axis="y") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
            dyLegend(width = 200, show = "follow") %>%
            dyRangeSelector(height = 20) 
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
