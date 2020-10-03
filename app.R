
# packages

library(shiny)
library(rsconnect)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(dygraphs)
library(shinythemes)

# UI

ui <- fluidPage(
    
    # Application title
    theme = shinythemes::shinytheme("yeti"),
    titlePanel(h1("OxAir - Hybrid AQ Data Map", align = "center")),
    
    # Sidebar with a slider input for number of bins 
    
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


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #my code below - fit into busmap code above    
    
    output$oxmap <- renderLeaflet({
        
        icons <- awesomeIcons(
            icon = 'bicycle',
            markerColor = 'yellow',
            library = 'ion'
        )
        
        map <- leaflet() %>% 
            setView(lng = -1.189, lat = 51.721, zoom = 11) %>% 
            addTiles() %>%
            addPolylines(data = master_df_pm, lat = ~X, lng = ~Y, group = "Journeys", color = ~CG_member) %>% #polylines should change color according to pollution concentration
            addAwesomeMarkers( #markers need to change location & content according to each CG_member
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
    
    #leaflet polylines & dygraph widgets should be connected so that when you hover over dygraph, location of reading shows up as a circle on leaflet map
    
    output$CGM1_pm2_5_timeseries <- renderDygraph({
        xts(x = master_df_pm$pm2_5, order.by = master_df_pm$datetime) %>%
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

# Run the application locally
shinyApp(ui, server)

# Run the application via shinyapps.io (NOT WORKING!!)

rsconnect::deployApp("/Users/kaylaschulte/shinyapps/oxair_hybrid_map/", account = 'kayla-schulte')

