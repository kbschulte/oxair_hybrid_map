## OxAir: Hybrid AQ Data Map (PROTOTYPE -Test run locally) ##
## 15/12/2020 ##
## Kayla Schulte ##

#load packages

packages <- c("shiny", "shinydashboard", "leaflet", "dplyr", "curl", "dygraphs", "dashboardthemes", "shinythemes", "xts","plyr", "sf") 

lapply(packages, require, character.only = TRUE)

# load data

load("/Users/kaylaschulte/shinyapps/test/CGM_data.rda")
load("/Users/kaylaschulte/shinyapps/test/qual.rda")

journeys=c("Journey 1" = "CGM1",
           "Journey 2" = "CGM2", 
           "Journey 3" = "CGM3",
           "Journey 4" = "CGM4", 
           "Journey 5" = "CGM5", 
           "Journey 6" = "CGM6")

pollutants=c("PM 2.5" = "pm2_5", 
             "PM 10" = "pm10", 
             "NO2" = "no2")

# Set journeyBy & pollutantBy to test the app locally

journeyBy <- "CGM2"
pollutantBy <- "no2"

journeyData <- subset(master_df_subset, CG_member == journeyBy)
journeyData_sf<-st_as_sf(journeyData,coords=c("X","Y"),crs=4326)
pollutantData <- journeyData_sf[c(pollutantBy, 'datetime')]
dygraph_data <- journeyData[c(pollutantBy, 'datetime')]

# UI

ui <- fluidPage(
  
  # Application title
  theme = shinythemes::shinytheme("yeti"),
  titlePanel(h1("OxAir - Hybrid AQ Data Map", align = "center")),
  
  dashboardBody(
    fluidRow(
      column(width = 12,
             box(width = NULL, status = "warning", align = "center",
                 p(
                   class = "text-muted",
                   paste("Note: each 'Journey' represents a different OxAir Competency Group member's path through Oxford. Data was collected at different dates and times. Hey Dad!"
                   )
                 ),
                 selectInput("journey", "Select Example Journey:", journeys),
                 radioButtons("pollutant", "Pollutant:", pollutants, inline = TRUE), 
                 
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = TRUE, style='padding:10px;',
                 leafletOutput(outputId = "oxmap", height = 400),
             )),
      column(width = 6,
             box(width = NULL, style='padding:10px;',
                 dygraphOutput("dygraph")
             ),
      )),
  ),
)


# SERVER

server <- function(input, output) {
  
  output$oxmap=renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -1.260552 , lat = 51.759327, zoom = 13)
  })
  
  observe({
    
    journeyBy <- input$journey
    pollutantBy <- input$pollutant
    journeyData <- subset(master_df_subset, CG_member == journeyBy)
    journeyData_sf<-st_as_sf(journeyData,coords=c("X","Y"),crs=4326)
    pollutantData <- journeyData_sf[c(pollutantBy, 'datetime')]
    
    if (pollutantBy == "no2") { 
      
      pollutantData <- pollutantData[!is.na(pollutantData$no2), ]
      pollutantData$color_bin <- cut(pollutantData$no2, breaks = c(0, 67, 134, 200, 267, 334, 400, 467, 534, 600, Inf))
      colfunc_no2 <- colorRampPalette(c("black", "#CCCC00"))
      pal <- colorFactor(palette = colfunc_no2(11),domain = pollutantData$color_bin, reverse = T)
      
    } else if (pollutantBy == "pm10") {
      
      pollutantData$color_bin <- cut(pollutantData$pm10, breaks = c(0, 11, 23, 35, 41, 47, 53, 58, 64, 70, Inf))
      colfunc_pm10 <- colorRampPalette(c("black", "#CCCCCC"))
      pal <- colorFactor(palette = colfunc_pm10(11),domain = pollutantData$color_bin, reverse = T)
      
    } else {
      
      pollutantData$color_bin <- cut(pollutantData$pm2_5, breaks = c(0, 11, 23, 35, 41, 47, 53, 58, 64, 70, Inf))
      colfunc_pm <- colorRampPalette(c("black", "#CCCCCC"))
      pal <- colorFactor(palette = colfunc_pm(11),domain = pollutantData$color_bin, reverse = T)
      
    }
    
    leafletProxy("oxmap", data = pollutantData) %>%
      clearMarkers() %>%
      addCircleMarkers(color= ~pal(pollutantData$color_bin), opacity = 0.5, radius = 5) %>%
      leaflet::addLegend(
        position = "bottomleft",
        values = ~color_bin,
        pal = pal,
        labels = NULL,
        title = "Legend")
    
  })
  
  observe({
    journeyBy <- input$journey
    
    CGM_qual <- subset(qual_subset, CG_member == journeyBy)
    CGM_qual_sf<-st_as_sf(CGM_qual,coords=c("X","Y"),crs=4326)
    CGM_qual_sf <- cbind(CGM_qual_sf, st_coordinates(CGM_qual_sf))
    
    icons <- awesomeIcons(icon = "whatever",
                          iconColor = "#FF0000",
                          library = "ion",
                          markerColor = "#FF0000")
    
    leafletProxy("oxmap", data = CGM_qual_sf) %>%
      addAwesomeMarkers(~Y, ~X, label = ~qual_data, icon = icons)
    
  })
  
  
  #leaflet polylines & dygraph widgets should be connected so that when you hover over dygraph, location of reading shows up as a circle on leaflet map
  output$dygraph <- renderDygraph({ #dygraph should change when specific CG_member is selected from dropdown menu, also include tabs for PM and NO2
    
    journeyBy <- input$journey
    pollutantBy <- input$pollutant
    journeyData <- subset(master_df_subset, CG_member == journeyBy)
    dygraph_data <- journeyData[c(pollutantBy, 'datetime')]
    
    if (pollutantBy == "no2") { 
      
      dygraph_data <- dygraph_data[!is.na(dygraph_data$no2), ]
      dygraph_data$color_bin <- cut(dygraph_data$no2, breaks = c(0, 67, 134, 200, 267, 334, 400, 467, 534, 600, Inf))
      colfunc_no2 <- colorRampPalette(c("black", "#CCCC00"))
      pal <- colorFactor(palette = colfunc_no2(11),domain = dygraph_data$color_bin, reverse = T)
      
    } else if (pollutantBy == "pm10") {
      
      dygraph_data <- dygraph_data[!is.na(dygraph_data$pm10), ]
      dygraph_data$color_bin <- cut(dygraph_data$pm10, breaks = c(0, 11, 23, 35, 41, 47, 53, 58, 64, 70, Inf))
      colfunc_pm <- colorRampPalette(c("black", "#CCCCCC"))
      pal <- colorFactor(palette = colfunc_pm(11),domain = pollutantData$color_bin, reverse = T)
      
    } else {
      
      dygraph_data <- dygraph_data[!is.na(dygraph_data$pm2_5), ]
      dygraph_data$color_bin <- cut(dygraph_data$pm2_5, breaks = c(0, 11, 23, 35, 41, 47, 53, 58, 64, 70, Inf))
      colfunc_pm <- colorRampPalette(c("black", "#CCCCCC"))
      pal <- colorFactor(palette = colfunc_pm(11), domain = dygraph_data$color_bin, reverse = T)
      
    }
    
    xts(x = dygraph_data, order.by = dygraph_data$datetime) %>%
      dygraph() %>%
      
      dyOptions(drawPoints = TRUE, pointSize = 4) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
      dyLegend(width = 200, show = "follow") %>%
      dyRangeSelector(height = 20) 
  }) 
  
}


# Run app -------------------------------
shinyApp(ui, server)
