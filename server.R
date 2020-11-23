# SERVER
server <- function(input, output) {
  
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
  
  output$CGM1_pm2_5_timeseries <- renderDygraph({ #dygraph should change when specific CG_member is selected from dropdown menu, also include tabs for PM and NO2
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