# Install and import required libraries
library(htmltools)
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  print(glimpse(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>% group_by(CITY_ASCII,LNG,LAT) %>% filter(BIKE_PREDICTION==max(BIKE_PREDICTION)) %>% select(BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,LABEL,DETAILED_LABEL)
  print(cities_max_bike)
  cities_max_bike$html_code <- lapply(cities_max_bike$LABEL, HTML)
  cities_max_bike$html_code_detailed <- lapply(cities_max_bike$DETAILED_LABEL, HTML)
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown == 'All') {
      #Render the city overview map
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(data=cities_max_bike,lng = ~LNG,lat = ~LAT, 
                           label=~html_code,
                           labelOptions = labelOptions(noHide = T),
                           #popup=~HTML(LABEL), 
                           color=~color_levels(BIKE_PREDICTION_LEVEL),radius=~ifelse(BIKE_PREDICTION_LEVEL=="medium",10,6))
        
      })
    }
    else {
      #Render the specific city map
      selected_city <- cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
      print(paste("selected_city",selected_city))
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(data=selected_city,lng = ~LNG,lat = ~LAT,label=~html_code_detailed, labelOptions = labelOptions(noHide = T))
        
      })
    } 
    # Execute code when users make selections on the dropdown 
    # Then render output plots with an id defined in ui.R
    # output$city_bike_map <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addCircleMarkers(data=cities_max_bike,lng = ~LNG,lat = ~LAT, popup=~paste("<b>", cities_max_bike$CITY_ASCII, "</b>"), color=~color_levels(BIKE_PREDICTION_LEVEL),radius=~ifelse(BIKE_PREDICTION_LEVEL=="medium",10,6))
    # 
    # })
    # selected_city <- cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
    # print(paste("selected_city",selected_city))
    # output$city_bike_map <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addCircleMarkers(data=selected_city,lng = ~LNG,lat = ~LAT, popup=~paste("<b>", cities_max_bike$CITY_ASCII, "</b>"), color=~color_levels(BIKE_PREDICTION_LEVEL),radius=~ifelse(BIKE_PREDICTION_LEVEL=="medium",10,6))
    #   
    # })
  })
  
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
