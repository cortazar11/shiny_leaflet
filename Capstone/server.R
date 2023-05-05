# Install and import required libraries
library(htmltools)
require(lubridate)
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
  city_weather_bike_df$FORECASTDATETIME <-as.POSIXct(city_weather_bike_df$FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S")
  city_weather_bike_df$HOUR <- hour(city_weather_bike_df$FORECASTDATETIME)
  city_weather_bike_df$html_code <- lapply(city_weather_bike_df$LABEL, HTML)
  city_weather_bike_df$html_code_detailed <- lapply(city_weather_bike_df$DETAILED_LABEL, HTML)
  print(city_weather_bike_df$FORECASTDATETIME)
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
  cities_max_bike <- city_weather_bike_df %>% group_by(CITY_ASCII,LNG,LAT) %>% filter(BIKE_PREDICTION==max(BIKE_PREDICTION)) %>% select(BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,LABEL,DETAILED_LABEL,html_code,html_code_detailed)
  #cities_max_bike$FORECASTDATETIME <- as.POSIXct(cities_max_bike$FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S")
  # cities_max_bike$html_code <- lapply(cities_max_bike$LABEL, HTML)
  # cities_max_bike$html_code_detailed <- lapply(cities_max_bike$DETAILED_LABEL, HTML)
  print(head(cities_max_bike))
  
  
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
      #selected_city <- cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
      selected_city <- city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown)
      #selected_city$HOUR <- hour(df$FORECASTDATETIME)
      print(head(selected_city))
      output$city_bike_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(data=selected_city,lng = ~LNG,lat = ~LAT,label=~html_code_detailed, labelOptions = labelOptions(noHide = T))
        
      })
      
      output$temp_line <- renderPlot({
        ggplot(selected_city,aes(x=FORECASTDATETIME,y=TEMPERATURE)) +
          geom_point() + 
          geom_line(color="yellow") +
          scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
          #scale_x_datetime(hour(selected_city$FORECASTDATETIME)) +
          labs(title = "Trend Line",x="Time (3 hours ahead)",y="TEMPERATURE (C)") +
          # Add labels to each point
          geom_text(aes(label = TEMPERATURE), nudge_y = 0.5)
      })
      
      output$bike_line <- renderPlot({
        ggplot(selected_city,aes(x=FORECASTDATETIME,y=BIKE_PREDICTION)) +
          geom_point() +
          geom_line() +
          scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
          #scale_x_datetime(hour(selected_city$FORECASTDATETIME)) +
          labs(title = "Bike Prediction Line",x="Time (3 hours ahead)",y="BIKE PREDICTION") +
          # Add labels to each point
          geom_text(aes(label = BIKE_PREDICTION), nudge_y = 0.5)
      })
      
      observeEvent(input$plot_click, {
        x <- input$plot_click$x
        y <- input$plot_click$y
        output$bike_date_output <- renderText(paste("You clicked on Time =", as.POSIXct(x,origin = "1970-01-01"), "and Bike Count Prediction =", y))
      })
      
      output$humidity_pred_chart <- renderPlot({
        ggplot(selected_city,aes(x=HUMIDITY,y=BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method ="lm", formula="y ~ poly(x, 4)") +
          labs(title = "Correlation Plot") 
          # geom_line() +
          # scale_x_datetime(date_labels = "%Y-%m-%d %H:%M:%S") +
          # #scale_x_datetime(hour(selected_city$FORECASTDATETIME)) +
          # labs(title = "Bike Prediction Line",x="Time (3 hours ahead)",y="BIKE PREDICTION") +
          # # Add labels to each point
          # geom_text(aes(label = BIKE_PREDICTION), nudge_y = 0.5)
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
