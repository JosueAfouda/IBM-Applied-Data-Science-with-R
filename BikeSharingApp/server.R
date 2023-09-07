# # Install and import required libraries
# require(shiny)
# require(ggplot2)
# require(leaflet)
# require(tidyverse)
# require(httr)
# require(scales)
# # Import model_prediction R which contains methods to call OpenWeather API
# # and make predictions
# source("model_prediction.R")
# 
# 
# test_weather_data_generation<-function(){
#   #Test generate_city_weather_bike_data() function
#   city_weather_bike_df<-generate_city_weather_bike_data()
#   stopifnot(length(city_weather_bike_df)>0)
#   print(head(city_weather_bike_df))
#   return(city_weather_bike_df)
# }
# 
# # Create a RShiny server
# shinyServer(function(input, output){
#   # Define a city list
#   
#   # Define color factor
#   color_levels <- colorFactor(c("green", "yellow", "red"), 
#                               levels = c("small", "medium", "large"))
#   city_weather_bike_df <- test_weather_data_generation()
#   
#   # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
#   # prediction for the city
#   
#   # Observe drop-down event
#   
#   # Then render output plots with an id defined in ui.R
#   
#   # If All was selected from dropdown, then render a leaflet map with circle markers
#   # and popup weather LABEL for all five cities
#   
#   # If just one specific city was selected, then render a leaflet map with one marker
#   # on the map and a popup with DETAILED_LABEL displayed
#   
# })


# Load required libraries
require(shiny)
require(leaflet)
require(tidyverse)

# Import the functions from model_prediction.R
source("model_prediction.R")

test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

city_weather_bike_df <- test_weather_data_generation()

# Load the cities data from selected_cities.csv
cities_df <- read.csv("selected_cities.csv")

# Load the trained regression model coefficients
model_coefficients <- read.csv("model.csv")

# Create a RShiny server
shinyServer(function(input, output){
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Create a reactive function to filter data based on selected city
  filtered_data <- reactive({
    if (is.null(input$selected_city)) {
      return(NULL)
    }
    city_weather_bike_df %>%
      filter(CITY_ASCII == input$selected_city)
  })
  
  # Create a Leaflet map
  output$city_bike_map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = ~sqrt(BIKE_PREDICTION) * 5,
        color = ~color_levels(BIKE_PREDICTION_LEVEL),
        fillOpacity = 0.7,
        popup = ~DETAILED_LABEL
      )
  })
  
  # Load the model coefficients and create a reactive function for predictions
  model_coeff <- reactive({
    if (is.null(input$selected_city)) {
      return(NULL)
    }
    model_coefficients %>%
      filter(Variable != "Intercept") %>%
      arrange(desc(Coef)) %>%
      select(Variable) %>%
      pull()
  })
  
  # Create a reactive function for making predictions based on selected city
  bike_predictions <- reactive({
    if (is.null(input$selected_city)) {
      return(NULL)
    }
    city_weather_bike_df %>%
      filter(CITY_ASCII == input$selected_city) %>%
      select({{ model_coeff() }}) %>%
      summarise_all(sum) %>%
      transmute(BIKE_PREDICTION = as.numeric(.)) %>%
      mutate(BIKE_PREDICTION_LEVEL = calculate_bike_prediction_level(BIKE_PREDICTION))
  })
  
  selected_city_data <- reactive({
    filtered_data() %>%
      mutate(HOUR = as.POSIXlt(FORECASTDATETIME)$hour)
  })
  
  output$temperature_trend_plot <- renderPlot({
    
    if (!is.null(selected_city_data())) {
      # Plot temperature trend for the selected city
      ggplot(selected_city_data(), aes(x = HOUR, y = TEMPERATURE)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = TEMPERATURE), hjust = 1.2, vjust = 0.5) +
        labs(title = "Temperature Trend for the Next 5 Days",
             #x = "Date and Time",
             x = "Hour",
             y = "Temperature (°C)") +
        theme_minimal()
    }
  })
  
})
