library(tidymodels)
library(tidyverse)

# 1. Download NOAA Weather Dataset
url <- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")
untar("noaa-weather-sample-data.tar.gz")

# 2. Extract and Read into Project
weather_df <- readr::read_csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(weather_df)
glimpse(weather_df)


# 3. Select Subset of Columns
df <- weather_df %>%
  select(HOURLYRelativeHumidity,
         HOURLYDRYBULBTEMPF,
         HOURLYWindSpeed,
         HOURLYStationPressure,
         HOURLYPrecip)

head(df, 10)

# 4. Clean Up Columns
unique(df$HOURLYPrecip)
df2 <- df %>%
  mutate(
    HOURLYPrecip = ifelse(HOURLYPrecip == "T", "0.0", HOURLYPrecip),
    HOURLYPrecip = str_remove(HOURLYPrecip, pattern = "s$")
  )
unique(df2$HOURLYPrecip)

# 5. Convert Columns to Numerical Types
glimpse(df2)

df3 <- df2 %>%
  mutate(HOURLYPrecip = as.numeric(HOURLYPrecip)) %>%
  drop_na(HOURLYPrecip)

glimpse(df3)

# 6. Rename Columns
clean_df <- df3 %>%
  rename(
    relative_humidity = HOURLYRelativeHumidity,
    dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
    wind_speed = HOURLYWindSpeed,
    station_pressure = HOURLYStationPressure,
    precip = HOURLYPrecip
  )

# 7. Exploratory Data Analysis
set.seed(1234)

split_object <- initial_split(
  data = clean_df,
  prop = 0.8
)

train_df <- training(split_object)
dim(train_df)

test_df <- testing(split_object)
dim(test_df)

list <-lapply(1:ncol(train_df),
              function(col) ggplot2::qplot(train_df[[col]],
                                           geom = "histogram",
                                           xlab = names(train_df)[col],
                                           bins = 30))

cowplot::plot_grid(plotlist = list)


############################## 8. Linear Regression #####################################

# Model 1: precip ~ relative_humidity
model1 <- lm(precip ~ relative_humidity, data = train_df)
summary(model1)  # Display regression summary
# Scatter plot
ggplot(train_df, aes(x = relative_humidity, y = precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot and Linear Regression for precip ~ relative_humidity")

# Model 2: precip ~ dry_bulb_temp_f
model2 <- lm(precip ~ dry_bulb_temp_f, data = train_df)
summary(model2)  # Display regression summary
# Scatter plot
ggplot(train_df, aes(x = dry_bulb_temp_f, y = precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot and Linear Regression for precip ~ dry_bulb_temp_f")

# Model 3: precip ~ wind_speed
model3 <- lm(precip ~ wind_speed, data = train_df)
summary(model3)  # Display regression summary
# Scatter plot
ggplot(train_df, aes(x = wind_speed, y = precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot and Linear Regression for precip ~ wind_speed")

# Model 4: precip ~ station_pressure
model4 <- lm(precip ~ station_pressure, data = train_df)
summary(model4)  # Display regression summary
# Scatter plot
ggplot(train_df, aes(x = station_pressure, y = precip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot and Linear Regression for precip ~ station_pressure")


###### 9. Improve the Model #############################
# Model 5 : Adding more features
model5 <- lm(precip ~ ., data = train_df)
summary(model5)

# Predictions for Model 5
model5_predictions <- predict(model5, train_df)

# Calculate R-squared for Model 5
model5_r_squared <- summary(model5)$r.squared
model5_r_squared

# Model 6: Polynomial Regression with Interaction Terms (excluding station_pressure)
model6 <- lm(precip ~ poly(relative_humidity, 2) + poly(dry_bulb_temp_f, 2) + poly(wind_speed, 2) +
               relative_humidity:dry_bulb_temp_f + relative_humidity:wind_speed +
               dry_bulb_temp_f:wind_speed, data = train_df)
summary(model6)  # Display regression summary

# Predictions for Model 6
model6_predictions <- predict(model6, train_df)
model6_r_squared <- summary(model6)$r.squared
model6_r_squared

############################# 10. Find Best Model ##########################

# Define a function to calculate R-squared
calculate_r_squared <- function(model, data) {
  predictions <- predict(model, data)
  ss_residual <- sum((data$precip - predictions)^2)
  ss_total <- sum((data$precip - mean(data$precip))^2)
  return(1 - (ss_residual / ss_total))
}

# Evaluate and compare the models on the testing set
models <- list(model1, model2, model3, model4, model5, model6)
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
r_squared_values <- numeric(length(models))

for (i in 1:length(models)) {
  r_squared_values[i] <- calculate_r_squared(models[[i]], test_df)
}

# Create a data frame to display the results
results <- data.frame(Model = model_names, R_squared = r_squared_values)

# Print the results
print(results)

# Find the best model based on R-squared
best_model <- model_names[which.max(r_squared_values)]
cat("\nBest Model Overall:", best_model, "\n")
