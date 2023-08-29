library(httr)
library(rvest)
library(lubridate)
library(dplyr)

# TASK 1: Get a COVID-19 pandemic Wiki page using HTTP request

get_wiki_covid19_page <- function() {
  
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  
  # - Use the `GET` function in httr library with a `url` argument and a `query` argument to get a HTTP response
  response <- GET(
    url = wiki_base_url, 
    query = list(title = "Template:COVID-19 testing by country")
  )
  # Use the `return` function to return the response
  
  return(response)
  
}

# Call the get_wiki_covid19_page function
html_content <- get_wiki_covid19_page()
print(html_content)

# TASK 2: Extract COVID-19 testing data table from the wiki HTML page

  # Get the root html node from the http response in task 1 
root_html_node <- read_html(html_content)
  # Get the table node from the root html node
table_node <- html_table(root_html_node)
print(table_node)
  # Read the table node and convert it into a data frame, and print the data frame for review
covide19_data <- table_node[[2]]
head(covide19_data)
tail(covide19_data)
str(covide19_data)

# TASK 3: Pre-process and export the extracted data frame

  # Print the summary of the dataframe
summary(covide19_data)

preprocess_covid_data_frame <- function(data_frame) {
  
  #shape <- dim(data_frame)
  
  # Remove the World row
  #data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- factor(data_frame$country)
  #data_frame$date <- as.factor(data_frame$date)
  data_frame$date <- dmy(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  
  #data_frame$'confirmed.tested.ratio' <- gsub(",", "", data_frame$'confirmed.tested.ratio')
  data_frame$'confirmed.tested.ratio' <- as.numeric(data_frame$confirmed.tested.ratio)
  
  data_frame$'tested.population.ratio' <- gsub(",", "", data_frame$'tested.population.ratio')
  data_frame$'tested.population.ratio' <- as.numeric(data_frame$tested.population.ratio)
  
  data_frame$'confirmed.population.ratio' <- as.numeric(data_frame$confirmed.population.ratio)
  
  return(data_frame)
}

  # call `preprocess_covid_data_frame` function and assign it to a new data frame
covid19_preprocess <- preprocess_covid_data_frame(covide19_data)

  # Print the summary of the processed data frame again
summary(covid19_preprocess)

  # Export the data frame to a csv file
write.csv(x = covid19_preprocess, row.names = FALSE, file = "covid.csv")


# TASK 4: Get a subset of the extracted data frame

  # Read covid_data_frame_csv from the csv file
covid <- read.csv("covid.csv", stringsAsFactors = TRUE)

  # Get the 5th to 10th rows, with two "country" "confirmed" columns
covid[5:10, c("country", "confirmed")]

# TASK 5: Calculate worldwide COVID testing positive ratio

  # Get the total confirmed cases worldwide
total_confirmed_cases <- sum(covid$confirmed)
total_confirmed_cases

  # Get the total tested cases worldwide
total_tested_cases <- sum(covid$tested)
total_tested_cases

  # Get the positive ratio (confirmed / tested)
positive_ratio <- total_confirmed_cases/total_tested_cases
positive_ratio


# TASK 6: Get a country list which reported their testing data

  # Get the `country` column
covid$country

  # Check its class
str(covid$country)

  # Convert the country column into character so that you can easily sort them
covid$country <- as.character(covid$country)

  # Sort the countries AtoZ
AtoZ_countries <- covid %>%
  select(country) %>%
  arrange(country)

  # Sort the countries ZtoA
ZtoA_countries <- covid %>%
  select(country) %>%
  arrange(desc(country))
  
  # Print the sorted ZtoA list
print(list(ZtoA_countries))

# TASK 7: Identify countries names with a specific pattern
pattern <- "^United"  # ^ indicates the start of the string
matching_countries <- covid$country[grep(pattern, covid$country)]
matching_countries

# TASK 8: Pick two countries you are interested, and then review their testing data
selected_columns <- c("country", "confirmed", "confirmed.population.ratio")
benin_subset <- subset(covid, country == "Benin", select = selected_columns)
germany_subset <- subset(covid, country == "Germany", select = selected_columns)


# TASK 9: Compare which one of the selected countries has a larger ratio of confirmed cases to population

if (benin_subset$confirmed.population.ratio > germany_subset$confirmed.population.ratio) {
  print(benin_subset$country)
} else {
  print(germany_subset$country)
}

# TASK 10: Find countries with confirmed to population ratio rate less than a threshold
covid %>%
  filter(confirmed.population.ratio < 0.01) %>%
  pull(country)
