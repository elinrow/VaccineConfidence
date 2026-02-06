# The following function can be used to create country-level data.
# This will clean the data so that any religious group with a sample size below
# a specified threshold will be combined into a category called 'Other'. 

# The function requires 3 inputs: the name of the dataset, the name of the country
# you want to create a dataset for, and the sample size threshold.

country_level_data <- function(dataset, country_name, threshold) {
  
  # This filters the dataset so that it only contains responses from the specified country.
  country_data <- dataset |> dplyr::filter(Country == country_name)
  
  # Use the droplevels function to ensure that any levels not relevant to the specified
  # country are removed.
  
  country_data <- droplevels(country_data)
  
  # Get the count of each religious group.
  religion_counts <- table(country_data$Religion)
  
  # Identify religions with a count less than the threshold.
  other_religions <- names(religion_counts[religion_counts < threshold])
  
  # Combine the religions below the threshold into one variable called 'Other'.
  levels(country_data$Religion)[levels(country_data$Religion) %in% other_religions] <- "Other"
  
  # Drop unused levels.
  country_data <- droplevels(country_data)
  
  return(country_data)
}

# The country name must be capitalised for this to work