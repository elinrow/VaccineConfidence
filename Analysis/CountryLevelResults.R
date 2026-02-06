countries <- c("Austria",  "Bulgaria", "Czechia",  "Finland", "Germany", "Ireland",  "Italy", "Poland", "Spain")

for(i in countries){
  df <- country_level_data(data, i, threshold = 100)
  result <- c_models(df, i) |>
    mutate(country = i)
  assign(paste0(i, "_results"), result)
}

Romania_data<- country_level_data(data, "Romania", threshold = 100)
Romania_results <- romania_models(Romania_data, "Romania") %>%
  mutate(country = "Romania")

country_level_results <- rbind(Austria_results, Bulgaria_results, Czechia_results, Finland_results, Germany_results,
      Ireland_results, Italy_results, Poland_results, Romania_results, Spain_results)

#write.csv(country_level_results, "countryLevelResults.csv")
