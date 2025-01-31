library(zoo)
#### Time Series Plots

ts_plots <- function(country) {
  
  library(countrycode)
  library(dplyr)

  setwd("~/Documents/Cyhoeddi/Data")
  data <- readRDS("vcp-global-data.rds")

  data$Country <- as.factor(data$Country)

  levels(data$Country)[levels(data$Country) %in% ("Czech Republic")] <- "Czechia"

  data <- data %>%
    filter(Country %in% c(
      "Austria", "Bulgaria", "Czechia", "Finland", "Germany",
      "Ireland", "Italy", "Poland", "Romania", "Spain"
    ))


  data$VaxImpChild <- as.factor(data$VaxImpChild)

  levels(data$VaxImpChild)[levels(data$VaxImpChild) == 1] <- "Strongly Agree"
  levels(data$VaxImpChild)[levels(data$VaxImpChild) == 2] <- "Somewhat Agree"
  levels(data$VaxImpChild)[levels(data$VaxImpChild) == 3] <- "Somewhat Disagree"
  levels(data$VaxImpChild)[levels(data$VaxImpChild) == 4] <- "Strongly Disagree"
  levels(data$VaxImpChild)[levels(data$VaxImpChild) %in% c(5, 9, 98, 99)] <- "Missing"

  data$VaxSaf <- as.factor(data$VaxSaf)

  levels(data$VaxSaf)[levels(data$VaxSaf) == 1] <- "Strongly Agree"
  levels(data$VaxSaf)[levels(data$VaxSaf) == 2] <- "Somewhat Agree"
  levels(data$VaxSaf)[levels(data$VaxSaf) == 3] <- "Somewhat Disagree"
  levels(data$VaxSaf)[levels(data$VaxSaf) == 4] <- "Strongly Disagree"
  levels(data$VaxSaf)[levels(data$VaxSaf) %in% c(5, 9, 98, 99)] <- "Missing"

  data$VaxEff <- as.factor(data$VaxEff)

  levels(data$VaxEff)[levels(data$VaxEff) == 1] <- "Strongly Agree"
  levels(data$VaxEff)[levels(data$VaxEff) == 2] <- "Somewhat Agree"
  levels(data$VaxEff)[levels(data$VaxEff) == 3] <- "Somewhat Disagree"
  levels(data$VaxEff)[levels(data$VaxEff) == 4] <- "Strongly Disagree"
  levels(data$VaxEff)[levels(data$VaxEff) %in% c(5, 9, 98, 99)] <- "Missing"

  data$VaxRel <- as.factor(data$VaxRel)

  levels(data$VaxRel)[levels(data$VaxRel) == 1] <- "Strongly Agree"
  levels(data$VaxRel)[levels(data$VaxRel) == 2] <- "Somewhat Agree"
  levels(data$VaxRel)[levels(data$VaxRel) == 3] <- "Somewhat Disagree"
  levels(data$VaxRel)[levels(data$VaxRel) == 4] <- "Strongly Disagree"
  levels(data$VaxRel)[levels(data$VaxRel) %in% c(5, 9, 98, 99)] <- "Missing"

  # Missing values not explained - exclude

  data <- data %>% filter(VaxImpChild != "Missing")
  data <- data %>% filter(VaxSaf != "Missing")
  data <- data %>% filter(VaxEff != "Missing")
  data <- data %>% filter(VaxRel != "Missing")

  per_imp <- data %>%
    group_by(Country, Year) %>%
    summarize(per_imp = sum(VaxImpChild %in% c("Strongly Agree", "Somewhat Agree")) / n() * 100)

  per_saf <- data %>%
    group_by(Country, Year) %>%
    summarize(per_saf = sum(VaxSaf %in% c("Strongly Agree", "Somewhat Agree")) / n() * 100)

  per_eff <- data %>%
    group_by(Country, Year) %>%
    summarize(per_eff = sum(VaxEff %in% c("Strongly Agree", "Somewhat Agree")) / n() * 100)

  per_rel <- data %>%
    group_by(Country, Year) %>%
    summarize(per_rel = sum(VaxRel %in% c("Strongly Agree", "Somewhat Agree")) / n() * 100)

  all_data <- merge(per_imp, per_saf, by = c("Country", "Year"))
  all_data <- merge(all_data, per_eff, by = c("Country", "Year"))
  all_data <- merge(all_data, per_rel, by = c("Country", "Year"))

  all_years <- seq(min(all_data$Year), max(all_data$Year), by = 1)


  country_data <- all_data %>%
    filter(Country == country)

  ts_data_imp <- zoo(country_data$per_imp, country_data$Year)
  ts_data_saf <- zoo(country_data$per_saf, country_data$Year)
  ts_data_eff <- zoo(country_data$per_eff, country_data$Year)
  ts_data_rel <- zoo(country_data$per_rel, country_data$Year)

  # Perform spline interpolation to fill missing years
  interpolated_imp <- na.approx(ts_data_imp, xout = all_years)
  interpolated_saf <- na.approx(ts_data_saf, xout = all_years)
  interpolated_eff <- na.approx(ts_data_eff, xout = all_years)
  interpolated_rel <- na.approx(ts_data_rel, xout = all_years)
  
  interpolated <- data.frame(
    Country = country,  # Use the first Country value for all rows
    Year = all_years,  # Extract the years from the interpolated data2
    per_imp = as.vector(interpolated_imp),
    per_saf = as.vector(interpolated_saf),
    per_eff = as.vector(interpolated_eff),
    per_rel = as.vector(interpolated_rel)
  )
 
  ########
  
  measles1 <- read_csv("MCV1.csv")
  colnames(measles1)[8] <- "Country"
  colnames(measles1)[10] <- "Year"
  colnames(measles1)[2] <- "Vaccine"
  colnames(measles1)[30] <- "Coverage"
  
  measles1 <- subset(measles1, select = c(Country,Year,Vaccine,Coverage))
  
  measles2 <- read_csv("MCV2.csv")
  colnames(measles2)[8] <- "Country"
  colnames(measles2)[10] <- "Year"
  colnames(measles2)[2] <- "Vaccine"
  colnames(measles2)[30] <- "Coverage"
  
  measles2 <- subset(measles2, select = c(Country,Year,Vaccine,Coverage))
  
  cov <- rbind(measles1,measles2) 
  
  cov <- cov %>%
    filter(Country == country,
           Year >= 2015) %>%
    pivot_wider(names_from = Vaccine, values_from = Coverage)
  
  colnames(cov)[3] <- "MCV1"
  colnames(cov)[4] <- "MCV2"
  
  com_data <- merge(cov,interpolated, by = c("Country", "Year"))
  
  ######

  color_palette <- wes_palette("GrandBudapest1", 4)
  red_color_palette <- wes_palette("Zissou1", 2)
  
  return(
    ggplot(com_data, aes(x = Year)) +
    geom_line(aes(y = MCV1, color = "MCV1"), size = 1.5) +
    geom_point(aes(y = MCV1, color = "MCV1"), size = 3) +
    geom_line(aes(y = MCV2, color = "MCV2"), size = 1.5) +
    geom_point(aes(y = MCV2, color = "MCV2"), size = 3) +
    geom_line(aes(y = per_imp, color = "I think vaccines are important for children to have"), size = 1.5) +
    geom_point(aes(y = per_imp, color = "I think vaccines are important for children to have"), size = 3) +
    geom_line(aes(y = per_saf, color = "I think vaccines are safe"), size = 1.5) +
    geom_point(aes(y = per_saf, color = "I think vaccines are safe"), size = 3) +
    geom_line(aes(y = per_eff, color = "I think vaccines are effective"), size = 1.5) +
    geom_point(aes(y = per_eff, color = "I think vaccines are effective"), size = 3) +
    geom_line(aes(y = per_rel, color = "I think vaccines are compatible with my religious beliefs"), size = 1.5) +
    geom_point(aes(y = per_rel, color = "I think vaccines are compatible with my religious beliefs"), size = 3) +
    geom_hline(yintercept = 95, linetype = "dashed", color = "black") +  # Horizontal line at y = 95
    annotate("text", x = 2014.9, y = 94, label = "HIT",
             hjust = -0.1, vjust = 0.5, color = "black", size = 3) + 
    labs(title = paste0(country),
         x = "Year",
         y = "Percentage (%)") +
    scale_color_manual(name = " ",values = c("MCV1" = red_color_palette[1], "MCV2" = red_color_palette[2], 
                                             "I think vaccines are important for children to have" = color_palette[1], 
                                             "I think vaccines are safe" = color_palette[2], 
                                             "I think vaccines are effective" = color_palette[3], 
                                             "I think vaccines are compatible with my religious beliefs" = color_palette[4])) + # Set colors for each group
    theme_minimal() + theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 3))) + ylim(60,100)
  )
}