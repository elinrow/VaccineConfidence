# Sample data

data3 <- readRDS("~/Downloads/vcp-global-data.rds")

data3$Country <- as.factor(data3$Country)

levels(data3$Country)[levels(data3$Country)%in%("Czech Republic")] <- "Czechia"

data3 <- data3 %>%
  filter(Country %in% c("Austria", "Bulgaria", "Czechia", "Finland", "Germany", "Ireland", "Italy", "Poland", "Romania", "Spain"))

data3$VaxImpChild <- as.numeric(data3$VaxImpChild)

data3$VaxSaf <- as.numeric(data3$VaxSaf)

data3$VaxEff <- as.numeric(data3$VaxEff)

data3$VaxRel <- as.numeric(data3$VaxRel)

data3 <- data3 %>% filter(!VaxImpChild %in% c(5,9))
data3 <- data3 %>% filter(!VaxSaf %in% c(5,9))
data3 <- data3 %>% filter(!VaxEff %in% c(5,9))
data3 <- data3 %>% filter(!VaxRel %in% c(5,9))

data3$VaxImpChild <- ifelse(data3$VaxImpChild %in% c(3,4), 4, 0)
data3$VaxSaf <- ifelse(data3$VaxSaf %in% c(3,4), 4, 0)
data3$VaxEff <- ifelse(data3$VaxEff %in% c(3,4), 4, 0)
data3$VaxRel <- ifelse(data3$VaxRel %in% c(3,4), 4, 0)

per_imp <- data3 %>% group_by(Country,Year) %>% summarize(per_imp = sum(VaxImpChild == 4) / n() * 100)

per_saf <- data3 %>% group_by(Country,Year) %>% summarize(per_saf = sum(VaxSaf == 4) / n() * 100)

per_eff <- data3 %>% group_by(Country,Year) %>% summarize(per_eff = sum(VaxEff == 4) / n() * 100)

per_rel <- data3 %>% group_by(Country,Year) %>% summarize(per_rel = sum(VaxRel == 4) / n() * 100)

###

# Function to perform country-specific interpolation
interpolate_country_imp <- function(country_data3) {
  # Convert the data3set to a time series object
  ts_data3 <- zoo(country_data3$per_imp, country_data3$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data3$Year), max(country_data3$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data3 <- na.approx(ts_data3, xout = all_years)
  
  # Create a data3 frame with the interpolated data3
  interpolated_df <- data.frame(
    Country = country_data3$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data3)),  # Extract the years from the interpolated data3
    per_imp = as.vector(interpolated_data3)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data3 by Country and apply interpolation for each group
filled_data3_imp <- per_imp %>%
  group_by(Country) %>%
  do(interpolate_country_imp(.))

###

# Function to perform country-specific interpolation
interpolate_country_saf <- function(country_data3) {
  # Convert the data3set to a time series object
  ts_data3 <- zoo(country_data3$per_saf, country_data3$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data3$Year), max(country_data3$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data3 <- na.approx(ts_data3, xout = all_years)
  
  # Create a data3 frame with the interpolated data3
  interpolated_df <- data.frame(
    Country = country_data3$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data3)),  # Extract the years from the interpolated data3
    per_saf = as.vector(interpolated_data3)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data3 by Country and apply interpolation for each group
filled_data3_saf <- per_saf %>%
  group_by(Country) %>%
  do(interpolate_country_saf(.))


###

# Function to perform country-specific interpolation
interpolate_country_eff <- function(country_data3) {
  # Convert the data3set to a time series object
  ts_data3 <- zoo(country_data3$per_eff, country_data3$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data3$Year), max(country_data3$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data3 <- na.approx(ts_data3, xout = all_years)
  
  # Create a data3 frame with the interpolated data3
  interpolated_df <- data.frame(
    Country = country_data3$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data3)),  # Extract the years from the interpolated data3
    per_eff = as.vector(interpolated_data3)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data3 by Country and apply interpolation for each group
filled_data3_eff <- per_eff %>%
  group_by(Country) %>%
  do(interpolate_country_eff(.))

###
# Function to perform country-specific interpolation
interpolate_country_rel <- function(country_data3) {
  # Convert the data3set to a time series object
  ts_data3 <- zoo(country_data3$per_rel, country_data3$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data3$Year), max(country_data3$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data3 <- na.approx(ts_data3, xout = all_years)
  
  # Create a data3 frame with the interpolated data3
  interpolated_df <- data.frame(
    Country = country_data3$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data3)),  # Extract the years from the interpolated data3
    per_rel = as.vector(interpolated_data3)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data3 by Country and apply interpolation for each group
filled_data3_rel <- per_rel %>%
  group_by(Country) %>%
  do(interpolate_country_rel(.))

###
# Function to perform country-specific interpolation
interpolate_country_conf <- function(country_data3) {
  # Convert the data3set to a time series object
  ts_data3 <- zoo(country_data3$per_conf, country_data3$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data3$Year), max(country_data3$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data3 <- na.approx(ts_data3, xout = all_years)
  
  # Create a data3 frame with the interpolated data3
  interpolated_df <- data.frame(
    Country = country_data3$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data3)),  # Extract the years from the interpolated data3
    per_conf = as.vector(interpolated_data3)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

###

fper_conf <- left_join(filled_data3_imp, filled_data3_saf, by = c('Country','Year')) %>%
  left_join(filled_data3_eff, by = c('Country','Year')) %>%
  left_join(filled_data3_rel, by = c('Country','Year')) 

# Convert Year to numeric (if it's a character) - Skip this if already numeric or Date
fper_conf$Year <- as.numeric(fper_conf$Year)

# Create a vector with all years from 2015 to 2022

# Filter countries with data3 for all years from 2015 to 2022
fin <- fper_conf %>%
  group_by(Country) 

# Linear interpolation better? Check and compare?

measles1 <- read_csv("Data/MCV1.csv")
colnames(measles1)[8] <- "Country"
colnames(measles1)[10] <- "Year"
colnames(measles1)[2] <- "Vaccine"
colnames(measles1)[30] <- "Coverage"

measles1 <- measles1 %>%
  subset(select = c(Country,Year,Vaccine,Coverage)) %>%
  filter(Country %in% data3$Country)

###

measles2 <- read_csv("Data/MCV2.csv")
colnames(measles2)[8] <- "Country"
colnames(measles2)[10] <- "Year"
colnames(measles2)[2] <- "Vaccine"
colnames(measles2)[30] <- "Coverage"

measles2 <- measles2 %>%
  subset(select = c(Country,Year,Vaccine,Coverage)) %>%
  filter(Country %in% data3$Country)

cov <- rbind(measleseur1,measleseur2)

cov <- cov %>%
  pivot_wider(names_from = Vaccine, values_from = Coverage)

colnames(cov)[4] <- "MCV1"
colnames(cov)[5] <- "MCV2"

finvax <- left_join(fin,cov, by = c("Country","Year")) %>%
  filter(Year >= 2015)

corr_mcv1 <- finvax %>%
  group_by(Country) %>%
  filter(sd(MCV1) != 0) %>%
  summarize(imp_cor = cor(per_imp, MCV1, method = "spearman"), saf_cor = cor(per_saf, MCV1, method = "spearman"), eff_cor = cor(per_eff, MCV1, method = "spearman"), rel_cor = cor(per_rel, MCV1, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV1", 36)) %>%
  mutate(Question = factor(Question, levels = c("imp_cor", "saf_cor", "eff_cor", "rel_cor")))  # Specify order


corr_mcv2 <- finvax %>%
  group_by(Country) %>%
  filter(sd(MCV2) != 0) %>%
  summarize(imp_cor = cor(per_imp, MCV2, method = "spearman"), saf_cor = cor(per_saf, MCV2, method = "spearman"), eff_cor = cor(per_eff, MCV2, method = "spearman"), rel_cor = cor(per_rel, MCV2, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV2", 28)) %>%
  mutate(Question = factor(Question, levels = c("imp_cor", "saf_cor", "eff_cor", "rel_cor")))  # Specify order

# Function to create the heatmap
create_heatmap <- function(data, dose_type) {
  ggplot(data, aes(x = Question, y = Country, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 4) +  # Add text labels
    scale_fill_gradient2(low = "purple", mid = "white", high = "green",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Correlation") +
    scale_x_discrete(labels = c("imp_cor" = "Vaccines are important for children to have", 
                                "saf_cor" = "Vaccines are safe", 
                                "eff_cor" = "Vaccines are effective",
                                "rel_cor" = "Vaccines are compatible with my beliefs")) +
    theme_minimal() +
    labs(title = paste("Correlation between", dose_type, "coverage and low vaccine confidence"),
         x = "Questions", y = "Country") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

# Plot heatmaps for MCV1 and MCV2
heatmap_MCV1_lc <- create_heatmap(corr_mcv1, "MCV1")
heatmap_MCV2_lc <- create_heatmap(corr_mcv2, "MCV2")

heatmap_MCV1_lc
heatmap_MCV2_lc