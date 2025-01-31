# Sample data

data2 <- readRDS("~/Downloads/vcp-global-data.rds")

data2$Country <- as.factor(data2$Country)

levels(data2$Country)[levels(data2$Country)%in%("Czech Republic")] <- "Czechia"

data2 <- data2 %>%
  filter(Country %in% c("Austria", "Bulgaria", "Czechia", "Finland", "Germany", "Ireland", "Italy", "Poland", "Romania", "Spain"))

data2$VaxImpChild <- as.numeric(data2$VaxImpChild)

data2$VaxSaf <- as.numeric(data2$VaxSaf)

data2$VaxEff <- as.numeric(data2$VaxEff)

data2$VaxRel <- as.numeric(data2$VaxRel)

data2 <- data2 %>% filter(!VaxImpChild %in% c(5,9))
data2 <- data2 %>% filter(!VaxSaf %in% c(5,9))
data2 <- data2 %>% filter(!VaxEff %in% c(5,9))
data2 <- data2 %>% filter(!VaxRel %in% c(5,9))

data2$VaxImpChild <- ifelse(data2$VaxImpChild %in% c(1,2), 1, 0)
data2$VaxSaf <- ifelse(data2$VaxSaf %in% c(1,2), 1, 0)
data2$VaxEff <- ifelse(data2$VaxEff %in% c(1,2), 1, 0)
data2$VaxRel <- ifelse(data2$VaxRel %in% c(1,2), 1, 0)

per_imp <- data2 %>% group_by(Country,Year) %>% summarize(per_imp = sum(VaxImpChild == 1) / n() * 100)

per_saf <- data2 %>% group_by(Country,Year) %>% summarize(per_saf = sum(VaxSaf == 1) / n() * 100)

per_eff <- data2 %>% group_by(Country,Year) %>% summarize(per_eff = sum(VaxEff == 1) / n() * 100)

per_rel <- data2 %>% group_by(Country,Year) %>% summarize(per_rel = sum(VaxRel == 1) / n() * 100)

###

# Function to perform country-specific interpolation
interpolate_country_imp <- function(country_data2) {
  # Convert the data2set to a time series object
  ts_data2 <- zoo(country_data2$per_imp, country_data2$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data2$Year), max(country_data2$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data2 <- na.approx(ts_data2, xout = all_years)
  
  # Create a data2 frame with the interpolated data2
  interpolated_df <- data.frame(
    Country = country_data2$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data2)),  # Extract the years from the interpolated data2
    per_imp = as.vector(interpolated_data2)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data2 by Country and apply interpolation for each group
filled_data2_imp <- per_imp %>%
  group_by(Country) %>%
  do(interpolate_country_imp(.))

###

# Function to perform country-specific interpolation
interpolate_country_saf <- function(country_data2) {
  # Convert the data2set to a time series object
  ts_data2 <- zoo(country_data2$per_saf, country_data2$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data2$Year), max(country_data2$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data2 <- na.approx(ts_data2, xout = all_years)
  
  # Create a data2 frame with the interpolated data2
  interpolated_df <- data.frame(
    Country = country_data2$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data2)),  # Extract the years from the interpolated data2
    per_saf = as.vector(interpolated_data2)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data2 by Country and apply interpolation for each group
filled_data2_saf <- per_saf %>%
  group_by(Country) %>%
  do(interpolate_country_saf(.))


###

# Function to perform country-specific interpolation
interpolate_country_eff <- function(country_data2) {
  # Convert the data2set to a time series object
  ts_data2 <- zoo(country_data2$per_eff, country_data2$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data2$Year), max(country_data2$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data2 <- na.approx(ts_data2, xout = all_years)
  
  # Create a data2 frame with the interpolated data2
  interpolated_df <- data.frame(
    Country = country_data2$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data2)),  # Extract the years from the interpolated data2
    per_eff = as.vector(interpolated_data2)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data2 by Country and apply interpolation for each group
filled_data2_eff <- per_eff %>%
  group_by(Country) %>%
  do(interpolate_country_eff(.))

###
# Function to perform country-specific interpolation
interpolate_country_rel <- function(country_data2) {
  # Convert the data2set to a time series object
  ts_data2 <- zoo(country_data2$per_rel, country_data2$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data2$Year), max(country_data2$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data2 <- na.approx(ts_data2, xout = all_years)
  
  # Create a data2 frame with the interpolated data2
  interpolated_df <- data.frame(
    Country = country_data2$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data2)),  # Extract the years from the interpolated data2
    per_rel = as.vector(interpolated_data2)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

# Group the data2 by Country and apply interpolation for each group
filled_data2_rel <- per_rel %>%
  group_by(Country) %>%
  do(interpolate_country_rel(.))

###
# Function to perform country-specific interpolation
interpolate_country_conf <- function(country_data2) {
  # Convert the data2set to a time series object
  ts_data2 <- zoo(country_data2$per_conf, country_data2$Year)
  
  # Create a sequence of years for interpolation
  all_years <- seq(min(country_data2$Year), max(country_data2$Year), by = 1)
  
  # Perform spline interpolation to fill missing years
  interpolated_data2 <- na.approx(ts_data2, xout = all_years)
  
  # Create a data2 frame with the interpolated data2
  interpolated_df <- data.frame(
    Country = country_data2$Country[1],  # Use the first Country value for all rows
    Year = as.numeric(index(interpolated_data2)),  # Extract the years from the interpolated data2
    per_conf = as.vector(interpolated_data2)  # Extract the interpolated values
  )
  
  return(interpolated_df)
}

###

fper_conf <- left_join(filled_data2_imp, filled_data2_saf, by = c('Country','Year')) %>%
  left_join(filled_data2_eff, by = c('Country','Year')) %>%
  left_join(filled_data2_rel, by = c('Country','Year')) 

# Convert Year to numeric (if it's a character) - Skip this if already numeric or Date
fper_conf$Year <- as.numeric(fper_conf$Year)

# Create a vector with all years from 2015 to 2022

# Filter countries with data2 for all years from 2015 to 2022
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
  filter(Country %in% data2$Country)

###

measles2 <- read_csv("Data/MCV2.csv")
colnames(measles2)[8] <- "Country"
colnames(measles2)[10] <- "Year"
colnames(measles2)[2] <- "Vaccine"
colnames(measles2)[30] <- "Coverage"

measles2 <- measles2 %>%
  subset(select = c(Country,Year,Vaccine,Coverage)) %>%
  filter(Country %in% data2$Country)

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
    labs(title = paste("Correlation between", dose_type, "coverage and vaccine confidence"),
         x = "Questions", y = "Country") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

# Plot heatmaps for MCV1 and MCV2
heatmap_MCV1_c <- create_heatmap(corr_mcv1, "MCV1")
heatmap_MCV2_c <- create_heatmap(corr_mcv2, "MCV2")

heatmap_MCV1_c
heatmap_MCV2_c
