ts_data <- function() {
  
  ts_data <- list()
  
  vcp_files <- list.files("Data/vcp_data")
  
  for(i in seq_along(vcp_files)){
    ts_data[[i]] <- read.csv(paste0("Data/vcp_data/",vcp_files[i]))
  }

  ts_data <- bind_rows(ts_data)[,-2]
  
  colnames(ts_data) <- c("Country", "Year", "VaxImpChild", "VaxSaf", "VaxEff", "VaxRel")
  
  ts_data$Country <- as.factor(ts_data$Country)

  levels(ts_data$Country)[levels(ts_data$Country) %in% ("Czech Republic")] <- "Czechia"
  
  countries_ts <- c("Austria", "Bulgaria", "Czechia", "Finland", "Germany",
    "Ireland", "Italy", "Poland", "Romania", "Spain")

  ts_data <- ts_data %>%
    filter(Country %in% countries_ts)

  ########
  measles1 <- read_csv("Data/MCV1.csv")
  colnames(measles1)[8] <- "Country"
  colnames(measles1)[10] <- "Year"
  colnames(measles1)[2] <- "Vaccine"
  colnames(measles1)[30] <- "Coverage"
  
  measles1 <- subset(measles1, select = c(Country,Year,Vaccine,Coverage))
  
  measles2 <- read_csv("Data/MCV2.csv")
  colnames(measles2)[8] <- "Country"
  colnames(measles2)[10] <- "Year"
  colnames(measles2)[2] <- "Vaccine"
  colnames(measles2)[30] <- "Coverage"
  
  measles2 <- subset(measles2, select = c(Country,Year,Vaccine,Coverage))
  
  cov <- rbind(measles1,measles2) 
  
  cov <- cov %>%
    filter(Year >= 2015) %>%
    pivot_wider(names_from = Vaccine, values_from = Coverage)
  
  colnames(cov)[3] <- "MCV1"
  colnames(cov)[4] <- "MCV2"
  
  com_ts_data <- merge(cov,ts_data, by = c("Country", "Year")) %>%
    mutate(VaxImpChild = round(VaxImpChild*100,2),
           VaxSaf = round(VaxSaf*100,2),
           VaxEff = round(VaxEff*100,2),
           VaxRel = round(VaxRel*100,2))
  
  ######
  
  write.csv(com_ts_data, "Results/tsData.csv")
  return(
    ggplot(com_ts_data, aes(x = Year)) +
      facet_wrap(~Country) +
    geom_line(aes(y = MCV1, color = "MCV1"), size = 1.5) +
    geom_point(aes(y = MCV1, color = "MCV1"), size = 3) +
    geom_line(aes(y = MCV2, color = "MCV2"), size = 1.5) +
    geom_point(aes(y = MCV2, color = "MCV2"), size = 3) +
    geom_line(aes(y = VaxImpChild, color = "I think vaccines are important for children to have"), size = 1.5) +
    geom_point(aes(y = VaxImpChild, color = "I think vaccines are important for children to have"), size = 3) +
    geom_line(aes(y = VaxSaf, color = "I think vaccines are safe"), size = 1.5) +
    geom_point(aes(y = VaxSaf, color = "I think vaccines are safe"), size = 3) +
    geom_line(aes(y = VaxEff, color = "I think vaccines are effective"), size = 1.5) +
    geom_point(aes(y = VaxEff, color = "I think vaccines are effective"), size = 3) +
    geom_line(aes(y = VaxRel, color = "I think vaccines are compatible with my religious beliefs"), size = 1.5) +
    geom_point(aes(y = VaxRel, color = "I think vaccines are compatible with my religious beliefs"), size = 3) +
    geom_hline(yintercept = 95, linetype = "dashed", color = "black") +  # Horizontal line at y = 95
    annotate("text", x = 2014.9, y = 94, label = "HIT",
             hjust = -0.1, vjust = 0.5, color = "black", size = 3) + 
    labs(x = "Year",
         y = "Percentage (%)") +
    scale_color_manual(name = " ",values = c("MCV1" = "purple", "MCV2" = "pink", 
                                             "I think vaccines are important for children to have" = "navy", 
                                             "I think vaccines are safe" = "darkred", 
                                             "I think vaccines are effective" = "orange", 
                                             "I think vaccines are compatible with my religious beliefs" = "darkgreen")) + # Set colors for each group
    theme_minimal() + theme(legend.position = "bottom") +
    guides(color = guide_legend(override.aes = list(size = 3))) 
  )
}