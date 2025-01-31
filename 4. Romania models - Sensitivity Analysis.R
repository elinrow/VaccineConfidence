romania_models <- function(country_data,country_name) {
  
  country_data$VaxImpChild <- as.character(country_data$VaxImpChild)
  
  country_data$VaxSaf <- as.character(country_data$VaxSaf)
  
  country_data$VaxEff <- as.character(country_data$VaxEff)
  
  country_data$VaxRel <- as.character(country_data$VaxRel)
  
  country_data <- country_data %>%
    mutate(VaxImpChild = case_when(
      VaxImpChild == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxImpChild == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxImpChild  # Retains other values as they are
    ))
  
  country_data <- country_data %>%
    mutate(VaxSaf = case_when(
      VaxSaf =="Somewhat Agree" ~ "somewhat agree/disagree",
      VaxSaf == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxSaf))
  
  country_data <- country_data %>%
    mutate(VaxEff = case_when(
      VaxEff == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxEff == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxEff))
  
  country_data <- country_data %>%
    mutate(VaxRel = case_when(
      VaxRel == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxRel == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxRel))
  
  country_data$VaxImpChild <- as.factor(country_data$VaxImpChild)
  
  country_data$VaxSaf <- as.factor(country_data$VaxSaf)
  
  country_data$VaxEff <- as.factor(country_data$VaxEff)
  
  country_data$VaxRel <- as.factor(country_data$VaxRel)
  
  
  # First step is to set the baseline categories for each variable.
  
  country_data <- within(country_data, Age <- relevel(Age, "18-24"))
  country_data <- within(country_data, Gender <- relevel(Gender, "Female"))
  country_data <- within(country_data, Religion <- relevel(Religion, "Other"))
  country_data <- within(country_data, Year <- relevel(Year, "2015"))
  country_data <- within(country_data, Education <- relevel(Education, "Primary or below"))
  country_data <- within(country_data, VaxImpChild <- relevel(VaxImpChild, "somewhat agree/disagree"))
  country_data <- within(country_data, VaxSaf <- relevel(VaxSaf,"somewhat agree/disagree"))
  country_data <- within(country_data, VaxEff<- relevel(VaxEff, "somewhat agree/disagree"))
  country_data <- within(country_data, VaxRel <- relevel(VaxRel,"somewhat agree/disagree"))  
  
  # Next, turn the data into a dataframe.
  
  country_data <- as.data.frame(country_data)
  
  # Create a model for each survey question.
  
  # Vaccines are important for children:
  
  m_imp <- multinom(VaxImpChild ~ Age + Gender + Education + Religion + Year, data = country_data)
  
  # Vaccines are safe:
  
  m_saf <- multinom(VaxSaf ~ Age + Gender + Education + Religion + Year, data = country_data)
  
  # Vaccines are effective
  
  m_eff <- multinom(VaxEff ~ Age + Gender + Education + Religion + Year, data = country_data)
  
  # Vaccines are compatible with my religious beliefs
  
  m_rel <- multinom(VaxRel ~ Age + Gender + Education + Religion + Year, data = country_data)
  
  # Next, create summary tables for each response for each survey question. 
  # These tables will include the OR values and their 95% confidence intervals, as well as
  # as their p-value.
  
  #Strongly agree that vaccines are important for children
  
  m_imp_1 <- as.data.frame(cbind(
    OR = exp(coef(m_imp))[1,],
    LCI = exp(confint(m_imp))[,,1][,1],
    UCI = exp(confint(m_imp))[,,1][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_imp) / summary(m_imp)$standard.errors))))[1,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  #Strongly disagree that vaccines are important for children
  
  m_imp_2 <- as.data.frame(cbind(
    OR = exp(coef(m_imp))[2,],
    LCI = exp(confint(m_imp))[,,2][,1],
    UCI = exp(confint(m_imp))[,,2][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_imp) / summary(m_imp)$standard.errors))))[2,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  
  #Strongly agree that vaccines are safe
  
  m_saf_1 <- as.data.frame(cbind(
    OR = exp(coef(m_saf))[1,],
    LCI = exp(confint(m_saf))[,,1][,1],
    UCI = exp(confint(m_saf))[,,1][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_saf) / summary(m_saf)$standard.errors))))[1,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  #Strongly disagree that vaccines are safe
  
  m_saf_2 <- as.data.frame(cbind(
    OR = exp(coef(m_saf))[2,],
    LCI = exp(confint(m_saf))[,,2][,1],
    UCI = exp(confint(m_saf))[,,2][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_saf) / summary(m_saf)$standard.errors))))[2,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  
  #Strongly agree that vaccines are effective
  
  m_eff_1 <- as.data.frame(cbind(
    OR = exp(coef(m_eff))[1,],
    LCI = exp(confint(m_eff))[,,1][,1],
    UCI = exp(confint(m_eff))[,,1][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_eff) / summary(m_eff)$standard.errors))))[1,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  #Strongly disagree that vaccines are effective
  m_eff_2 <- as.data.frame(cbind(
    OR = exp(coef(m_eff))[2,],
    LCI = exp(confint(m_eff))[,,2][,1],
    UCI = exp(confint(m_eff))[,,2][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_eff) / summary(m_eff)$standard.errors))))[2,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  
  #Strongly agree that vaccines are compatible with my religious beliefs
  
  m_rel_1 <- as.data.frame(cbind(
    OR = exp(coef(m_rel))[1,],
    LCI = exp(confint(m_rel))[,,1][,1],
    UCI = exp(confint(m_rel))[,,1][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_rel) / summary(m_rel)$standard.errors))))[1,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  # Strongly disagree that vaccines are compatible with my religious beliefs
  
  m_rel_2 <- as.data.frame(cbind(
    OR = exp(coef(m_rel))[2,],
    LCI = exp(confint(m_rel))[,,2][,1],
    UCI = exp(confint(m_rel))[,,2][,2],
    p_value = (2 * (1 - pnorm(abs(coef(m_rel) / summary(m_rel)$standard.errors))))[2,]
  )) %>%
    mutate(
      var = rownames(.),
      sig = case_when(
        p_value < 0.01 ~ "Very Significant",
        p_value <= 0.05 ~ "Significant",
        TRUE ~ "Not Significant"
      )
    )
  
  
  # The below code saves the tables made above into the environment.
  # Each table will include the name of the country specified in the function.
  
  assign(paste0(country_name, "_imp_1"), m_imp_1, envir = .GlobalEnv)
  assign(paste0(country_name, "_imp_2"), m_imp_2, envir = .GlobalEnv)
  
  assign(paste0(country_name, "_saf_1"), m_saf_1, envir = .GlobalEnv)
  assign(paste0(country_name, "_saf_2"), m_saf_2, envir = .GlobalEnv)
  
  assign(paste0(country_name, "_eff_1"), m_eff_1, envir = .GlobalEnv)
  assign(paste0(country_name, "_eff_2"), m_eff_2, envir = .GlobalEnv)
  
  assign(paste0(country_name, "_rel_1"), m_rel_1, envir = .GlobalEnv)
  assign(paste0(country_name, "_rel_2"), m_rel_2, envir = .GlobalEnv)
}



