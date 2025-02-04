pooled_models <- function(data, threshold) {
  
  data$VaxImpChild <- as.character(data$VaxImpChild)
  
  data$VaxSaf <- as.character(data$VaxSaf)
  
  data$VaxEff <- as.character(data$VaxEff)
  
  data$VaxRel <- as.character(data$VaxRel)
  
  data <- data %>%
    mutate(VaxImpChild = case_when(
      VaxImpChild == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxImpChild == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxImpChild  # Retains other values as they are
    ))
  
  data <- data %>%
    mutate(VaxSaf = case_when(
      VaxSaf =="Somewhat Agree" ~ "somewhat agree/disagree",
      VaxSaf == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxSaf))
  
  data <- data %>%
    mutate(VaxEff = case_when(
      VaxEff == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxEff == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxEff))
  
  data <- data %>%
    mutate(VaxRel = case_when(
      VaxRel == "Somewhat Agree" ~ "somewhat agree/disagree",
      VaxRel == "Somewhat Disagree" ~ "somewhat agree/disagree",
      TRUE ~ VaxRel))
  
  data$VaxImpChild <- as.factor(data$VaxImpChild)
  
  data$VaxSaf <- as.factor(data$VaxSaf)
  
  data$VaxEff <- as.factor(data$VaxEff)
  
  data$VaxRel <- as.factor(data$VaxRel)
  
  data <- droplevels(data)
  
  # Get the count of each religious group.
  religion_counts <- table(data$Religion)
  
  # Identify religions with a count less than the threshold.
  other_religions <- names(religion_counts[religion_counts < threshold])
  
  # Combine the religions below the threshold into one variable called 'Other'.
  levels(data$Religion)[levels(data$Religion) %in% other_religions] <- "Other"
  
  # Drop unused levels.
  data <- droplevels(data)
  
  
  # First step is to set the baseline categories for each variable.
  
  data <- within(data, Age <- relevel(Age, "18-24"))
  data <- within(data, Gender <- relevel(Gender, "Female"))
  data <- within(data, Religion <- relevel(Religion, "Atheist/Agnostic/No Religion"))
  data <- within(data, Year <- relevel(Year, "2015"))
  data <- within(data, Education <- relevel(Education, "Primary or below"))
  data <- within(data, VaxImpChild <- relevel(VaxImpChild, "somewhat agree/disagree"))
  data <- within(data, VaxSaf <- relevel(VaxSaf,"somewhat agree/disagree"))
  data <- within(data, VaxEff<- relevel(VaxEff, "somewhat agree/disagree"))
  data <- within(data, VaxRel <- relevel(VaxRel,"somewhat agree/disagree"))  
  
  # Next, turn the data into a dataframe.
  
  data <- as.data.frame(data)
  
  # Create a model for each survey question.
  
  # Vaccines are important for children:
  
  m_imp <- multinom(VaxImpChild ~ Age + Gender + Education + Religion + Year, data = data)
  
  # Vaccines are safe:
  
  m_saf <- multinom(VaxSaf ~ Age + Gender + Education + Religion + Year, data = data)
  
  # Vaccines are effective
  
  m_eff <- multinom(VaxEff ~ Age + Gender + Education + Religion + Year, data = data)
  
  # Vaccines are compatible with my religious beliefs
  
  m_rel <- multinom(VaxRel ~ Age + Gender + Education + Religion + Year, data = data)
  
  # Next, create summary tables for each response for each survey question. 
  # These tables will include the OR values and their 95% confidence intervals, as well as
  # as their p-value.
  
  #Strongly agree that vaccines are important for children
  
  m_imp_1 <- as.data.frame(cbind(
    OR = exp(coef(m_imp))[1,],
    LCI = exp(confint(m_imp))[,,1][,1],
    UCI = exp(confint(m_imp))[,,1][,2]
  )) %>%
    mutate(question = "importance",
           answer = "strongly_agree") %>%
    rownames_to_column(var = "var")
  
  #Strongly disagree that vaccines are important for children
  
  m_imp_2 <- as.data.frame(cbind(
    OR = exp(coef(m_imp))[2,],
    LCI = exp(confint(m_imp))[,,2][,1],
    UCI = exp(confint(m_imp))[,,2][,2]
  )) %>%
    mutate(question = "importance",
           answer = "strongly_disagree") %>%
    rownames_to_column(var = "var")
  
  m_imp_table <- rbind(m_imp_1,m_imp_2)
  
  #Strongly agree that vaccines are safe
  
  m_saf_1 <- as.data.frame(cbind(
    OR = exp(coef(m_saf))[1,],
    LCI = exp(confint(m_saf))[,,1][,1],
    UCI = exp(confint(m_saf))[,,1][,2]
  )) %>%
    mutate(question = "safety",
           answer = "strongly_agree") %>%
    rownames_to_column(var = "var")
  
  
  #Strongly disagree that vaccines are safe
  
  m_saf_2 <- as.data.frame(cbind(
    OR = exp(coef(m_saf))[2,],
    LCI = exp(confint(m_saf))[,,2][,1],
    UCI = exp(confint(m_saf))[,,2][,2]
  )) %>%
    mutate(question = "safety",
           answer = "strongly_disagree") %>%
    rownames_to_column(var = "var")
  
  m_saf_table <- rbind(m_saf_1,m_saf_2)
  
  
  #Strongly agree that vaccines are effective
  
  m_eff_1 <- as.data.frame(cbind(
    OR = exp(coef(m_eff))[1,],
    LCI = exp(confint(m_eff))[,,1][,1],
    UCI = exp(confint(m_eff))[,,1][,2]
  )) %>%
    mutate(question = "effectiveness",
           answer = "strongly_agree") %>%
    rownames_to_column(var = "var")
  
  #Strongly disagree that vaccines are effective
  m_eff_2 <- as.data.frame(cbind(
    OR = exp(coef(m_eff))[2,],
    LCI = exp(confint(m_eff))[,,2][,1],
    UCI = exp(confint(m_eff))[,,2][,2]
  )) %>%
    mutate(question = "effectiveness",
           answer = "strongly_disagree") %>%
    rownames_to_column(var = "var")
  
  m_eff_table <- rbind(m_eff_1,m_eff_2)
  
  #Strongly Agree that vaccines are compatible with my beliefs
  
  m_rel_1 <- as.data.frame(cbind(
    OR = exp(coef(m_rel))[1,],
    LCI = exp(confint(m_rel))[,,1][,1],
    UCI = exp(confint(m_rel))[,,1][,2]
  )) %>%
    mutate(question = "religious_beliefs",
           answer = "strongly_agree") %>%
    rownames_to_column(var = "var")
  
  # Strongly disagree that vaccines are compatible with my religious beliefs
  
  m_rel_2 <- as.data.frame(cbind(
    OR = exp(coef(m_rel))[2,],
    LCI = exp(confint(m_rel))[,,2][,1],
    UCI = exp(confint(m_rel))[,,2][,2]
  )) %>%
    mutate(question = "religious_beliefs",
           answer = "strongly_disagree") %>%
    rownames_to_column(var = "var")
  
  m_rel_table <- rbind(m_rel_1,m_rel_2)
  
  # The below code saves the tables made above into the environment.
  # Each table will include the name of the country specified in the function.
  
  pooled_results <- rbind(m_imp_table, m_saf_table, m_eff_table, m_rel_table) %>%
    filter(var != "(Intercept)")
  
  assign("pooled_results", pooled_results, envir = .GlobalEnv)
}
