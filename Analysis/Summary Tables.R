table_data <- data |>
  mutate(Religion = case_when(
    Religion == "Jewish" ~ "Other",
    Religion == "Hindu" ~ "Other",
    Religion == "Buddhist" ~ "Other",
    TRUE ~ Religion
  ))

table_data$VaxImpChild <- factor(
  table_data$VaxImpChild,
  levels = rev(c("Strongly Disagree", "Somewhat Disagree", "Do not know", "Somewhat Agree", "Strongly Agree"))
)

table_data$VaxSaf <- factor(
  table_data$VaxSaf,
  levels = rev(c("Strongly Disagree", "Somewhat Disagree", "Do not know", "Somewhat Agree", "Strongly Agree"))
)

table_data$VaxEff <- factor(
  table_data$VaxEff,
  levels = rev(c("Strongly Disagree", "Somewhat Disagree", "Do not know", "Somewhat Agree", "Strongly Agree"))
)

table_data$VaxRel <- factor(
  table_data$VaxRel,
  levels = rev(c("Strongly Disagree", "Somewhat Disagree", "Do not know", "Somewhat Agree", "Strongly Agree"))
)
  
label(table_data$VaxImpChild) <- "I think vaccines are important for children to have"
label(table_data$VaxSaf) <- "I think vaccines are safe"
label(table_data$VaxEff) <- "I think vaccines are effective"
label(table_data$VaxRel) <- "I think vaccines are compatible with my religious beliefs"

table_html <- table1(~ Age + Gender + Religion + Education + VaxImpChild + VaxSaf + VaxEff + VaxRel| Country, 
                     data = table_data, caption = "Country-level characteristics")

table_html

table <- table1(~ Country, data = table_data)

pre_data <- droplevels(pre_data)
  

table2 <- table1(~ Country, data = pre_data)

table2
