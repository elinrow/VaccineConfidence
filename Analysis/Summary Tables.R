library(htmltools)

table_data <- data |>
  mutate(Religion = case_when(
    Religion == "Jewish" ~ "Other",
    Religion == "Hindu" ~ "Other",
    Religion == "Buddhist" ~ "Other",
    TRUE ~ Religion
  ),
  VaxImpChild = case_when(
    VaxImpChild == "Somewhat Agree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxImpChild == "Somewhat Disagree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxImpChild == "Do not know" ~ "Somewhat Agree / Disagree / Do not know",
    TRUE ~ VaxImpChild
  ),
  VaxSaf = case_when(
    VaxSaf == "Somewhat Agree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxSaf == "Somewhat Disagree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxSaf == "Do not know" ~ "Somewhat Agree / Disagree / Do not know",
    TRUE ~ VaxSaf
  ),
  VaxEff = case_when(
    VaxEff == "Somewhat Agree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxEff == "Somewhat Disagree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxEff == "Do not know" ~ "Somewhat Agree / Disagree / Do not know",
    TRUE ~ VaxEff
  ),
  VaxRel = case_when(
    VaxRel == "Somewhat Agree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxRel == "Somewhat Disagree" ~ "Somewhat Agree / Disagree / Do not know",
    VaxRel == "Do not know" ~ "Somewhat Agree / Disagree / Do not know",
    TRUE ~ VaxRel
  )
  )

table_data$VaxImpChild <- factor(
  table_data$VaxImpChild,
  levels = rev(c("Strongly Disagree", "Somewhat Agree / Disagree / Do not know", "Strongly Agree"))
)

table_data$VaxSaf <- factor(
  table_data$VaxSaf,
  levels = rev(c("Strongly Disagree", "Somewhat Agree / Disagree / Do not know", "Strongly Agree"))
)

table_data$VaxEff <- factor(
  table_data$VaxEff,
  levels = rev(c("Strongly Disagree", "Somewhat Agree / Disagree / Do not know", "Strongly Agree"))
)

table_data$VaxRel <- factor(
  table_data$VaxRel,
  levels = rev(c("Strongly Disagree", "Somewhat Agree / Disagree / Do not know", "Strongly Agree"))
)
  
label(table_data$VaxImpChild) <- "I think vaccines are important for children to have"
label(table_data$VaxSaf) <- "I think vaccines are safe"
label(table_data$VaxEff) <- "I think vaccines are effective"
label(table_data$VaxRel) <- "I think vaccines are compatible with my religious beliefs"

table_html <- table1(~ VaxImpChild + VaxSaf + VaxEff + VaxRel|Country, 
                     data = table_data, caption = " ")

save_html(table_html, file = "Results/table1_output.html")

table_html <- table1(~ Age + Gender + Religion + Education + Year|Country, 
                     data = table_data, caption = " ")

save_html(table_html, file = "Results/table1_output.html")
