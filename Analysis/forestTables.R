library(forestploter)
library(stringr)

# Load your data
dt <- read_csv(here("Results", "pooledResults.csv"))

# Step 1: Format strongly agree
agree_df <- dt %>%
  filter(answer == "strongly_agree") %>%
  mutate(
    OR_sa = sprintf("%.2f (%.2f–%.2f)", OR, LCI, UCI),
    plot_col_sa = ""
  ) %>%
  select(question, var, plot_col_sa, OR_sa, OR_sa_num = OR, LCI_sa = LCI, UCI_sa = UCI)

# Step 2: Format strongly disagree
disagree_df <- dt %>%
  filter(answer == "strongly_disagree") %>%
  mutate(
    OR_sd = sprintf("%.2f (%.2f–%.2f)", OR, LCI, UCI),
    plot_col_sd = ""
  ) %>%
  select(question, var, plot_col_sd, OR_sd, OR_sd_num = OR, LCI_sd = LCI, UCI_sd = UCI)

# Step 3: Join both sets
combined <- full_join(agree_df, disagree_df, by = c("question", "var")) %>%
  mutate(row_type = "data") %>%
  # Add new columns to hold the color based on significance
  mutate(
    # A result is significant if its CI does not cross 1
    sig_color_sa = case_when(
      LCI_sa > 1 | UCI_sa < 1 ~ "#465977", # Custom color for significance
      TRUE ~ "grey50" # Grey for non-significant
    ),
    sig_color_sd = case_when(
      LCI_sd > 1 | UCI_sd < 1 ~ "#B30000",
      TRUE ~ "grey50"
    )
  )

# Step 4: Create header rows for each question
headers <- dt %>%
  distinct(question) %>%
  mutate(
    var = str_to_title(str_replace_all(question, "_", " ")),  # Pretty header
    plot_col_sa = "",  # Empty instead of NA to avoid printing "NA"
    OR_sa = "",
    plot_col_sd = "",
    OR_sd = "",
    OR_sa_num = NA_real_,
    LCI_sa = NA_real_,
    UCI_sa = NA_real_,
    OR_sd_num = NA_real_,
    LCI_sd = NA_real_,
    UCI_sd = NA_real_,
    row_type = "header"
  ) %>%
  mutate(
    sig_color_sa = NA_character_,
    sig_color_sd = NA_character_
  ) %>%
  select(names(combined), sig_color_sa, sig_color_sd)  # Ensure same column order

# Step 5: Bind headers and data
full_table <- bind_rows(headers, combined) %>%
  arrange(
    factor(question, levels = c("importance", "effectiveness", "safety", "religious_beliefs")),
    desc(row_type)
  )

# Step 6: Prepare table data for forestploter
plot_data <- full_table |>
  filter(question == "importance")

forest_data <- plot_data |>
  select(c("var", "plot_col_sa", "OR_sa", "plot_col_sd", "OR_sd")) |>
  mutate(var = ifelse(var == "Importance", " ",var)) |>
  rename("I think vaccines are important\nfor children to have" = "var",
         "                        " = "plot_col_sa",
         "                         " = "plot_col_sd",
         "Strongly Agree" = "OR_sa",
         "Strongly Disagree" = "OR_sd")

# Step 7: Draw the forest plot with the corrected arguments
forest(
  forest_data,
  est = list(plot_data$OR_sa_num, plot_data$OR_sd_num),
  lower = list(plot_data$LCI_sa, plot_data$LCI_sd),
  upper = list(plot_data$UCI_sa, plot_data$UCI_sd),
  ci_column = c(2, 4),
  ref_line = 1,
  xlim = c(0, 3),
  box_size = 0.5,
  line_height = unit(8, "mm")
)

plot_data <- full_table |>
  filter(question == "effectiveness")

forest_data <- plot_data |>
  select(c("var", "plot_col_sa", "OR_sa", "plot_col_sd", "OR_sd")) |>
  mutate(var = ifelse(var == "Effectiveness", " ",var)) |>
  rename("I think vaccines are effective" = "var",
         "                        " = "plot_col_sa",
         "                         " = "plot_col_sd",
         "Strongly Agree" = "OR_sa",
         "Strongly Disagree" = "OR_sd")

# Step 7: Draw the forest plot with the corrected arguments
forest(
  forest_data,
  est = list(plot_data$OR_sa_num, plot_data$OR_sd_num),
  lower = list(plot_data$LCI_sa, plot_data$LCI_sd),
  upper = list(plot_data$UCI_sa, plot_data$UCI_sd),
  ci_column = c(2, 4),
  ref_line = 1,
  xlim = c(0, 3),
  box_size = 0.5,
  line_height = unit(8, "mm")
)

plot_data <- full_table |>
  filter(question == "religious_beliefs")

forest_data <- plot_data |>
  select(c("var", "plot_col_sa", "OR_sa", "plot_col_sd", "OR_sd")) |>
  mutate(var = ifelse(var == "Religious Beliefs", " ",var)) |>
  rename("I think vaccines are compatible\nwith my religious beliefs" = "var",
         "                        " = "plot_col_sa",
         "                         " = "plot_col_sd",
         "Strongly Agree" = "OR_sa",
         "Strongly Disagree" = "OR_sd")

# Step 7: Draw the forest plot with the corrected arguments
forest(
  forest_data,
  est = list(plot_data$OR_sa_num, plot_data$OR_sd_num),
  lower = list(plot_data$LCI_sa, plot_data$LCI_sd),
  upper = list(plot_data$UCI_sa, plot_data$UCI_sd),
  ci_column = c(2, 4),
  ref_line = 1,
  xlim = c(0, 3),
  box_size = 0.5,
  line_height = unit(8, "mm")
)

