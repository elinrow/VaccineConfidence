ts_data <- read.csv("Results/tsData.csv")

corr_mcv1 <- ts_data %>%
  group_by(Country) %>%
  filter(sd(MCV1) != 0) %>%
  summarise(imp_cor = cor(VaxImpChild, MCV1, method = "spearman"), 
            saf_cor = cor(VaxSaf, MCV1, method = "spearman"), 
            eff_cor = cor(VaxEff, MCV1, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV1",27)) %>%
  mutate(Question = factor(Question, levels = c("imp_cor", "saf_cor", "eff_cor")))  # Specify order

corr_mcv1_rel <- ts_data %>%
  group_by(Country) %>%
  filter(Year != 2018) %>%
  filter(sd(MCV1) != 0) %>%
  summarise(rel_cor = cor(VaxRel, MCV1, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV1",9)) %>%
  mutate(Question = factor(Question, levels = c("rel_cor")))  # Specify order

corr_mcv1 <- rbind(corr_mcv1, corr_mcv1_rel) |>
  dplyr::arrange(desc(Country))

corr_mcv2 <- ts_data %>%
  filter(!Country %in% c("Finland","Ireland")) %>%
  group_by(Country) %>%
  filter(sd(MCV2) != 0) %>%
  summarize(imp_cor = cor(VaxImpChild, MCV2, method = "spearman"), 
            saf_cor = cor(VaxSaf, MCV2, method = "spearman"), 
            eff_cor = cor(VaxEff, MCV2, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV2", 21)) %>%
  mutate(Question = factor(Question, levels = c("imp_cor", "saf_cor", "eff_cor")))  # Specify order

corr_mcv2_rel <- ts_data %>%
  filter(!Country %in% c("Finland","Ireland")) %>%
  group_by(Country) %>%
  filter(Year != 2018) %>%
  filter(sd(MCV2) != 0) %>%
  summarise(rel_cor = cor(VaxRel, MCV2, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV2",7)) %>%
  mutate(Question = factor(Question, levels = c("rel_cor")))  # Specify order

corr_mcv2_fin <- ts_data %>%
  filter(Country == "Finland",
         Year != 2015) %>%
  group_by(Country) %>%
  filter(sd(MCV2) != 0) %>%
  summarize(imp_cor = cor(VaxImpChild, MCV2, method = "spearman"), 
            saf_cor = cor(VaxSaf, MCV2, method = "spearman"), 
            eff_cor = cor(VaxEff, MCV2, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV2",3)) %>%
  mutate(Question = factor(Question, levels = c("imp_cor", "saf_cor", "eff_cor")))  # Specify order

corr_mcv2_fin_rel <- ts_data %>%
  filter(Country == "Finland") %>%
  group_by(Country) %>%
  filter(!Year %in% c(2015,2018)) %>%
  filter(sd(MCV2) != 0) %>%
  summarise(rel_cor = cor(VaxRel, MCV2, method = "spearman")) %>%
  pivot_longer(cols = -Country, names_to = "Question", values_to = "Correlation") %>%
  mutate(Dose = rep("MCV2",1)) %>%
  mutate(Question = factor(Question, levels = c("rel_cor")))

corr_mcv2 <- rbind(corr_mcv2, corr_mcv2_rel,
                   corr_mcv2_fin, corr_mcv2_fin_rel) |>
  dplyr::arrange(desc(Country))

# Function to create the heatmap
create_heatmap <- function(data, dose_type) {
  ggplot(data, aes(x = Question, y = Country, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 4) +  # Add text labels
    scale_fill_gradient2(low = "#FF6565", mid = "white", high = "#71A7F7",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Correlation") +
    scale_x_discrete(labels = c("imp_cor" = "Vaccines are important\nfor children to have", 
                                "saf_cor" = "Vaccines are safe", 
                                "eff_cor" = "Vaccines are effective",
                                "rel_cor" = "Vaccines are compatible\nwith my beliefs")) +
    scale_y_discrete(limits = rev) + 
    theme_minimal() +
    labs(x = "Questions", y = "Country") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"))
}

# Plot heatmaps for MCV1 and MCV2
heatmap_MCV1_c <- create_heatmap(corr_mcv1, "MCV1")
heatmap_MCV2_c <- create_heatmap(corr_mcv2, "MCV2")

heatmap_MCV1_c
heatmap_MCV2_c
