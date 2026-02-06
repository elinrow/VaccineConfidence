pooled_models(data, threshold = 100)

pooled_results_a <- pooled_results |>
  mutate(var = stringr::str_replace(var, "Year", "Year: "),
         var = stringr::str_replace(var, "Religion", "Religion: "),
         var = stringr::str_replace(var, "Gender", "Gender: "),
         var = stringr::str_replace(var, "Education", "Education: "),
         var = stringr::str_replace(var, "Age", "Age: ")) |>
  filter(answer == "strongly_agree")

pooled_plot_a <- model_plot(pooled_results_a)

ggsave("Results/pooled_plot_a.png", plot = pooled_plot_a, bg = "white",
       width = 18, height = 18)

pooled_results_b <- pooled_results |>
  mutate(var = stringr::str_replace(var, "Year", "Year: "),
         var = stringr::str_replace(var, "Religion", "Religion: "),
         var = stringr::str_replace(var, "Gender", "Gender: "),
         var = stringr::str_replace(var, "Education", "Education: "),
         var = stringr::str_replace(var, "Age", "Age: ")) |>
  filter(answer == "strongly_disagree")

pooled_plot_b <- model_plot(pooled_results_b)

ggsave("Results/pooled_plot_b.png", plot = pooled_plot_b, bg = "white",
       width = 18, height = 18)

write.csv(pooled_results, "Results/pooledResults.csv")

