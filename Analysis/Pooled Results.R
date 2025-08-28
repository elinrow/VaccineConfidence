pooled_models(data, threshold = 100)

pooled_results <- pooled_results |>
  mutate(var = stringr::str_replace(var, "Year", "Year: "),
         var = stringr::str_replace(var, "Religion", "Religion: "),
         var = stringr::str_replace(var, "Gender", "Gender: "),
         var = stringr::str_replace(var, "Education", "Education: "),
         var = stringr::str_replace(var, "Age", "Age: "))

pooled_plot <- model_plot(pooled_results)

ggsave("Results/pooled_plot.png", plot = pooled_plot, bg = "white",
       width = 18, height = 18)

write.csv(pooled_results, "Results/pooledResults.csv")

