pooled_models(data, threshold = 100)

pooled_plot <- model_plot(pooled_results)

ggsave("Results/pooled_plot.png", plot = pooled_plot, bg = "white",
       width = 10)

write.csv(pooled_results, "Results/pooledResults.csv")

