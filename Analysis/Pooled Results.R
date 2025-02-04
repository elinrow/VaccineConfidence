pooled_models(data, threshold = 100)

pooled_plot <- model_plot(pooled_results)

ggsave("Results/pooled_plot.png", plot = pooled_plot)

write.csv(pooled_results, "Results/pooledResults.csv")
