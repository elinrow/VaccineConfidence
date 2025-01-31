# Code to Run

library(patchwork)

pooled_models(data, threshold = 100)

######

pool_imp_1 <- model_plot(Pooled_imp_1) + labs(
                  subtitle = "Strongly Agree")


pool_imp_2 <- model_plot(Pooled_imp_2) + labs(
                  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
                  )

combined_plot_1 <- pool_imp_1 + pool_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 


combined_plot_1

#######

pool_saf_1 <- model_plot(Pooled_saf_1) + labs(
  subtitle = "Strongly Agree")

pool_saf_2 <- model_plot(Pooled_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

combined_plot_2 <- pool_saf_1 + pool_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

combined_plot_2

##########

pool_eff_1 <- model_plot(Pooled_eff_1) + labs(
  subtitle = "Strongly Agree")

pool_eff_2 <- model_plot(Pooled_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

combined_plot_3 <- pool_eff_1 + pool_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

combined_plot_3

##########

pool_rel_1 <- model_plot(Pooled_rel_1) + labs(
  subtitle = "Strongly Agree")

pool_rel_2 <- model_plot(Pooled_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

combined_plot_4 <- pool_rel_1 + pool_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

combined_plot_4
