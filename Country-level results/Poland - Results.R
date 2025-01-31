poland <- country_level_data(data, "Poland", threshold = 100)
c_models(poland, "Poland")

pol_imp_1 <- model_plot(Poland_imp_1) + labs(
  subtitle = "Strongly Agree")


pol_imp_2 <- model_plot(Poland_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

pol_plot_1 <- pol_imp_1 + pol_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Poland - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

pol_plot_1

###
pol_saf_1 <- model_plot(Poland_saf_1) + labs(
  subtitle = "Strongly Agree")


pol_saf_2 <- model_plot(Poland_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

pol_plot_2 <- pol_saf_1 + pol_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Poland - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

pol_plot_2

###

pol_eff_1 <- model_plot(Poland_eff_1) + labs(
  subtitle = "Strongly Agree")


pol_eff_2 <- model_plot(Poland_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

pol_plot_3 <- pol_eff_1 + pol_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Poland - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

pol_plot_3

###

pol_rel_1 <- model_plot(Poland_rel_1) + labs(
  subtitle = "Strongly Agree")


pol_rel_2 <- model_plot(Poland_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

pol_plot_4 <- pol_rel_1 + pol_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Poland - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

pol_plot_4