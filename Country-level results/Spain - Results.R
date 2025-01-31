spain <- country_level_data(data, "Spain", threshold = 100)
c_models(spain, "Spain")

spa_imp_1 <- model_plot(Spain_imp_1) + labs(
  subtitle = "Strongly Agree")


spa_imp_2 <- model_plot(Spain_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

spa_plot_1 <- spa_imp_1 + spa_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Spain - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

spa_plot_1

###
spa_saf_1 <- model_plot(Spain_saf_1) + labs(
  subtitle = "Strongly Agree")


spa_saf_2 <- model_plot(Spain_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

spa_plot_2 <- spa_saf_1 + spa_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Spain - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

spa_plot_2

###

spa_eff_1 <- model_plot(Spain_eff_1) + labs(
  subtitle = "Strongly Agree")


spa_eff_2 <- model_plot(Spain_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

spa_plot_3 <- spa_eff_1 + spa_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Spain - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

spa_plot_3

###

spa_rel_1 <- model_plot(Spain_rel_1) + labs(
  subtitle = "Strongly Agree")


spa_rel_2 <- model_plot(Spain_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

spa_plot_4 <- spa_rel_1 + spa_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Spain - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

spa_plot_4