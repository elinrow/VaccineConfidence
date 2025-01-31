romania <- country_level_data(data, "Romania", threshold = 100)
romania_models(romania, "Romania")

rom_imp_1 <- model_plot(Romania_imp_1) + labs(
  subtitle = "Strongly Agree")


rom_imp_2 <- model_plot(Romania_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

rom_plot_1 <- rom_imp_1 + rom_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Romania - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

rom_plot_1

###
rom_saf_1 <- model_plot(Romania_saf_1) + labs(
  subtitle = "Strongly Agree")


rom_saf_2 <- model_plot(Romania_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

rom_plot_2 <- rom_saf_1 + rom_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Romania - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

rom_plot_2

###

rom_eff_1 <- model_plot(Romania_eff_1) + labs(
  subtitle = "Strongly Agree")


rom_eff_2 <- model_plot(Romania_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

rom_plot_3 <- rom_eff_1 + rom_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Romania - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

rom_plot_3

###

rom_rel_1 <- model_plot(Romania_rel_1) + labs(
  subtitle = "Strongly Agree")


rom_rel_2 <- model_plot(Romania_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

rom_plot_4 <- rom_rel_1 + rom_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Romania - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

rom_plot_4