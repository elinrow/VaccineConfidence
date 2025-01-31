ireland <- country_level_data(data, "Ireland", threshold = 100)
c_models(ireland, "Ireland")

ire_imp_1 <- model_plot(Ireland_imp_1) + labs(
  subtitle = "Strongly Agree")


ire_imp_2 <- model_plot(Ireland_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ire_plot_1 <- ire_imp_1 + ire_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Ireland - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ire_plot_1

###
ire_saf_1 <- model_plot(Ireland_saf_1) + labs(
  subtitle = "Strongly Agree")


ire_saf_2 <- model_plot(Ireland_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ire_plot_2 <- ire_saf_1 + ire_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Ireland - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ire_plot_2

###

ire_eff_1 <- model_plot(Ireland_eff_1) + labs(
  subtitle = "Strongly Agree")


ire_eff_2 <- model_plot(Ireland_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ire_plot_3 <- ire_eff_1 + ire_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Ireland - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ire_plot_3

###

ire_rel_1 <- model_plot(Ireland_rel_1) + labs(
  subtitle = "Strongly Agree")


ire_rel_2 <- model_plot(Ireland_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ire_plot_4 <- ire_rel_1 + ire_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Ireland - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ire_plot_4