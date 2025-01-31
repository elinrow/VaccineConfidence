italy <- country_level_data(data, "Italy", threshold = 100)
c_models(italy, "Italy")

ita_imp_1 <- model_plot(Italy_imp_1) + labs(
  subtitle = "Strongly Agree")


ita_imp_2 <- model_plot(Italy_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ita_plot_1 <- ita_imp_1 + ita_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Italy - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ita_plot_1

###
ita_saf_1 <- model_plot(Italy_saf_1) + labs(
  subtitle = "Strongly Agree")


ita_saf_2 <- model_plot(Italy_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ita_plot_2 <- ita_saf_1 + ita_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Italy - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ita_plot_2

###

ita_eff_1 <- model_plot(Italy_eff_1) + labs(
  subtitle = "Strongly Agree")


ita_eff_2 <- model_plot(Italy_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ita_plot_3 <- ita_eff_1 + ita_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Italy - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ita_plot_3

###

ita_rel_1 <- model_plot(Italy_rel_1) + labs(
  subtitle = "Strongly Agree")


ita_rel_2 <- model_plot(Italy_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ita_plot_4 <- ita_rel_1 + ita_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Italy - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ita_plot_4