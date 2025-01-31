library(cowplot)
library(wesanderson)
library(patchwork)

austria <- country_level_data(data, "Austria", threshold = 100)
c_models(austria, "Austria")

aus_imp_1 <- model_plot(Austria_imp_1) + labs(
  subtitle = "Strongly Agree")


aus_imp_2 <- model_plot(Austria_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

aus_plot_1 <- aus_imp_1 + aus_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Austria - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

aus_plot_1

###
aus_saf_1 <- model_plot(Austria_saf_1) + labs(
  subtitle = "Strongly Agree")


aus_saf_2 <- model_plot(Austria_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

aus_plot_2 <- aus_saf_1 + aus_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Austria - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

aus_plot_2

###

aus_eff_1 <- model_plot(Austria_eff_1) + labs(
  subtitle = "Strongly Agree")


aus_eff_2 <- model_plot(Austria_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

aus_plot_3 <- aus_eff_1 + aus_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Austria - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

aus_plot_3

###

aus_rel_1 <- model_plot(Austria_rel_1) + labs(
  subtitle = "Strongly Agree")


aus_rel_2 <- model_plot(Austria_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

aus_plot_4 <- aus_rel_1 + aus_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Austria - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

aus_plot_4
