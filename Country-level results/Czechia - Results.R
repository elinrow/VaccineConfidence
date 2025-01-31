library(cowplot)
library(wesanderson)
library(patchwork)

czechia <- country_level_data(data, "Czechia", threshold = 100)
c_models(czechia, "Czechia")

cze_imp_1 <- model_plot(Czechia_imp_1) + labs(
  subtitle = "Strongly Agree")


cze_imp_2 <- model_plot(Czechia_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

cze_plot_1 <- cze_imp_1 + cze_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Czechia - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

cze_plot_1

###
cze_saf_1 <- model_plot(Czechia_saf_1) + labs(
  subtitle = "Strongly Agree")


cze_saf_2 <- model_plot(Czechia_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

cze_plot_2 <- cze_saf_1 + cze_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Czechia - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

cze_plot_2

###

cze_eff_1 <- model_plot(Czechia_eff_1) + labs(
  subtitle = "Strongly Agree")


cze_eff_2 <- model_plot(Czechia_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

cze_plot_3 <- cze_eff_1 + cze_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Czechia - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

cze_plot_3

###

cze_rel_1 <- model_plot(Czechia_rel_1) + labs(
  subtitle = "Strongly Agree")


cze_rel_2 <- model_plot(Czechia_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

cze_plot_4 <- cze_rel_1 + cze_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Czechia - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

cze_plot_4
