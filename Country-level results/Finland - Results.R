library(cowplot)
library(wesanderson)
library(patchwork)

finland <- country_level_data(data, "Finland", threshold = 100)
c_models(finland, "Finland")

fin_imp_1 <- model_plot(Finland_imp_1) + labs(
  subtitle = "Strongly Agree")


fin_imp_2 <- model_plot(Finland_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

fin_plot_1 <- fin_imp_1 + fin_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Finland - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

fin_plot_1

###
fin_saf_1 <- model_plot(Finland_saf_1) + labs(
  subtitle = "Strongly Agree")


fin_saf_2 <- model_plot(Finland_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

fin_plot_2 <- fin_saf_1 + fin_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Finland - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

fin_plot_2

###

fin_eff_1 <- model_plot(Finland_eff_1) + labs(
  subtitle = "Strongly Agree")


fin_eff_2 <- model_plot(Finland_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

fin_plot_3 <- fin_eff_1 + fin_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Finland - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

fin_plot_3

###

fin_rel_1 <- model_plot(Finland_rel_1) + labs(
  subtitle = "Strongly Agree")


fin_rel_2 <- model_plot(Finland_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

fin_plot_4 <- fin_rel_1 + fin_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Finland - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

fin_plot_4