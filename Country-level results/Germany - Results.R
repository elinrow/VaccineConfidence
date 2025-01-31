library(cowplot)
library(wesanderson)
library(patchwork)

germany <- country_level_data(data, "Germany", threshold = 100)
c_models(germany, "Germany")

ger_imp_1 <- model_plot(Germany_imp_1) + labs(
  subtitle = "Strongly Agree")


ger_imp_2 <- model_plot(Germany_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ger_plot_1 <- ger_imp_1 + ger_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Germany - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ger_plot_1

###
ger_saf_1 <- model_plot(Germany_saf_1) + labs(
  subtitle = "Strongly Agree")


ger_saf_2 <- model_plot(Germany_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ger_plot_2 <- ger_saf_1 + ger_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Germany - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ger_plot_2

###

ger_eff_1 <- model_plot(Germany_eff_1) + labs(
  subtitle = "Strongly Agree")


ger_eff_2 <- model_plot(Germany_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ger_plot_3 <- ger_eff_1 + ger_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Germany - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ger_plot_3

###

ger_rel_1 <- model_plot(Germany_rel_1) + labs(
  subtitle = "Strongly Agree")


ger_rel_2 <- model_plot(Germany_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

ger_plot_4 <- ger_rel_1 + ger_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Germany - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

ger_plot_4