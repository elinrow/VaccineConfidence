library(cowplot)
library(wesanderson)
library(patchwork)

bulgaria <- country_level_data(data, "Bulgaria", threshold = 100)
c_models(bulgaria, "Bulgaria")

bul_imp_1 <- model_plot(Bulgaria_imp_1) + labs(
  subtitle = "Strongly Agree")


bul_imp_2 <- model_plot(Bulgaria_imp_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

bul_plot_1 <- bul_imp_1 + bul_imp_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Bulgaria - Vaccines are important for children to have",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

bul_plot_1

###
bul_saf_1 <- model_plot(Bulgaria_saf_1) + labs(
  subtitle = "Strongly Agree")


bul_saf_2 <- model_plot(Bulgaria_saf_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

bul_plot_2 <- bul_saf_1 + bul_saf_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Bulgaria - Vaccines are safe",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

bul_plot_2

###

bul_eff_1 <- model_plot(Bulgaria_eff_1) + labs(
  subtitle = "Strongly Agree")


bul_eff_2 <- model_plot(Bulgaria_eff_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

bul_plot_3 <- bul_eff_1 + bul_eff_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Bulgaria - Vaccines are effective",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

bul_plot_3

###

bul_rel_1 <- model_plot(Bulgaria_rel_1) + labs(
  subtitle = "Strongly Agree")


bul_rel_2 <- model_plot(Bulgaria_rel_2) + labs(
  subtitle = "Strongly Disagree") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()  
  )

bul_plot_4 <- bul_rel_1 + bul_rel_2 + plot_layout(guides = 'collect') +
  plot_annotation(title = "Bulgaria - Vaccines are compatible with my beliefs",
                  theme = theme(plot.title = element_text(hjust = 0.5))) 

bul_plot_4