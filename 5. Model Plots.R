library(ggplot2)
library(dplyr)

model_plot <- function(dataset) {
  
  dataset <- dataset %>%
    mutate(Significance = ifelse(
      LCI > 1 & UCI > 1 | LCI < 1 & UCI < 1,
      "Significant",
      "Not Significant"
    ), 
    Variable = rownames(.)) %>%
    filter(Variable != "(Intercept)")
  
  
  # Create the plot
  p <- ggplot(dataset, aes(x = OR, y = Variable, colour = Significance)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = LCI, xmax = UCI)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black") + 
    labs(
      x = "Odds Ratio",
      y = " "
    ) + 
    scale_color_manual(values = custom_colors, drop = FALSE) +  
    theme_minimal() +
    guides(colour = guide_legend(override.aes = list(size = 3)))
  
  return(p)
}  
