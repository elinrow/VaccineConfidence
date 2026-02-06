model_plot <- function(dataset) {
  
  pooled_plot <- ggplot(dataset, aes(x = OR, y = var, colour = question, 
                                            shape = ifelse((LCI > 1 & UCI > 1) | (LCI < 1 & UCI < 1),
                                                           "Significant", "Not Significant"))) +
    geom_point(position = position_dodge(width = 0.8), size = 4)+
    geom_errorbar(
            aes(xmin = as.numeric(LCI), xmax = as.numeric(UCI)),
            position = position_dodge(width = 0.8),
            width = 0,
            size = 0.75  # This controls the thickness of the error bar line
          ) +
    facet_wrap(~answer, scales = "free_x", ncol = 2) +  # Facet by 'answer'
    scale_color_manual(
      name = "Question",  # Change legend title
      values = c("importance" = "navy", "safety" = "darkred", "effectiveness" = "orange", "religious_beliefs" = "darkgreen"),  # Define colors
      labels = c("importance" = "Vaccines are important for children to have", "safety" = "Vaccines are safe", 
                 "effectiveness" = "Vaccines are effective", "religious_beliefs" = "Vaccines are compatible with my religious beliefs")  # Rename legend labels
    ) + 
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    theme_minimal() +
    labs(
      x = "Odds Ratio (OR)",
      y = "Variables") + 
    scale_shape_manual(
      name = "Significance",
      values = c("Significant" = 17, "Not Significant" = 1)
    ) + 
    theme(legend.position = "bottom",
          strip.text = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(size = 18),
          axis.title = ggplot2::element_text(size = 18, face = "bold"),
          legend.text = ggplot2::element_text(size = 18),
          legend.title = ggplot2::element_blank()
          ) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2),
                    shape = ggplot2::guide_legend(ncol = 1))
  
  return(pooled_plot)
}  
