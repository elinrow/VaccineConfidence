model_plot <- function(dataset) {
  
  pooled_plot <- ggplot(dataset, aes(x = OR, y = var, colour = question, 
                                            shape = ifelse((LCI > 1 & UCI > 1) | (LCI < 1 & UCI < 1),
                                                           "Significant", "Not Significant"))) +
    geom_pointrange(aes(xmin = LCI, xmax = UCI), position = position_dodge(width = 0.5)) +
    facet_wrap(~answer, labeller = labeller(answer = c("strongly_agree" = "Strongly Agree", "strongly_disagree" = "Strongly Disagree"))) +  # Facet by 'answer'
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
      y = "Variables",
      title = "Responses to Survey Questions on Vaccine Confidence") + 
    scale_shape_manual(
      name = "Significance",
      values = c("Significant" = 17, "Not Significant" = 1)  # Choose your own shapes!
    ) + 
    theme(legend.position = "right")
  
  return(pooled_plot)
}  
