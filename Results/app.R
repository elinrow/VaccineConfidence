# Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(table1)

# Load Data
data <- read.csv("cleanedData.csv")
pooled <- read.csv("pooledResults.csv")[,-1] %>%
  mutate(OR = round(OR,2),
         LCI = round(LCI,2),
         UCI = round(UCI,2))

country_level <- read.csv("countryLevelResults.csv")[,-1] %>%
  mutate(OR = round(OR,2),
         LCI = round(LCI,2),
         UCI = round(UCI,2))

colnames(pooled) <- c("Variable", "OR", "LCI", "UCI", "Question", "Response")
colnames(country_level) <- c("Variable", "OR", "LCI", "UCI", "Question", "Response", "Country")

ts_data <- read.csv("tsData.csv")[,-1] %>%
  pivot_longer(cols = -c(Country,Year), names_to = "Variable", values_to = "Level")


# Convert categorical variables to factors
pooled$Variable <- as.factor(pooled$Variable)
pooled$Question <- as.factor(pooled$Question)
pooled$Response <- as.factor(pooled$Response)

country_level$Variable <- as.factor(country_level$Variable)
country_level$Question <- as.factor(country_level$Question)
country_level$Response <- as.factor(country_level$Response)
country_level$Country <- as.factor(country_level$Country)

ts_data$Country <- as.factor(ts_data$Country)
ts_data$Variable <- as.factor(ts_data$Variable)


# Define UI
ui <- navbarPage("Vaccine Confidence in Europe",
                 
                 # Summary Table Tab
                 tabPanel("Summary of Characteristics",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country", "Select Country:", 
                                          choices = unique(data$Country), 
                                          selected = unique(data$Country), 
                                          multiple = TRUE),
                              
                              selectInput("char_vars", "Select Variables:", 
                                          choices = c(colnames(data)[!colnames(data) %in% c("Country", "Response")], 
                                                      "VaxImpChild", "VaxSaf", "VaxEff", "VaxRel"),  # Add Vax variables
                                          selected = c("Gender", "Age", "Religion", "Education", "VaxImpChild", "VaxSaf", "VaxEff", "VaxRel"), 
                                          multiple = TRUE)
                            ),
                            mainPanel(
                              DTOutput("summaryTable")
                            )
                          )
                 ),
                 
                 # Pooled Model Results Tab
                 tabPanel("Pooled Model Results",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var_select", "Select Variable:", 
                                          choices = levels(pooled$Variable), 
                                          selected = levels(pooled$Variable)[1], 
                                          multiple = TRUE),
                              
                              selectInput("question_select", "Select Question:", 
                                          choices = levels(pooled$Question), 
                                          selected = levels(pooled$Question), 
                                          multiple = TRUE),
                              
                              selectInput("response_select", "Select Response:", 
                                          choices = levels(pooled$Response), 
                                          selected = levels(pooled$Response), 
                                          multiple = TRUE)
                            ),
                            
                            mainPanel(
                              plotlyOutput("plot"),
                              br(),
                              DTOutput("pooledTable")
                            )
                          )
                 ),
                 tabPanel("Country-level Model Results",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country_select", "Select Country:", 
                                          choices = levels(country_level$Country), 
                                          selected = levels(country_level$Country)[1], 
                                          multiple = TRUE),
                              
                              selectInput("var_select2", "Select Variable:", 
                                          choices = levels(country_level$Variable), 
                                          selected = levels(country_level$Variable)[1], 
                                          multiple = TRUE),
              
                              selectInput("question_select2", "Select Question:", 
                                          choices = levels(country_level$Question), 
                                          selected = levels(country_level$Question), 
                                          multiple = TRUE),
                              
                              selectInput("response_select2", "Select Response:", 
                                          choices = levels(country_level$Response), 
                                          selected = levels(country_level$Response), 
                                          multiple = TRUE)
                            ),
                            
                            mainPanel(
                              plotlyOutput("plot2"),
                              br(),
                              DTOutput("countryTable")
                            )
                          )
                 ),
                 tabPanel("Time Series Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country_select2", "Select Country:", 
                                          choices = levels(ts_data$Country), 
                                          selected = levels(ts_data$Country)[1], 
                                          multiple = TRUE),
                              
                              selectInput("var_select3", "Select Variable:", 
                                          choices = levels(ts_data$Variable), 
                                          selected = levels(ts_data$Variable), 
                                          multiple = TRUE)
                            ),
                            
                            mainPanel(
                              plotlyOutput("plot3"),
                              br(),
                              DTOutput("countryTable2")
                            )
                          )
                 )
)

# Define Server
server <- function(input, output) {
  
  output$summaryTable <- renderDT({
    
    # Filter selected country
    summary_data <- data %>%
      filter(Country %in% input$country) %>%
      select(Country, all_of(input$char_vars)) %>%
      pivot_longer(cols = -Country, names_to = "Variable", values_to = "Level") %>%
      group_by(Country, Variable, Level) %>%
      summarise(Count = n(),
                Percentage = round(100 * n() / nrow(data[data$Country %in% input$country,]), 1),
                .groups = "drop")
    
    # Reorder columns for a clean look
    summary_data <- summary_data %>%
      select(Country, Variable, Level, Count, Percentage)
    
    # Render the table using DT
    datatable(summary_data,
              caption = "Country-level Summary of Selected Characteristics",
              filter = "top",
              options = list(pageLength = 10, autoWidth = TRUE),
              escape = FALSE)
  })
  
  # Filtered Data for Plot
  filtered_data <- reactive({
    pooled %>%
      filter(Variable %in% input$var_select,
             Question %in% input$question_select,
             Response %in% input$response_select)
  })
  
  output$plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = OR, y = Variable, colour = Question, 
                                     shape = ifelse((LCI > 1 & UCI > 1) | (LCI < 1 & UCI < 1), 
                                                    "Significant", "Not Significant"))) +
      geom_pointrange(aes(xmin = LCI, xmax = UCI), position = position_dodge(width = 0.5)) +
      facet_wrap(~Response) +
      scale_color_manual(
        name = "Question",
        values = c("importance" = "navy", "safety" = "darkred", "effectiveness" = "orange", "religious_beliefs" = "darkgreen")
      ) + 
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
      theme_minimal() +
      labs(
        x = "Odds Ratio (OR)",
        y = "Variables",
        title = "Pooled Model Results: Responses to Survey Questions on Vaccine Confidence"
      ) + 
      scale_shape_manual(name = "Significance", values = c("Significant" = 17, "Not Significant" = 1)) + 
      theme(legend.position = "right")
    
    ggplotly(p)
  })
  
  output$pooledTable <- renderDT({
    datatable(filtered_data(), 
              caption = "PooledModel Results", 
              filter = list(position = "top", clear = FALSE),
              options = list(pageLength = 10, autoWidth = TRUE),
              escape = FALSE)
  })
  
  filtered_data_2 <- reactive({
    country_level %>%
      filter(Variable %in% input$var_select2,
             Question %in% input$question_select2,
             Response %in% input$response_select2,
             Country %in% input$country_select)
  })
  
  # Render Interactive Plot
  output$plot2 <- renderPlotly({
    p2 <- ggplot(filtered_data_2(), aes(x = OR, y = Variable, colour = Question, 
                                     shape = ifelse((LCI > 1 & UCI > 1) | (LCI < 1 & UCI < 1), 
                                                    "Significant", "Not Significant"))) +
      geom_pointrange(aes(xmin = LCI, xmax = UCI), position = position_dodge(width = 0.5)) +
      facet_wrap(~Response + Country) +
      scale_color_manual(
        name = "Question",
        values = c("importance" = "navy", "safety" = "darkred", "effectiveness" = "orange", "religious_beliefs" = "darkgreen")
      ) + 
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
      theme_minimal() +
      labs(
        x = "Odds Ratio (OR)",
        y = "Variables",
        title = "Responses to Survey Questions on Vaccine Confidence: By Country"
      ) + 
      scale_shape_manual(name = "Significance", values = c("Significant" = 17, "Not Significant" = 1)) + 
      theme(legend.position = "right")
    
    ggplotly(p2)
  })
  
  # Render Interactive Table
  output$countryTable <- renderDT({
    datatable(filtered_data_2(), 
              caption = "Country_level Model Results", 
              filter = list(position = "top", clear = FALSE),
              options = list(pageLength = 10, autoWidth = TRUE),
              escape = FALSE)
  })
  
  filtered_data_3 <- reactive({
    ts_data %>%
      filter(Variable %in% input$var_select3,
             Country %in% input$country_select2)
  })
  output$plot3 <- renderPlotly({
    df <- filtered_data_3()
    print(df)
    
    p3 <- ggplot(df, aes(x = Year, colour = Variable)) +
      facet_wrap(~Country) +
      geom_line(aes(y = Level), size = 1.5) +  # Add line plot
      geom_point(aes(y = Level), size = 3) +   # Add points
      geom_hline(yintercept = 95, linetype = "dashed", color = "black") +  
      annotate("text", x = min(df$Year, na.rm = TRUE), y = 94, label = "HIT",
               hjust = -0.1, vjust = 0.5, color = "black", size = 3) + 
      labs(x = "Year", y = "Percentage (%)") +
      scale_color_manual(name = " ", values = c("MCV1" = "purple", "MCV2" = "pink", 
                                                "VaxImpChild" = "navy", 
                                                "VaxSaf" = "darkred", 
                                                "VaxEff" = "orange", 
                                                "VaxRel" = "darkgreen")) + 
      theme_minimal() + theme(legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(size = 3))) 
    
    ggplotly(p3)
  })
  
  output$countryTable2 <- renderDT({
    datatable(filtered_data_3(), 
              caption = "Country_level Model Results", 
              filter = list(position = "top", clear = FALSE),
              options = list(pageLength = 10, autoWidth = TRUE),
              escape = FALSE)
  })
}

# Run the App
shinyApp(ui = ui, server = server)
