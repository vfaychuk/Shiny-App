library(shiny)
library(tidyverse)
library(vroom)
library(plotly)

# Load data ----
phish <- vroom("email_phishing_data.csv")

# Only numeric columns for variable selection
numericVars <- phish %>% select(where(is.numeric)) %>% names()

# Define UI ----
ui <- fluidPage(
  
  # Inputs
  selectInput("labelChoice", 
              "Select Email Type (0 = ham, 1 = phishing)", 
              choices = c(0, 1)),
  
  selectInput("xvar", "Select X Variable", choices = numericVars),
  selectInput("yvar", "Select Y Variable", choices = numericVars),
  
  # Outputs
  tableOutput("tabSummary"),
  tableOutput("tabTopLinks"),
  plotOutput("figScatter", height = "500px", width = "600px"),
  
  h3("Correlation Heatmap"),
  plotlyOutput("figCorr", height = "500px", width = "600px"),
  
)

# Define Server ----
server <- function(input, output) {
  
  # Filter data by label
  filteredData <- reactive({
    phish %>% filter(label == as.numeric(input$labelChoice))
  })
  
  
  # Summary table
  output$tabSummary <- renderTable({
    filteredData() %>%
      summarise(across(
        where(is.numeric),
        \(x) mean(x, na.rm = TRUE)
      )) %>%
      pivot_longer(
        everything(),
        names_to = "Feature",
        values_to = "Mean Value"
      )
  })
  
  # Top link-heavy emails
  output$tabTopLinks <- renderTable({
    filteredData() %>%
      arrange(desc(num_links)) %>%
      select(num_words, num_links, num_spelling_errors, num_urgent_keywords) %>%
      head(10)
  })
  
  # Scatter plot
  output$figScatter <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = .data[[input$xvar]],
                 y = .data[[input$yvar]])) +
      geom_point(alpha = 0.6)
  })
  
  
  # ggplot correlation → converted to plotly
  output$figCorr <- renderPlotly({
    
    df <- filteredData() %>% 
      select(where(is.numeric)) %>% 
      select(where(~ sd(.x, na.rm = TRUE) > 0))  # remove constant columns
    
    corr_mat <- cor(df, use = "complete.obs")
    
    corr_long <- corr_mat %>%
      as.data.frame() %>%
      rownames_to_column("Var1") %>%
      pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")
    
    p <- ggplot(corr_long, aes(Var1, Var2, fill = Correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white",
                           high = "red", midpoint = 0) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
}

# Run App ----
shinyApp(ui = ui, server = server)
