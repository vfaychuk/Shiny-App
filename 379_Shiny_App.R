library(shiny)
library(tidyverse)
library(vroom)
library(plotly)

### Load data
# The label column tell you that 0 is a regular email "Ham" 
# and 1 tells you that it a scam email "phishing"

# As a heads up this Data does have half a million obs so it does take a second 
# for all the information to load but when it does it works just fine after
phish <- vroom("email_phishing_data.csv")
# Transform into numeric
numericVars <- phish %>% select(where(is.numeric)) %>% names()
# create UI
ui <- fluidPage(
  titlePanel("Phishing Email Analysis Dashboard"),
  
  sidebarLayout( sidebarPanel( selectInput("labelChoice", 
                "Select Email Type (0 = ham, 1 = phishing)", 
                # I kept getting erros when trying to rename the choises so 
                # this was my way to work arround it
                  choices = c(0, 1)),
                               
                selectInput("xvar", "Select X Variable", choices = numericVars),
                selectInput("yvar", "Select Y Variable", choices = numericVars),
                               
                br(),
                p("Use the tabs on the right to explore the data.")
  ),
  # Separated the Visuals into tabs so its more interactive and organized for 
    # the user
  mainPanel(
    tabsetPanel(
      
      #Summary Tab
      tabPanel("Summary Stats",
               h3("Mean Summary of Numeric Features"),
               tableOutput("tabSummary"),
               
               h3("Top Emails by Link Count"),
               tableOutput("tabTopLinks")
      ),
      
      #Scatter Plot
      tabPanel("Scatterplot",
               h3("Scatter Plot"),
               plotOutput("figScatter", height = "450px")
      ),
      
      #Correlation Heatmap
      tabPanel("Correlation Heatmap",
               
               h3("Correlation Heatmap"),
               div(style="max-width: 850px; margin:auto;",
                   plotlyOutput("figCorr", height = "550px")
               )
      )
    )
  )
  )
)

# Create the Server
server <- function(input, output) {
  
  filteredData <- reactive({
    phish %>% filter(label == as.numeric(input$labelChoice))
  })
  # Summary
  output$tabSummary <- renderTable({
    filteredData() %>%
      summarise(across(
        where(is.numeric),
        \(x) mean(x, na.rm = TRUE)
      )) %>%
      pivot_longer(everything(), names_to = "Feature", values_to = "Mean Value")
  })
  # Top Links across both Ham and Phishing Emails
  output$tabTopLinks <- renderTable({
    filteredData() %>%
      arrange(desc(num_links)) %>%
      select(num_words, num_links, num_spelling_errors, num_urgent_keywords) %>%
      head(10)
  })
  # Scatter Plot
  
  # I wanted to do a ggplotly like i did for the scatter plot but i count not 
  # since the data is so long so i was forced to go with a regular plot
  output$figScatter <- renderPlot({
    filteredData() %>%
      ggplot(aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(alpha = 0.6)
  })
  # Correlation Heat Map
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
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
