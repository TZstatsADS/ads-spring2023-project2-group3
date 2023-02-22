library(shiny)
library(ggplot2)
library(dplyr)

salary_data <- read.csv("AnnualSalary.csv", stringsAsFactors = FALSE)

# Define server
server <- function(input, output) {
  # First analysis
  filtered_data <- reactive({
    salary_data %>%
      filter(Agency.Name == input$agency, Fiscal.Year == input$year)
  })
  
  # Create base difference bar graph
  output$base_diff_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Title.Description, y = base_diff)) +
      geom_bar(stat = "identity") +
      labs(title = "Salary Difference by Job Title and Sex",
           x = "Job Title",
           y = "Male-Female Salary Difference") +
      scale_fill_manual(values = c("blue", "red")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  })
  
  # Create percent difference bar graph
  output$percent_diff_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Title.Description, y = percent_diff)) +
      geom_bar(stat = "identity") +
      labs(title = "Salary Percent Difference by Job Title and Sex",
           x = "Job Title",
           y = "Male-Female Salary Percent Difference") +
      scale_fill_manual(values = c("blue", "red")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  })
  
  #second analysis
  filtered_data2 <- reactive({
    salary_data %>%
      filter(Agency.Name == input$agency, Title.Description == input$title) %>%
      select(Fiscal.Year, base_diff, percent_diff) %>%
      group_by(Fiscal.Year) %>%
      summarise(avg_base_diff = mean(base_diff), avg_percent_diff = mean(percent_diff))
  })
  
  # Create line plot
  output$salary_plot <- renderPlot({
    if (input$metric == "base_diff") {
      ylab <- "Male-Female Salary Difference"
      yvar <- "avg_base_diff"
    } else {
      ylab <- "Male-Female Salary Percent Difference"
      yvar <- "avg_percent_diff"
    }
    
    ggplot(data = filtered_data2(), aes(x = Fiscal.Year, y = !!sym(yvar))) +
      geom_line() +
      labs(title = "Average Salary Difference over Years",
           x = "Year",
           y = ylab)
  })
}

# Run the app
server

