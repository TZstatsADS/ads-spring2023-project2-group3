library(shinydashboard)
library(plotly)
library(lubridate)
library(plyr)
library(dplyr)
library(forcats)
library(shiny)
library(ggplot2)
library(readr)

set.seed(1111)
  
df <- read.csv("NYC_Jobs.csv")
salary_data <-read.csv("AnnualSalary.csv", stringsAsFactors = FALSE)


# Creating the server function with output and input parameters
server <- function(input, output) {
  #JobPosting
  # Creating the Agency Plot
  output$linePlot_agency <- renderPlotly({
    
    df_a <- df_agency[df_agency$Agency == input$agency, ]
    
    fig_a <- plot_ly(df_a, x = ~Posting.Date, y = ~Freq, type = 'scatter', mode = 'lines') %>%
      layout(title = paste(input$agency, 'counts with Posting Date'),
             xaxis = list(title = 'Posting Date'),
             yaxis = list(title = input$agency),
             plot_bgcolor = "#e5ecf6")
    
    fig_a
    
  })
  
  # Creating the Posting Type Plot
  output$linePlot_posting_type <- renderPlotly({
    
    df_p <- df_posting_type[df_posting_type['Posting Type'] == input$posting, ]
    
    fig_p <- plot_ly(df_p, x = ~Posting.Date, y = ~Freq, type = 'scatter', mode = 'lines') %>%
      layout(title = paste(input$posting, 'counts with Posting Date'),
             xaxis = list(title = 'Posting Date'),
             yaxis = list(title = input$posting),
             plot_bgcolor = "#e5ecf6")
    
    fig_p
    
  })
  
  # Creating the Career Level Plot
  output$linePlot_career <- renderPlotly({
    
    df_c <- df_career[df_career['Career Level'] == input$career, ]
    
    fig_c <- plot_ly(df_c, x = ~Posting.Date, y = ~Freq, type = 'scatter', mode = 'lines')%>%
      layout(title = paste(input$career, 'counts with Posting Date'),
             xaxis = list(title = 'Posting Date'),
             yaxis = list(title = input$career),
             plot_bgcolor = "#e5ecf6")
    
    fig_c
  })
  #Gender
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


#run the app
server
