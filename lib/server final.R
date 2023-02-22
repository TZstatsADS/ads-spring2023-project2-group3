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
salaries <- read_csv("salaries.csv")


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
  
  #payroll
  filtered_data3 <- reactive({
    salaries %>%
      filter(borough == input$borough)
  })
  # Create a reactive expression to calculate the average base salary by year
  salary_by_years_active <- reactive({
    filtered_data3() %>%
      group_by(years_active) %>%
      dplyr::summarize(avg_salary = mean(base_salary, na.rm = TRUE))%>%
      ungroup()
  })
  
  
  # Create the salary progression plot
  output$salary_plot <- renderPlot({
    ggplot(salary_by_years_active(), aes(x = years_active, y = avg_salary)) +
      geom_point() +
      stat_smooth(method = loess, se = TRUE) +
      labs(x = "Years Active", y = "Average Base Salary") +
      ggtitle(paste0(input$title, " Salary Progression By Borough"))
  })
  
  filtered_data4 <- reactive({
    salaries %>%
      filter(#title_description == input$title,
        agency_name == input$agency) %>%
      arrange(start_date)
  })
  sala_freq = transform(salaries,Agency_Frequency=ave(seq(nrow(salaries)),agency_name,FUN=length))
  sala_top =  group_by(salaries, agency_name) %>% 
    dplyr::summarize(Agency_Frequency = n()) %>% 
    arrange(desc(Agency_Frequency)) %>% 
    top_n(10)
  output$barPlot <- renderPlot({
    
    #top_n(sala_freq, 15, Agency_Frequency)
    ggplot(sala_top, aes(x = agency_name, y = Agency_Frequency,fill = agency_name)) + 
      geom_bar(stat='identity') +
      labs(title = element_text("What is the most popular agency?")) +
      ylab("Frequency of agency")+  geom_col(width = 0.5) +
      coord_flip()
    
  })
  output$meanplot <- renderPlot({
    salaries %>%
      group_by(agency_name) %>%
      dplyr::summarize(mean_salary = mean(base_salary)) %>%arrange(desc(mean_salary))%>%
      top_n(10) %>%
      ggplot(aes(x = factor(agency_name), y = mean_salary, fill = agency_name)) +
      geom_col() +
      labs(title = element_text("Top 10 of mean salary"), x = "Agency name", y = "Mean salary")+
      coord_flip()
  })
  
  output$scatterplot <- renderPlot({
    p = ggplot(data = filtered_data4(), aes(x=input$agency, y=base_salary)) + geom_point(color = 'blue') +
      xlab("Agency Name") +
      ylab("Base Salary") +
      ggtitle(paste0(input$agency, " Salary scater plot")) +
      scale_color_gradient(low = "blue", high = "red")
    plot(p)
    
  })
  
  # Create plot
  output$plot <- renderPlot({
    ggplot(filtered_data4(), aes(x = years_active, y = base_salary)) +
      geom_smooth() + 
      geom_point() + 
      xlab("Years Active") +
      ylab("Base Salary") +
      ggtitle(paste0(input$title, " Salary Progression")) +
      scale_color_gradient(low = "blue", high = "red")
  })
  
}  


#run the app
server
