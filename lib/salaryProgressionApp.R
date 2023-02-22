# Uploading the data set. This is a cut down version of the larger dataset 
library(readr)
library(dplyr)
library(lubridate)
salaries <- read_csv("salaries.csv")

#Building the App

library(shiny)
library(ggplot2)
library(shinythemes)
# Define UI
ui <- fluidPage(
  # App title
  titlePanel("Salary Progression by Agency"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("borough", "Select Borough:",
                  choices = sort(unique(salaries$borough))),
      selectInput(inputId = "agency",
                  label = "Choose an agency:",
                  choices = sort(unique(salaries$agency_name))),
      
      # selectInput(inputId = "title",
      #             label = "Choose an title:",
      #             choices = unique(salaries$title_description))
    ),
    mainPanel(
      plotOutput("salary_plot"),
      plotOutput("barPlot"),
      plotOutput("meanplot"),
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "plot"),
      
      
      
      
    )
  )
)

## Warning: The select input "title" contains a large number of options; consider
## using server-side selectize for massively improved performance. See the Details
## section of the ?selectizeInput help topic.
server <- function(input, output) {
  # Create a reactive expression to filter the dataset based on the selected borough
  filtered_data1 <- reactive({
    salaries %>%
      filter(borough == input$borough)
  })
  # Create a reactive expression to calculate the average base salary by year
  salary_by_years_active <- reactive({
    filtered_data1() %>%
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
  
  
  library(forcats)
  # Create reactive data set filtered by title
  filtered_data <- reactive({
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
    p = ggplot(data = filtered_data(), aes(x=input$agency, y=base_salary)) + geom_point(color = 'blue') +
      xlab("Agency Name") +
      ylab("Base Salary") +
      ggtitle(paste0(input$agency, " Salary scater plot")) +
      scale_color_gradient(low = "blue", high = "red")
    plot(p)
    
  })
  
  # Create plot
  output$plot <- renderPlot({
    ggplot(filtered_data(), aes(x = years_active, y = base_salary)) +
      geom_smooth() + 
      geom_point() + 
      xlab("Years Active") +
      ylab("Base Salary") +
      ggtitle(paste0(input$title, " Salary Progression")) +
      scale_color_gradient(low = "blue", high = "red")
  })
  
  
}

#To run the app
shinyApp(ui = ui, server = server)
