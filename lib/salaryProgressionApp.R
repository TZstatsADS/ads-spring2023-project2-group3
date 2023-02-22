
# Uploading the data set and getting an idea of how much Agencies there are 
library(readr)
full_payroll_data <- read_csv("Citywide_Payroll_Data__Fiscal_Year_.csv")
## Rows: 5109775 Columns: 17
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (9): Agency Name, Last Name, First Name, Mid Init, Agency Start Date, Wo...
## dbl (8): Fiscal Year, Payroll Number, Base Salary, Regular Hours, Regular Gr...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

#Here's a function that takes in an agencies name as the input and outputs 
#a dataframe with only that agency, the base salary, title_description, and the years worked at company 

library(dplyr)
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(lubridate)
## Loading required package: timechange
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
# lets cut down the full dataset 
salaries <- full_payroll_data %>%
  subset(select = c('Agency Name','Base Salary','Title Description','Agency Start Date', 'Work Location Borough'))
colnames(salaries) <- c("agency_name", "base_salary",'title_description', "start_date", 'borough')
salaries$start_date <- mdy(salaries$start_date)
salaries <- salaries %>%
  mutate(years_active = as.numeric(difftime(Sys.Date(), start_date, units = "weeks"))/52)
salaries$years_active <- round(salaries$years_active)
salaries$borough[salaries$borough == "Bronx"] <- "BRONX"
salaries$borough[salaries$borough == "Queens"] <- "QUEENS"
salaries$borough[salaries$borough == "Manhattan"] <- "MANHATTAN"
salaries$borough[salaries$borough == "Richmond"] <- "RICHMOND"
salaries <- salaries %>%
  subset(years_active > -1) %>%
  subset(years_active < 70) %>%
  subset(base_salary > 10000 )

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
    salaries%>%
      filter(borough == input$borough)
  })
  # Create a reactive expression to calculate the average base salary by year
  salary_by_years_active <- reactive({
    filtered_data1() %>%
      group_by(years_active) %>%
      summarize(avg_salary = mean(base_salary, na.rm = TRUE))
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
      summarise(mean_salary = mean(base_salary)) %>%arrange(desc(mean_salary))%>%
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

