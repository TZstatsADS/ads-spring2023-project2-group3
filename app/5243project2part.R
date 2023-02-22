library(shiny)
library(ggplot2)
library(dplyr)

# Load data
salary_data <- read.csv("Downloads/AnnualSalary.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  # First analysis
  titlePanel("Salary Differences by Agency, Year, and Sex"),
  sidebarLayout(
    sidebarPanel(
      selectInput("agency", "Select an Agency:", choices = unique(salary_data$Agency.Name)),
      selectInput("year", "Select a Year:", choices = unique(salary_data$Fiscal.Year)),
      selectInput("sex", "Select a Sex:", choices = c("male", "female"), selected = "male")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Base Difference", plotOutput("base_diff_plot")),
        tabPanel("Percent Difference", plotOutput("percent_diff_plot"))
      )
    )
  ),
  # Second analysis
  titlePanel("Average Salary Difference by Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput("agency", "Select an Agency:", choices = unique(salary_data$Agency.Name)),
      selectInput("title", "Select a Title Description:", choices = unique(salary_data$Title.Description)),
      radioButtons("metric", "Select a Metric:", choices = c("base_diff", "percent_diff"), selected = "base_diff")
    ),
    mainPanel(
      plotOutput("salary_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  # First analysis
  filtered_data <- reactive({
    salary_data %>%
      filter(Agency.Name == input$agency, Fiscal.Year == input$year)
  })
  
  # Create base difference bar graph
  output$base_diff_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Title.Description, y = base_diff, fill = input$sex)) +
      geom_bar(stat = "identity") +
      labs(title = "Salary Difference by Job Title and Sex",
           x = "Job Title",
           y = "Salary Difference") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_bw()
  })
  
  # Create percent difference bar graph
  output$percent_diff_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Title.Description, y = percent_diff, fill = input$sex)) +
      geom_bar(stat = "identity") +
      labs(title = "Salary Percent Difference by Job Title and Sex",
           x = "Job Title",
           y = "Salary Percent Difference") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_bw()
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
      ylab <- "Average Salary Difference"
      yvar <- "avg_base_diff"
    } else {
      ylab <- "Average Salary Percent Difference"
      yvar <- "avg_percent_diff"
    }
    
    ggplot(data = filtered_data2(), aes(x = Fiscal.Year, y = !!sym(yvar))) +
      geom_line() +
      labs(title = "Average Salary Difference by Year",
           x = "Year",
           y = ylab)
  })
}

# Run the app
shinyApp(ui, server)

