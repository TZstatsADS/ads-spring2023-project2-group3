library(shiny)
library(ggplot2)
library(dplyr)

# Load data
salary_data <- read.csv("AnnualSalary.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  # First analysis
  titlePanel("Male-Female Base Salary Difference by Agency and Title"),
  sidebarLayout(
    sidebarPanel(
      selectInput("agency", "Select an Agency:", choices = unique(salary_data$Agency.Name)),
      selectInput("year", "Select a Year:", choices = unique(salary_data$Fiscal.Year))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Difference", plotOutput("base_diff_plot")),
        tabPanel("Percent Difference", plotOutput("percent_diff_plot"))
      )
    )
  ),
  # Second analysis
  titlePanel("Male-Female Base Salary Difference over Years"),
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

