# Uploading the data set. This is a cut down version of the larger dataset 
library(readr)
library(dplyr)
library(lubridate)
salaries <- read_csv("salaries_reduce.csv")


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
