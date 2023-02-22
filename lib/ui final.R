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
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"
  
df <- read.csv("NYC_Jobs.csv")
salary_data <-read.csv("AnnualSalary.csv", stringsAsFactors = FALSE)
salaries <- read_csv("salaries.csv")


# shiny app
# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Jobs Over Time", tabName = "dashboard1", icon = icon("dollar-sign")),
    menuItem("Salary", tabName = "dashboard2", icon = icon("dollar-sign")),
    menuItem("Payroll", tabName = "dashboard3", icon = icon("dollar-sign")),
    menuItem("Appendix", tabName = "Appendix", icon = icon("fas fa-asterisk"))
  )
)

# body 
body <- dashboardBody(
  
  # home
  tabItems(
    tabItem(tabName = "Home", fluidPage(
      fluidRow(box(width = 15, title = "Introduction", status = "primary",
                   solidHeader = TRUE, h3("Insights into NYC Jobs"),
                   h4("By Kaitlyn Brown, Wen Chen, Linda Lin, Nixon Mckenzie, Zerui Zhang, Tianyi Zhu"),
                   h5("The purpose of this app is to increase the transparency around NYC government Jobs."),
                   h5("Answers are valuable to people seeking or currently working these jobs, to be able to have more information and pay transparency. Also valuable to these agencies to better understand why their postings may not be attracting applicants or why they may not be retaining their employees."))),
      fluidRow(box(width = 15, title = "Targeted User", status = "primary", solidHeader=TRUE,
                   h5("We believe that the application would be useful for anyone who is interested in Jobs in NYC."))),
      fluidRow(box(width = 15, title = "How to take advanage of the APP", status = "primary",
                   solidHeader = TRUE,
                   h5("How to use the APP"),
                   tags$div(tags$ul(
                     tags$li("The", strong("first"), "tab: Introduction"),
                     tags$li("The", strong("second"), "tab: Job Postings Over Time"),
                     tags$li("The", strong("third"), "tab: Gender Annual Salary Gap"),
                     tags$li("The", strong("fourth"), "tab: Salary Progression"),
                     tags$li("The", strong("fifth"),"tab: Appendix and data sources")
                     
                   ))
      ))
    )),
    
    
    
    
    
    #job posting
    tabItem(tabName = "dashboard1",fluidPage(
      fluidRow(box(title = "Choose An Agency",
                   width = 4,
                   status = "warning", 
                   solidHeader = TRUE, 
                   collapsible = TRUE,
                   column(width = 8,
                          solidHeader = TRUE, status = "warning",
                          selectInput("agency", "Select an Agency",
                                      choices = unique(df$Agency),
                                      selected = unique(df$Agency)[0]
                          )
                   )),
               box(
                 title = "counts of Agency over time",
                 column(offset = 1,
                        width = 10,
                        status = "primary",
                        plotlyOutput("linePlot_agency")),
                 width = 8, solidHeader = TRUE, status = "warning"
               )
      ),
      fluidRow(box(title = "Choose a Posting Type",
                   width = 4,
                   status = "warning", 
                   solidHeader = TRUE, 
                   collapsible = TRUE,
                   column(width = 8,
                          solidHeader = TRUE, status = "warning",
                          selectInput("posting", "Select a Posting Type",
                                      choices = unique(df$Posting.Type),
                                      selected = unique(df$Posting.Type)[0]
                          )
                   )),
               box(
                 title = "counts of Posting Type",
                 column(offset = 1,
                        width = 10,
                        status = "primary",
                        plotlyOutput("linePlot_posting_type")),
                 width = 8, solidHeader = TRUE, status = "warning"
               )
      ),
      fluidRow(box(title = "Choose a Career Level",
                   width = 4,
                   status = "warning", 
                   solidHeader = TRUE, 
                   collapsible = TRUE,
                   column(width = 8,
                          solidHeader = TRUE, status = "warning",
                          selectInput("career", "Select a Career Level",
                                      choices = unique(df$Career.Level),
                                      selected = unique(df$Career.Level)[0]
                          )
                   )),
               box(
                 title = "counts of Career Level",
                 column(offset = 1,
                        width = 10,
                        status = "primary",
                        plotlyOutput("linePlot_career")),
                 width = 8, solidHeader = TRUE, status = "warning"
               )
      )
    )),
    
    
    #salary 
    tabItem(tabName = "dashboard2", fluidPage(
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
    )),
    
    
    #payroll
    tabItem(tabName = "dashboard3", fluidPage(
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
    )),
    
    #appendix 
    tabItem(tabName = "Appendix", fluidPage( 
      HTML(
        "<h2> Data Sources </h2>
                <h4><p><li>NYC Jobs Data: <a href='https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t'>NYC Jobs database</a></li></h4>
                <h4><li>NYC City Wide payroll Data: <a href='https://data.cityofnewyork.us/City-Government/Citywide-Payroll-Data-Fiscal-Year-/k397-673e'>NYC Jobs database</a></li></h4>"
      ),
      
      titlePanel("Disclaimers "),
      
      HTML(
        " <p>It is debateable whether Covid has ended or not, we realize that our data could be somewhat misleading as time goes.</p>",
        " <p>Also, the modern technologies are developing at a fast pace, it is likely that jobs and salaries are changing greatly over the years.
 </p>"),
      
      titlePanel("Acknowledgement  "),
      
      HTML(
        " <p>This application is built using R shiny app.</p>",
        "<p>The following R packages were used:</p>
                <li>Shinydashboard</li>
                <li>Plotly</li>
                <li>Lubridate</li><li>Plyr</li><li>Dplyr</li><li>Forcats</li><li>Shiny</li><li>ggplot2</li>
                <li>readr</li>"
      ),
      
      titlePanel("Contacts"),
      
      HTML(
        " <p>For more information please contact</p>",
        " <p>Brown, Kaitlyn(keb2234@columbia.edu) </p>",
        " <p>Chen, Wen(cw3229@columbia.edu)</p>",
        " <p>Lin, Linda(yl5144@columbia.edu) </p>",
        " <p>Mckenzie, Nixon(nnm2132@columbia.edu)</p>",
        " <p>Zhang, Zerui(zz2999@columbia.edu) </p>",
        " <p>Zhu, Tianyi(tz2538@columbia.edu) </p>")
      
    )
    )
  )
)

# header
header <- dashboardHeader(
  title = strong("Insights into NYC Government Jobs"),
  titleWidth = 400
)

# Creating the dashboard object for UI
ui <- dashboardPage(header, sidebar, body, skin = skin)