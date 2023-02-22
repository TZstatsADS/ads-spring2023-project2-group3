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
  
df <- read.csv("~/gr5243/NYC_Jobs.csv")
salary_data <-read.csv("~/gr5243/AnnualSalary.csv", stringsAsFactors = FALSE)
salaries <- read_csv("~/gr5243/salaries.csv")

# Payroll analysis


#job posting analysis

df$Posting.Date <- parse_date_time(df$Posting.Date, orders = c('mdy', 'dmy'))

df_agency <- ddply(df, .(df$Agency, df$Posting.Date), nrow)
names(df_agency) <- c("Agency", "Posting.Date", "Freq")

df_posting_type <- ddply(df, .(df$Posting.Type, df$Posting.Date), nrow)
names(df_posting_type) <- c("Posting Type", "Posting.Date", "Freq")

df_career <- ddply(df, .(df$Career.Level, df$Posting.Date), nrow)
names(df_career) <- c("Career Level", "Posting.Date", "Freq")

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
                   h5("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"),
                   h5("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))),
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
        " <p>XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.</p>",
        " <p>XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
shinyApp(ui, server)