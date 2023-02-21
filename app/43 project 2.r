library(shinydashboard)
library(plotly)
library(lubridate)
library(dplyr)
library(plyr)

set.seed(1111)
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"

df <- read.csv("~/gr5243/NYC_Jobs.csv")

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
    menuItem("Appendix", tabName = "Appendix1", icon = icon("fas fa-asterisk"))
  )
)

# body 
body <- dashboardBody(
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
                     tags$li("The", strong("second"), "tab: XXXXXXXXXXXXXXXXXXXXXXX"),
                     tags$li("The", strong("third"), "tab: XXXXXXXXXXXXXXXXXXXXXX"),
                     tags$li("The", strong("fourth"), "tab: XXXXXXXXXXXXXXXXXXXXXX"),
                     tags$li("The", strong("fifth"),"tab: Appendix and data sources")
                     
                   ))
      ))
    )),
    
    tabItem("dashboard1",
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
            
    )
  ),
  
    tabItem(tabName = "Appendix1", fluidPage( 
      HTML(
      "<h2> Data Sources </h2>
                <h4> <p><li>NYC Jobs Data: <a href='https://data.cityofnewyork.us/City-Government/NYC-Jobs/kpav-sd4t'>NYC Jobs database</a></li></h4>"
    ),
    
    titlePanel("Disclaimers "),
    
    HTML(
      " <p>XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.</p>",
      " <p>XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 </p>"),
    
    titlePanel("Acknowledgement  "),
    
    HTML(
      " <p>This application is built using R shiny app.</p>",
      "<p>The following R packages were used in to build this RShiny application:</p>
                <li>Shinytheme</li>
                <li>Tidyverse</li>
                <li>Dyplr</li><li>Plyr</li><li>lubridate</li><li>Plotly</li>
                <li>ggplot2</li>"
    ),
    
    titlePanel("Contacts"),
    
    HTML(
      " <p>For more information please feel free to contact</p>",
      " <p>Brown, Kaitlyn(keb2234@columbia.edu) </p>",
      " <p>Chen, Wen(cw3229@columbia.edu)</p>",
      " <p>Lin, Linda(yl5144@columbia.edu) </p>",
      " <p>Mckenzie, Nixon(nnm2132@columbia.edu)</p>",
      " <p>Zhang, Zerui(zz2999@columbia.edu) </p>",
      " <p>Zhu, Tianyi(tz2538@columbia.edu) </p>")
   
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
}

shinyApp(ui, server)