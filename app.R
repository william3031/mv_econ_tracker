#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(scales)
library(shinydashboard)
library(shinycssloaders)
library(apputils)

#disable scientific notation
options(scipen = 999)

#data
# dates -------------------------------------------------------------------------------------------------------------
abs_latest_week <- "27 June"
abs_publication_date <- "15 July 2020"

##read in data -----------------------------------------------------------------------------------------------------------------------------
#jobs and wages ************
jobs_wages_index <- read_csv("app_data/jobs_wages_index.csv")

jobs_wages_by_age_data <- read_csv("app_data/jobs_wages_by_age_data.csv")  %>% 
    mutate(age_group = factor(age_group, levels = c("Under 20", "20-29", "30-39", "40-49",
                                                    "50-59", "60-69", "70 and over", "All ages")))

jobs_index <- jobs_wages_index %>% 
    filter(type == "Jobs")

wages_index <- jobs_wages_index %>% 
    filter(type == "Wages")

# jobs and wages by age and gender graph
jobs_wages_list <- c("Jobs", "Wages")

jobs_wages_by_age_data_males <- jobs_wages_by_age_data %>% 
    filter(sex == "Males") 

jobs_wages_by_age_data_females <- jobs_wages_by_age_data %>% 
    filter(sex == "Females") 

#the app ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# dashboard input
header <- dashboardHeader(
    title = "MV Economic Tracker",
    titleWidth = 300,
    tags$li(a(img(src = 'mvcc_logo.jpg',
                  height = "45px"),
              style = "padding-top:2px; padding-bottom:2px;"),
            class = "dropdown")
)

# sidebar tabs
sidebar <- dashboardSidebar(
    width = 250,
    sidebarMenu(
        menuItem("Home", tabName = "home"),
        menuItem("Jobs and wages (Vic.)",
                 menuSubItem("Change for all industries", tabName = "jobs_wages_vic"),
                 menuSubItem("Change by age and gender", tabName = "jobs_wages_age")
        ),
        menuItem("Jobkeeper",
                 menuSubItem("Jobkeeper map", tabName = "jobkeeper_map"),
                 menuSubItem("Jobkeeper table", tabName = "jobkeeper_table")
        ),
        menuItem("Jobseeker and Youth Allowance",
                 menuSubItem("Jobseeker map", tabName = "jobseeker_map"),
                 menuSubItem("Jobseeker table", tabName = "jobseeker_table")
        ),
        menuItem("Unemployment rate",
                 menuSubItem("Unemployment rate map", tabName = "unemp_map"),
                 menuSubItem("Unemployment rate table", tabName = "unemp_table")
        ),
        menuItem("Notes", tabName = "notes")
    )
)

# the body
body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "home",
                fluidRow(
                    box(title = 'MV Economic tracker', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "jobs_wages_vic",
                fluidRow(
                    box(title = 'Jobs and wages (Victoria)', tags$body(HTML(glue("Percentage change from 14 March to {abs_latest_week}"))), width = 12)
                ),
                fluidRow(plotlyOutput("jobs_wages_line")
                ),
                fluidRow(
                    box(title = 'Source', tags$body(HTML(glue("ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia </br>",
                                                         "Last updated {abs_publication_date}"))), width = 12)
                ),
        ),
        tabItem(tabName = "jobs_wages_age",
                fluidRow(
                    box(title = 'Jobs and wages (Victoria)',
                        tags$body(HTML(glue("Percentage change from 14 March to {abs_latest_week} by age and gender for all industries"))), width = 12)
                ),
                fluidRow(
                    box(selectInput(inputId = "jobs_wages_input",
                                    label = "Select the data type",
                                    choices = jobs_wages_list),
                    ),
                ),
                fluidRow(plotlyOutput("jobs_wages_age_bar")
                ),
                fluidRow(
                    box(title = 'Source', tags$body(HTML(glue("ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia </br>",
                                                              "Last updated {abs_publication_date}"))), width = 12)
                ),
        ),
        tabItem(tabName = "jobkeeper_map",
                fluidRow(
                    box(title = 'Jobkeeper data (postcodes)', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "jobkeeper_table",
                fluidRow(
                    box(title = 'Jobkeeper table', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "jobseeker_map",
                fluidRow(
                    box(title = 'Jobseeker data (SA2)', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "jobseeker_table",
                fluidRow(
                    box(title = 'Jobseeker table', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "unemp_map",
                fluidRow(
                    box(title = 'Labour force and unemployment data (SA2)', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "unemp_table",
                fluidRow(
                    box(title = 'Labour force and unemployment table', tags$body(HTML("text")), width = 12)
                )
        ),
        tabItem(tabName = "notes",
                fluidRow(
                    box(title = 'Notes', tags$body(HTML("text")), width = 12)
                )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    # tables ###############################################
    jobs_wages_by_age_data_females_filtered <- reactive({
        jobs_wages_by_age_data_females %>% 
            filter(type == input$jobs_wages_input)
    })
    
    jobs_wages_by_age_data_males_filtered <- reactive({
        jobs_wages_by_age_data_males %>% 
            filter(type == input$jobs_wages_input)
    })
    
    # graphs ###############################################
    # jobs wages plotly change line
    output$jobs_wages_line <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobs_index, x = ~date, y = ~values, name = "Jobs", mode = "lines+markers") %>% 
            add_trace(data = wages_index, x = ~date, y = ~values, name = "Wages", mode = "lines+markers") %>% 
            layout(xaxis = list(title = 'Date'), yaxis = list(title = "Change % (from 14 March)"))
    })
    
    # jobs wages plotly change line
    output$jobs_wages_age_bar <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobs_wages_by_age_data_males_filtered(), x = ~age_group, y = ~latest_week, type = 'bar', name = 'Males') %>% 
            add_trace(data = jobs_wages_by_age_data_females_filtered(), x = ~age_group, y = ~latest_week, type = 'bar', name = 'Females') %>% 
            layout(xaxis = list(title = 'Age'), yaxis = list(title = "Change % (from 14 March)"))
    })

}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)