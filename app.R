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
##read in data -----------------------------------------------------------------------------------------------------------------------------
#jobs and wages ************
jobs_wages_index <- read_csv("app_data/jobs_wages_index.csv")

jobs_wages_by_age_data <- read_csv("app_data/jobs_wages_by_age_data.csv")

ind_div_list <- c("All industries", "Agriculture, forestry & fishing", "Mining", "Manufacturing", "Electricity, gas,
                  water & waste services", "Construction", "Wholesale trade", "Retail trade", "Accommodation & food services",
                  "Transport, postal & warehousing", "Information media & telecommunications", "Financial & insurance services",
                  "Rental, hiring & real estate services", "Professional, scientific & technical services",
                  "Administrative & support services", "Public administration & safety", "Education & training",
                  "Health care & social assistance", "Arts & recreation services", "Other services")

jobs_index <- jobs_wages_index %>% 
    filter(type == "jobs")

wages_index <- jobs_wages_index %>% 
    filter(type == "wages")

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
        menuItem("Jobs and wages (Vic.)", tabName = "jobs_wages_vic"),
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
                    box(title = 'Jobs and wages (Victoria)', tags$body(HTML("Percentage change from 14 March")), width = 12)
                ),
                fluidRow(plotlyOutput("jobs_wages_line")
                ),
                fluidRow(
                    box(title = 'Source', tags$body(HTML("ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia </br>")), width = 12)
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
    
    # jobs wages plotly change line
    output$jobs_wages_line <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobs_index, x = ~date, y = ~values, name = "jobs", mode = "lines+markers") %>% 
            add_trace(data = wages_index, x = ~date, y = ~values, name = "wages", mode = "lines+markers") %>% 
            layout(xaxis = list(title = 'Date'), yaxis = list(title = "Change % (from 14 March)"))
    })

}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)