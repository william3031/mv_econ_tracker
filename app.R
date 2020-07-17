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
library(tmap)
library(leaflet)
library(tmaptools)
library(DT)

#disable scientific notation
options(scipen = 999)

# data #########
## dates -------------------------------------------------------------------------------------------------------------
abs_latest_week <- "27 June"
abs_publication_date <- "15 July 2020"
jobkeeper_date <- "24 June 2020"
jobkeeper_text <- "Numbers are based on the total number of <b>processed applications for organisations</b> for the April fortnights – 30 March 2020 to 26 April 2020 (as at midnight 3 June 2020)"
jobseeker_month <- "Jun 2020" # in this format mmm YYYY
jobseeker_update <- "17 July 2020"


## jobs and wages ####
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

## jobkeeper #### 
# mv shapefile
mv_shp <- st_read("app_data/shp/mvcc_boundary.shp")

# jobkeeper data
postcodes_jk <- st_read("app_data/shp/postcodes_simplified.shp")

# raw data
jk_raw <- read_csv("app_data/jk_raw.csv")

# join to shp
jk_join <- left_join(postcodes_jk, jk_raw) %>%  
    filter(!is.na(count)) 

# mv postcodes
jk_mv_postcodes <- jk_raw %>% 
    mutate(postcode = str_trim(postcode)) %>%
    filter(postcode != "TOTAL") %>% 
    filter(postcode %in% c("3031", "3032", "3033", "3034", "3039", "3040", "3041", "3042")) %>% 
    adorn_totals() %>% 
    mutate(count = comma(count)) %>% 
    rename(Postcode = postcode, Count = count)

## jobseeker #################################
jobseeker_table_long <- read_csv("app_data/jobseeker_table_long.csv")

sa2_greater <- st_read("app_data/shp/sa2_2016_gmel.shp") %>% 
    select(-sa2_code)

js_data_list <- c("Total JobSeeker and Youth allowance recipients", "Percentage aged 15-64 y.o. on either JobSeeker or Youth Allowance", 
                  "JobSeeker payment recipients", "Youth Allowance recipients")

js_month_list <- jobseeker_table_long %>% 
    distinct(month) %>% 
    arrange(desc(month)) %>% 
    pull()

jobseeker_table_filtered <- jobseeker_table_long %>% 
    filter(month == jobseeker_month) %>% 
    filter(data_type == "Percentage aged 15-64 y.o. on either JobSeeker or Youth Allowance")

js_map_join <- left_join(sa2_greater, jobseeker_table_filtered)

# the app ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
                    box(title = 'MV Economic tracker', tags$body(HTML("Click the tabs on the left</br>",
                                                                      "</br> More to be added </br>",
                                                                      "</br> Last updated: 17 July 2020")), width = 12)
                )
        ),
        ## abs data ####
        tabItem(tabName = "jobs_wages_vic",
                fluidRow(
                    box(title = 'Jobs and wages (Victoria)', tags$body(HTML(glue("Percentage change from 14 March to {abs_latest_week}."))), width = 12)
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
                        tags$body(HTML(glue("Percentage change from 14 March to {abs_latest_week} by age and gender for all industries."))), width = 12)
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
                                                              "Last updated {abs_publication_date}"))), width = 12),
                ),
        ),
        ## jobkeeper data ####
        tabItem(tabName = "jobkeeper_map",
                fluidRow(
                    box(title = 'Jobkeeper data (all postcodes)', tags$body(HTML(glue("{jobkeeper_text}</br>",
                                                                                      "</br>Click on the map to see the counts."))), width = 12)
                ),
                fluidRow(tmapOutput("jobkeeper_pc_map")
                ),
                box(title = 'Source', tags$body(HTML(glue("Treasury JobKeeper postcode data </br>",
                                                            "Last updated {jobkeeper_date}</br>",
                                                            "Note: Postcode polygons have been simplified."))), width = 12)
        ),
        tabItem(tabName = "jobkeeper_table",
                fluidRow(
                    box(title = 'Jobkeeper data (MV postcodes)', tags$body(HTML(glue("{jobkeeper_text}</br>",
                                                                             "</br> Noting that:</br>",
                                                                             "* 3031 includes Kensington and the parts of Flemington within the City of Melbourne</br>",
                                                                             "* 3032 includes Maribyrnong (City of Maribyrnong) </br>",
                                                                             "* 3042 includes Keilor Park (City of Brimbank)"))), width = 12)
                ),
                fluidRow(DTOutput("jobkeeper_mv_table")
                ),
                box(title = 'Source', tags$body(HTML(glue("Treasury JobKeeper postcode data </br>",
                                                          "Last updated {jobkeeper_date}"))), width = 12)
        ),
        ## jobseeker data ####
        tabItem(tabName = "jobseeker_map",
                fluidRow(
                    box(title = 'Jobseeker data (SA2)', tags$body(HTML(glue("Percentage aged 15-64 y.o. on either JobSeeker or Youth Allowance for {jobseeker_month}.</br>",
                                                                            "</br>Click on the map to see the percentages."))), width = 12)
                ),
                fluidRow(tmapOutput("jobseeker_map")
                ),
                box(title = 'Source', tags$body(HTML(glue("Department of Social Services, JobSeeker Payment and Youth Allowance recipients – monthly profile </br>",
                                                          "Last updated {jobseeker_update}"))), width = 12)
        ),
        tabItem(tabName = "jobseeker_table",
                fluidRow(
                    box(title = 'Jobseeker table', tags$body(HTML("text")), width = 12)
                )
        ),
        ## salm data ####
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
    # abs data
    jobs_wages_by_age_data_females_filtered <- reactive({
        jobs_wages_by_age_data_females %>% 
            filter(type == input$jobs_wages_input)
    })
    
    jobs_wages_by_age_data_males_filtered <- reactive({
        jobs_wages_by_age_data_males %>% 
            filter(type == input$jobs_wages_input)
    })
    
    # jobkeeper data
    output$jobkeeper_mv_table <- renderDT(jk_mv_postcodes,options = list(dom = 't'))
    
    # jobseeker data    
    
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
    
    # maps #####################################################
    output$jobkeeper_pc_map <- renderTmap({
        tmap_mode("view")
        tm_shape(jk_join, bbox = tmaptools::bb(mv_shp)) +
            tm_fill("count") +
            tm_borders(alpha = 0.5, col = "grey") +
            tm_shape(mv_shp) +
            tm_borders(alpha = 0.5, col = "purple", lwd = 2)
    })
    
    output$jobseeker_map <- renderTmap({
        tmap_mode("view")
        tm_shape(js_map_join, bbox = tmaptools::bb(mv_shp)) +
            tm_fill("values") +
            tm_borders(alpha = 0.5, col = "grey") +
            tm_shape(mv_shp) +
            tm_borders(alpha = 0.5, col = "purple", lwd = 2)
    })

}



# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)