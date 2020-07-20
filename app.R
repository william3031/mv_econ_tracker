#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(shinydashboard)
library(tmap)
library(leaflet)
library(tmaptools)
library(DT)
library(RColorBrewer)
library(sf)
library(lubridate)
library(shinycssloaders)

#disable scientific notation
options(scipen = 999)

# data #########
## dates -------------------------------------------------------------------------------------------------------------
# date updated
date_updated <- format(Sys.time(), '%d %B %Y')
#abs
abs_publication_date <- "15 July 2020"
# jobkeeper
jobkeeper_publication_date <- "24 June 2020"
jobkeeper_data_date <- "April 2020"
jobkeeper_text <- "Numbers are based on the total number of <b>processed applications for organisations</b> for the April fortnights – 30 March 2020 to 26 April 2020 (as at midnight 3 June 2020)"
#jobseeker
jobseeker_publication_date <- "17 July 2020"
# salm
salm_publication_date <- "27 March 2020"


## jobs and wages data ####
jobs_wages_index <- read_csv("app_data/jobs_wages_index.csv")

jobs_wages_by_age_data <- read_csv("app_data/jobs_wages_by_age_data.csv")  %>% 
    mutate(age_group = factor(age_group, levels = c("Under 20", "20-29", "30-39", "40-49",
                                                    "50-59", "60-69", "70 and over", "All ages")))

jobs_index <- jobs_wages_index %>% 
    filter(type == "Jobs")

wages_index <- jobs_wages_index %>% 
    filter(type == "Wages")

abs_latest_week <- jobs_wages_index %>% 
    tail(1) %>% 
    select(date) %>% 
    pull() %>% 
    format(., format = "%d %B") 

# jobs and wages by age and gender graph
jobs_wages_list <- c("Jobs", "Wages")

jobs_wages_by_age_data_males <- jobs_wages_by_age_data %>% 
    filter(sex == "Males") 

jobs_wages_by_age_data_females <- jobs_wages_by_age_data %>% 
    filter(sex == "Females") 

## jobkeeper data #### 
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
    mutate(count = format(count, big.mark = ",")) %>% 
    rename(Postcode = postcode, Count = count)

jk_mv_num <- jk_mv_postcodes %>% 
    filter(Postcode == "Total") %>% 
    select(Count) %>% 
    pull()

## jobseeker data #################################
jobseeker_table_long <- read_csv("app_data/jobseeker_table_long.csv")

sa2_greater <- st_read("app_data/shp/sa2_2016_gmel.shp") %>% 
    select(-sa2_code)

js_data_list <- c("Percentage aged 15-64 on either JobSeeker or Youth Allowance",
                  "Total JobSeeker and Youth allowance recipients")

js_month_list <- jobseeker_table_long %>% 
    distinct(month) %>% 
    arrange(desc(month)) %>% 
    pull()

jobseeker_month <- js_month_list[1]
jobseeker_month_formatted <- format(ymd(jobseeker_month), "%b %Y")
jobseeker_month_long <- format(ymd(jobseeker_month), "%B %Y")
jobseeker_first <- js_month_list[length(js_month_list)]
jobseeker_first_month_formatted <- format(ymd(jobseeker_first), "%b %Y")

jobseeker_table_filtered <- jobseeker_table_long %>% 
    filter(month == jobseeker_month) %>% 
    filter(data_type == "Percentage aged 15-64 on either JobSeeker or Youth Allowance") %>% 
    rename(percentage = values)

js_map_join <- left_join(sa2_greater, jobseeker_table_filtered, by = "sa2_name")

jobseeker_joined <- read_csv("app_data/jobseeker_joined.csv")

region_list <- c("Ascot Vale", "Essendon - Aberfeldie", "Flemington", "Moonee Ponds",
                 "Airport West", "Keilor East", "Niddrie - Essendon West", "Strathmore",
                 "City of Moonee Valley", "Greater Melbourne")

selected_regions <- c("City of Moonee Valley", "Greater Melbourne")

jobseeker_large_first <- jobseeker_joined %>% 
    filter(month  == jobseeker_first) %>% 
    mutate(month = format(month, "%b %Y")) %>% 
    mutate(data_type = if_else(data_type == "Total JobSeeker and Youth allowance recipients",
                               "recipients_first", "of_pop_first")) %>% 
    pivot_wider(names_from = data_type, values_from = values) %>% 
    select(-month)

jobseeker_large_current <- jobseeker_joined %>% 
    filter(month == jobseeker_month) %>% 
    mutate(month = format(month, "%b %Y")) %>% 
    mutate(data_type = if_else(data_type == "Total JobSeeker and Youth allowance recipients",
                               "recipients_last", "of_pop_last")) %>%  
    pivot_wider(names_from = data_type, values_from = values) %>% 
    select(-region, -month) 

jobseeker_large <- bind_cols(jobseeker_large_first, jobseeker_large_current) %>% 
    mutate(recipients_change = recipients_last - recipients_first) %>% 
    mutate(recipients_first = format(recipients_first, big.mark = ",")) %>% 
    mutate(recipients_last = format(recipients_last, big.mark = ",")) %>% 
    mutate(recipients_change = format(recipients_change, big.mark = ","))
# rename the columns - do it this way!!!
colnames(jobseeker_large) <- c("Region",
                               paste0("Recipients ", jobseeker_first_month_formatted),
                               paste0("As % of 15-64 pop. ", jobseeker_first_month_formatted),
                               paste0("Recipients ", jobseeker_month_formatted),
                               paste0("As % of 15-64 pop. ", jobseeker_month_formatted),
                               "Change")

js_mv_num <- jobseeker_joined %>% 
    filter(month == jobseeker_month) %>% 
    filter(region == "City of Moonee Valley") %>% 
    filter(data_type == "Total JobSeeker and Youth allowance recipients") %>% 
    select(values) %>% 
    mutate(values = format(values, big.mark = ",")) %>% 
    pull()

# salm data ##########################
sa2_vic_current_unemp_rate <- read_csv("app_data/salm_unemp_rate_current_sa2.csv")

unemp_rate_map_join <- left_join(sa2_greater, sa2_vic_current_unemp_rate, by = "sa2_name") 

salm_current_month <- sa2_vic_current_unemp_rate %>% 
    tail(1) %>% 
    select(date) %>% 
    pull() %>% 
    format(., format = "%B %Y")

salm_data_list <- c("Unemployment rate %", "No. of unemployed", "Labour force")

salm_chart_data <- read_csv("app_data/salm_chart_data.csv")

salm_table_data <- read_csv("app_data/salm_table_data.csv") %>% 
    mutate(`No. of unemployed` = format(`No. of unemployed`, big.mark = ","),
           `Labour force` = format(`Labour force`, big.mark = ","))

unemp_rate_mv_num <- salm_table_data %>% 
    filter(Region == "City of Moonee Valley") %>% 
    select(`Unemployment rate %`) %>% 
    pull()
    
# the app ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# dashboard input
header <- dashboardHeader(
    title = "MV Economic Tracker",
    titleWidth = 250,
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
                 menuSubItem("Jobseeker graph", tabName = "jobseeker_graph"),
                 menuSubItem("Jobseeker table", tabName = "jobseeker_table")
        ),
        menuItem("Unemployment and labour force",
                 menuSubItem("Unemployment rate map", tabName = "unemp_map"),
                 menuSubItem("Unemployment graph", tabName = "unemp_graph"),
                 menuSubItem("Unemployment table", tabName = "unemp_table")
        ),
        menuItem("Notes", tabName = "notes")
    )
)

# the body
body <- dashboardBody(
    
    tabItems(
        ## home ####
        tabItem(tabName = "home",
                fluidRow(
                    box(title = 'Economic data tracker',
                        tags$body(HTML(glue("Click the tabs on the left for detailed information.</br>",
                                            "</br>Last updated: {date_updated}"))), width = 12)
                ),
                fluidRow(
                    valueBoxOutput("vbox_jobkeep")  %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                    valueBoxOutput("vbox_jobseek")  %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                    valueBoxOutput("vbox_unemp_rate")  %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                fluidRow(
                    helpText("*Some postcodes are shared with areas outside of the municipality.")
                ),
        ),
        ## abs data ####
        tabItem(tabName = "jobs_wages_vic",
                fluidRow(
                    box(title = 'Jobs and wages (Victoria)',
                        tags$body(HTML(glue("Weekly payroll jobs and wages data for Victoria. Percentage change from 14 March to {abs_latest_week}."))), width = 12)
                ),
                fluidRow(plotlyOutput("jobs_wages_line") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                fluidRow(
                    box(title = 'Source:',
                        tags$a(href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/6160.0.55.001", target="_blank",
                               "ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia"),
                        tags$body(HTML(glue("</br>Last updated {abs_publication_date}"))), width = 12)
                ),
        ),
        tabItem(tabName = "jobs_wages_age",
                fluidRow(
                    box(title = 'Jobs and wages (Victoria)',
                        tags$body(HTML(glue("Weekly payroll jobs and wages data for Victoria. Percentage change from 14 March to {abs_latest_week} by age and gender for all industries."))), width = 12)
                ),
                fluidRow(
                    box(selectInput(inputId = "jobs_wages_input",
                                    label = "Select the data type",
                                    choices = jobs_wages_list),
                    ),
                ),
                fluidRow(plotlyOutput("jobs_wages_age_bar") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                fluidRow(
                    box(title = 'Source:',
                        tags$a(href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/6160.0.55.001", target="_blank",
                               "ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia"),
                        tags$body(HTML(glue("</br>Last updated {abs_publication_date}"))), width = 12)
                ),
        ),
        ## jobkeeper ####
        tabItem(tabName = "jobkeeper_map",
                fluidRow(
                    box(title = 'Jobkeeper data (all postcodes)',
                        tags$body(HTML(glue("{jobkeeper_text}</br>",
                                            "</br>Click on the map to see the counts."))), width = 12)
                ),
                fluidRow(tmapOutput("jobkeeper_pc_map") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Source:',
                    tags$a(href="https://treasury.gov.au/coronavirus/jobkeeper/data", target="_blank",
                           "Treasury JobKeeper postcode data"),
                    tags$body(HTML(glue("</br>Last updated {jobkeeper_publication_date}</br>",
                                                            "Note: Postcode polygons have been simplified."))), width = 12)
        ),
        tabItem(tabName = "jobkeeper_table",
                fluidRow(
                    box(title = 'Jobkeeper data (MV postcodes)',
                        tags$body(HTML(glue("{jobkeeper_text}</br>",
                                                                             "</br> Noting that:</br>",
                                                                             "* 3031 Flemington (partly in the City of Melbourne) and Kensington (mostly in the City of Melbourne)</br>",
                                                                             "* 3032 Ascot Vale and Travancore, as well as Maribyrnong (City of Maribyrnong) </br>",
                                                                             "* 3033 Keilor East </br>",
                                                                             "* 3034 Avondale Heights </br>",
                                                                             "* 3039 Moonee Ponds </br>",
                                                                             "* 3040 Aberfeldie, Essendon and Essendon West </br>",
                                                                             "* 3041 Essendon Fields, Essendon North, Strathmore and Strathmore Heights </br>",
                                                                             "* 3042 Airport West and Niddrie, as well as Keilor Park (City of Brimbank)"))), width = 12)
                ),
                fluidRow(DTOutput("jobkeeper_mv_table") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Source:',
                    tags$a(href="https://treasury.gov.au/coronavirus/jobkeeper/data", target="_blank",
                           "Treasury JobKeeper postcode data"),
                    tags$body(HTML(glue("</br>Last updated {jobkeeper_publication_date}</br>",
                                        "Note: Postcode polygons have been simplified."))), width = 12)
        ),
        ## jobseeker####
        tabItem(tabName = "jobseeker_map",
                fluidRow(
                    box(title = 'Jobseeker data (SA2)',
                        tags$body(HTML(glue("Percentage of the population aged 15-64 on either JobSeeker or Youth Allowance (excluding students and apprentices) for {jobseeker_month_formatted}.</br>",
                                                                            "</br>Click on the map to see the percentages."))), width = 12)
                ),
                fluidRow(tmapOutput("jobseeker_map") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:',
                    tags$a(href="https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile", target="_blank",
                           "Department of Social Services, JobSeeker Payment and Youth Allowance recipients – monthly profile; "),
                    tags$a(href="https://www.abs.gov.au/AUSSTATS/abs@.nsf/mf/3235.0", target="_blank",
                           "ABS, 3235.0 - Regional Population by Age and Sex (2018)"),
                    tags$body(HTML(glue("</br>Last updated {jobseeker_publication_date}"))), width = 12)
        ),
        tabItem(tabName = "jobseeker_graph",
                fluidRow(
                    box(title = 'JobSeeker and Youth Allowance (excluding students and apprentices) recipients',
                        tags$body(HTML("Total recipients and normalised to the 15-64 y.o. population. ",
                                       "The regions shown are SA2 areas within the City of Moonee Valley shown as well as Moonee Valley and Greater Melbourne.")), width = 12)
                ),
                fluidRow(
                    box(selectInput(inputId = "js_graph_input",
                                    label = "Select the data type",
                                    choices = js_data_list,
                                    selected = "Percentage aged 15-64 on either JobSeeker or Youth Allowance"),
                    ),
                    box(checkboxGroupInput(inputId = "js_region_input",
                                           label = "Select the regions",
                                           choices = region_list,
                                           selected = selected_regions,
                                           inline = TRUE),
                    ),
                ),
                fluidRow(plotlyOutput("jobseeker_lines") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:',
                    tags$a(href="https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile", target="_blank",
                           "Department of Social Services, JobSeeker Payment and Youth Allowance recipients – monthly profile; "),
                    tags$a(href="https://www.abs.gov.au/AUSSTATS/abs@.nsf/mf/3235.0", target="_blank",
                           "ABS, 3235.0 - Regional Population by Age and Sex (2018)"),
                    tags$body(HTML(glue("</br>Last updated {jobseeker_publication_date}"))), width = 12)
        ),
        tabItem(tabName = "jobseeker_table",
                fluidRow(
                    box(title = 'JobSeeker and Youth Allowance (excluding students and apprentices) recipients',
                        tags$body(HTML("Total recipients and normalised to 15-64 y.o. population. ",
                                       "The regions shown are SA2 areas within the City of Moonee Valley shown as well as Moonee Valley and Greater Melbourne.")), width = 12)
                ),
                fluidRow(DTOutput("jobseeker_large_table") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:', 
                    tags$a(href="https://data.gov.au/data/dataset/jobseeker-payment-and-youth-allowance-recipients-monthly-profile", target="_blank",
                           "Department of Social Services, JobSeeker Payment and Youth Allowance recipients – monthly profile; "),
                    tags$a(href="https://www.abs.gov.au/AUSSTATS/abs@.nsf/mf/3235.0", target="_blank",
                           "ABS, 3235.0 - Regional Population by Age and Sex (2018)"),
                    tags$body(HTML(glue("</br>Last updated {jobseeker_publication_date}"))), width = 12),
        ),
        ## salm data ####
        tabItem(tabName = "unemp_map",
                fluidRow(
                    box(title = 'Labour force and unemployment data (SA2)',
                        tags$body(HTML(glue("Smoothed unemployment rate (%), {salm_current_month}</br>",
                                            "</br>Click on the map to see the unemployment rate."))), width = 12)
                ),
                fluidRow(tmapOutput("salm_unemp_map") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:',
                    tags$a(href="https://www.employment.gov.au/small-area-labour-markets-publication-0", target="_blank",
                           "Department of Education, Skills and Employment, Small Area Labour Markets publication"),
                    tags$body(HTML(glue("Last updated {salm_publication_date}"))), width = 12)
        ),
        tabItem(tabName = "unemp_graph",
                fluidRow(
                    box(title = 'Unemployment and labour force',
                        tags$body(HTML(glue("Unemployment rate, number of unemployed and labour force. ",
                                            "The regions shown are SA2 areas within the City of Moonee Valley shown as well as Moonee Valley and Greater Melbourne."))), width = 12)
                ),
                fluidRow(
                    box(selectInput(inputId = "salm_data_input",
                                    label = "Select the data type",
                                    choices = salm_data_list,
                                    selected = "Unemployment rate %"),
                    ),
                    box(checkboxGroupInput(inputId = "salm_region_input",
                                           label = "Select the regions",
                                           choices = region_list,
                                           selected = selected_regions,
                                           inline = TRUE),
                    ),
                ),
                fluidRow(plotlyOutput("salm_lines") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:',
                    tags$a(href="https://www.employment.gov.au/small-area-labour-markets-publication-0", target="_blank",
                           "Department of Education, Skills and Employment, Small Area Labour Markets publication"),
                    tags$body(HTML(glue("Last updated {salm_publication_date}"))), width = 12)
        ),
        tabItem(tabName = "unemp_table",
                fluidRow(
                    box(title = 'Labour force and unemployment table',
                        tags$body(HTML(glue("Unemployment rate, number of unemployed and labour force - {salm_current_month}. ",
                                            "The regions shown are SA2 areas within the City of Moonee Valley shown as well as Moonee Valley and Greater Melbourne."))), width = 12)
                ),
                fluidRow(DTOutput("salm_large_table") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8))
                ),
                box(title = 'Sources:', 
                    tags$a(href="https://www.employment.gov.au/small-area-labour-markets-publication-0", target="_blank",
                           "Department of Education, Skills and Employment, Small Area Labour Markets publication"),
                    tags$body(HTML(glue("Last updated {salm_publication_date}"))), width = 12)
        ),
        ## Notes ####
        tabItem(tabName = "notes",
                fluidRow(
                    box(title = 'Jobeeker and Youth Allowance:',
                        tags$body(HTML(glue("The <b>JobSeeker</b> payment replaced the Newstart allowance in March 2020 and is available to Australian residents aged between 22 and 65 years, who are unemployed and looking for work or are unable to do their usual work for a short period. ",
                                            "<b>Youth Allowance</b> is available to those aged between 16 and 21 years looking for full time work, as well as others who are studying or doing an apprenticeship. Not all of these recipients are unemployed. To be unemployed, one must be actively looking for work. The data here only includes those who are not students nor apprentices."))), width = 12),
                    box(title = 'Unemployment and labour force',
                        tags$body(HTML(glue("Unemployment and labour force data is from the Small Area Labour Markets publication. ",
                                            "The <b> labour force</b> is the population aged 15 years and over that are either in work, or actively looking for work in a region. ",
                                            "The <b> number of unemployed </b> is the part of the labour force that are looking for work. ",
                                            "The <b> unemployment rate </b> is calculated as the <b>number of unemployed / number in the labour force * 100</b>."))), width = 12),
                    box(title = 'More information:',
                        tags$body(HTML("Contact the Research and Facilities team if you have any queries.")), width = 12)
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
    output$jobkeeper_mv_table <- renderDT(jk_mv_postcodes,
                                          options = list(dom = 't',
                                                         columnDefs = list(list(className = 'dt-right', targets = 2))
                                          ))
    
    # jobseeker data   
    jobseeker_joined_filtered <- reactive({
        jobseeker_joined %>% 
            filter(data_type == input$js_graph_input) %>% 
            filter(region %in% input$js_region_input) 
    })
    
    output$jobseeker_large_table <- renderDT(jobseeker_large,
                                             options = list(dom = 't',
                                                            columnDefs = list(list(className = 'dt-right', targets = c(2, 4, 6)))
                                             ))
    
    # salm data
    salm_chart_data_filtered <- reactive({
        salm_chart_data %>% 
            filter(data_type == input$salm_data_input) %>% 
            filter(region %in% input$salm_region_input)
    })
    
    output$salm_large_table <- renderDT(salm_table_data,
                                        options = list(dom = 't',
                                                       columnDefs = list(list(className = 'dt-right', targets = c(3, 4)))
                                        ))
    
    # graphs ###############################################
    # jobs wages plotly change line
    output$jobs_wages_line <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobs_index, x = ~date, y = ~values, name = "Jobs", mode = "lines+markers") %>% 
            add_trace(data = wages_index, x = ~date, y = ~values, name = "Wages", mode = "lines+markers") %>% 
            layout(xaxis = list(title = 'Date'), yaxis = list(title = "Change % (from 14 March)")) %>%
            layout(hovermode = "x unified")
    })
    
    # jobs wages plotly change line
    output$jobs_wages_age_bar <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobs_wages_by_age_data_males_filtered(), x = ~age_group, y = ~latest_week, type = 'bar', name = 'Males') %>% 
            add_trace(data = jobs_wages_by_age_data_females_filtered(), x = ~age_group, y = ~latest_week, type = 'bar', name = 'Females') %>% 
            layout(xaxis = list(title = 'Age'), yaxis = list(title = "Change % (from 14 March)")) %>%
            layout(hovermode = "x unified")
    })
    
    # jobseeker lines
    output$jobseeker_lines <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = jobseeker_joined_filtered(), x = ~month, y = ~values,
                      mode = "lines+markers", color = ~region) %>% 
            layout(xaxis = list(title = 'Month'), yaxis = list(title = "Value")) %>%
            layout(hovermode = "x unified")
    })
    
    # salm lines
    output$salm_lines <- renderPlotly({
        plot_ly() %>% 
            add_trace(data = salm_chart_data_filtered(), x = ~date, y = ~values,
                      mode = "lines+markers", color = ~region) %>% 
            layout(xaxis = list(title = 'Month'), yaxis = list(title = "Value")) %>%
            layout(hovermode = "x unified")
    })
    
    # maps #####################################################
    output$jobkeeper_pc_map <- renderTmap({
        tmap_mode("view")
        tm_shape(jk_join, bbox = tmaptools::bb(mv_shp)) +
            tm_fill("count",
                    popup.vars = c("Recipients " = "count"),
                    popup.format=list(count=list(digits=0))) +
            tm_borders(alpha = 0.5, col = "grey") +
            tm_shape(mv_shp) +
            tm_borders(alpha = 0.5, col = "purple", lwd = 2)
    })
    
    output$jobseeker_map <- renderTmap({
        tmap_mode("view")
        tm_shape(js_map_join, bbox = tmaptools::bb(mv_shp)) +
            tm_fill("percentage",
                    popup.vars = c("% 15-64y.o. " = "percentage"),
                    popup.format=list(percentage=list(digits=1))) +
            tm_borders(alpha = 0.5, col = "grey") +
            tm_shape(mv_shp) +
            tm_borders(alpha = 0.5, col = "purple", lwd = 2)
    })
    
    output$salm_unemp_map <- renderTmap({
        tmap_mode("view")
        tm_shape(unemp_rate_map_join, bbox = tmaptools::bb(mv_shp)) +
            tm_fill("rate",
                    popup.vars = c("Unemployment rate % " = "rate"),
                    popup.format=list(rate=list(digits=1))) +
            tm_borders(alpha = 0.5, col = "grey") +
            tm_shape(mv_shp) +
            tm_borders(alpha = 0.5, col = "purple", lwd = 2)
    })
    
    # valueboxes #############
    output$vbox_jobkeep <- renderValueBox({
            valueBox(value = tags$p(jk_mv_num, style = "font-size: 150%;"),
                    subtitle = glue("Jobkeeper recipients in Moonee Valley postcodes* ({jobkeeper_data_date})"),
                    width = 4, color = "yellow")
    })
    
    output$vbox_jobseek <- renderValueBox({
        valueBox(value = tags$p(js_mv_num, style = "font-size: 150%;"),
                 subtitle = glue("Jobseeker and Youth Allowance (excluding students and apprentices) recipients ({jobseeker_month_long})"),
                 width = 4, color = "yellow")
    })
    
    output$vbox_unemp_rate <- renderValueBox({
        valueBox(value = tags$p(glue("{unemp_rate_mv_num}%"), style = "font-size: 150%;"),
                 subtitle = glue("Unemployment rate % ({salm_current_month})"),
                 width = 4, color = "yellow")
    })

}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)