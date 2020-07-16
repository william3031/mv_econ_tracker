#add libraries -don't use pacman - it won't work when published to shinyapps
library(shiny)
library(tidyverse)
library(glue)
library(readxl)
library(janitor)
library(plotly)
library(scales)
library(shinydashboard)

#disable scientific notation
options(scipen = 999)

#data
##read in data - this is reading from an extract of 19/259311
iris



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

sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(
        menuItem("Home", tabName = "Home"),
        menuItem("Jobs and wages (Vic.)", tabName = "jobs_wages_vic"),
        menuItem("Jobkeeper", tabName = "jobkeeper_postcodes"),
        menuItem("Jobseeker and Youth Allowance", tabName = "jobseeker_youth_allow"),
        menuItem("Unemployment rate", tabName = "unemployment"),
        menuItem("Notes", tabName = "notes")
    )
)

body <- dashboardBody(
    #supress error messages
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #bigger text in boxes
    tags$p(
        tags$style(HTML('.box { font-family: Helvetica, Arial, sans-serif; font-size: 14px; }'))
    ),
    
    tabItems(
        # First tab content
        tabItem(tabName = "measures",
                #select
                fluidRow(
                    box(selectInput(inputId = "selected_theme",
                                    label = "Select a theme",
                                    choices = theme_list),
                    ),
                    box(uiOutput("measure_output"))
                ),
                
                # infoBoxes dynamic colours based on function in server
                fluidRow(
                    infoBoxOutput("ibox_theme"),
                    infoBoxOutput("ibox_sd"),
                    infoBoxOutput("ibox_cat")
                ),
                
                # info about the graph
                fluidRow(
                    box(title = "Measure", width = 3, background = "black", textOutput('title')),
                    box(title = "Definition", width = 3, background = "black", textOutput('definition')),
                    box(title = "Source", width = 3, background = "black", textOutput('source')),
                    box(title = "Target rationale", width = 3, background = "black", textOutput('target_source'))
                ),
                
                # the graph
                fluidRow(plotlyOutput("measure_graph") %>% 
                             withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                ),
                
                
                #about progress towards desired change
                fluidRow(
                    infoBoxOutput("ibox_desired"),
                    infoBoxOutput("ibox_progress"),
                    infoBoxOutput("ibox_influence")
                ),
                
                #commentary and rationale below the graph
                fluidRow(
                    box(title = "Commentary", width = 6, textOutput('commentary')),
                    box(title = "Rationale for measure", width = 6, textOutput('rationale'))
                ),
        ),
        # progress by theme content
        tabItem(tabName = "progress",
                h2("Summary of progress towards the 2040 targets by theme"),
                br(),
                fluidRow(
                    infoBox(title = "Theme", value = "Fair", color = "red", width = 3, icon=icon(list(src = "fair.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Fair") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Thriving", color = "blue", width = 3, icon=icon(list(src = "thriving.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Thriving") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Connected", color = "purple", width = 3, icon=icon(list(src = "connected.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Connected") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Green", color = "green", width = 3, icon=icon(list(src = "green.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Green") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Theme", value = "Beautiful", color = "yellow", width = 3, icon=icon(list(src = "beautiful.png", width="80px"), lib="local")),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(theme_pct %>% filter(theme == "Beautiful") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    box(title = "Notes", "Figures are rounded and may not add up to 100 per cent")
                ),
        ),
        
        # progress by council influence
        tabItem(tabName = "influence",
                h2("Summary of progress towards the 2040 targets by level of Council influence"),
                br(),
                fluidRow(
                    infoBox(title = "Council's infulence", value = "Lead", icon = shiny::icon("users"), color = "black", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Lead") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Lead") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Lead") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Council's infulence", value = "Contribute", icon = shiny::icon("handshake"), color = "black", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Contribute") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Contribute") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Contribute") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    infoBox(title = "Council's infulence", value = "Advocate", icon = shiny::icon("bullhorn"), color = "black", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Advocate") %>% select(pct_positive) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Positive' progress",
                             icon = shiny::icon("smile"), color = "aqua", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Advocate") %>% select(pct_na) %>% pull(), accuracy = 1L), subtitle = "of measures with 'N/A' progress",
                             icon = shiny::icon("meh"), color = "orange", width = 3),
                    valueBox(value = percent(influence_pct %>% filter(influence == "Advocate") %>% select(pct_negative) %>% pull(), accuracy = 1L), subtitle = "of measures with 'Negative' progress",
                             icon = shiny::icon("frown"), color = "maroon", width = 3),
                ),
                fluidRow(
                    box(title = "Notes", "Figures are rounded and may not add up to 100 per cent")
                ),
        ),
        
        #circumplex 2
        tabItem(tabName = "circ2",
                fluidRow(
                    box(tags$body(HTML("<b>Progress towards targets by measure</b></br>",
                                       "</br>You can hover over the wedges to see the the current state and the target for each measure.</br>",
                                       "</br> The graph shows progress towards the target for <b>selected measures only</b> (those measured as a percentage where the desired outcomes is an increase).",
                                       "The darker shading indicates the current state while the lighter shading indicates where we would like to be in 2040 (the target). Real percentage values have been used.</br>",
                                       "<br>Note: The chart size is fixed.</br>")), width = 12)
                ),
                fluidRow(
                    girafeOutput("circ2") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - fair
        tabItem(tabName = "summary_fair",
                fluidRow(
                    infoBox(title = "Theme", value = "Fair", color = "red", width = 12, icon=icon(list(src = "fair.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_fair_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - thriving
        tabItem(tabName = "summary_thriving",
                fluidRow(
                    infoBox(title = "Theme", value = "Thriving", color = "blue", width = 12, icon=icon(list(src = "thriving.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_thriving_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - connected
        tabItem(tabName = "summary_connected",
                fluidRow(
                    infoBox(title = "Theme", value = "Connected", color = "purple", width = 12, icon=icon(list(src = "connected.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_connected_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - green
        tabItem(tabName = "summary_green",
                fluidRow(
                    infoBox(title = "Theme", value = "Green", color = "green", width = 12, icon=icon(list(src = "green.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_green_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #theme tabs - beautiful
        tabItem(tabName = "summary_beautiful",
                fluidRow(
                    infoBox(title = "Theme", value = "Beautiful", color = "yellow", width = 12, icon=icon(list(src = "beautiful.png", width="80px"), lib="local")),
                    box(ind_theme_text, width = 12)
                ),
                fluidRow(
                    plotlyOutput("towards_beautiful_graph") %>% 
                        withSpinner(color="#31788F", type = getOption("spinner.type", default = 8)),
                )
        ),
        
        #third tab
        tabItem(tabName = "notes",
                fluidRow(
                    box(title = 'MV2040', tags$body(HTML("The MV2040 Strategy is Council's long-term strategy and vision for a healthy 'city of neighbourhoods'.",
                                                         "Given Council's commitment and investment in MV2040 - it will be important to develop systems to ensure we're making evidenced based progress towards our Vision's 20 strategic directions with associated targets.</br>",
                                                         "</br>This MV2040 Outcomes Framework will assist Council to monitor, evaluate and report on its progress towards a healthy city.</br>")), width = 12),
                    box(title = "Notes:", tags$body(HTML("This is a work in progress. Contact research@mvcc.vic.gov.au if you have any queries.</br>",
                                                         "<br>Last updated: 14 May 2020")), width = 12)
                )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    #filtered_measure list
    filtered_measures <- reactive({
        indicator_list %>% 
            filter(theme == input$selected_theme) %>% 
            select(measure) %>% 
            pull()
    })
    
    #takes filtered input by theme and outputs measures for selection
    output$measure_output <- renderUI({
        selectInput(inputId = "selected_measure",
                    label = "Select a measure",
                    choices = filtered_measures())
    })
    
    #selected_id from measure
    selected_id <- reactive({
        indicator_list %>% 
            filter(measure == input$selected_measure) %>% 
            select(id) %>% 
            pull()
    })
    
    #get values - from id in function
    get_vals <- reactive({
        ind_vals(selected_id())
    })
    
    #plotly graph
    output$measure_graph <- renderPlotly({  
        print(
            make_plotly(get_vals())
        )
    })
    #text - strategic direction
    output$strategic_direction <- renderText({
        glue("{get_vals()$strategic_direction}")
    })
    #text - theme
    output$theme <- renderText({
        glue("{get_vals()$theme}")
    })
    #text - category
    output$category <- renderText({
        glue("{get_vals()$category}")
    })
    #text - title
    output$title <- renderText({
        glue("{get_vals()$title}")
    })
    #text - definition
    output$definition <- renderText({
        glue("{get_vals()$definition}")
    })
    #text - source
    output$source <- renderText({
        glue("{get_vals()$source}")
    })
    #text - commentary
    output$commentary <- renderText({
        glue("{get_vals()$commentary}")
    })
    #text - target_source
    output$target_source <- renderText({
        glue("{get_vals()$target_source}")
    })
    #text - council's influence
    output$council_inf <- renderText({
        glue("{get_vals()$council_inf}")
    })
    #text - rationale
    output$rationale <- renderText({
        glue("{get_vals()$rationale}")
    })
    #text - desired change with baseline number
    output$desired_change <- renderText({
        glue("{get_vals()$desired_change} from baseline value towards target")
    })
    #text - change progress
    output$change_progress <- renderText({
        glue("{get_vals()$change_progress}")
    })
    #text - target source
    output$target_source <- renderText({
        glue("{get_vals()$target_source}")
    })
    
    # the theme box
    output$ibox_theme <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            ic <- apputils::icon(list(src = "fair.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "red", icon = ic)
        }
        else if (input$selected_theme  == "Thriving")
        {
            ic <- apputils::icon(list(src = "thriving.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "blue", icon = ic)
        }
        else if (input$selected_theme  == "Connected")
        {
            ic <- apputils::icon(list(src = "connected.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "purple", icon = ic)
        }
        else if (input$selected_theme  == "Green")
        {
            ic <- apputils::icon(list(src = "green.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "green", icon = ic)
        }
        else {
            ic <- apputils::icon(list(src = "beautiful.png", width = "80px"), lib = "local")
            infoBox(title = "Theme", textOutput('theme'), width = 4, color = "yellow", icon = ic)
        }
    })
    
    
    # the strategic direction box
    output$ibox_sd <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "red", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "blue", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "purple", icon = icon("map-signs"))
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "green", icon = icon("map-signs"))
        }
        else {
            infoBox(title = "Strategic Direction", textOutput('strategic_direction'), width = 4, color = "yellow", icon = icon("map-signs"))
        }
    })
    
    # the category box
    output$ibox_cat <- renderInfoBox({
        if (input$selected_theme  == "Fair")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "red", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Thriving")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "blue", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Connected")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "purple", icon = icon("chart-bar"))
        }
        else if (input$selected_theme  == "Green")
        {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "green", icon = icon("chart-bar"))
        }
        else {
            infoBox(title = "Category", textOutput('category'), width = 4, color = "yellow", icon = icon("chart-bar"))
        }
    })
    
    # desired change box
    output$ibox_desired <- renderInfoBox({
        if (get_vals()$desired_change  == "Increase")
        {
            infoBox(title = "Desired change", textOutput('desired_change'), icon = shiny::icon("arrow-up"), color = "black")
        }
        else {
            infoBox(title = "Desired change", textOutput('desired_change'), icon = shiny::icon("arrow-down"), color = "black")
        }
    })
    
    # the progress towards desired change box
    output$ibox_progress <- renderInfoBox({
        if (get_vals()$change_progress  == "Positive")
        {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("smile"), color = "aqua")
        }
        else if (get_vals()$change_progress  == "Negative")
        {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("frown"), color = "maroon")
        }
        else {
            infoBox(title = "Progress", textOutput('change_progress'), icon = shiny::icon("meh"), color = "orange")
        }
    })
    
    # the influence box
    output$ibox_influence <- renderInfoBox({
        if (get_vals()$council_inf  == "Lead")
        {
            infoBox(title = "Council's influence", textOutput('council_inf'), icon = shiny::icon("users"), color = "black")
        }
        else if (get_vals()$council_inf  == "Contribute")
        {
            infoBox(title = "Council's influence", textOutput('council_inf'), icon = shiny::icon("handshake"), color = "black")
        }
        else {
            infoBox(title = "Council's influence", textOutput('council_inf'), icon = shiny::icon("bullhorn"), color = "black")
        }
    })
    
    #individual plotly graphs for theme progress
    output$towards_fair_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_fair, "#E55048")
        )
    })
    output$towards_thriving_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_thriving, "#31788F")
        )
    })
    output$towards_connected_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_connected, "#6A4479")
        )
    })
    output$towards_green_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_green, "#4EA546")
        )
    })
    output$towards_beautiful_graph <- renderPlotly({  
        print(
            make_theme_plotly(towards_beautiful, "#E3A51E")
        )
    })
    output$circ2 <- renderGirafe({  
        girafe(ggobj = circ2gg, width_svg = 12, height_svg = 12,
               options = list(
                   opts_hover(css = "fill:aqua;")
               ))
    })
    
}

# for the app
ui <- dashboardPage(header,
                    sidebar,
                    body)

shinyApp(ui, server)