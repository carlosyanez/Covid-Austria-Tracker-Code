################################################################################################
################################################################################################
#### Active Covid cases in Austria
#### Author : Carlos Yanez Santibanez
################################################################################################
################################################################################################

# Load required packages
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(ggthemes)
library(htmltools)


# Shiny UI's function

header <- dashboardHeader(title = "Covid in Austria"#, 
                                           #    # put tracking code in html file
                                           )

sidebar <-dashboardSidebar(

  sidebarMenu(

    uiOutput("source_link"),
    uiOutput("measurement_times"),
    uiOutput("blank1"),
    uiOutput("blank2"),
    uiOutput("blank3"),
    menuItem("Filters", icon = icon("filter"),
             
             selectizeInput('state', 'State', choices = "Loading..."),
             dateRangeInput("date", "Dates", start = NULL,
                            end =NULL, min = NULL,
                            max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                            language = "en", separator = " to ", width = NULL)
    )
    )
)


body <-   dashboardBody(
#ui <- fluidPage(theme = "bootstrap.css",
#                tags$head(includeHTML(("google_analytics.html"))),
                
                # App title ----,
                tags$head(includeHTML(("google_analytics.html"))),
                fluidRow(plotly::plotlyOutput("daily_active_plot")),
                fluidRow(plotly::plotlyOutput("daily_new_plot")),
                fluidRow(plotly::plotlyOutput("daily_positive_plot0")),
                fluidRow(plotly::plotlyOutput("daily_positive_plot1")),
                fluidRow(plotly::plotlyOutput("daily_testing_plot")),
                fluidRow(plotly::plotlyOutput("daily_hospital_plot0")),
                fluidRow(plotly::plotlyOutput("daily_hospital_plot1"))
                               )



ui <- dashboardPage(
  header,
  sidebar,
  body
)


server <- function(input, output,session) {
    
    #Get Data from files
   
    rds_file <- "retrieved_data.rds"
    rds_url <- "https://github.com/carlosyanez/covid_austria_tracker/raw/master/retrieved_data.rds"
  
   
    # download.file(rds_url,rds_file)
  
   retrieved_data <- readRDS(rds_file)
   colour_scale <- unique(retrieved_data$retrieved_data %>% select(State,state_colour))
   
   source("plotting_functions.R") 
   
    url <- a("AGES COVID19 Dashboard",
             href="https://covid19-dashboard.ages.at/")
  
   results <- reactiveValues()  
   filter_value <-"Austria"
    
   results$filter_value <- filter_value
   data_to_plot <- retrieved_data$retrieved_data
   
   results$new_plot <- new_cases_plot(data_to_plot,filter_value,colour_scale)
   results$active_plot <- active_cases_plot(data_to_plot,filter_value,colour_scale)    
   results$testing_plot <- testing_results_plot(data_to_plot,filter_value,colour_scale)
   results$positive_plot0 <- pos_plot00(data_to_plot,filter_value,colour_scale)    
   results$positive_plot1 <- pos_plot01(data_to_plot,colour_scale)  
   results$hospital_plot0 <- load_plot00(data_to_plot,filter_value,colour_scale) 
   results$hospital_plot1 <- load_plot01(data_to_plot,colour_scale)    
   
   message("Initial Load")
   
 
   updateDateRangeInput(session, "date",
                          start= min(data_to_plot$Date),
                          end = max(data_to_plot$Date),
                          min =  min(data_to_plot$Date),
                          max = max(data_to_plot$Date))
 
   updateSelectizeInput(session, 'state', choices = unique(data_to_plot$State), server = TRUE)
   
   observeEvent(input$state,
                {
                  filter_value <- input$state
                  start_date <- input$date[[1]]
                  end_date<- input$date[[2]]
                  if(length(input$date)==0){
                    data_to_plot <-   retrieved_data$retrieved_data
                  }else{
                  data_to_plot <- retrieved_data$retrieved_data  %>% filter(Date>=start_date & Date<=end_date)
                  }
                  
                  results$new_plot <- new_cases_plot(data_to_plot,filter_value,colour_scale)
                  results$active_plot <- active_cases_plot(data_to_plot,filter_value,colour_scale)    
                  results$testing_plot <- testing_results_plot(data_to_plot,filter_value,colour_scale)
                  results$positive_plot0 <- pos_plot00(data_to_plot,filter_value,colour_scale)    
                  results$positive_plot1 <- pos_plot01(data_to_plot,colour_scale)  
                  results$hospital_plot0 <- load_plot00(data_to_plot,filter_value,colour_scale) 
                  results$hospital_plot1 <- load_plot01(data_to_plot,colour_scale)    
                  
                })
   
   observeEvent(input$date,
                {
                  filter_value <- input$state
                  start_date <- input$date[[1]]
                  end_date<- input$date[[2]]
                  if(length(input$date)==0){
                    data_to_plot <-   retrieved_data$retrieved_data
                  }else{
                    data_to_plot <- retrieved_data$retrieved_data  %>% filter(Date>=start_date & Date<=end_date)
                  }
                  
                  results$new_plot <- new_cases_plot(data_to_plot,filter_value,colour_scale)
                  results$active_plot <- active_cases_plot(data_to_plot,filter_value,colour_scale)    
                  results$testing_plot <- testing_results_plot(data_to_plot,filter_value,colour_scale)
                  results$positive_plot0 <- pos_plot00(data_to_plot,filter_value,colour_scale)    
                  results$positive_plot1 <- pos_plot01(data_to_plot,colour_scale)  
                  results$hospital_plot0 <- load_plot00(data_to_plot,filter_value,colour_scale) 
                  results$hospital_plot1 <- load_plot01(data_to_plot,colour_scale)    
                  
                })
    
    output$source_link <- renderUI({
        tagList("Source: ", url)
    })
    # Generate from and to Reference
    
    output$measurement_times <- renderUI({
        tagList("Data updated daily")
    })
    
    output$blank1 <- renderUI({
      tagList("====")
    })
    output$blank2 <- renderUI({
      tagList("====")
    })
    output$blank3 <- renderUI({
      tagList("====")
    })
    
    # Plot
    output$daily_active_plot <- plotly::renderPlotly({
        ggplotly(results$active_plot)
    })
    
    output$daily_new_plot <- plotly::renderPlotly({
      ggplotly(results$new_plot)
    })
    
    output$daily_positive_plot0 <- plotly::renderPlotly({
      ggplotly(results$positive_plot0)
    })
    
    output$daily_positive_plot1 <- plotly::renderPlotly({
      ggplotly(results$positive_plot1)
    })
    
    output$daily_testing_plot <- plotly::renderPlotly({
      ggplotly(results$testing_plot)
    })
    
    output$daily_hospital_plot0 <- plotly::renderPlotly({
      ggplotly(results$hospital_plot0)
    })
    
    output$daily_hospital_plot1 <- plotly::renderPlotly({
      ggplotly(results$hospital_plot1)
    })
    
}

shinyApp(ui = ui, server = server) 

