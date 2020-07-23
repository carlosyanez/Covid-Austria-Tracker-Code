################################################################################################
################################################################################################
#### Active Covid cases in Austria
#### Author : Carlos Yanez Santibanez
################################################################################################
################################################################################################

# Load required packages
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(ggthemes)


# Shiny UI's function
ui <- fluidPage(theme = "bootstrap.css",
                
                # App title ----
                fluidRow(
                titlePanel("Evolution of Active Cases in Austria"),
                uiOutput("source_link"),
                uiOutput("measurement_times")),
                fluidRow(plotly::plotlyOutput("daily_active_plot")),
                fluidRow(plotly::plotlyOutput("daily_new_plot"))
                )
                
server <- function(input, output) {
    
    #Get Data from files
   
    rds_file <- "retrieved_data.rds"
    rds_url <- "https://github.com/carlosyanez/covid_austria_tracker/raw/master/retrieved_data.rds"
  

     download.file(rds_url,rds_file)

  
   covid_austria <- readRDS(rds_file)
   
   active_plot <- covid_austria$active_plot
   new_plot <-  covid_austria$new_plot
    
    url <- a("Amtliches Dashboard COVID19 - Bundesministerium für Soziales, Gesundheit, Pflege und Konsumentenschutz",
             href="https://info.gesundheitsministerium.at/dashboard_Epidem.html?l=en")
    
    
    output$source_link <- renderUI({
        tagList("Source: ", url)
    })
    # Generate from and to Reference
    
    output$measurement_times <- renderUI({
        tagList("Data downloaded daily during the morning hours")
    })
    

    # Plot
    output$daily_active_plot <- plotly::renderPlotly({
        ggplotly(active_plot)
    })
    
    output$daily_new_plot <- plotly::renderPlotly({
        ggplotly(new_plot)
    })
}

shinyApp(ui = ui, server = server) 

