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

  
   retrieved_data <- readRDS(rds_file)
   
   active_plot <-  ggplot(data=(retrieved_data$retrieved_data %>% filter(!(State %in% c("Total")))),
                           aes(x=Date,y=active,fill=State)) +
     geom_bar(stat="identity") +
     theme_economist_white() + 
     
     geom_line(data=(retrieved_data$retrieved_data %>% filter(State %in% c("Total"))),
               aes(x=Date, y=active), colour="blue") +
     labs(title="Active Cases in Austria per State",
          x="Date",
          y="Active Cases",
          caption="Data: https://info.gesundheitsministerium.at" ) +
     theme(legend.position = "bottom",
           plot.title = element_text(size=14),
           axis.title.x = element_text(size = 10),
           axis.text.x = element_text(angle = 0, hjust = 1,size = 10),
           axis.title.y = element_text(size = 10),
           axis.text.y = element_text(size = 10),
           strip.text.x = element_text(size = 12),
           strip.text.y = element_text(size = 12, angle = 90),
           legend.title=element_text(size=8),
           legend.text=element_text(size=8))+
     scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
     scale_fill_tableau(breaks=(retrieved_data$retrieved_data %>% filter(!(State %in% c("Total"))) %>% pull(State) %>% unique(.)))
   
   
   new_plot <-  ggplot(data=(retrieved_data$retrieved_data %>% filter(!(State %in% c("Total")))),
                       aes(x=Date,y=new_cases,fill=State)) +
     geom_bar(stat="identity") +
     theme_economist_white() + 
     
     geom_line(data=(retrieved_data$retrieved_data %>% filter(State %in% c("Total"))),
               aes(x=Date, y=new_cases), colour="blue") +
     labs(title="New Daily Cases in Austria per State",
          x="Date",
          y="New Cases",
          caption="Data: https://info.gesundheitsministerium.at" ) +
     theme(legend.position = "bottom",
           plot.title = element_text(size=14),
           axis.title.x = element_text(size = 10),
           axis.text.x = element_text(angle = 0, hjust = 1,size = 10),
           axis.title.y = element_text(size = 10),
           axis.text.y = element_text(size = 10),
           strip.text.x = element_text(size = 12),
           strip.text.y = element_text(size = 12, angle = 90),
           legend.title=element_text(size=8),
           legend.text=element_text(size=8))+
     scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
     scale_fill_tableau(breaks=(retrieved_data$retrieved_data %>% filter(!(State %in% c("Total"))) %>% pull(State) %>% unique(.)))
    
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

