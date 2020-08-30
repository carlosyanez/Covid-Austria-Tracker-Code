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
                fluidRow(plotly::plotlyOutput("daily_new_plot")),
                fluidRow(plotly::plotlyOutput("daily_age_plot")),
                fluidRow(plotly::plotlyOutput("daily_positive_plot"))
                
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
     labs(title="Active Cases in Austria",
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
     labs(title="New Daily Cases in Austria",
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
    
   positive_plot <- retrieved_data$retrieved_data_general %>%
     select(Date,positive_rate) %>%
     ggplot(aes(x=Date,y=positive_rate)) + geom_line(color="pink") +
     geom_point(color="red") +
     theme_economist_white() + 
     labs(title="Positive Rate",
          x="Date",
          y="Positive Rate (%)",
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
           legend.text=element_text(size=8)) +
     scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
   
   age_plot <- retrieved_data$retrieved_data_general %>%
     select(-GesTestungen,-PositivGetestet,-positive_rate) %>%
     pivot_longer(-Date) %>% filter(!(Date=="2020-06-29")) %>%
     mutate(value=ifelse(value<0,0,value)) 
   
   f<- tibble(name=unique(age_plot$name), 
              Group=factor(unique(age_plot$name),levels=c(">84","75-84","65-74","55-64",
                                                          "45-54","35-44","25-34",
                                                          "15-24","5-14","<5")))
   
   age_plot <-  age_plot  %>% left_join(f,by="name") %>%
     select(Date,Group,value) %>%
     ggplot(aes(x=Date,y=value,fill=Group)) + geom_bar(stat="identity") +
     theme_economist_white() + 
     labs(title="New Cases per Age Group",
          x="Date",
          y="Cases",
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
     scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
   
   
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
    
    output$daily_positive_plot <- plotly::renderPlotly({
      ggplotly(positive_plot)
    })
    
    output$daily_age_plot <- plotly::renderPlotly({
      ggplotly(age_plot)
    })
}

shinyApp(ui = ui, server = server) 

