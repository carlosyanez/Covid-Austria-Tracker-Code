################################################################################################
################################################################################################
#### Active Covid cases in Austria
#### Author : Carlos Yanez Santibanez
################################################################################################
################################################################################################

# Load required packages
library(shinydashboard)
library(tidyverse)
library(ggiraph)
library(lubridate)
library(ggthemes)
library(htmltools)
library(showtext)


# Shiny UI's function

header <- dashboardHeader(title = "Covid in Austria"#, 
                                           #    # put tracking code in html file
                                           )

sidebar <-dashboardSidebar(

  sidebarMenu(

    uiOutput("source_link"),
    uiOutput("source_link2"),
    uiOutput("measurement_times"),
#    uiOutput("blank1"),
#    uiOutput("blank2"),
#    uiOutput("blank3"),
    menuItem("Filters", icon = icon("filter"),
             
             selectizeInput('state', 'State', choices = "Loading..."),
             selectizeInput('dates_pre', 'Predefined Dates', choices = "Loading..."),
             dateRangeInput("date", "Free Dates Selection", start = NULL,
                            end =NULL, min = NULL,
                            max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                            language = "en", separator = " to ", width = NULL)
    )
    )
)


body <-   dashboardBody(
                # App title ----,
                tags$head(includeHTML(("google_analytics.html"))),
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", width = "100%",
                    tabPanel("New and Active", 
                             girafeOutput("daily_active_plot",width="100%"),
                             girafeOutput("daily_new_plot",width="100%")),
                    tabPanel("7-Day Trend", 
                             girafeOutput("daily_sevenday_plot0",width="100%"),
                             girafeOutput("daily_sevenday_plot1",width="100%")),
                    tabPanel("R Factor", 
                             girafeOutput("R_plot0",width="100%"),
                             girafeOutput("R_plot1",width="100%")),
                    tabPanel("Testing", 
                             girafeOutput("daily_testing_plot",width="100%")),
                    tabPanel("Positivity", 
                             girafeOutput("daily_positive_plot0",width="100%"),
                             girafeOutput("daily_positive_plot1",width="100%")),
                    tabPanel("Hospital Load", 
                             girafeOutput("daily_hospital_plot0",width="100%"),
                             girafeOutput("daily_hospital_plot1",width="100%"))
                    
                  )
                
              )


ui <- dashboardPage(
  header,
  sidebar,
  body
)


server <- function(input, output,session) {
    
  #####Get Data from files
  
  domain<-isolate(session$clientData$url_hostname)
  message(domain)
  rds_file <- "retrieved_data.rds"
  
  
  if(grepl( "shinyapps.io", domain, fixed = TRUE)){
     rds_url <- "https://github.com/carlosyanez/covid_austria_tracker/raw/master/retrieved_data.rds"
    download.file(rds_url,rds_file)
  }
  
   retrieved_data <- readRDS(rds_file)
   retrieved_data$retrieved_data <- retrieved_data$retrieved_data %>% mutate(State_fct=as.factor(State))
   data_to_plot <- retrieved_data$retrieved_data
   
   
   ###plotting functions
   source("plotting_functions.R") 
   
   ### parameters
   
    url <- a("AGES COVID19 Dashboard",
             href="https://covid19-dashboard.ages.at/")
    url2 <- a("AGES - R Factor",
             href="https://www.ages.at/wissen-aktuell/publikationen/epidemiologische-parameter-des-covid19-ausbruchs-oesterreich-20202021/")
  
   results <- reactiveValues()  
   results$pre <- "No Filter"
   filter_value <-"Austria"
   chart_hover_inv <-"opacity:0.55;"
   chart_hover <- "stroke-width:2;"
   chart_width_svg <- 12
   chart_height_svg <- 6
   

  predefined_dates <- c("No Filter",
                        "Last 7 Days",
                        "Last 4 Weeks",
                        "Last 3 Months",
                        "Last 6 Months")
  
   results$filter_value <- filter_value
   
  ####LOAD
   
  message("Initial Load")
   
 
   updateDateRangeInput(session, "date",
                          start= min(data_to_plot$Date),
                          end = max(data_to_plot$Date),
                          min =  min(data_to_plot$Date),
                          max = max(data_to_plot$Date))
 
   updateSelectizeInput(session, 'dates_pre', choices = predefined_dates, server = TRUE, selected="No Filter")
   updateSelectizeInput(session, 'state', choices = unique(data_to_plot$State), server = TRUE,selected = "Austria")
   
   toListenPlot <- reactive({
     list(input$state,
          input$date,
          input$dates_pre
          )
   })
   
     observeEvent(toListenPlot(),
                {
                  filter_value <- input$state
                  start_date <- input$date[[1]]
                  end_date<- input$date[[2]]
                  pre_date <- input$dates_pre
                  
                  ##check if predefined dates changed
                  
                  if(!(results$pre==pre_date)){
                    
                    end_date <- max(retrieved_data$retrieved_data$Date)
                    first_date <- min(retrieved_data$retrieved_data$Date)
                    date_selector <- c(first_date,
                                       end_date -ddays(6),
                                       end_date - dweeks(4),
                                       end_date - dmonths(4),
                                       end_date - dmonths(6)
                    )
  
                    start_date <- date_selector[which(input$dates_pre == predefined_dates)]
                    
                    if(length(as.character(start_date))==0){start_date <- first_date}
                    
                    updateDateRangeInput(session, "date",
                                         start= start_date,
                                         end = end_date,
                                         min =first_date,
                                         max = end_date)  
                    
                  } 
                  


                  if(length(input$date)==0){
                    data_to_plot <-   retrieved_data$retrieved_data
                  }else{
                    data_to_plot <- retrieved_data$retrieved_data  %>% filter(Date>=start_date & Date<=end_date)
                  }
                  
                  hospital_data1 <- hospital_load_data(data_to_plot,filter_value,grid_value=FALSE)
                  hospital_data2 <- hospital_load_data(data_to_plot,filter_value,grid_value=TRUE)
                  
                  results$new_plot <- general_plotter(data_to_plot,"columns","CasesDaily",filter_value,
                                                      "New Cases","State",plot_caption1,state_colour_scale)
                  results$active_plot <- general_plotter(data_to_plot,"columns","ActiveCases",filter_value,
                                                         "Active Cases","State",plot_caption1,state_colour_scale)
                  results$testing_plot <- general_plotter(data_to_plot,"columns","TestedDaily",filter_value,
                                                          "Daily Tests","State",plot_caption1,state_colour_scale)
                  results$positive_plot0 <- general_plotter(data_to_plot,"lines","Positivity",filter_value,
                                                            "Positivity Rate(%)","State",plot_caption1,state_colour_scale) 
                  results$positive_plot1 <- general_plotter(data_to_plot,"linegrid","Positivity",filter_value,
                                                            "Positivity Rate(%)","State",plot_caption1,state_colour_scale)    
                  results$hospital_plot0 <- general_plotter(hospital_data1,"lines","load_value",filter_value,
                                                            "Beds Load (%)","Load Type",plot_caption1,cscale=beds_colour_scale) 
                  results$hospital_plot1 <- general_plotter(hospital_data2,"linegrid","load_value",filter_value,
                                                            "Beds Load (%)","State",plot_caption1,cscale=beds_colour_scale)
                  results$sevenday_plot0 <-general_plotter(data_to_plot,"lines","SevenDayIncidence",filter_value,
                                                           "7 Day Incidence","State",plot_caption1,state_colour_scale)
                  results$sevenday_plot01 <-general_plotter(data_to_plot,"linegrid","SevenDayIncidence",filter_value,
                                                            "7 Day Incidence","State",plot_caption1,state_colour_scale)
                  results$R_plot00 <-general_plotter(data_to_plot,"lines","R_eff",filter_value,
                                                     "R Factor","State",plot_caption2,state_colour_scale)
                  results$R_plot01 <-general_plotter(data_to_plot,"linegrid","R_eff",filter_value,
                                                     "R Factor","State",plot_caption2,state_colour_scale)
                  
                })
   
    
    output$source_link <- renderUI({
        tagList("Source: ", url)
    })
    
    output$source_link2 <- renderUI({
      tagList("and ",url2)
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
    output$daily_active_plot <- renderGirafe({
        girafe(ggobj=results$active_plot, width_svg = chart_width_svg, height_svg = chart_height_svg,
               options = list(
                 opts_hover_inv(css = chart_hover_inv),
                 opts_hover(css = chart_hover)
               ))
    })
    
    output$daily_new_plot <- renderGirafe({
      girafe(ggobj=results$new_plot, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_positive_plot0 <- renderGirafe({
      girafe(ggobj=results$positive_plot0, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_positive_plot1 <- renderGirafe({
      girafe(ggobj=results$positive_plot1, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_testing_plot <- renderGirafe({
      girafe(ggobj=results$testing_plot, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_hospital_plot0 <- renderGirafe({
      girafe(ggobj=results$hospital_plot0, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_hospital_plot1 <- renderGirafe({
      girafe(ggobj=results$hospital_plot1, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_sevenday_plot0 <- renderGirafe({
      girafe(ggobj=results$sevenday_plot0, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$daily_sevenday_plot1 <- renderGirafe({
      girafe(ggobj=results$sevenday_plot01, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$R_plot0 <- renderGirafe({
      girafe(ggobj=results$R_plot00, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    output$R_plot1 <- renderGirafe({
      girafe(ggobj=results$R_plot01, width_svg = chart_width_svg, height_svg = chart_height_svg,
             options = list(
               opts_hover_inv(css = chart_hover_inv),
               opts_hover(css = chart_hover)
             ))
    })
    
    
}

shinyApp(ui = ui, server = server) 

