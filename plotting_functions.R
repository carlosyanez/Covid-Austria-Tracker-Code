

plot_formatter <- function(plot_var,plot_title,xlabel,ylabel,plot_caption,cscale){
  
  plot_var <- plot_var +
             theme_economist_white() + 
            labs(title=plot_title,
            x=xlabel,
            y=ylabel,
            caption=plot_caption) +
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
    scale_fill_manual(labels = cscale$State , values = cscale$state_colour ) +
    scale_colour_manual(labels = cscale$State , values = cscale$state_colour ) 
  
    plot_var
}


new_cases_plot <- function(retrieved_data,filter_value,colour_scale){
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(State %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(State %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((State==filter_value)))
    data2 <- data1 %>% mutate(AnzahlFaelle=0)
    colour_scale <- colour_scale %>% filter(State==filter_value)
    
  }
  
  new_plot <-  ggplot(data=data1,
                      aes(x=Date,y=AnzahlFaelle,fill=State)) +
    geom_bar(stat="identity") 
  
   if(nrow(data2)!=0 & filter_value == "Austria"){
    new_plot <- new_plot + geom_line(data=data2,
                                     aes(x=Date, y=AnzahlFaelle), colour="blue")
    }  
  

  new_plot <- plot_formatter(new_plot,
                                paste("New Daily Cases in",filter_value),
                                "Date",
                                "New Cases",
                                "Data: https://covid19-dashboard.ages.at/",
                                colour_scale)
  
  new_plot
}

active_cases_plot <- function(retrieved_data,filter_value,colour_scale){
  
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(State %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(State %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((State==filter_value)))
    data2 <- data1 %>% mutate(Active=0)
    colour_scale <- colour_scale %>% filter(State==filter_value)
  }
  
  
  active_plot <-  ggplot(data=data1,
                         aes(x=Date,y=Active,fill=State)) +
    geom_bar(stat="identity") 
  
  
  if(nrow(data2)!=0 & filter_value == "Austria"){
    active_plot <- active_plot + geom_line(data=data2,
                                     aes(x=Date, y=Active, colour=State))
  }  
  
  active_plot <- plot_formatter(active_plot,
                                paste("Active Cases in",filter_value),
                                "Date",
                                "Active Cases",
                                "Data: https://covid19-dashboard.ages.at/",
                                colour_scale)
  active_plot
  
}

testing_results_plot <- function(retrieved_data,filter_value,colour_scale){
  
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(State %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(State %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((State==filter_value)))
    data2 <- data1 %>% mutate(TestGesamt=0)
    colour_scale <- colour_scale %>% filter(State==filter_value)
  }
  
  testing_plot <-  ggplot(data=data1,aes(x=Date,y=TestGesamt,fill=State)) +
    geom_bar(stat="identity")
  
  
  if(nrow(data2)!=0 & filter_value == "Austria"){
    testing_plot <- testing_plot + geom_line(data=data2,aes(x=Date, y=TestGesamt), colour="blue")
  }   
  

  testing_plot <- plot_formatter(testing_plot,
                                paste("Daily Testing in",filter_value),
                                "Date",
                                "Tests Done",
                                "Data: https://covid19-dashboard.ages.at/",
                                colour_scale)
  testing_plot

}

pos_plot00 <- function(retrieved_data,filter_value,colour_scale){

  if(filter_value=="") filter_value <-"Austria"
  
  positive_plot0 <- retrieved_data %>%
    filter(State==filter_value) %>%
    ggplot(aes(x=Date,y=Positivity,color=State)) + geom_line() +
    geom_point() 
  
  colour_scale <- colour_scale %>% filter(State==filter_value)
  
  
  positive_plot0 <- plot_formatter(positive_plot0,
                                 paste("Positivity Rate in",filter_value),
                                 "Date",
                                 "Positivity Rate (%)",
                                 "Data: https://covid19-dashboard.ages.at/",
                                 colour_scale)
  positive_plot0
  
}
pos_plot01 <- function(retrieved_data,colour_scale){
  
  positive_plot1 <- retrieved_data %>%
    filter(State!="Austria") %>%
    ggplot(aes(x=Date,y=Positivity,color=State)) + geom_line() +
    geom_point() 
  
  positive_plot1 <- plot_formatter(positive_plot1,
                                   paste("Positivity Rate per State"),
                                   "Date",
                                   "Positivity Rate (%)",
                                   "Data: https://covid19-dashboard.ages.at/",
                                   colour_scale) +
                       facet_wrap(State ~.)
  
  positive_plot1
  
  
}

load_plot00 <- function(retrieved_data,filter_value,colour_scale){
  
  if(filter_value!=""){
    retrieved_data <- retrieved_data %>%
    filter(State==filter_value)
    
    
  }else{
    retrieved_data <- retrieved_data %>%
      filter(State=="Austria")
    filter_value<-"Austria"
    colour_scale <- colour_scale %>% filter(State==filter_value) %>% mutate(State="Hospital_Load")
    }
  
 
  
  ICU_Load <- tribble(~State,~state_colour,
                      "ICU_Load","purple")
  
  colour_scale <- rbind(colour_scale,ICU_Load)
  
  load_plot0 <- retrieved_data %>% select(Date,Hospital_Load,ICU_Load,FZHosp,FZICU) %>%
    pivot_longer(c(-Date,-FZHosp,-FZICU),values_to="value",names_to="Type") %>%
    ggplot(aes(x=Date,y=value,color=Type)) + geom_line() +
    geom_point()
  
  load_plot0 <- plot_formatter(load_plot0,
                                   paste("Hospital Load in",filter_value),
                                   "Date",
                                   "Load Percentage (%)",
                                   "Data: https://covid19-dashboard.ages.at/",
                                   colour_scale) 
   
  
 load_plot0
}

load_plot01 <- function(retrieved_data,colour_scale){
  
  plotting_data <- retrieved_data %>%  select(Date,Hospital_Load,ICU_Load,FZHosp,FZICU,State) %>%
    filter(State!="Austria") %>%
    pivot_longer(c(-Date,-FZHosp,-FZICU,-State),values_to="value",names_to="Type") 
 
  load_plot1 <- plotting_data %>% ggplot(aes(x=Date,y=value,color=Type)) + geom_line() +
                geom_point()
  
 

  colour_scale <- tribble(~State,~state_colour,
                          "Hospital_Load","blue",
                          "ICU_Load","purple")
  

  load_plot1 <- plot_formatter(load_plot1,
                               paste("Hospital Load per State"),
                               "Date",
                               "Load Percentage (%)",
                               "Data: https://covid19-dashboard.ages.at/",
                               colour_scale)  +
                facet_wrap(State ~.)
  
  load_plot1
}

