

new_cases_plot <- function(retrieved_data,filter_value){
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(Bundesland %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(Bundesland %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((Bundesland==filter_value)))
    data2 <- data1 %>% mutate(AnzahlFaelle=0)
  }
  
  new_plot <-  ggplot(data=data1,
                      aes(x=Date,y=AnzahlFaelle,fill=Bundesland)) +
    geom_bar(stat="identity") 
  
   if(nrow(data2)!=0 & filter_value == "Austria"){
    new_plot <- new_plot + geom_line(data=data2,
                                     aes(x=Date, y=AnzahlFaelle), colour="blue")
    }  
  
  new_plot <- new_plot +
    theme_economist_white() + 
    labs(title=paste("New Daily Cases in",filter_value),
         x="Date",
         y="Active Cases",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
    scale_fill_gdocs(breaks=(retrieved_data %>% filter(!(Bundesland %in% c("Austria"))) %>% pull(Bundesland) %>% unique(.)))
  
  new_plot
}

active_cases_plot <- function(retrieved_data,filter_value){
  
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(Bundesland %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(Bundesland %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((Bundesland==filter_value)))
    data2 <- data1 %>% mutate(Active=0)
  }
  
  active_plot <-  ggplot(data=data1,
                         aes(x=Date,y=Active,fill=Bundesland)) +
    geom_bar(stat="identity") 
  
  
  if(nrow(data2)!=0 & filter_value == "Austria"){
    active_plot <- active_plot + geom_line(data=data2,
                                     aes(x=Date, y=Active), colour="blue")
  }  
  
  active_plot <- active_plot + theme_economist_white() + 
    labs(title=paste("Active Cases in",filter_value),
         x="Date",
         y="Active Cases",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
    scale_fill_gdocs(breaks=(retrieved_data %>% filter(!(Bundesland %in% c("Austria"))) %>% pull(Bundesland) %>% unique(.)))
  
  active_plot
  
}

testing_results_plot <- function(retrieved_data,filter_value){
  
  
  if(filter_value=="Austria"){
    data1 <- (retrieved_data %>% filter(!(Bundesland %in% c("Austria"))))
    data2 <- (retrieved_data %>% filter(Bundesland %in% c("Austria")))
  }else{
    data1 <- (retrieved_data %>% filter((Bundesland==filter_value)))
    data2 <- data1 %>% mutate(TestGesamt=0)
  }
  
  testing_plot <-  ggplot(data=data1,aes(x=Date,y=TestGesamt,fill=Bundesland)) +
    geom_bar(stat="identity")
  
  
  if(nrow(data2)!=0 & filter_value == "Austria"){
    testing_plot <- testing_plot + geom_line(data=data2,aes(x=Date, y=TestGesamt), colour="blue")
  }   
  
  testing_plot <- testing_plot +
    theme_economist_white() + 
    labs(title=paste("Daily Testing in",filter_value),
         x="Date",
         y=paste("Active Cases in",filter_value),
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
    scale_fill_gdocs(breaks=(retrieved_data %>% filter(!(Bundesland %in% c("Austria"))) %>% pull(Bundesland) %>% unique(.)))
  
  testing_plot
  
}

pos_plot00 <- function(retrieved_data,filter_value){

  if(filter_value=="") filter_value <-"Austria"
  
  positive_plot0 <- retrieved_data %>%
    filter(Bundesland==filter_value) %>%
    ggplot(aes(x=Date,y=Positivity,color=Bundesland)) + geom_line() +
    geom_point() +
    theme_economist_white() + 
    labs(title=paste("Positivity Rate in",filter_value),
         x="Date",
         y="Positivity Rate (%)",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
  
  positive_plot0
}
pos_plot01 <- function(retrieved_data){
  
  positive_plot1 <- retrieved_data %>%
    filter(Bundesland!="Austria") %>%
    ggplot(aes(x=Date,y=Positivity,color=Bundesland)) + geom_line() +
    geom_point() +
    theme_economist_white() + 
    labs(title="Positive Rate - Per State",
         x="Date",
         y="Positive Rate (%)",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    facet_wrap(Bundesland ~.)
  
  positive_plot1
  
  
}

load_plot00 <- function(retrieved_data,filter_value){
  
  if(filter_value!=""){
    retrieved_data <- retrieved_data %>%
    filter(Bundesland==filter_value)
  }else{
    retrieved_data <- retrieved_data %>%
      filter(Bundesland=="Austria")
    filter_value=="Austria"
    }
  
  load_plot0 <- retrieved_data %>% select(Date,Hospital_Load,ICU_Load,FZHosp,FZICU) %>%
    pivot_longer(c(-Date,-FZHosp,-FZICU),values_to="value",names_to="Type") %>%
    ggplot(aes(x=Date,y=value,color=Type)) + geom_line() +
    geom_point() +
    theme_economist_white() + 
    labs(title=paste("Hospital Load in",filter_value),
         x="Date",
         y="Load Percentage (%)",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
  
 load_plot0
}

load_plot01 <- function(retrieved_data){
  
  load_plot1 <- retrieved_data %>%  select(Date,Hospital_Load,ICU_Load,FZHosp,FZICU,Bundesland) %>%
    filter(Bundesland!="Austria") %>%
    pivot_longer(c(-Date,-FZHosp,-FZICU,-Bundesland),values_to="value",names_to="Type") %>%
    ggplot(aes(x=Date,y=value,color=Type)) + geom_line() +
    geom_point() +
    theme_economist_white() + 
    labs(title="Hospital Load - Per State",
         x="Date",
         y="Load Percentage (%)",
         caption="Data: https://covid19-dashboard.ages.at/" ) +
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
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    facet_wrap(Bundesland ~.)
  
  load_plot1
}

