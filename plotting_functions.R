state_colour_scale <- tribble(~Bundesland,~State,~state_colour,
                  "Österreich","Austria","#313695",
                  "Austria","Austria","#313695",
                  "Burgenland","Burgenland","#A50026",
                  "Kärnten","Carinthia","#D73027",
                  "Niederösterreich","Lower Austria","#F46D43",
                  "Oberösterreich","Upper Austria","#FDAE61",
                  "Salzburg","Salzburg","#fed090",
                  "Steiermark","Styria","#d6d65a",
                  "Tirol","Tyrol", "#E0F3F8",
                  "Vorarlberg","Vorarlberg","#ABD9E9",
                  "Wien","Vienna","#74ADD1" ) %>%
             mutate(State_fct=as.factor(State))

beds_colour_scale <- tribble(~Type,~state_colour,
                             "Hospital Load","blue",
                             "ICU Load","purple") %>%
             mutate(State_fct=as.factor(Type))


plot_caption1 <- "Data: Österreichische Agentur für Ernährungssicherheit"
plot_caption2 <- "Data: Österreichische Agentur für Ernährungssicherheit"

font_add_google("Roboto","Roboto")
showtext_auto()

hospital_load_data <- function(retrieved_data,filter_value,grid_value=FALSE){
  
  if(grid_value==FALSE){
    
    data <- retrieved_data %>% filter(State==filter_value)
    
  }else{
    
    data <- retrieved_data %>% filter(!(State==filter_value))
  }
  data %>%
    select(Date,State,Hospital_Load,ICU_Load,FZHosp,FZICU,FZHospFree,FZICUFree) %>%
    pivot_longer(c(-Date,-FZHosp,-FZICU,-FZHospFree,-FZICUFree,-State),values_to="load_value",names_to="Type") %>%
    mutate(State_fct=as.factor(str_replace(Type,"_"," ")))       %>%
    select(Date,State,State_fct,load_value)
}

general_plotter <- function(retrieved_data,chart_type,y_values,filter_value,y_label,group_name,plot_caption,cscale){
  
  plotting_data <- retrieved_data %>% select(Date,State,y_value=matches(y_values),State_fct) %>% 
    filter(!is.na(y_value)) %>%
    mutate(tooltip=str_c("Date: ", str_c(day(Date),month(Date,label=TRUE),year(Date),sep=" "),
                         "\n",
                         group_name,": ",State,
                         "\n",
                         y_label,": ",  format(round(y_value,2), nsmall=2, big.mark=","))) %>% 
    arrange(Date)
  
  
  if((chart_type=="columns" & filter_value=="Austria")|chart_type=="linegrid"){
    plotting_data <- plotting_data %>% filter(!(State  %in% filter_value))
  }else{
    plotting_data <- (plotting_data %>% filter((State %in% filter_value)))
  }
  
  if(chart_type=="linegrid"){
    plot_title <- str_c(y_label," per State ")
    
  }else{
    plot_title <- str_c(y_label," in ", filter_value)
  }
  
  colour_scale <- cscale %>% filter(State_fct %in% unique(plotting_data$State_fct)) %>% arrange(State_fct)

  if(chart_type=="columns"){
    p <- plotting_data %>% ggplot() +
                           geom_col_interactive(aes(x=Date,y=y_value,fill=State_fct,
                                                    tooltip=tooltip,data_id=State))
    type<-1
    
    
  }
  
  if(chart_type=="lines"){
    p <- plotting_data %>% ggplot(aes(x=Date,y=y_value,
                                      colour=State_fct, 
                                      #fill=State_fct,
                                      group=State_fct,
                                      tooltip=tooltip,data_id=State)) +
                            geom_line_interactive() +
                            geom_point_interactive(size=0.6)
    
    type<-2
    
  }  
  
  if(chart_type=="linegrid"){

    p <- plotting_data %>% ggplot(aes(x=Date,y=y_value,
                                      colour=State_fct,
                                      #fill=State_fct,
                                      group=State_fct,
                                      tooltip=tooltip,data_id=State)) +
                           geom_line_interactive() +
                           geom_point_interactive(size=0.6) +
                           facet_wrap(State ~.)
    
    type<-2
    
    
  }
  
  
  p <-  p + theme_economist_white() + 
    labs(title=plot_title,
         x="Date",
         y=y_label,
         caption=plot_caption) +
    theme(legend.position = "right",
          plot.title = element_text(size=16,family="Roboto"),
          axis.title.x = element_text(size = 12,family="Roboto"),
          axis.text.x = element_text(angle = 0, hjust = 1,size = 12,family="Roboto"),
          axis.title.y = element_text(size = 12,family="Roboto"),
          axis.text.y = element_text(size = 12,family="Roboto"),
          strip.text.x = element_text(size = 14,family="Roboto"),
          strip.text.y = element_text(size = 14, angle = 90,family="Roboto"),
          legend.title=element_text(size=10,family="Roboto"),
          legend.text=element_text(size=10,family="Roboto")) +
          scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
  

  
  if(type==1){
    p <- p + scale_fill_manual(group_name, values = (colour_scale %>% select(state_colour) %>% pull(.)) )
  }
  if(type==2){
    p <- p +  scale_colour_manual(group_name, values = (colour_scale %>% select(state_colour) %>% pull(.)) ) 
  }
  
  p 
}
