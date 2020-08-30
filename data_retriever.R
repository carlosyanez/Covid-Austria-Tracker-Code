
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)

library(lubridate)
library(ggthemes)

setwd("/opt/shinyserver/apps/covid_vienna")
special_flag <-1
file_path<-"/opt/shinyserver/apps/covid_vienna/data/"

if(special_flag==1){
      extract_filename <- c("Bundesland","Bezirke",
                            "AllgemeinDaten","Altersverteilung",
                            "GenesenTodesFaelleBL")

      temp <- "/opt/shinyserver/apps/covid_vienna/data.zip"
      #temp <- "./data.zip"
      download.file("https://info.gesundheitsministerium.at/data/data.zip",temp)
      unzip(temp,paste(extract_filename,".csv",sep=""),exdir=file_path)

      file.rename(paste(file_path,extract_filename,".csv",sep=""),
            paste(file_path,extract_filename,"_",Sys.Date(),".csv",sep=""))
 
     file.rename("./data.zip",
                  paste("./data","_",Sys.Date(),".zip",sep=""))
  
    #  unlink(temp)
      rm(extract_filename)
}

####Retrieve data from csv files.

file_list <- tibble(filename=list.files(path=file_path))

file_active <- file_list[grep("Bundesland",file_list$filename),]

active_data <- read.csv2(paste(file_path,file_active[1,]$filename,sep=""),sep=";")
if(nrow(file_active)>1){
  for (i in 2:nrow(file_active)){
    active_data_i <- read.csv2(paste(file_path,file_active[i,]$filename,sep=""),sep=";")
    active_data <-rbind(active_data,active_data_i)
    
  }
  rm(active_data_i,i,file_active)
}

active_data <-  active_data  %>%
  mutate(Date=as.Date(as.character(substr(Timestamp,1,10),
                                   format = "%y-%m-%d"))) %>%
                  select(Date,Bundesland,Anzahl, GKZ)

active_totals <- active_data %>% select(Date,Anzahl) %>%
                  group_by(Date) %>% 
                  summarise(Anzahl=sum(Anzahl)) %>%
                   ungroup() %>% 
                    mutate(Bundesland="Total",GKZ="0")

active_data <- rbind(active_data,active_totals) %>% 
               select(State=Bundesland,active=Anzahl,Date=Date,GKZ=GKZ) 

rm(active_totals)


file_cumul <- file_list %>% filter(grepl("Bezirk",filename))

cumul_data <- read.csv(paste(file_path,file_cumul[1,]$filename,sep=""),sep=";")
if(nrow(file_cumul)>1){
  for (i in 2:nrow(file_cumul)){
    cumul_data_i <- read.csv(paste(file_path,file_cumul[i,]$filename,sep=""),sep=";")
    cumul_data <-rbind(cumul_data,cumul_data_i)
    
  }
   rm(cumul_data_i,i,file_cumul)
}

cumul_data <-  cumul_data  %>% 
               mutate(Date=as.Date(as.character(substr(Timestamp,1,10),
                                        format = "%y-%m-%d")),
         GKZ=floor(GKZ/100)) %>%
         group_by(Date,GKZ) %>% 
         summarise(Anzahl=sum(Anzahl)) %>% 
         ungroup()

cumul_totals <- cumul_data %>% select(Date,Anzahl) %>%
                group_by(Date) %>% 
                summarise(Anzahl=sum(Anzahl)) %>%
                ungroup() %>% mutate(GKZ="0")

cumul_data <- rbind(cumul_data,cumul_totals) %>% select(cumul=Anzahl,Date=Date,GKZ=GKZ) 
rm(cumul_totals)


file_gt <- file_list %>% filter(grepl("GenesenTodesFaelleBL",filename))

gt_data <- read.csv(paste(file_path,file_gt[1,]$filename,sep=""),sep=";")
if(nrow(file_gt)>1){
  for (i in 2:nrow(file_gt)){
    gt_data_i <- read.csv(paste(file_path,file_gt[i,]$filename,sep=""),sep=";")
    gt_data <-rbind(gt_data,gt_data_i)
    
  }
  rm(gt_data_i,i,file_gt)
}

gt_data <-  gt_data  %>% 
  mutate(Date=as.Date(as.character(substr(Timestamp,1,10),
                                   format = "%y-%m-%d"))) %>%
        select(-Timestamp)

gt_totals <- gt_data %>% select(Date,Genesen,Todesfälle) %>%
  group_by(Date) %>% 
  summarise(Genesen=sum(Genesen),
            Todesfälle=sum(Todesfälle)) %>%
  ungroup() %>%  mutate(Bundesland="Total",GKZ="0")

gt_data <- rbind(gt_data,gt_totals) %>%
            mutate(GnT=Genesen+Todesfälle) %>%
            select(GnT,Genesen,Todesfälle,Date=Date,GKZ=GKZ) 

rm(gt_totals)


file_ag <- file_list %>% filter(grepl("AllgemeinDaten",filename))

ag_data <- read.csv(paste(file_path,file_ag[1,]$filename,sep=""),sep=";")
if(nrow(file_ag)>1){
  for (i in 2:nrow(file_ag)){
    ag_data_i <- read.csv(paste(file_path,file_ag[i,]$filename,sep=""),sep=";")
    ag_data <-rbind(ag_data,ag_data_i)
    
  }
  rm(ag_data_i,i,file_ag)
}

ag_data <-  ag_data  %>% 
  mutate(Date=as.Date(as.character(substr(Timestamp,1,10),
                                   format = "%y-%m-%d"))) %>%
  select(Date,PositivGetestet,GesTestungen)


file_av <- file_list %>% filter(grepl("Altersverteilung",filename))

av_data <- read.csv(paste(file_path,file_av[1,]$filename,sep=""),sep=";")
if(nrow(file_av)>1){
  for (i in 2:nrow(file_av)){
    av_data_i <- read.csv(paste(file_path,file_av[i,]$filename,sep=""),sep=";")
    av_data <-rbind(av_data,av_data_i)
    
  }
  rm(av_data_i,i,file_av)
}

av_data <-  av_data  %>% 
  mutate(Date=as.Date(as.character(substr(Timestamp,1,10),
                                   format = "%y-%m-%d"))) %>%
  pivot_wider(values_from = Anzahl, names_from=Altersgruppe) %>% select(-Timestamp)

rm(file_list)

####Process
retrieved_data <-active_data %>% 
                left_join(cumul_data,by=c("Date","GKZ")) %>%
                left_join(gt_data,by=c("Date","GKZ"))

retrieved_data[is.na(retrieved_data$cumul),]$cumul <-0

dates <- tibble(Date=unique(cumul_data[order(cumul_data$Date),]$Date)) %>%
 mutate(previous_date=data.table::shift(Date)) 

retrieved_data <- retrieved_data %>% left_join(dates,by="Date") %>% 
  left_join((cumul_data  %>% select(previous_date=Date,previous_cumul=cumul,GKZ=GKZ)),
            by=c("previous_date","GKZ")) 

retrieved_data[is.na(retrieved_data$previous_cumul),]$previous_cumul <-0

retrieved_data <- retrieved_data %>% 
  mutate(new_cases=ifelse(is.na(previous_date),0,cumul-previous_cumul)) %>%
  mutate(new_cases=ifelse(new_cases<0,0,new_cases))

rm(active_data,cumul_data,gt_data,dates)

retrieved_data_general <- ag_data %>%
                          left_join(av_data,by="Date")

dates <- tibble(Date=unique(ag_data[order(ag_data$Date),]$Date)) %>%
  mutate(previous_date=data.table::shift(Date)) 


retrieved_data_general <- retrieved_data_general %>% 
  left_join(dates,by="Date") %>%
  left_join(retrieved_data_general %>% 
            mutate(previous_date=Date) %>% select(-Date),
            by="previous_date") %>%
  select(-previous_date)

retrieved_data_general <- retrieved_data_general %>%
  pivot_longer(cols = -Date) %>%
  mutate(name = sub('*.[x,y]', '', name)) 

retrieved_data_general[is.na(retrieved_data_general$value),]$value <-0

retrieved_data_general <- retrieved_data_general %>% group_by(Date, name) %>%
  summarise(diff = -diff(value)) %>% ungroup() %>%
  pivot_wider(names_from = name, values_from = diff) %>%
  mutate(positive_rate=100*PositivGetestet/GesTestungen)


if(special_flag==1){
      covid_austria <-vector(mode = "list", length = 0)

      covid_austria$retrieved_data <- retrieved_data
      covid_austria$retrieved_data_general <- retrieved_data_general
      covid_austria$timestamp <- now()
    saveRDS(covid_austria, file="retrieved_data.rds") 
}

