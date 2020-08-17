
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
# find wd and set here
file_path<-"./data/"

if(special_flag==1){
      extract_filename <- c("Bundesland","Bezirke")

      temp <- "./data.zip"

      download.file("https://info.gesundheitsministerium.at/data/data.zip","temp")
      unzip(temp,paste(extract_filename,".csv",sep=""),exdir=file_path)

      file.rename(paste(file_path,extract_filename,".csv",sep=""),
            paste(file_path,extract_filename,"_",Sys.Date(),".csv",sep=""))
 
     file.rename("./data.zip",
                  paste("./data","_",Sys.Date(),".zip",sep=""))
  
    #  unlink(temp)
      rm(extract_filename)
}



file_list <- tibble(filename=list.files(path=file_path))

file_active <- file_list[grep("Bundesland",file_list$filename),]

active_data <- read.csv2(paste(file_path,file_active[1,]$filename,sep=""),sep=";")
if(nrow(file_active)>1){
  for (i in 2:nrow(file_active)){
    active_data_i <- read.csv2(paste(file_path,file_active[i,]$filename,sep=""),sep=";")
    active_data <-rbind(active_data,active_data_i)
    
  }
 # rm(active_data_i,file_list,file_path,i)
}

active_data <-  active_data  %>%
  mutate(date=as.Date(as.character(substr(Timestamp,1,10),
                                   format = "%y-%m-%d"))) %>%
                  select(date,Bundesland,Anzahl, GKZ)

active_totals <- active_data %>% select(date,Anzahl) %>% group_by(date) %>% summarise(Anzahl=sum(Anzahl)) %>%
  ungroup() %>% mutate(Bundesland="Total",GKZ="0")

active_data <- rbind(active_data,active_totals) %>% select(State=Bundesland,active=Anzahl,Date=date,GKZ=GKZ) 
rm(active_totals)


file_cumul <- file_list %>% filter(grepl("Bezirk",filename))

cumul_data <- read.csv(paste(file_path,file_cumul[1,]$filename,sep=""),sep=";")
if(nrow(file_cumul)>1){
  for (i in 2:nrow(file_cumul)){
    cumul_data_i <- read.csv(paste(file_path,file_cumul[i,]$filename,sep=""),sep=";")
    cumul_data <-rbind(cumul_data,cumul_data_i)
    
  }
  # rm(cumul_data_i,file_list,file_path,i)
}

cumul_data <-  cumul_data  %>% 
  mutate(date=as.Date(as.character(substr(Timestamp,1,10),
                                        format = "%y-%m-%d")),
         GKZ=floor(GKZ/100)) %>%
  group_by(date,GKZ) %>% summarise(Anzahl=sum(Anzahl)) %>% ungroup()

cumul_totals <- cumul_data %>% select(date,Anzahl) %>% group_by(date) %>% summarise(Anzahl=sum(Anzahl)) %>%
  ungroup() %>% mutate(GKZ="0")

cumul_data <- rbind(cumul_data,cumul_totals) %>% select(cumul=Anzahl,Date=date,GKZ=GKZ) 
rm(cumul_totals)

retrieved_data <-active_data %>%  left_join(cumul_data,by=c("Date","GKZ"))
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

if(special_flag==1){
      covid_austria <-vector(mode = "list", length = 0)

      covid_austria$retrieved_data <- retrieved_data
      covid_austria$timestamp <- now()
    saveRDS(covid_austria, file="retrieved_data.rds") 
}

system("ls")
