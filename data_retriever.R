library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(magrittr)
library(lubridate)

#setwd("/opt/shinyserver/apps/covid_vienna")
#file_path<-"/opt/shinyserver/apps/covid_vienna/"
file_path <-"./"

extract_filename <- c("CovidFaelle_Timeline",
                      "CovidFallzahlen",
                      "CovidFaelle_Timeline_GKZ",
                      "CovidFaelle_Altersgruppe",
                      "CovidFaelleDelta"
                      )



#brewer.pal(11,"RdYlBu")
state_translation <- tribble(~Bundesland,~State,~state_colour,
                             "Österreich","Austria","#313695",
                             "Austria","Austria","#313695",
                             "Burgenland","Burgenland","#A50026",
                             "Kärnten","Carinthia","#D73027",
                             "Niederösterreich","Lower Austria","#F46D43",
                             "Oberösterreich","Upper Austria","#FDAE61",
                             "Salzburg","Salzburg","#FEE090",
                             "Steiermark","Styria","#FFFFBF",
                             "Tirol","Tyrol", "#E0F3F8",
                             "Vorarlberg","Vorarlberg","#ABD9E9",
                             "Wien","Vienna","#74ADD1" )

     

#temp <- "/opt/shinyserver/apps/covid_vienna/data.zip"
temp <- "./data.zip"
download.file("https://covid19-dashboard.ages.at/data/data.zip",temp)
unzip(temp,paste(extract_filename,".csv",sep=""),exdir=file_path)
file.rename(temp,
            paste("./data","_",Sys.Date(),".zip",sep=""))

data <-list()

for(i in 1:length(extract_filename)){
  
  data[[i]] <- as_tibble(read.csv2(paste(extract_filename[i],".csv",sep=""),stringsAsFactors=FALSE))
}
names(data) <- extract_filename

state_stats <- data$CovidFaelle_Timeline %>% select(Bundesland,AnzEinwohner) %>% unique(.)

state_stats <- state_translation %>% left_join(state_stats,by="Bundesland")

a <- data$CovidFaelle_Timeline %>% select(-AnzEinwohner,-BundeslandID) %>%
  mutate(Date=dmy(str_sub(Time,1,10)),
         Bundesland=ifelse(Bundesland=="Österreich","Österreich",Bundesland)) %>%
  group_by(Date,Bundesland) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
  ungroup()

message("2")
c <- data$CovidFallzahlen %>%
  mutate(Date=dmy(Meldedat)) %>% 
  mutate(Bundesland=ifelse(Bundesland=="Alle","Österreich",Bundesland)) %>%
  select(-Meldedat,-MeldeDatum,-BundeslandID) 

d <- c %>% select(Date,Bundesland,TestGesamt) %>%
  mutate(previous_date=Date-days(1)) 
  
d<- d %>% left_join (d %>% select(-previous_date),
                by=c("previous_date"="Date","Bundesland")) %>%
      mutate(TestGesamt.y=ifelse(is.na(TestGesamt.y),0,TestGesamt.y),
             TestGesamt=TestGesamt.x-TestGesamt.y) %>%
     select(-TestGesamt.y,-TestGesamt.x) %>%
    select(-previous_date) %>%
    mutate(TestGesamt=ifelse(TestGesamt<0 | is.na(TestGesamt),0,TestGesamt))

c <- c %>% mutate(TestGesamtSum=TestGesamt) %>%
     select(-TestGesamt) %>% left_join(d,by=c("Date","Bundesland"))


a <- a  %>% left_join(c,by=c("Date","Bundesland")) %>%
 mutate(Positivity=ifelse(!is.na(TestGesamt) & TestGesamt>0,100*AnzahlFaelle/TestGesamt,0),
        Positivity=ifelse(Positivity>100,mean(Positivity),Positivity),
        Active = AnzahlFaelleSum-(AnzahlTotSum+AnzahlGeheiltSum),
        Hospital_Load=100*(FZHosp)/(FZHosp+FZHospFree),
        ICU_Load=100*(FZICU)/(FZICU+FZICUFree)) %>%
  left_join(state_stats,by="Bundesland")%>%
   select(-Bundesland)

a<- a%>% replace(is.na(.) , 0)

sevendays1 <- a %>% select(State,Date,Active7=Active) %>% mutate(Date=Date+ddays(7))

a<- a %>% left_join(sevendays1 , by=c("State","Date")) %>%
  mutate(sevenday_net=Active-Active7) %>%
  mutate(sevenday_netpc = 10^5*sevenday_net/AnzEinwohner,
             sevenday_newpc = 10^5*AnzahlFaelle7Tage/AnzEinwohner)



covid_austria <-vector(mode = "list", length = 0)

covid_austria$retrieved_data <- a 
covid_austria$first_date <- min(a$Date)
covid_austria$last_date <- max(a$Date)
covid_austria$updated <-  e[1,]$VersionsDate

saveRDS(covid_austria, file="retrieved_data.rds") 
message("complete")
