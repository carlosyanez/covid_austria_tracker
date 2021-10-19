library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(magrittr)
library(lubridate)
library(fs)

dir_create("temp")
#dir_create("retrieved")
file_path <- "temp/"
temp <- "temp/data.zip"
  
extract_filename <- c("CovidFaelle_Timeline",
                      "CovidFallzahlen",
                      "CovidFaelle_Timeline_GKZ",
                      "CovidFaelle_Altersgruppe",
                      "CovidFaelleDelta"
                      )



state_translation <- tribble(~Bundesland,~State,~state_colour,
                             "Österreich","Austria","#313695",
                             "Austria","Austria","#313695",
                             "Burgenland","Burgenland","#A50026",
                             "Kärnten","Carinthia","#D73027",
                             "Niederösterreich","Lower Austria","#F46D43",
                             "Oberösterreich","Upper Austria","#FDAE61",
                             "Salzburg","Salzburg","#fed090",
                             "Steiermark","Styria","#FFFFBF",
                             "Tirol","Tyrol", "#E0F3F8",
                             "Vorarlberg","Vorarlberg","#ABD9E9",
                             "Wien","Vienna","#74ADD1" )

     


download.file("https://covid19-dashboard.ages.at/data/data.zip",temp,method="libcurl")
unzip(temp,paste(extract_filename,".csv",sep=""),exdir=file_path)

data <-list()

for(i in 1:length(extract_filename)){
  
  data[[i]] <- as_tibble(read.csv2(paste("temp/",extract_filename[i],".csv",sep=""),stringsAsFactors=FALSE))
}
names(data) <- extract_filename

state_stats <- data$CovidFaelle_Timeline %>% select(Bundesland,AnzEinwohner) %>% unique(.)

state_stats <- state_translation %>% left_join(state_stats,by="Bundesland")

a <- data$CovidFaelle_Timeline %>% select(-AnzEinwohner,-BundeslandID) %>%
  mutate(Date=dmy(str_sub(Time,1,10)),
         Bundesland=ifelse(Bundesland=="Österreich","Österreich",Bundesland)) %>%
  group_by(Date,Bundesland) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)),.groups = "drop") 

daterange <- seq(min(a$Date), max(a$Date), by="days")

bl <- unique(a$Bundesland)
combinations <- crossing(Date=daterange,Bundesland=bl)


a <- combinations %>% left_join(a, by=c("Date","Bundesland"))

message("2")

c <- data$CovidFallzahlen %>%
  mutate(Date=dmy(Meldedat)) %>% 
  mutate(Bundesland=ifelse(Bundesland=="Alle","Österreich",Bundesland)) %>%
  select(-Meldedat,-MeldeDatum,-BundeslandID) 


d <- combinations %>% filter(Date>=min(c$Date)) %>% 
  left_join(c,by=c("Date","Bundesland")) %>% 
  select(Date,Bundesland,TestGesamt) %>%
  group_by(Bundesland)  %>% arrange(Date) %>% fill(TestGesamt) %>%
  mutate(previous_date=Date-days(1)) 


  
d<- d %>% left_join (d %>% select(-previous_date),
                by=c("previous_date"="Date","Bundesland")) %>%
      mutate(TestGesamt.y=ifelse(is.na(TestGesamt.y),0,TestGesamt.y),
             TestGesamt=TestGesamt.x-TestGesamt.y) %>%
     select(-TestGesamt.y,-TestGesamt.x) %>%
    select(-previous_date) %>%
    mutate(TestGesamt=ifelse(TestGesamt<0 | is.na(TestGesamt),0,TestGesamt)) 
  

d <- d %>% left_join(c %>% mutate(TestGesamtSum=TestGesamt) %>% select(-TestGesamt) , by=c("Date","Bundesland")) %>%
  group_by(Bundesland) %>% arrange(Date) %>% fill(TestGesamtSum) %>% ungroup()


a <- a  %>% left_join(d,by=c("Date","Bundesland")) %>%
 mutate(Positivity=ifelse(!is.na(TestGesamt) & TestGesamt>0,100*AnzahlFaelle/TestGesamt,0),
        Positivity=ifelse(Positivity>100,mean(Positivity),Positivity),
        Active = AnzahlFaelleSum-(AnzahlTotSum+AnzahlGeheiltSum),
        Hospital_Load=100*(FZHosp)/(FZHosp+FZHospFree),
        ICU_Load=100*(FZICU)/(FZICU+FZICUFree)) %>%
  left_join(state_stats,by="Bundesland")%>%
   select(-Bundesland)

a<- a%>%   group_by(State)  %>%
  arrange(Date) %>%
  fill(TestGesamtSum,Hospital_Load,ICU_Load,Positivity) %>%
  ungroup()


sevendays1 <- a %>% select(State,Date,Active7=Active) %>% mutate(Date=Date+ddays(7))

a<- a %>% left_join(sevendays1 , by=c("State","Date")) %>%
  mutate(sevenday_net=Active-Active7) %>%
  mutate(sevenday_netpc = 10^5*sevenday_net/AnzEinwohner,
             sevenday_newpc = 10^5*AnzahlFaelle7Tage/AnzEinwohner) %>%
  select(Date,State,state_colour,
         Population=AnzEinwohner,
         CasesDaily=AnzahlFaelle,
         CasesCumulative=AnzahlFaelleSum,
         Cases7Days=AnzahlFaelle7Tage,
         DeceasedDaily=AnzahlTotTaeglich,
         DeceasedCumulative=AnzahlTotSum,
         RecoveredDaily=AnzahlGeheiltTaeglich,
         RecoveredCumulative=AnzahlGeheiltSum,
         TestedDaily=TestGesamt,
         TestedCumulative=TestGesamtSum,
         FZICU,FZHosp,FZICUFree,FZHospFree,
         Positivity,
         ActiveCases=Active,
         Active7,
         Hospital_Load,ICU_Load,
         sevenday_net,sevenday_netpc,sevenday_newpc,
         SevenDayIncidence=SiebenTageInzidenzFaelle
         )


#### Download R factor data

r_factor <-read_csv2("https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff_bundesland.csv",
                     col_types="ccccc")

  
r_at <- read_csv2("https://www.ages.at/fileadmin/AGES2015/Wissen-Aktuell/COVID19/R_eff.csv",col_types="cccc")
r_at$Bundesland <- "Österreich"

r_factor_all <- rbind(r_factor,r_at) %>%
  mutate(Date=parse_date(Datum),R_eff=as.numeric(sub(",", ".", R_eff, fixed = TRUE))) %>% 
  select(Date,Bundesland,R_eff) %>%
  left_join(state_translation, by="Bundesland") %>% select(-Bundesland,-state_colour)

a <- a %>% left_join(r_factor_all,by=c("State","Date"))

covid_austria <-vector(mode = "list", length = 0)

covid_austria$retrieved_data <- a 
covid_austria$first_date <- min(a$Date)
covid_austria$last_date <- max(a$Date)

saveRDS(covid_austria, file="retrieved_data.rds") 
message("complete")
