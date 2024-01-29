library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(here)
library(tibble)
library(tidyverse)
library(readxl)
library(purrr)
library(rqdatatable)
library(lubridate)
library(readxl)
library(readr)
library(hms)

setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/")
#SWN
#pathSWN_2023<-here("A.2 Outfall Assessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SWN_2023_provisional/")
#pathSWN_2022

wd <- 'C:/Users/givens/Box/WQIP-Annual-Report-2022-23/'

inPath <- paste0(wd,'OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SWN_2023_provisional')
outPath <- paste0(wd, '/OutfallAssessments/DryWeatherLoadingCalcs/Output/')


getwd()

data_filespathSWN_2023 = list.files('C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/SWN_2023_provisional/', pattern = "*.csv")
data_filespathSWN_2023

df <- data_filespathSWN_2023 %>%
  map(function(x) {
    read.csv(paste0(inPath, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_SWN_2023 <- df %>%
  filter(!is.na(Flow..cfs.)) %>%
  filter(SourceFile != 'NARCO_LAPAZ.csv') %>%
  filter(SourceFile != 'J06-10011-1.csv') %>%
filter(SourceFile != 'J01-9066-2.csv') %>%
  filter(SourceFile != 'J01-9264-1.csv')

i <- sapply(DailyQ_SWN_2023, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_SWN_2023[i] <- lapply(DailyQ_SWN_2023[i], as.character)

str(DailyQ_SWN_2023)

DailyQ_SWN_2023$Inst.Time = substr(DailyQ_SWN_2023$Inst.Time, 1, nchar(DailyQ_SWN_2023$`Inst.Time`)-4) 

DailyQ_SWN_2023 <- DailyQ_SWN_2023 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))

DailyQ_SWN_2023$Station = sub("./", "", DailyQ_SWN_2023$Station)
DailyQ_SWN_2023$Station = sub(".csv", "", DailyQ_SWN_2023$Station)
DailyQ_SWN_2023$Station = sub("_all", "", DailyQ_SWN_2023$Station)


DailyQ_SWN_2023[!duplicated(DailyQ_SWN_2023[c(1,2)]), ]  

DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9005-1"] <- "J01-9005-1 (J03P05)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9007-1"] <- "J01-9007-1 (J02P05)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9008-1"] <- "J01-9008-1 (J01P30)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9066-1"] <- "J01-9066-1 (J01P04)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9066-2"] <- "J01-9066-2 (J01P03)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9131-1"] <- "J01-9131-1 (J01P28)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9144-1"] <- "J01-9144-1 (J01P23)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9144-4"] <- "J01-9144-1 (J01P26)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9224-1"] <- "J01-9224-1 (J01P24)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9224-2"] <- "J01-9224-2 (J01P25)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9264-1"] <- "J01-9264-1 (J01P06)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9992-1"] <- "J01-9992-1 (J01P27)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-10004-1"] <- "J01-10004-1 (J01P01)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-10017-1"] <- "J01-10017-1 (J01TBN4)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-10019-1"] <- "J01-10019-1 (J01P33)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J03-9368-1"] <- "J03-9368-1 (J03TBN1)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J03-9368-2"] <- "J03-9368-1 (J03TBN2)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J06-9079-1"] <- "J06-9079-1 (J06P03)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J06-10011-1"] <- "J06-10011-1 (J06P01)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J07-9109-4"] <- "J07-9109-4 (J07P02)"

getwd()

values <- list()  
values[['RainInf']] <- 'C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Output/RainInf.rds'
RainInf<-  readRDS(values[["RainInf"]])

library(tidyverse)
library(ggpubr)
library(rstatix)




wetseasonbf_SWN<- DailyQ_SWN_2023_d %>%
  filter(date> '2021-9-30' & date < '2022-4-1') %>%
  group_by(Station) %>%
   mutate(median_wet = median(Flow..cfs.)) %>%
  mutate(average_wet=mean(Flow..cfs.)) %>%
  mutate(sd_wet=sd(Flow..cfs.)) %>%
  select('Station', 'median_wet', 'average_wet', 'sd_wet') %>%
  unique()


#2023

DailyQ_SWN_2023_d <-   left_join(DailyQ_SWN_2023, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 


dryseasonbf_SWN<- DailyQ_SWN_2023_d %>%
  filter(date > '2023-3-30' & date < '2023-10-1') %>%
  group_by(Station) %>%
  mutate(median_dry = median(Flow..cfs.)) %>%
  mutate(average_dry=mean(Flow..cfs.)) %>%
  mutate(sd_dry=sd(Flow..cfs.)) %>%
  select('Station', 'median_dry', 'average_dry', 'sd_dry') %>%
  unique()

allbf_SWN<- DailyQ_SWN_2023_d %>%
  group_by(Station) %>%
  mutate(median_all = median(Flow..cfs.)) %>%
  mutate(average_all=mean(Flow..cfs.)) %>%
  mutate(sd_all=sd(Flow..cfs.)) %>%
  select('Station', 'median_all', 'average_all', 'sd_all') %>%
  unique()
  

write_csv(wetseasonbf_SWN, path = 'outPath')

write_csv(dryseasonbf_SWN, paste0(outPath,  'dryseasonbf_SWN.csv'))

write_csv(allbf_SWN, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/allbf_SWN.csv')) 

wetseasonbf_SWNstat<- DailyQ_SWN_2022_d %>%
  filter(date> '2021-9-30' & date < '2022-4-1') %>%
  select('Station', 'Flow..cfs.')

names(wetseasonbf_SWNstat)[2] <- "Wet_season_cfs"

dryseasonbf_SWNstat<- DailyQ_SWN_2022_d %>%
  filter(date > '2022-3-30' & date < '2022-10-1') %>%
  select('Station', 'Flow..cfs.')

names(dryseasonbf_SWNstat)[2] <- "Dry_season_cfs"

merge(SWNstat, merge(dryseasonbf_SWNstat, wetseasonbf_SWNstat, by="Station", all.x=TRUE, all.y=TRUE), by = "Station", all.x = TRUE, all.y = TRUE)
#  Year Site3 Site1 Site2

