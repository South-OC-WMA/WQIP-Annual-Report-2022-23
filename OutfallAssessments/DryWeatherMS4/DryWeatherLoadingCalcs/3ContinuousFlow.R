source('DryWeatherLoadingCalcs/1projectsetup.R')
inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/ProvisionalFlow/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

getwd()

## Task 3: Import flow data
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Flow Data Analysis
#S:\Environmental Res\Environmental Studies Unit\Projects\WQIP\data\Hach flow data
#https://app.box.com/s/in2b5wsc0mugf2ifboibf4l7r68voree
#


#Continuous flow data from 2018-20
#For calcs presented in 2019-20 Annual data report, three folders (2018 prioritization, 2019-20 OCFS, and 2020 Coto de Caza)

#use for loop to iterate analysis over a list of dataframes, then combine list of data frames
#https://datacarpentry.org/semester-biology/materials/for-loops-R/
# https://stackoverflow.com/questions/33203800/unzip-a-zip-file

#TM flow data


path<-paste0(inPath, 'Detailed_Flow_Monitoring_Data_Summer_Dry_Clean')
path

data_files = list.files(path=path, pattern = "*.csv")
data_files

df <- data_files %>%
  map(function(x) {
    read.csv(paste0(path, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

DailyQ_TM <- df %>%
  filter(!is.na(Flow_cfs)) 

i <- sapply(DailyQ_TM, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_TM[i] <- lapply(DailyQ_TM[i], as.character)

str(DailyQ_TM)

#DailyQ_2018p$Inst.Time = substr(DailyQ_2018p$Inst.Time, 1, nchar(DailyQ_2018p$`Inst.Time`)-4) 

DailyQ_TM <- DailyQ_TM %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Datetime, format = '%Y-%m-%d')) %>%
  select('Station', 'date', 'Flow_cfs') %>% #find mean daily discharge
  group_by(Station, date) %>% 
  mutate(Flow_cfs_md=mean(Flow_cfs))

DailyQ_TM$Station = sub("./", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_Dry_Summer_Clean.csv", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_Dry_Clean.csv", "", DailyQ_TM$Station)
DailyQ_TM$Station = sub("_all", "", DailyQ_TM$Station)

DailyQ_TM$Station<-sub("K01-12032-2", "K01-12032-2 (K01P11)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-242-1", "L01-242-1 (L07P16)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-724-1", "L01-724-1 (L01S01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-727-1", "L01-727-1 (L01S04)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L01-728-7", "L01-728-7 (L01S03)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-166-3", "L02-166-3 (L02P26)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-246-1", "L02-246-1 (L11P01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-374-1", "L02-374-1 (L02P50)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-622-2", "L02-622-2 (L02P32)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L02-640-1", "L02-640-1 (L11P02)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-662-3", "L03-662-3 (L03P16)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-691-1", "L03-691-1 (L03P09)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("L04-136-1u", "L04-136-1u (L04P07)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M00.1-071-3", "M00.1-071-3 (M00S04)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M01-042-1", "M01-042-1 (M01S01)",DailyQ_TM$Station)
DailyQ_TM$Station<-sub("M02-085-1", "M02-085-1 (M02P06)",DailyQ_TM$Station)

DailyQ_TM <- DailyQ_TM %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique() %>%
  filter(Station != "L03-708-11 (L03P05)"|Station !="L02-166-3 (L02P26)"|Station !="L02-246-1 (L11P01)"|Station != "L02-374-1 (L02P50)"|Station != "L04-136-1u (L04P07)")

saveRDS(DailyQ_TM, file = paste(outPath, 'DailyQ_TM.rds')) 
write_csv(DailyQ_TM, path = paste(outPath, 'DailyQ_TM.csv'))

##2018 Reprioritization

getwd()

path<-paste0(inPath, 'Folder_1_from_2018_Outfall_Reprioritization')
path

data_files = list.files(path=path, pattern = "*.csv")
data_files

df <- data_files %>%
  map(function(x) {
    read.csv(paste0(path, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_2018R <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_2018R, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_2018R[i] <- lapply(DailyQ_2018R[i], as.character)

str(DailyQ_2018R)

#DailyQ_2018p$Inst.Time = substr(DailyQ_2018p$Inst.Time, 1, nchar(DailyQ_2018p$`Inst.Time`)-4) 

DailyQ_2018R <- DailyQ_2018R %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()

DailyQ_2018R$Station = sub("./", "", DailyQ_2018R$Station)
DailyQ_2018R$Station = sub(".csv", "", DailyQ_2018R$Station)
DailyQ_2018R$Station = sub("_all", "", DailyQ_2018R$Station)


DailyQ_2018R$Station<-sub("J01-9008-1", "J01-9008-1 (J01P30)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("J01-9131-1", "J01-9131-1 (J01P28)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("J01-9992-1", "J01-9992-1 (J01P27)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("K01-12138-1", "K01-12138-1 (K01TBN1)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L01-724-1", "L01-724-1 (L01S01)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L01-727-1", "L01-727-1 (L01S04)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L02-622-2", "L02-622-2 (L02P32)",DailyQ_2018R$Station)
DailyQ_2018R$Station<-sub("L03-693-1", "L03-693-1 (L03P11)",DailyQ_2018R$Station)


DailyQ_2018R <- DailyQ_2018R %>%
  #filter(DailyQ_201920Cont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') %>%
  filter(Station != 'J01-9008-1 (J01P30)'|Station != 'J01-9131-1 (J01P28)'|Station != 'J01-9992-1 (J01P27)'|Station != 'L03-693-1 (L03P11)')


DailyQ_2018R[!duplicated(DailyQ_2018R[c(1,2)]), ]   

str(DailyQ_2018R)

saveRDS(DailyQ_2018R, paste0(outPath, 'DailyQ_2018R.rds')) 
write_csv(DailyQ_2018R, paste0(outPath, 'DailyQ_2018R.csv'))


##2019-20 OFP##
library(tidyverse)
library(here)
path<-here(paste0(inPath, 'Folder_2_from_2020_Outfall_Flow_Plots'))
path

data_files = list.files(path=path, pattern = "*.csv")
data_files

df <- data_files %>%
  map(function(x) {
    read.csv(paste0(path, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_2020OFP <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_2020OFP, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_2020OFP[i] <- lapply(DailyQ_2020OFP[i], as.character)

str(DailyQ_2020OFP)

DailyQ_2020OFP$Inst.Time = substr(DailyQ_2020OFP$Inst.Time, 1, nchar(DailyQ_2020OFP$`Inst.Time`)-4) 

DailyQ_2020OFP <- DailyQ_2020OFP %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()

DailyQ_2020OFP$Station = sub("./", "", DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station = sub(".csv", "", DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station = sub("_all", "", DailyQ_2020OFP$Station)


DailyQ_2020OFP$Station<-sub("J01-9066-2", "J01-9066-2 (J01P03)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J01-9131-1", "J01-9131-1 (J01P28)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J01-9992-1", "J01-9992-1(J01P27)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J03-9221-1", "J03-9221-1 (J03P02)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J06-9079-1", "J06-9079-1 (J06P03)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("J06-10011-1", "J06-10011-1 (J06P01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("K01-12126-1", "K01-12126-1 (K01S01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L01-728-3", "L01-728-3 (L01S02)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L01-766-2", "L01-766-2 (L01S06)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-214-2", "L03-214-2 (L03P18)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-316-3", "L03-316-3 (L03P12)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("L03-074-1", "L03-074-1 (L03B01)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("M01-050-4", "M01-050-4 (M01@CGV)",DailyQ_2020OFP$Station)
DailyQ_2020OFP$Station<-sub("K01-12177-1", "K01-12177-1 (K01P07)",DailyQ_2020OFP$Station)


DailyQ_2020OFP <- DailyQ_2020OFP %>%
  #select(-SourceFile) %>%
  #filter(DailyQ_2020OFPCont > 4) %>% #filter away days from when flow meter not working correctly for J01P27 and J01P28
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-06') %>%
  filter(Station != 'J01-9992-1 (J01P27)'|date != '2018-04-08') %>%
  filter(Station != 'J01-9992-1 (J01P27)'| Station != 'J01-9066-2 (J01P03)'| Station != 'J01-9131-1 (J01P28)'| Station != 'J03-9221-1 (J03P02)'| Station != 'J06-10011-1 (J06P01)'|Station != 'J06-9079-1 (J06P03)')

DailyQ_2020OFP[!duplicated(DailyQ_2020OFP[c(1,2)]), ]   

str(DailyQ_2020OFP)
saveRDS(DailyQ_2020OFP, paste0(outPath, 'DailyQ_2020OFP.rds')) 
write_csv(DailyQ_2020OFP, paste0(outPath, 'DailyQ_2020OFP.csv'))


#Continuous flow data OCFS, 2021
pathOCFS_2021<-paste0(inPath, 'OCFS_2021_provisional')
pathOCFS_2021

data_filespathOCFS_2021 = list.files(path=pathOCFS_2021, pattern = "*.csv")
data_filespathOCFS_2021

df <- data_filespathOCFS_2021 %>%
  map(function(x) {
    read.csv(paste0(pathOCFS_2021, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_OCFS_2021 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_OCFS_2021, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_OCFS_2021[i] <- lapply(DailyQ_OCFS_2021[i], as.character)

str(DailyQ_OCFS_2021)

DailyQ_OCFS_2021$Inst.Time = substr(DailyQ_OCFS_2021$Inst.Time, 1, nchar(DailyQ_OCFS_2021$`Inst.Time`)-5) 

DailyQ_OCFS_2021 <- DailyQ_OCFS_2021 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y'))  %>%
  group_by(Station, date) %>% #find mean daily discharge
  mutate(Flow_cfs_md=mean(Flow..cfs.)) %>%
  select('Station', 'date', 'Flow_cfs_md') %>%
  unique()
  

DailyQ_OCFS_2021$Station = sub("./", "", DailyQ_OCFS_2021$Station)
DailyQ_OCFS_2021$Station = sub(".csv", "", DailyQ_OCFS_2021$Station)



DailyQ_OCFS_2021[!duplicated(DailyQ_OCFS_2021[c(1,2)]), ]   

str(DailyQ_OCFS_2021)
saveRDS(DailyQ_OCFS_2021, file = paste0(outPath, 'DailyQ_OCFS_2021.rds')) 
write_csv(DailyQ_OCFS_2021, path = paste0(outPath, 'DailyQ_OCFS_2021.csv')) 

#SWN (mean daily dishcarge from Hydstra)
pathSWN_2023<-paste0(inPath, 'SWN_2023_provisional')
pathSWN_2023

data_filespathSWN_2023 = list.files(path=pathSWN_2023, pattern = "*.csv")
data_filespathSWN_2023

df <- data_filespathSWN_2023 %>%
  map(function(x) {
    read.csv(paste0(pathSWN_2023, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_SWN_2023 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_SWN_2023, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_SWN_2023[i] <- lapply(DailyQ_SWN_2023[i], as.character)

str(DailyQ_SWN_2023)

DailyQ_SWN_2023$Inst.Time = substr(DailyQ_SWN_2023$Inst.Time, 1, nchar(DailyQ_SWN_2023$`Inst.Time`)-4) 

DailyQ_SWN_2023 <- DailyQ_SWN_2023 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
  select('Station', 'date', 'Flow..cfs.')

DailyQ_SWN_2023$Station = sub("./", "", DailyQ_SWN_2023$Station)
DailyQ_SWN_2023$Station = sub(".csv", "", DailyQ_SWN_2023$Station)


DailyQ_SWN_2023[!duplicated(DailyQ_SWN_2023[c(1,2)]), ]   

DailyQ_SWN_2023 <- rename(DailyQ_SWN_2023, Flow_cfs_md = Flow..cfs.)

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
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J03-9368-2"] <- "J03-9368-2 (J03TBN2)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J06-9079-1"] <- "J06-9079-1 (J06P03)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J06-10011-1"] <- "J06-10011-1 (J06P01)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J07-9109-4"] <- "J07-9109-4 (J07P02)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J03-9221-1"] <- "J03-9221-1 (J03P02)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J03-9216-1"] <- "J03-9216-1 (J03P01)"
DailyQ_SWN_2023$Station[DailyQ_SWN_2023$Station == "J01-9364-3"] <- "J01-9364-3 (J01P21)"


#Don't use NARCO_LAPAZ, J06-10011-1, J01-9264-1, J01-9066-2?
DailyQ_SWN_2023 <- DailyQ_SWN_2023 %>%
  filter(Station !="NARCO_LAPAZ" & Station !="J01-9264-1 (J01P06)" & Station !="J01-9066-2 (J01P03)" & Station !="J06-10011-1 (J06P01)" & Station != "J01-10017-1 (J01TBN4)" & Station != "J01-9005-3" & Station != "J03-9216-1 (J03P01)")
         
         

str(DailyQ_SWN_2023)
saveRDS(DailyQ_SWN_2023, paste0(outPath, 'DailyQ_SWN_2023.rds')) 
write_csv(DailyQ_SWN_2023, paste0(outPath, 'DailyQ_SWN_2023.csv')) 


#DFM with Hach Flow meters, 2023 (for OCFS)
pathDFM_2023<-paste0(inPath, 'HachFL900_2023_provisional')
pathDFM_2023

data_filespathDFM_2023 = list.files(path=pathDFM_2023, pattern = "*.csv")
data_filespathDFM_2023

df <- data_filespathDFM_2023 %>%
  map(function(x) {
    read.csv(paste0(pathDFM_2023, "/", x)) %>%
      mutate(SourceFile = x)
  }
  ) %>%
  reduce(rbind)

df

DailyQ_DFM_2023 <- df %>%
  filter(!is.na(Flow..cfs.)) 

i <- sapply(DailyQ_DFM_2023, is.factor)  #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
DailyQ_DFM_2023[i] <- lapply(DailyQ_DFM_2023[i], as.character)

str(DailyQ_DFM_2023)

DailyQ_DFM_2023$Inst.Time = substr(DailyQ_DFM_2023$Inst.Time, 1, nchar(DailyQ_DFM_2023$`Inst.Time`)-4) 

DailyQ_DFM_2023 <- DailyQ_DFM_2023 %>%
  mutate(Station = SourceFile) %>%
  mutate(date = as.Date(Inst.Time, format = '%m/%d/%Y')) %>%
select('Station', 'date', 'Flow..cfs.')

DailyQ_DFM_2023$Station = sub("./", "", DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station = sub(".csv", "", DailyQ_DFM_2023$Station)

DailyQ_DFM_2023[!duplicated(DailyQ_DFM_2023[c(1,2)]), ] 

DailyQ_DFM_2023 <- rename(DailyQ_DFM_2023, Flow_cfs_md = Flow..cfs.)

DailyQ_DFM_2023$Station<-sub("K01-12058-1", "K01-12058-1 (K01P08)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("K01-12058-2", "K01-12058-1 (K01P09)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("J01-9224-2", "J01-9224-2 (J01P25)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("L02-246-1", "L02-246-1 (L11P01)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("L02-374-1", "L02-374-1 (L02P50)",DailyQ_DFM_2023$Station)
DailyQ_DFM_2023$Station<-sub("L02-374-1", "L02-374-1 (L02P50)",DailyQ_DFM_2023$Station)




str(DailyQ_DFM_2023)
saveRDS(DailyQ_DFM_2023, paste0(outPath, 'DailyQ_DFM_2023.rds')) 
write_csv(DailyQ_DFM_2023, paste0(outPath, 'DailyQ_DFM_2023.csv')) 


##Filter out wet weather from continuous flow data
#join to station and rain gauge#

path<-paste0(outPath)
data_files = list.files(path=path, pattern = "*.rds")
data_files

values <- list()  
values[['RainInf']] <- paste0(outPath, 'RainInf.rds')

RainInf<-  readRDS(values[["RainInf"]]) %>%
  select('Date', 'FACILITYID', 'wet_within_72')

str(RainInf)

#R put a space for some reason in " DailyQ_TM.rds"
values <- list()  
values[[' DailyQ_TM']] <- paste0(outPath, ' DailyQ_TM.rds')
DailyQ_TM <-  readRDS(values[[" DailyQ_TM"]])     

str(DailyQ_TM)

DailyQ_TM_d <-   left_join(DailyQ_TM, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_TM_d, file = paste0(outPath, 'DailyQ_TM_d.rds')) 
write_csv(DailyQ_TM_d, path = paste0(outPath, 'DailyQ_TM_d.csv'))  


values <- list()  
values[['DailyQ_2018R']] <- paste0(outPath, 'DailyQ_2018R.rds')
DailyQ_2018R <-  readRDS(values[["DailyQ_2018R"]])     

DailyQ_2018R_d <-   left_join(DailyQ_2018R, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_2018R_d, paste0(outPath, 'DailyQ_2018R_d.rds')) 
write_csv(DailyQ_2018R_d, paste0(outPath, 'DailyQ_2018R_d.csv'))     


values <- list()  
values[['DailyQ_2020OFP']] <- paste0(outPath, 'DailyQ_2020OFP.rds')
DailyQ_2020OFP <-  readRDS(values[["DailyQ_2020OFP"]])     

DailyQ_2020OFP_d <-   left_join(DailyQ_2020OFP, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE)

saveRDS(DailyQ_2020OFP_d, paste0(outPath, 'DailyQ_2020OFP_d.rds')) 
write_csv(DailyQ_2020OFP_d, paste0(outPath,'DailyQ_2020OFP_d.csv'))     

values <- list()  
values[['DailyQ_OCFS_2021']] <- paste0(outPath, 'DailyQ_OCFS_2021.rds')
DailyQ_OCFS_2021 <-  readRDS(values[["DailyQ_OCFS_2021"]]) 
DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L03-693-1"] <- "L03-693-1 (L03P11)"
DailyQ_OCFS_2021$Station[DailyQ_OCFS_2021$Station == "L02-166-3"] <- "L02-166-3 (L02P26)"

values <- list()  
values[['DailyQ_SWN_2023']] <- paste0(outPath, 'DailyQ_SWN_2023.rds')
DailyQ_SWN_2023 <-  readRDS(values[["DailyQ_SWN_2023"]])


DailyQ_OCFS_2021_d <-   left_join(DailyQ_OCFS_2021, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 

saveRDS(DailyQ_OCFS_2021_d, file = paste0(outPath, 'DailyQ_OCFS_2021_d.rds')) 
write_csv(DailyQ_OCFS_2021_d, path = paste0(outPath, 'DailyQ_OCFS_2021_d.csv'))

DailyQ_SWN_2023_d <-   left_join(DailyQ_SWN_2023, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 

saveRDS(DailyQ_SWN_2023_d, paste0(outPath, 'DailyQ_SWN_2023_d.rds')) 
write_csv(DailyQ_SWN_2023_d, paste0(outPath, 'DailyQ_SWN_2023_d.csv'))


values <- list()  
values[['DailyQ_DFM_2023']] <- paste0(outPath, 'DailyQ_DFM_2023.rds')
DailyQ_DFM_2023 <-  readRDS(values[["DailyQ_DFM_2023"]])



DailyQ_DFM_2023_d <-   left_join(DailyQ_DFM_2023, RainInf, by = c('date'='Date', 'Station'='FACILITYID')) %>%   #dry days only, each flow measurment
  filter(wet_within_72==FALSE) 


saveRDS(DailyQ_DFM_2023_d, file = paste0(outPath, 'DailyQ_DFM_2023_d.rds')) 
write_csv(DailyQ_DFM_2023_d, path = paste0(outPath, 'DailyQ_DFM_2023_d.csv'))

inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/ProvisionalFlow/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

values <- list() 
values[['DailyQ_TM_d']] <- paste0(outPath, 'DailyQ_TM_d.rds')
DailyQ_TM_d <-  readRDS(values[["DailyQ_TM_d"]])

values <- list() 
values[['DailyQ_2018R_d']] <- paste0(outPath, 'DailyQ_2018R_d.rds')
DailyQ_2018R_d <-  readRDS(values[["DailyQ_2018R_d"]])

values <- list()
values[['DailyQ_2020OFP_d']] <- paste0(outPath, 'DailyQ_2020OFP_d.rds')
DailyQ_2020OFP_d <-  readRDS(values[["DailyQ_2020OFP_d"]])

values[['DailyQ_OCFS_2021_d']] <- paste0(outPath, 'DailyQ_OCFS_2021_d.rds')
DailyQ_OCFS_2021_d <-  readRDS(values[["DailyQ_OCFS_2021_d"]])

values[['DailyQ_SWN_2023_d']] <- paste0(outPath, 'DailyQ_SWN_2023_d.rds')
DailyQ_SWN_2023_d <-  readRDS(values[["DailyQ_SWN_2023_d"]]) 

values[['DailyQ_DFM_2023_d']] <- paste0(outPath, 'DailyQ_DFM_2023_d.rds')
DailyQ_DFM_2023_d <-  readRDS(values[["DailyQ_DFM_2023_d"]]) 


#2015-16
AnnualQ201516TM <- DailyQ_TM_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()


AnnualQ201516R <- DailyQ_2018R_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2015-09-30" & date < "2016-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2015-16') %>%
  ungroup()

AnnualQ201516<-full_join(AnnualQ201516TM, AnnualQ201516R,by=c('Station','MY')) %>%
  full_join(.,AnnualQ201516OFP, by=c('Station','MY'))

#2016-17
AnnualQ201617TM <- DailyQ_TM_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()


AnnualQ201617R <- DailyQ_2018R_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2016-09-30" & date < "2017-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md))  %>%
  mutate(MY = 'MY2016-17') %>%
  ungroup()

AnnualQ201617<-full_join(AnnualQ201617TM, AnnualQ201617R, by=c('Station','MY')) %>%
  full_join(.,AnnualQ201617OFP, by=c('Station','MY'))

#2017-18
AnnualQ201718TM <- DailyQ_TM_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTMcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718R <- DailyQ_2018R_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2017-09-30" & date < "2018-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2017-18') %>%
  ungroup()

AnnualQ201718<-full_join(AnnualQ201718TM, AnnualQ201718R, by=c('Station','MY')) %>%
  full_join(., AnnualQ201718OFP, by=c('Station','MY'))

#2018-19

AnnualQ201819TM <- DailyQ_TM_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianTM = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819R <- DailyQ_2018R_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianRcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2018-09-30" & date < "2019-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2018-19') %>%
  ungroup()

AnnualQ201819<-full_join(AnnualQ201819TM, AnnualQ201819R, by=c('Station','MY')) %>%
  full_join(.,AnnualQ201819OFP, by=c('Station','MY'))

#2019-20

AnnualQ201920OFP <- DailyQ_2020OFP_d %>%  
  filter(date > "2019-09-30" & date < "2020-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOFPcfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2019-20') %>%
  ungroup()

AnnualQ201920<-AnnualQ201920OFP

#2020-21

AnnualQ20202021O <- DailyQ_OCFS_2021_d %>%  
  filter(date > "2020-09-30" & date < "2021-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianOCFS2021cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2020-21') %>%
  ungroup()

AnnualQ202021<-AnnualQ20202021O

#2021-22

#AnnualQ20212022S <- DailyQ_SWN_2022_d %>%  
#filter(date > "2021-09-30" & date < "2022-10-01") %>%
#group_by(Station) %>%
#as_tibble() %>%
#group_by(Station) %>%
#summarise(medianSWN2022cfs = median(Flow_cfs_md)) %>%
#mutate(MY = 'MY2021-22') %>%
#ungroup()

#AnnualQ20212022dfm <- DailyQ_DFM_2022_d %>%  
#filter(date > "2021-09-30" & date < "2022-10-01") %>%
#group_by(Station) %>%
#as_tibble() %>%
#group_by(Station) %>%
#summarise(medianDFM2022cfs = median(Flow_cfs_md)) %>%
#mutate(MY = 'MY2021-22') %>%
#ungroup()


#AnnualQ202122 <-full_join(AnnualQ20212022dfm,AnnualQ20212022S, by=c('Station','MY'))

#2022-23

AnnualQ20222023S <- DailyQ_SWN_2023_d %>%  
  filter(date > "2022-09-30" & date < "2023-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianSWN2023cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2022-23') %>%
  ungroup()

AnnualQ20222023dfm <- DailyQ_DFM_2023_d %>%  
  filter(date > "2022-09-30" & date < "2023-10-01") %>%
  group_by(Station) %>%
  as_tibble() %>%
  group_by(Station) %>%
  summarise(medianDFM2023cfs = median(Flow_cfs_md)) %>%
  mutate(MY = 'MY2022-23') %>%
  ungroup()


AnnualQ202223 <-full_join(AnnualQ20222023dfm,AnnualQ20222023S, by=c('Station','MY'))


MedianQ_cont<-bind_rows(AnnualQ201516,AnnualQ201617,AnnualQ201718,AnnualQ201819,AnnualQ201920,AnnualQ202021, AnnualQ202223) 



saveRDS(MedianQ_cont, file = paste0(outPath, 'MedianQ_cont.rds'))
write_csv(MedianQ_cont, path = paste0(outPath, 'MedianQ_cont.csv'))

#Make a column with preferred median discharge measurement per outfall - Med_2023 and Median_prior
values <- list()  
values[['MedianQ_cont']] <- paste0(outPath, 'MedianQ_cont.rds')
MedianQ_contCalcs <-  readRDS(values[["MedianQ_cont"]]) %>%
  filter(Station != 'K01-12177-1 (K01P07)' | MY != 'MY2018-19') %>%
  filter(Station != 'L01-728-3 (L01S02)' | MY != 'MY2018-19') %>%
  filter(Station != 'L01-728-7 (L01S03)' | MY != 'MY2016-17') %>%
  filter(Station != 'L01-747-2' | MY != 'MY2015-16') %>%
  filter(Station != 'L01-766-2 (L01S06)' | MY != 'MY2018-19') %>%
  filter(Station != 'L02-166-3 (L02P26)' | MY != 'MY2015-16') %>%
  filter(Station != 'L03-073-3' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-074-1 (L03B01)' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-214-2 (L03P18)' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-316-3 (L03P12)' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-316-3 (L03P12)' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-418-8 (L03P12)' | MY != 'MY2017-18') %>%
  filter(Station != 'L03-693-1 (L03P11)' | MY != 'MY2015-16') %>%
  filter(Station != 'L05-489-3' | MY != 'MY2018-18') %>%
  filter(Station != 'L05-489-7' | MY != 'MY2015-16') %>%
  filter(Station != 'M01-050-4 (M01@CGV)' | MY != 'MY2018-19') %>%
  filter(Station != 'M02-052-3' | MY != 'MY2016-17') %>%
  filter(Station != 'J03-9221-1 (J03P02)' | MY != 'MY2018-19') %>%
  filter(Station != 'L02-246-1 (L11P11)' | MY != 'MY2015-16') %>%
  filter(Station != 'L05-489-3' | MY != 'MY2018-19') %>%
  filter(Station != 'L03-418-8' | MY != 'MY2017-18') %>% 


  mutate(Qcont4calc2023 = coalesce(medianSWN2023cfs, medianDFM2023cfs)) %>%
  mutate(Qcont4calcPrior = coalesce(medianOFPcfs, medianOCFS2021cfs)) %>%
  mutate(Qcont4calcPrior = coalesce(Qcont4calcPrior,medianRcfs)) %>%
  mutate(Qcont4calcPrior= coalesce(Qcont4calcPrior, medianTMcfs)) %>%
  mutate(Qcont4calcPrior= coalesce(Qcont4calcPrior, medianTM)) %>%
   select('Station', 'Qcont4calc2023', 'Qcont4calcPrior')
 
MedianQ_contCalcs$Station[MedianQ_contCalcs$Station == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 
colnames(MedianQ_contCalcs)[colnames(MedianQ_contCalcs) == "Station"] <- "FACILITYID"

saveRDS(MedianQ_contCalcs, file = paste0(outPath, 'MedianQ_contCalcs.rds'))
write_csv(MedianQ_contCalcs, path = paste0(outPath, 'MedianQ_contCalcs.csv'))


  
##TO DO:  make MonitoringYear consistent colnames(MedianQ_cont)[colnames(MedianQ_cont) == "MY"] <- "MonitoringYear"