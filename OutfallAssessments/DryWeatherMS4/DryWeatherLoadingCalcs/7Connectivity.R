## Determine connectivity adjustment for each outfall
#use field screening dataset, select flow condition and connectivity and Facility, determine monitoring year, and find average for each outfall and monitoring year
source('DryWeatherLoadingCalcs/1projectsetup.R')
inPath <- paste0(wd)
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')


values <- list()  
values[['FieldScreen_dry']] <- paste0(outPath, 'FieldScreen_dry.rds') 
  FieldScreen_dry <-  readRDS(values[["FieldScreen_dry"]]) %>% 
    as_tibble() %>% 
    ungroup() %>% 
  select(FacilityID, FlowConnectivity)

  file_url<- paste0(inPath, "/MajorOutfalls_2023.csv")
  MO<-read.csv(file_url) %>%
    select('Facility.Identifier', 'PERSISTENTFLOW')
    
   
Connectivity1a <- left_join(MO, FieldScreen_dry, by=c('Facility.Identifier'='FacilityID'))

#Flow conditions:  0:nothing selected;  1: Flowing; 2:Pooled or Ponded;  3: Dry;  4:  Other - See Comments; NA: 'No Value' selected
#Connectivity1a$FlowConnectivity <-sub("Direct Connection", "2",Connectivity1a$FlowConnectivity)
#Connectivity1a$FlowConnectivity<-sub("None - Flow Infiltrates or Outfall is Dry", "1",Connectivity1a$FlowConnectivity)
#Connectivity1a$FlowConnectivity<-sub("Partial - Significant Distance", "2", Connectivity1a$FlowConnectivity)
#Connectivity1$FlowConnectivity<-sub("4", "Undetermined",Connectivity1$FlowConnectivity)


Connectivity1 <-  Connectivity1a %>%
  mutate(Connectivity = ifelse(FlowConnectivity == 'None - Flow Infiltrates or Outfall is Dry',"0",ifelse(FlowConnectivity == 'Direct Connection',"1", ifelse(FlowConnectivity== 'Partial - Significant Distance',"0.5", ifelse(FlowConnectivity=='NA',"0.77",ifelse(is.na(FlowConnectivity), "0.77", "0.77")))))) %>%   #if direct connection assign 1; if no connection assign 0; otherwise assign 0.77
  as_tibble() %>%
  replace_na(list(Connectivity='0.77')) %>%
  #mutate(Date = as.Date(date, format = '%Y-%m-%d')) %>%
  #filter(!is.na(Date)) %>%
  #filter(Date>"2016-10-01" & Date <"2021-10-01") %>%
  mutate(Connectivity=as.numeric(Connectivity)) 


str(Connectivity1)

Connectivity1 <- Connectivity1 %>%
  #select(-Date) %>%
  group_by(Facility.Identifier) %>%
  mutate(avgCnx=(mean(Connectivity))) %>%
  ungroup() %>%
  select(Facility.Identifier,avgCnx, PERSISTENTFLOW) %>%
  unique()
#group_by(FACILITYID) %>%
#mutate(avgCnxAll=(mean(Connectivity)))

Connectivity1$Facility.Identifier[Connectivity1$Facility.Identifier == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 

saveRDS(Connectivity1, paste0(outPath, 'Connectivity.rds'))
write_csv(Connectivity1, path = paste0(outPath, 'Connectivity.csv'))

saveRDS(Connectivity1, paste0(outPath, 'Connectivity_pf.rds'))
write_csv(Connectivity1, path = paste0(outPath, 'Connectivity_pf.csv'))

##Stop here

#Find average connectivity for each outfall by monitoring year; add a column for monitoring year and summarize average connectivity per MY for each outfall
Connectivity1$MonitoringYear<- ifelse(Connectivity1$Date>(as.Date("2020-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                                      ifelse(Connectivity1$Date>(as.Date("2019-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                             ifelse(Connectivity1$Date>(as.Date("2018-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                                    ifelse(Connectivity1$Date>(as.Date("2017-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                           ifelse(Connectivity1$Date>(as.Date("2016-09-30", origin="1900-01-01")) & Connectivity1$Date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                                  NA)))))



Connectivity1 <- Connectivity1 %>%
  select(-Date) %>%
  group_by(FACILITYID) %>%
  mutate(avgCnx=(mean(Connectivity))) %>%
  ungroup() %>%
  select(FACILITYID,avgCnx) %>%
  unique()
  #group_by(FACILITYID) %>%
  #mutate(avgCnxAll=(mean(Connectivity))) 

saveRDS(Connectivity1, file = paste0(outPath, 'Connectivity.rds'))
write_csv(Connectivity1, path = paste0(outPath, 'Connectivity.csv'))


#Connectivityall <- Connectivity1 %>%
  #select(FACILITYID, avgCnxAll) %>%
  #unique() %>%
  #right_join(., MajorOutfalls_lc1)

##Use if multiple years are in the original file
Connectivity202223<-Connectivity1 %>%
  filter(MonitoringYear=='MY2022-232') %>%
  right_join(.,MajorOutfalls_lc1) %>%
  right_join(.,Connectivityall) %>%
  select(FACILITYID, MonitoringYear, avgCnx, avgCnxAll) %>%
  unique() %>%
  replace_na(list(MonitoringYear='MY2022-23')) %>% #use all connectivity if no connectivity obs for monitoring year
  replace_na(list(avgCnxAll=0.77)) %>%
  mutate(avgCnxAll2=coalesce(avgCnx,avgCnxAll)) %>%
  select(FACILITYID,MonitoringYear,avgCnxAll2) %>%
  unique()

names(Connectivity202223)[names(Connectivity202223)=='avgCnxAll2'] <- 'avgCnx'               

saveRDS(Connectivity1, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.rds'))
write_csv(Connectivity1, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/Connectivity.csv'))
