## Task 10*: Multiply annual flow volume results (Task 7) with connectivity adjustment (Task 8)
#join datasets

source('DryWeatherLoadingCalcs/1projectsetup.R')

inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

values <- list()  
values[['DailyCFandPond_trib2']] <- paste0(outPath, 'DailyCFandPond_trib2.rds')
DailyCFandPond_trib2 <-  readRDS(values[["DailyCFandPond_trib2"]])  
 
#includes all outfalls, including those without delineated tributaries

values <- list()  
values[['DryDaysYear']] <- paste0(outPath, 'DryDaysYear.rds')
DryDaysYear <-  readRDS(values[["DryDaysYear"]]) %>%
  filter(!is.na(FACILITYID)) %>%
  filter(MonitoringYear=='MY2022-23')  

DryDaysYear[DryDaysYear == ""] <- NA

DryDaysYear <- DryDaysYear %>%
  filter(!is.na(FACILITYID))

values <- list()  
values[['Connectivity']] <- paste0(outPath, 'Connectivity.rds')
Connectivity <-  readRDS(values[["Connectivity"]])   #includes all outfalls, including those without delineated tributaries
  

file_url<- paste0(inPath, "MajorOutfalls_2023.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier')


#Calculate total annual flow by jurisdiction and monitoring year - should only be for those with a result in DischargePCF3
AnnualFlow <- full_join(DailyCFandPond_trib2, Connectivity, by=c('FACILITYID'='Facility.Identifier')) %>%
  unique() %>%
  left_join(., DryDaysYear, by=c('FACILITYID')) %>%
  group_by(FACILITYID, JURISDICTI3) %>%
  mutate(Qadj_Qall=avgCnx*dry_days*DischargePCF3) 

saveRDS(AnnualFlow, file = paste0(outPath, 'AnnualFlow.rds'))
write_csv(AnnualFlow, path = paste0(outPath, 'AnnualFlow.csv' ))


#Find annual flow volume by jurisdiction
#Remove outfalls after volumes determined

file_url<- paste0(wd, "/juris_area_summary.csv")
SOCWMA_Cities<-read.csv(file_url) %>%
  select('jurisdicti', 'acres_juris_soc') %>%
  filter(!is.na(acres_juris_soc)) %>%
  filter(!is.na(jurisdicti))

AnnualFlow_Juriscon<-AnnualFlow %>% #remove outfalls upstream of most downstream outfall closest to receiving water
  as_tibble() %>%
  ungroup() %>%
  filter(FACILITYID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FACILITYID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FACILITYID != "L05-049-2" & FACILITYID != "L05-049-1" & FACILITYID != "L05-489-7" & FACILITYID !="L05-489-3" & FACILITYID !="L05-489-4") %>%  #Horno Basin
  filter(FACILITYID != "J03-9234-8" & FACILITYID != "J03-9234-6" & FACILITYID !="J03-9234-5" & FACILITYID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FACILITYID != "L03-141-1" & FACILITYID != "L03-141-3" & FACILITYID != "L03-141-2" & FACILITYID != "L03-172-2" & FACILITYID != "L03-172-3" & FACILITYID != "L03-073-3" & FACILITYID != "L03-073-4" & FACILITYID != "L03-073-5" & FACILITYID != "L03-074-2" & FACILITYID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FACILITYID != "L04-136-1u (L04P07)" | FACILITYID != "L04-136-1(L04P07)") %>%
  filter(FACILITYID != "J03-9199-2" & FACILITYID != "J03-9190-1" & FACILITYID != "J03-9199-1") %>%
  filter(FACILITYID != "K01-12156-6"   & FACILITYID != "K01-12156-4") %>% #Salt Creek
  filter(FACILITYID != "M02-052-3" & FACILITYID != "M02-052-4" & FACILITYID != "M02-032-1" & FACILITYID != "M02-085-1 (M02P06)" & FACILITYID != "M02-085-2" & FACILITYID != "M02-013-1" & FACILITYID != "M02-086-1" & FACILITYID != "M02-015-1" & FACILITYID != "M02-028-2 (M02P08)" & FACILITYID != 'M02-061-7' & FACILITYID != 'M02-102-1') %>%   #Segunda Deshecha Channel
  filter(FACILITYID != "M01-008-1" & FACILITYID != "M01-060-3" & FACILITYID != "M01-124-4") %>%
  filter(FACILITYID != "M00.1-070-6" & FACILITYID != "M00.1-070-4" & FACILITYID != "M00.1-070-3" & FACILITYID != "M00.1-070-2" & FACILITYID != 'M00.1-070-1' & FACILITYID !=  "M00.1-071-1 (M00S04)" & FACILITYID !=  "M00.1-071-4 (M00S04)" & FACILITYID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FACILITYID != "I01-11503-3"  & FACILITYID != "I01-11503-4" & FACILITYID != "I01-11502-1" & FACILITYID != "I01-11216-3" & FACILITYID != "I01-11216-2 (I02P12)" & FACILITYID != "I01-11216-1 (I02P13)" & FACILITYID != "I01-11216-4 (I02P14)" & FACILITYID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)")  %>%
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L03-240-1 (L03P14)")  %>%
  select(JURISDICTI3, Qadj_Qall) %>%
  full_join(., SOCWMA_Cities, by=c('JURISDICTI3'='jurisdicti')) %>% 
  unique() %>%
  filter(!is.na(Qadj_Qall)) %>%
  group_by(JURISDICTI3) %>%
  mutate(QTot=sum(Qadj_Qall)) %>% 
  mutate(DischargeJ_af = QTot/43560) %>%
  mutate(QperArea_cfperacre=QTot/acres_juris_soc) %>%
  select(JURISDICTI3, DischargeJ_af, QperArea_cfperacre) %>%
  unique() %>%
  ungroup() %>%
  filter(!is.na(DischargeJ_af)) 
  

saveRDS(AnnualFlow_Juriscon, file = paste0(outPath, 'AnnualFlow_Juriscon.rds'))  
write_csv(AnnualFlow_Juriscon, path = paste0(outPath, 'AnnualFlow_Juriscon.csv'))


#Determine Annual flow by jurisdiction for unsampled outfalls (not in Appendix M, and also ponded outfalls in Appendix M)

#find outfalls with flow measurements but not sampled

file_url<- paste0(inPath, "MajorOutfalls_2023.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier') 

MO$Facility.Identifier[MO$Facility.Identifier == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 


values <- list()  
values[['AnnualFlow']] <- paste0(outPath, 'AnnualFlow.rds')
AnnualFlow <-  readRDS(values[["AnnualFlow"]]) 

MOp<- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint') %>%
  arc.select() %>%
  tibble::as_tibble() %>%
  select('FACILITYID','SAMPLEDRY')

MOp$FACILITYID[MOp$FACILITYID == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 

MO <- left_join(MO, MOp, by=c('Facility.Identifier'='FACILITYID'))

AnnualFlow_USa <- AnnualFlow %>%
  right_join(., MO, by=c('FACILITYID' = 'Facility.Identifier')) %>%
  filter(is.na(SAMPLEDRY)|SAMPLEDRY==2) %>%
  #filter(Q_DailyavgCF >'-0.99') %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall) 
   
saveRDS(AnnualFlow_USa, file = paste0(outPath, 'AnnualFlow_USa.rds'))
write_csv(AnnualFlow_USa, path = paste0(outPath, 'AnnualFlow_USa.csv'))  #put in connectivity by hand in csv#save csv as AnnualFlow_USa; use same connectivity as previous years if no data; if no observation, use 0.77


#outfalls sampled but without any flow measurements         
AnnualFlow_USb <- AnnualFlow %>%
  right_join(., MO, by=c('FACILITYID' = 'Facility.Identifier')) %>%
  filter(SAMPLEDRY =='1' & (Q4calcsall=='-0.99')) %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall) %>%   
  unique() 


AnnualFlow_US <- bind_rows(AnnualFlow_USa, AnnualFlow_USb)   #flow from unsampled outfalls, both measured flow and estimates at ponded outfalls
  

#AnnualFlow_USj <- AnnualFlow_US %>%
  #select(JURISDICTI3, QJur_US) %>%
  #unique() %>%
  #group_by(JURISDICTI3) %>%
  #mutate(QJur_US=sum(QJur_US)) %>%
  #unique() 

#AnnualFlow_US <-AnnualFlow_US %>%
  #select(-QJur_US) %>%
  #left_join(.,AnnualFlow_USj)

saveRDS(AnnualFlow_US,  paste0(outPath,'AnnualFlow_US.rds'))
write_csv(AnnualFlow_US, path = paste0(outPath, 'AnnualFlow_US.csv'))


#by jurisdiction
values <- list()  
values[['AnnualFlow_US']] <- paste0(outPath, 'AnnualFlow_US.rds')

AnnualFlow_US_j <- AnnualFlow_US %>%
  filter(FACILITYID != "M01-008-1" & FACILITYID != "M01-060-3" & FACILITYID != "M01-124-4") %>%
  filter(FACILITYID != "M00.1-070-6" & FACILITYID != "M00.1-070-4" & FACILITYID != "M00.1-070-3" & FACILITYID != "M00.1-070-2" & FACILITYID != 'M00.1-070-1' & FACILITYID !=  "M00.1-071-1 (M00S04)" & FACILITYID !=  "M00.1-071-4 (M00S04)" & FACILITYID !=  "M00.1-071-3 (M00S04)") %>% #coastal SC
  filter(FACILITYID != "I01-11503-3"  & FACILITYID != "I01-11503-4" & FACILITYID != "I01-11502-1" & FACILITYID != "I01-11216-3" & FACILITYID != "I01-11216-2 (I02P12)" & FACILITYID != "I01-11216-1 (I02P13)" & FACILITYID != "I01-11216-4 (I02P14)" & FACILITYID != "I01-11217-1") %>%  #Laguna Canyon Wash
  filter(FACILITYID != "L01-613-1" & FACILITYID != "L01-728-7 (L01S03)")  %>%
  filter(FACILITYID != "J06-9362-1 (J06-P03)")  %>% #Dairy Fork
  filter(FACILITYID != "J01-9082-5 (J02P08)") %>% #Wood Canyon
  filter(FACILITYID != "L05-049-2" & FACILITYID != "L05-489-7" & FACILITYID !="L05-489-4" & FACILITYID !="L05-489-3") %>%  #Horno Basin
  filter(FACILITYID != "J03-9234-8" & FACILITYID != "J03-9234-6" & FACILITYID !="J03-9234-5" & FACILITYID !="K01-12032-2 (K01P11)") %>% #Niguel Storm Drain  
  filter(FACILITYID != "L03-141-1" & FACILITYID != "L03-141-3" & FACILITYID != "L03-141-2" & FACILITYID != "L03-172-2" & FACILITYID != "L03-172-3" & FACILITYID != "L03-073-3" & FACILITYID != "L03-073-4" & FACILITYID != "L03-073-5" & FACILITYID != "L03-074-2" & FACILITYID != "L03-074-1 (L03B01)") %>% #Oso Creek
  filter(FACILITYID != "L04-136-1u (L04P07)") %>%
  filter(FACILITYID != "J03-9199-2" & FACILITYID != "J03-9190-1" & FACILITYID != "J03-9199-1") %>%
  filter(FACILITYID != "K01-12156-6"   & FACILITYID != "K01-12156-4") %>% #Salt Creek
  filter(FACILITYID != "M02-052-3" & FACILITYID != "M02-052-4" & FACILITYID != "M02-013-1" & FACILITYID != "M02-086-1" & FACILITYID != "M02-015-1" & FACILITYID != "M02-028-2 (M02P08)" & FACILITYID != 'M02-061-7' & FACILITYID != 'M02-102-1') %>% #egunda Deshecha Channel

unique() %>%
  group_by(JURISDICTI3) %>%
  
  mutate(QJur_US=sum(Qadj_Qall)) %>%
  
  
  
unique() 

  saveRDS(AnnualFlow_US_j, paste0(outPath, 'AnnualFlow_US_j.rds'))
write_csv(AnnualFlow_US_j, path = paste0(outPath, 'AnnualFlow_US_j.csv'))

#Dataset with flow from sampled outfalls with flow
AnnualFlow_s <- AnnualFlow %>%
  right_join(., MO, by=c('FACILITYID' = 'Facility.Identifier')) %>%
  filter(SAMPLEDRY =='1'& Q4calcsall != '-0.99') %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall) %>%
  unique() 
  

saveRDS(AnnualFlow_s, paste0(outPath, 'AnnualFlow_s.rds'))
write_csv(AnnualFlow_s, paste0(outPath, 'AnnualFlow_s.csv'))

#combine sampled and unsampled for all outfalls and total jurisdictional flow volume

AnnualFlow_all <- bind_rows(AnnualFlow_s, AnnualFlow_US)
saveRDS(AnnualFlow_all, paste0(outPath, 'AnnualFlow_all.rds'))
write_csv(AnnualFlow_all, paste0(outPath, 'AnnualFlow_all.csv'))


#by jurisdiction

values <- list()  
values[['AnnualFlow_s']] <- paste0(outPath, 'AnnualFlow_s.rds')
AnnualFlow_S_j <-  readRDS(values[["AnnualFlow_s"]]) %>%
  as_tibble() %>%
  filter(FACILITYID != "L05-049-1" & FACILITYID != "M02-032-1" & FACILITYID != "M02-085-1 (M02P06)" & FACILITYID != "M02-085-2"
         & FACILITYID !="L05-489-3") %>% 
  select(c(FACILITYID, JURISDICTI3, Qadj_Qall)) %>%
  group_by(JURISDICTI3) %>%
  mutate(QJur_S=sum(Qadj_Qall)) %>%
  ungroup() %>%
  unique() 
  
values <- list() 
values[['AnnualFlow_US_j']] <- paste0(outPath, 'AnnualFlow_US_j.rds')
AnnualFlow_US_j <-  readRDS(values[["AnnualFlow_US_j"]]) %>%
  select(c(FACILITYID, JURISDICTI3, Qadj_Qall, QJur_US))

AnnualFlow_J2<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'FACILITYID', 'Qadj_Qall')) %>%
  select(FACILITYID, JURISDICTI3, Qadj_Qall, QJur_US,QJur_S) %>%
  unique()

saveRDS(AnnualFlow_J2, paste0(outPath, 'AnnualFlow_J2.rds'))
write_csv(AnnualFlow_J2, paste0(outPath, 'AnnualFlow_J2.csv'))

AnnualFlow_J2short<-full_join(AnnualFlow_S_j, AnnualFlow_US_j, by=c('JURISDICTI3', 'FACILITYID', 'Qadj_Qall')) %>%
  rowwise() %>%
  mutate(DischargeJ_cf = sum(QJur_S, QJur_US, na.rm=TRUE)) %>%
  mutate(DischargeJ_af = DischargeJ_cf/43560) %>%   #convert to acre-ft; 1 acre-feet = 43560 cf
  unique() %>%
  select(JURISDICTI3, DischargeJ_cf, DischargeJ_af) %>%
unique() %>%
  group_by(JURISDICTI3) %>%
  mutate(DischargeJ_cf = sum(DischargeJ_cf)) %>%
  mutate(DischargeJ_af = sum(DischargeJ_af)) %>%
  unique()


saveRDS(AnnualFlow_J2short, paste0(outPath, 'AnnualFlow_J2short.rds'))
write_csv(AnnualFlow_J2short, paste0(outPath, 'AnnualFlow_J2short.csv'))

#for print output (Jurisdictions, cnx)

values <- list()  
values[['Connectivity']] <- paste0(outPath,'Connectivity.rds')
Connectivity <-  readRDS(values[["Connectivity"]])


values[['AnnualFlow_all']] <- paste0(outPath, 'AnnualFlow_all.rds')
AnnualFlow_all <-  readRDS(values[["AnnualFlow_all"]]) 

values[['AnnualCF2b']] <- paste0(outPath, 'AnnualCF2b.rds')
AnnualCF2b <-  readRDS(values[["AnnualCF2b"]]) %>%
  select(FACILITYID, Jurisdiction, JURISDICTI3, AnnualCF) %>%
  unique()

file_url<- paste0(inPath, "MajorOutfalls_2023.csv")
MO<-read.csv(file_url) %>%
  select('Facility.Identifier', 'JURISDICTI')

MO$Facility.Identifier[MO$Facility.Identifier == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)" 

values <- list() 
values[['AnnualFlow_J2']] <- paste0(outPath, 'AnnualFlow_J2.rds')
AnnualFlow_J2 <-  readRDS(values[["AnnualFlow_J2"]]) %>%
  select(FACILITYID, JURISDICTI3, QJur_US, QJur_S)


AnnualFlow_all_print <- AnnualFlow_all %>%
    full_join(., Connectivity, by=c('FACILITYID' = 'Facility.Identifier')) %>%
  full_join(., AnnualFlow_J2short) %>%
  full_join(., MO, by=c('FACILITYID' = 'Facility.Identifier')) %>%
  full_join(., AnnualCF2b, by=c('FACILITYID', 'JURISDICTI' = 'Jurisdiction', 'JURISDICTI3')) %>%
  full_join(., AnnualFlow_J2, by=c('FACILITYID', 'JURISDICTI3')) %>%
  select(JURISDICTI, FACILITYID, JURISDICTI3, AnnualCF, avgCnx, Qadj_Qall, QJur_S, QJur_US)

  
 

write_csv(AnnualFlow_all_print, paste0(outPath, '/AnnualFlow_all_print.csv'))
