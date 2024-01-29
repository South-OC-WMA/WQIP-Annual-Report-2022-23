## Import outfall observations 

source('DryWeatherLoadingCalcs/1projectsetup.R')
inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/ProvisionalFlow/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

getwd()

url_Outfall_Inspect<-"C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/ScheduleInventory/Outfall2023/OCEnvRes_MP_12152023.xlsx"  #use MP export from ArcGIS Pro"
#format so that volumetric discharge is on the same row

Outfall_Inspect<-read_excel(url_Outfall_Inspect) %>%
  select(Jurisdiction, RecordType, Inspected, ObsWeather, FacilityID, AvgDischarge, ObsRecordDate, FlowCondition, PersistentFlow, FlowConnectivity, IDIC, Priority, PrioritizationScore2022,IDIC, Conductivity, FloatDischargeCFS, VolumeDischargeCFS, LastDischargeTrend, ScourWaterCondition, DissWaterCondition, TrashCondition, TrashLocation, MaintenanceIssue, SampleDry, SampleWet, OCFSLocation, PointY, PointX, Comments) %>%
  filter(ObsRecordDate > "2022-09-30" & ObsRecordDate < "2023-10-02")

Outfall_Inspect$FacilityID[Outfall_Inspect$FacilityID == "L03-241-4 (L03P14)"] <- "L03-241-4"

Outfall_Inspect_v <- Outfall_Inspect %>%
  filter(RecordType == 'Flow Maintenance') %>%
  select(FacilityID, ObsRecordDate, VolumeDischargeCFS)

Outfall_Inspect_fl <- Outfall_Inspect %>%
  filter(RecordType == 'Field Screening') %>% 
  select(FacilityID, Jurisdiction, ObsRecordDate, ObsWeather, AvgDischarge, FlowCondition, PersistentFlow, FlowConnectivity, Priority, PrioritizationScore2022, Conductivity, Inspected,  FloatDischargeCFS,SampleDry, PointY, PointX) 


Outfall_Inspect_ <- left_join(Outfall_Inspect_fl, Outfall_Inspect_v, by=c('FacilityID', 'ObsRecordDate')) %>%
  select(FacilityID, Jurisdiction, ObsRecordDate, ObsWeather, AvgDischarge, FlowCondition, PersistentFlow,FlowConnectivity,  Priority, PrioritizationScore2022, Conductivity, Inspected,  FloatDischargeCFS, VolumeDischargeCFS, SampleDry, PointY, PointX) 

Outfall_Sample_ <- left_join(Outfall_Inspect_fl, Outfall_Inspect_v, by=c('FacilityID', 'ObsRecordDate')) %>%
  select(Jurisdiction, Inspected, ObsWeather, FacilityID, AvgDischarge, ObsRecordDate, FlowCondition, PersistentFlow,FlowConnectivity,FloatDischargeCFS,VolumeDischargeCFS, SampleDry, PointY, PointX) %>%
  filter(ObsWeather==1 & SampleDry=='Yes') %>%
  select(FacilityID, ObsRecordDate, FlowCondition, FloatDischargeCFS, VolumeDischargeCFS)


str(FieldScreening)


#date formatting


# Save processed dataset

saveRDS(Outfall_Inspect_, file = paste0(inPath, 'FieldScreening.rds')) 
write_csv(Outfall_Inspect_, path = paste0(inPath, 'FieldScreening.csv'))

library(stringr)


##Task 5.1: Filter out wet weather from instantaneous flow data
#use outfall field screen data from outafll with persistent flow (need to redefine persistent flow)

values <- list()  
values[['FieldScreening']] <- paste0(inPath, 'FieldScreening.rds')
FieldScreeningb <-  readRDS(values[["FieldScreening"]]) 
  
FieldScreeningb <- within(FieldScreeningb, FacilityID[FacilityID == 'L04-136-1 (L04P07)' & FlowCondition == 'Flowing'] <- 'L04-136-1u (L04P07)')
FieldScreeningb <- within(FieldScreeningb, FacilityID[FacilityID == 'L04-136-1 (L04P07)' & FlowCondition == 'Flowing'] <- 'L04-136-1u (L04P07)')

str(FieldScreeningb)


FieldScreeningFL<-FieldScreeningb %>%
  select('FacilityID', 'FloatDischargeCFS', 'ObsRecordDate', 'AvgDischarge') %>%
  group_by(FacilityID, ObsRecordDate) %>%
  mutate(FloatDischargeCFS=mean(FloatDischargeCFS)) %>%
  unique()

FieldScreeningV<-FieldScreeningb %>%
  select('FacilityID', 'VolumeDischargeCFS', 'ObsRecordDate') %>%
  group_by(FacilityID, ObsRecordDate) %>%
  mutate(VolumeDischargeCFS=mean(VolumeDischargeCFS)) %>%
  unique() %>%
  filter(!is.na(VolumeDischargeCFS))


FieldScreeningc <- left_join(FieldScreeningFL,FieldScreeningV, by=c('FacilityID', 'ObsRecordDate'))

FieldScreeningFC<-FieldScreeningb %>%
  select('FacilityID', 'ObsRecordDate', 'FlowCondition', 'PersistentFlow', 'SampleDry') %>%
  unique() %>%
  right_join(., FieldScreeningc, by=c('FacilityID', 'ObsRecordDate'))



FieldScreeningC<-FieldScreeningb %>%
  select('FacilityID','ObsRecordDate', 'FlowConnectivity') %>%
  unique() 

FieldScreeningC<-FieldScreeningC %>%
  mutate(across(where(is.character), ~na_if(.,""))) %>%
  filter(!is.na(FlowConnectivity)) %>%
  right_join(., FieldScreeningFC, by=c('FacilityID', 'ObsRecordDate'))


values <- list()  
values[['RainInf']] <- paste0(outPath, 'RainInf.rds')
RainInf <-  readRDS(values[["RainInf"]]) 


FieldScreen_dry <- FieldScreeningC %>%  #show only volumetric if there is volumetric and floating leaf, show average flow if pooled ponded, later
  left_join(., RainInf, by = c('FacilityID' = 'FACILITYID','ObsRecordDate' = 'Date')) %>%
  filter(wet_within_72 == "FALSE") %>%
  mutate(FloatDischargeCFS = ifelse(FlowCondition=='Dry', 0,FloatDischargeCFS)) %>%
  mutate(VolumeDischargeCFS = ifelse(FlowCondition=='Dry', 0,VolumeDischargeCFS)) %>%
  group_by(FacilityID, ObsRecordDate) %>%
  mutate(QInscfsV = mean(c(VolumeDischargeCFS))) %>%
  mutate(QInscfsF = mean(c(FloatDischargeCFS))) %>%
  mutate(QInscfsFadj=QInscfsF*0.69) %>%
  mutate(QInscfs= coalesce(QInscfsV, QInscfsV))  %>%
  mutate(QInscfs= coalesce(QInscfs, QInscfsFadj))


 
saveRDS(FieldScreen_dry, file = paste0(outPath, 'FieldScreen_dry.rds'))
write_csv(FieldScreen_dry, path = paste0(outPath, 'FieldScreen_dry.csv'))

#select outfalls with flow measurements


#list of major outfalls with instantenous flow measurement for 2023
MO_if <- FieldScreen_dry %>%
  ungroup() %>%
  select('FacilityID', 'QInscfs') %>%
  unique() %>%
  filter(!is.na(QInscfs)) %>%
  group_by(FacilityID) %>%
  mutate(QInscfs=mean(QInscfs)) %>%
  unique()

saveRDS(MO_if, file = paste0(outPath, 'MO_if.rds'))
write_csv(MO_if, path = paste0(outPath, 'MO_if.csv'))

#list of major outfalls with no instantenous flow measurement for 2023
MO<-read_csv(paste0(wd, '/DryWeatherLoadingCalcs/Input/MajorOutfalls_2023.csv')) %>%
  select('Facility Identifier')

MO$`Facility Identifier`[MO$`Facility Identifier` == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)"


MO_nf <- left_join(MO, MO_if, by=c('Facility Identifier'='FacilityID')) %>%
  select('Facility Identifier', 'QInscfs') %>%
  filter(is.na(QInscfs)) %>%
  unique()

saveRDS(MO_nf, file = paste0(outPath, 'MO_nf.rds'))
write_csv(MO_nf, path = paste0(outPath, 'MO_nf.csv'))

#get average of volumetric flow
swVolumetric<- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.Flow_Meter_Locations') %>%
  arc.select() %>%
  tibble::as_tibble() %>%
  select(StationCode, Jurisdiction,GlobalID, ForeignLocationGUID) %>%
  right_join(., swVolumetricObs <- arc.open('C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/OutfallFieldScreen/Results/SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.Flow_Meter_Observations') %>%
               arc.select(), by = c('GlobalID'= "FlowMeterLocGUID")) %>%
  filter(!is.na(ForeignLocationGUID)) %>%
           select(StationCode, Jurisdiction, VolumeDischargeCFS, HandMeterDischarge) %>%
  group_by(StationCode) %>%
  mutate(AVGvDISCHARGE = mean(VolumeDischargeCFS, na.rm=TRUE)) %>%
  mutate(AVGfmDISCHARGE = mean(HandMeterDischarge, na.rm=TRUE)) %>%
  mutate(Qcalcs = coalesce(AVGvDISCHARGE, AVGfmDISCHARGE))

saveRDS(swVolumetric, file = paste0(outPath, 'swVolumetric.rds'))
write_csv(swVolumetric, path = paste0(outPath, 'swVolumetric.csv'))
  

#join with major outfalls and instant flow measurements for 2023

MO <- read_csv(paste0(wd, '/MajorOutfalls_2023.csv')) %>%
  select('Facility Identifier', 'AVGDISCHARGE', 'JURISDICTI')

MO$'Facility Identifier'[MO$'Facility Identifier' == "L04-136-1u (L04P07)"] <- "L04-136-1 (L04P07)"

MOallinsQ <- right_join(swVolumetric, MO, by=c('StationCode' = 'Facility Identifier', 'Jurisdiction' = 'JURISDICTI')) %>%
  select('StationCode', 'Jurisdiction', 'Qcalcs', 'AVGDISCHARGE') %>%
  unique() %>%
  left_join(., MO_if, by=c('StationCode' = 'FacilityID'))


str(swVolumetric)
#join together for one table showing all flow types and create a column with preferred flow for calcs

allInst <- full_join(MOallinsQ, MO_if, by=c('StationCode'= 'FacilityID', 'QInscfs')) %>%
  unique() %>%
  mutate(AVGQadj = AVGDISCHARGE*0.69)%>%
  mutate(Q4calcs = coalesce(QInscfs, Qcalcs)) %>%
  mutate(Q4calcs = coalesce(Q4calcs, AVGQadj)) 

write_csv(allInst, path = paste0(outPath, 'allInst.csv'))
saveRDS(allInst, file = paste0(outPath, 'allInst.rds'))




