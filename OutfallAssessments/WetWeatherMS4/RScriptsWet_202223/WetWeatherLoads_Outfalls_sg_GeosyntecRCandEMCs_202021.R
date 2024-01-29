
#   ____________________________________________________________________________
#   Land Use EMC Calculations                                               ####
library(tidyverse)
library(lubridate)
library(data.table)
library(magrittr)
library(readxl)
library(dplyr)
library(here)
library(tidyr)
library(readr)
library(hms)
library(arcgisbinding) #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary") #install bridge manually from GitHub:  https://github.com/R-ArcGIS/r-bridge/releases/tag/v1.0.1.232#
arc.check_product()

source('project_setupww.R')

getwd()
setwd('C:/Users/givens/Box/Outfall Assessments/WetWeatherLoadingCalcs')

#   ____________________________________________________________________________
#   Function Definitions                                                    ####

#   ____________________________________________________________________________
#   Data Import                                                             ####

##  ..................................WetWeatherDataandSALsAssessment2020-21


# EMC data 
getwd()

emc1 <- "C:/Users/givens/Box/Outfall Assessments/WetWeatherLoadingCalcs/Input/WetWeatherDataandSALsAssessment2020-21.xlsx"

emc <- read_excel(emc1,'Horizon wide units') %>%
  mutate(`TN - mg/L` = `Nitrate + Nitrite as N - mg/L` + `Nitrogen, Total Kjeldahl - mg/L`) %>%
  filter(!is.na(Station))


#emc$FacilityID<-sub("SC06-146-2", "SC06-146-2 (M00P02)",emc$FacilityID)
emc$Station<-sub("L01-724-4", "L01-724-4 (L01P03)",emc$Station)
emc$Station<-sub("L01-728-5", "L01-728-5 (L01-DP)",emc$Station)
emc$Station<-sub("L02-166-2", "L02-166-2 (L02P25)",emc$Station)
emc$Station<-sub("J07-9109-4", "J07-9109-4 (J07P02)",emc$Station)
emc$Station<-sub("J01-9992-1", "J01-9992-1 (J01P27)",emc$Station)
#emc$Station<-sub("J07P02", "J07-9109-4 (J07P02)",emc$Station) 
#emc$Station<-sub("L03P05", "L03-708-11 (L03P05)",emc$Station) 
emc$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",emc$Station) 


write.csv(emc, file = 'Output_202021/emc.csv')


##  ............................................................................
##  Hydrologic Info                                                         ####

# Nearest coop rain gage to swDischargePoint Facility's
stn_near <- read_csv('WetWeatherLoadingCalcs/Input/Rain/outfall_near_rain_gage.csv')
#stn_near$FacilityID<-sub("SC06-146-2", "SC06-146-2 (M00P02)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L01-724-4", "L01-724-4 (L01P03)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L01-728-5", "L01-728-5 (L01-DP)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L02-166-2", "L02-166-2 (L02P25)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L03-708-11", "L03-708-11 (L03P05)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("J07-9109-4", "J07-9109-4 (J07P02)",stn_near$FacilityID)

# Hydstra rainfall data Point format
#dr_rain <- read_csv('A.2 Outfall Assessments/WetWeatherLoadingCalcs/Input/Rain/SOCWMA_coop_202021.CSV', skip = 3, col_names = c('Time', 'Trabuco Forestry', 'Sulphur Creek Dam', 'El Toro', 'Palisades Reservoir', 'Laguna Beach', 'Dana Point', 'Upper Aliso')) 
dr_rain <- read_csv('Input/Rain/SOCWMA_coop_202021.CSV', skip = 3, col_names=c('Time', 'TRABUCO', 'SULPHURCREEK', 'ELTORO', 'PALISADES', 'LAGUNABEACH', 'DANAPOINT'))  #Tucker had been included previously
  
getwd()
##  ............................................................................
##  Land Use Info                                                           ####

# swDischargePoint tributary acreage 
stn_acres <- arc.open('SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Tributary') %>% 
  arc.select() %>% 
  tbl_df() %>% 
  select(GlobalID, FACILITYID, area_acres) %>%
  rename(GlobalID_tribarea=GlobalID)

stn_acres$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("L02-366-1 (L02-P14)", "L02-366-1 (MVL02P14)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",stn_acres$FACILITYID)
stn_acres$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",stn_acres$FACILITYID)

write.csv(stn_acres, file = 'Output_202021/stn_acres.csv')

# Land use runoff coefficients (Geosyntec)
landuse_Geosyntec <- data.frame(
  RC_Group = c('Agriculture', 'Commercial', 'Developed Open Space', 'Education', 'Freeway', 'Light Industrial', 'Multi-family Residential', 'Open Space', 'Single Family Residential', 'Streets and Roads', 'Water'),
  RC = c(0.222, 0.486, 0.217, 0.422, 0.450, 0.414, 0.485, 0.078, 0.405, 0.471, 0.797),
  stringsAsFactors = FALSE
)

# sbpat land use emc values
sbpat_emc_Geosyntec <- read_excel('Input/sbpat_EMC_Geosyntec.xlsx') %>% 
  gather(EMC_Group, EMC, `agriculture`:WATER) 

write.csv(sbpat_emc_Geosyntec, file = 'Output_202021/sbpat_emc_Geosyntec.csv')

# Land use codes & runoff coefficient group (SBPAT, Geosyntec)
sbpat_landuse_Geosyntec <- read_csv('Input/sbpat_landuse_Geosyntec.csv') %>%
  mutate_each(funs(as.numeric), LSPC_LU_CODE) %>% 
  filter(!is.na(Description))

#sbpat_landuse_EMC <- read_csv('data/processed/sbpat_landuse_LSPC_EMC.csv') %>%
# mutate_each(funs(as.numeric), LU_CODE) %>% 
  #mutate(
   # EMC_Group_Final = EMC_Group,
   # EMC_Group_Final = #ifelse(LU_CODE %in% c(1400, 1410, 1418, 1460, 1414, 1415, 1416, 1412, 1450, 1417, 1411, 1413),
                             #'Transportation',
                             #ifelse(LU_CODE %in% c(1260, 1261, 1266, 1263, 1264, 1265, 1262),
                                    #'Education',
                                    #EMC_Group_Final))) 
                      #ifelse(LSPC_LU_DESC %in% c(TROTH, TRANS),
                           #  'Transportation',
                           #  ifelse(LSPC_LU_DESC %in% c(EDU),
                               #     'Education',
                                #    EMC_Group_Final)))

R9_Cities <- read_csv('juris_area_summary.csv') %>%
  select(jurisdicti, acres_juris_soc)


# Land use by jurisdiction sum_Area_ACRES.x is the area within R9 after clip; Area is total acres (including outside of R9)
# Use codes from 

  
# swDischargePoint land use data
# land use information for each outfall tributary

landuse_trib_202122 <- read_csv('trib_lu_area.csv') %>%
  select(-OBJECTID) %>%
 #%>%  #For area around Oso Reservoir, assume trib areas contribute to area for storm flow calcs, remove the areas only affecting dry weather loading calcs
  filter(FACILITYID!='L03-214-2')%>%
  filter(FACILITYID!='L03-073-3')%>%
  filter(FACILITYID!='L03-141-1')

landuse_trib_202122$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("K01-12177-1", "K01-12177-1 (K01P07)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L02-166-3", "L02-166-3 (L02P26)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L02-366-1 (L02-P14)", "L02-366-1 (MVL02P14)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L03-662-3", "L03-662-3 (L03P16)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("L01-728-8", "L01-728-8d",landuse_trib_202122$FACILITYID)
landuse_trib_202122$FACILITYID<-sub("SC11-035-1", "SC11-035-1d",landuse_trib_202122$FACILITYID)


  #filter(HRU_Composite_LSPC_LU_CODE!=80) #remove areas of water bodies
  
write.table(landuse_trib_202122, file = 'clipboard-16384', sep = '\t')

write.csv(landuse_trib_202122, file = 'Output_202021/landuse_trib_202122.csv')

getwd()
#   ____________________________________________________________________________
#   Formatting and intermediate data products                               ####

# Combine stations and acreage
EMCacres <- left_join(
  emc,
  stn_acres,
  by = c('Station' = 'FACILITYID')
)

str(EMCacres)

# Combine stations and near coop gauge
EMCRain <- left_join(
  EMCacres,
  stn_near,
  by=c('Station'='FACILITYID')  
) 
#%>% 
  #mutate(
    #Time = ymd_hm(`Collected Date`))
    #Date = date(Time))

str(EMCRain)

# Total rainfall in reporting period for over .1 inch storms
rain_total <- dr_rain %>% 
  gather(Station, Total, -Time) %>% 
  filter(!is.na(Total)) %>% 
  mutate(
    Time = mdy_hm(Time),
    Date = date(Time)) %>% 
  group_by(Station, Date) %>% 
  summarise(Total = sum(Total)) %>% 
  filter(
    Date >= values$ReportStart & Date <= values$ReportEnd,
    Total >= 0.1
  ) %>% 
  group_by(Station) %>% 
  summarise(Rain_Gage_Total = sum(Total))

write.csv(rain_total, file = 'Output_202021/rain_total.csv')

getwd()
# Combine stations and rain gauge total - put Collected Date in simple date format
EMCRainTot <- left_join(
  EMCRain,
  rain_total,
  by = c('station' = 'Station')
) 

# Calculate weighted sample volumes for each composite sample from EMC data - typically don't need to do this since only each outfall is sampled once.   An outfall would be sampled twice if there were problems the first try, and there wasn't enough sample volume for everything
#emc_vol <- emc %>% 
  #filter(
    #!is.na(`Volume Sampled -cf`),
    #Date >= values$ReportStart & Date <= values$ReportEnd
  #) %>% 
  select(Station, FacilityID, Date, `G/C`, `Composite Begin`, `Composite End`, `Volume Sampled -cf`, Type) %>%
  distinct(Station, FacilityID, Date,`Composite Begin`, `Composite End`, `Volume Sampled -cf`, `G/C`, Type) 
  #Run the following if more than one composite sample was collected throughout the storm#
  #group_by(Station) %>%
  #mutate('Volume Sampled Total' = ifelse('G/c'=="Composite", sum(`Volume Sampled -cf`), `Volume Sampled -cf`))  
  #ungroup() %>% 
  #mutate('Volume Weighting' = `Volume Sampled -cf` / `Volume Sampled Total`)
  

# Combine stations with sample volumes
#stn <- left_join(
  #emc_vol,
  #stn,
  #by = c('FacilityID' = 'FacilityID')
#)

# Rainfall totals for composite events  dr_rain <- read_csv('data/raw/Event_Rainfall.csv', skip = 3, col_names = c('Time', 'Trabuco Forestry', 'Sulphur Creek Dam', 'El Toro', 'Palisades Reservoir', 'Laguna Beach')) 
event_total <- dr_rain %>%
  gather(Station, Total, -Time) %>%
  filter(!is.na(Total)) %>%
  mutate(
    Time = mdy_hm(Time),
    Date = date(Time)) 

  event_total$Date<-as.Date(event_total$Date)
  
  EMCRainTotb <- EMCRainTot %>%
    mutate(
      `Composite Begin` = as.Date(`Composite Start Time`, format = "%m/%d/%Y")) %>%
    mutate(
      `Composite End` =as.Date(`Composite End Time`, format = "%m/%d/%Y"))
  
  str(EMCRainTotb)
  
 
  event_totalb <- event_total  %>%  
    select(-Time) %>%   #only composite samples, grab samples are left out
    group_by(Date, Station) %>%
    summarise(Total = sum(Total)) %>%
    as.data.table() %>%
    left_join(
      EMCRainTotb,
      .,
      by = c('station' = 'Station')
    ) %>%
    filter(!is.na(`Composite Begin`)) %>%
    group_by(Station, `Composite Begin`) %>%
    
    summarise(
      #Rain_Event_Total = sum(Total[(Date) >= (as.Date(`Composite Start Time`) - days(1)) & Date <= as.Date(`Composite End Time`)])) %>%
      Rain_Event_Total = sum(Total[as.Date(Date) >= (as.Date(`Composite Begin`) - days(1)) & Date <= as.Date(`Composite End`)])) %>%
    ungroup()
  #filter(!is.na(`Composite Start Time`)) 
  
  # Combine stations and event rainfall totals
  EMCRainTot2 <- left_join(
    EMCRainTotb,
    event_totalb,
    by = c('Station', 'Composite Begin' = 'Composite Begin')
  )
  
  write.csv(rain_total, file = 'Output_202021/rain_total.csv')
  saveRDS(EMCRainTot2, file = 'Output_202021/EMCRainTot2.rds')
  write.csv(EMCRainTot2, file = 'Output_202021/EMCRainTot2.csv')

 
 #jurisdictional land-use
  
landuse_jurisdiction_2022 <- read_csv('juris_lu_summary.csv')  
  

# Combine jurisdiction land use with Geosyntec rc and sbpat emc categories and run-off coefficient  see line 582
landuse_lspc_juris_2022<-distinct(sbpat_landuse_Geosyntec, LSPC_LU_CODE, .keep_all=TRUE) %>%
  select(LSPC_LU_DESC, LSPC_LU_CODE, RC_Group, EMC_Group
  ) %>%
  left_join(., landuse_Geosyntec) %>%
  left_join(
    .,
    landuse_jurisdiction_2022,
    by = c('LSPC_LU_DESC'='lspc_lu_edit')) 
#%>% 
#ungroup()
write_csv(landuse_lspc_juris_2022, paste0(getwd(), '/Output_202021/landuse_lspc_juris_2022.csv'))

SOCWMA_lu<-landuse_lspc_juris_2022 %>%
  group_by(EMC_Group) %>%
  mutate(EMC_groupArea=sum(acres)) %>%
  ungroup() %>%
  select(EMC_Group, EMC_groupArea) %>%
  unique()

write_csv(SOCWMA_lu, paste0(getwd(), '/Output_202021/SOCWMA_lu.csv'))


# Combine station land use with Geosyntec rc and sbpat emc categories and run-off coefficient
  landuse_lspc_trib_2022<-distinct(sbpat_landuse_Geosyntec, LSPC_LU_CODE, .keep_all=TRUE) %>%
    select(LSPC_LU_DESC, LSPC_LU_CODE, RC_Group, EMC_Group
    ) %>%
    left_join(., landuse_Geosyntec) %>%
    left_join(
      .,
      landuse_trib_202122,
      by = c('LSPC_LU_DESC'='LSPC_LU_EDIT')) 
  #%>% 
  #ungroup()
  write_csv(landuse_lspc_trib_2022, paste0(getwd(), '/Output_202021/landuse_lspc_trib_2022.csv'))
 
  landuse_lspc_trib_2022 <- read_csv('Output_202021/landuse_lspc_trib_2022.csv')
  
getwd()
  
  MO<- "MajorOutfalls_2022.xlsx"
  MO <- read_excel(MO,'Sheet1') %>% 
    select(`Facility Identifier`, Jurisdiction)
  
  MO_LU <- landuse_lspc_trib_2022 %>% 
    left_join(., MO, by=c('FACILITYID'='Facility Identifier')) %>%
    filter(!is.na(Jurisdiction))
  
  write_csv(MO_LU, paste0(getwd(), '/WetWeatherLoadingCalcs/Output_202021/MO_LU.csv'))
  
#   ____________________________________________________________________________
#   Data analysis section                                                   ####

##  ............................................................................
##  Determine runoff coefficients                                           ####

# Runoff coefficient 'RC' reality  #43560 cf in an acre-foot
stn_RC_202021 <- EMCRainTot2 %>% 
  mutate(
    RC_Event = `Stormflow Volume -cf` / (area_acres * (Rain_Event_Total / 12)*43560)   #- input parameters have been updated Jan 2020
  ) %>% 
    mutate(
      RC_Annual = `AnnualFlow-cf`/ (area_acres * (Rain_Gage_Total/12)*43560)
    )
  #mutate(
    #RC_Actual2 = `Storm Volume`/ (area_acres * (Rain_Event_Total / 12)*43560)  #volume sampled is not the entire storm
  #)

  # Runoff coefficient 'RC' modeled used for report
RC_M <- landuse_lspc_trib_2022 %>% 
  group_by(FACILITYID) %>%  
  filter(!is.na(RC)) %>%
  summarise(
    RC_M = sum(RC * AREA) / sum(AREA)
  )

write_csv(RC_M, paste0(getwd(), '/Output_202021/RC_M.csv'))



#Runoff coefficient use adjusted for comparison



# Combine modeled and actual 'RC', then calculate 'RC' correction factor  #Actual discharge incorrect - input parameters have been updated Jan 2020
stn_RC_202021 <- stn_RC_202021 %>% 
  left_join(
    .,
    RC_M,
    by = c('Station' = 'FACILITYID')
  ) %>% 
  mutate(
    RC_CF = RC_Annual / RC_M
  )
  
RC <- sbpat_landuse_Geosyntec %>%
  select(RC_Group, RC) %>%
  unique()
  
# Leave modeled (Geosyntec) RC as-is without correction factors for v2 analysis  sg:how is this different from landuse_rcGeosyntec - if RC_Actual were more accurate, than use the adjusted RC (Geosyntec) with CF applied
RC_WMA <- landuse_lspc_trib_2022 %>% 
  group_by(RC_Group, LSPC_LU_DESC, FACILITYID) %>%
  summarise(
    RC_WMA = sum(RC * AREA) / sum(AREA))

RC_Annual_M <- stn_RC_202021 %>%  #show RC for each station on SWN
  select(Station, RC_Annual, area_acres) %>%
  group_by(Station) %>%  
  filter(!is.na(RC_Annual)) %>%
  summarise(
    RC_Ann_M = sum(RC_Annual * area_acres) / sum(area_acres)
  )
 
# weight by actual, and then by corrected RC


stn_RC_202021<-stn_RC_202021 %>% 
  left_join(
    .,
    RC_WMA,
    by=c('Station'='FACILITYID')) %>%
  filter(!is.na(Station)) %>%
  left_join(
    ., 
    RC_Annual_M, 
    by=c('Station' = 'Station')
  )

write_csv(stn_RC_202021, paste0(getwd(), '/Output_202021/stn_RC_202021.csv'))

landuse_wcf<-landuse_lspc_trib_2022 %>% 
  right_join(
    .,
    stn_RC_202021,
    by = c('FACILITYID' = 'Station', 'RC'='RC_WMA', 'LSPC_LU_DESC'='LSPC_LU_DESC', 'RC_Group')
  ) %>%
  mutate (
    RCandCF=RC_CF*RC
  ) %>%
  filter(!is.na(RC_Annual)) 
  
#create corrected WMA RCs 
landuse_wcf2 <- landuse_wcf %>% 
 select(FACILITYID, RCandCF, AREA, RC_Group) %>% 
  filter(!is.na(AREA)) %>%
  group_by(RC_Group) %>%
  filter(!is.na(RCandCF)) %>%
  summarise(
    RC_WMAcf = sum(RCandCF * AREA) / sum(AREA))   #add in row for RC for agriculture from standard RC (0.222)


landuse_ag <- data.frame(
  RC_Group = c('Agriculture'),
  RC_WMAcf = c(0.222),
  stringsAsFactors = FALSE
)

landuse_wcf2 <- bind_rows(landuse_ag, landuse_wcf2)

getwd()

write_csv(landuse_wcf2, paste0(getwd(), '/Output_202021/landuse_wcf2.csv'))


RC_WMAcf_M2 <- landuse_lspc_trib_2022 %>% 
  select(RC_Group, FACILITYID, AREA) %>% 
  left_join(., landuse_wcf2, 
            by=c('RC_Group' = 'RC_Group')) %>% 
  
  group_by(FACILITYID) %>%
  summarise(
    RC_WMAcf_M = sum(RC_WMAcf * AREA) / sum(AREA))


write_csv(RC_WMAcf_M, paste0(getwd(), '/Output_202021/RC_WMAcf_M.csv'))

write_csv(RC_WMAcf_M2, paste0(getwd(), '/Output_202021/RC_WMAcf_M2.csv'))


stn_RC_202021b<-
  full_join(
    stn_RC_202021,
    landuse_wcf2,
    by = c('RC_Group' = 'RC_Group')) %>%
  left_join(., RC_WMAcf_M, by=c('Station' = 'FACILITYID')) %>%
  left_join(., RC_WMAcf_M2, by= c('Station' = 'FACILITYID', 'RC_Group', 'RC_WMAcf'))
         
# Total stormwater runoff for each station, use RC_M for v2 analysis
stn_RC_202021b <- stn_RC_202021b %>% 
  mutate(`Stormwater Volume (acre-ft)_RC` = RC_WMA * AREA*Rain_Gage_Total/12) %>%
  mutate(
    `Stormwater Volume (acre-ft)_RC_M` = RC_M * area_acres * (Rain_Gage_Total/ 12)
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_Annual` = RC_Annual * area_acres * (Rain_Gage_Total/ 12)
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf` = RC_WMAcf * AREA * (Rain_Gage_Total/ 12)
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf_M` = RC_WMAcf_M * area_acres * (Rain_Gage_Total/ 12))
# Leave modeled (Geosyntec) RC as-is without correction factors for v2 analysis  
write_csv(stn_RC_202021b, paste0(getwd(), '/Output_202021/stn_RC_202021b.csv'))


##  ............................................................................
##  EMC and loadings                                                        ####
library(tidyverse)
# Annual loading

emc <- emc %>%
  mutate(
    `Composite Begin` = as.Date(`Composite Start Time`, format = "%m/%d/%Y")) %>%
  mutate(
    `Composite End` =as.Date(`Composite End Time`, format = "%m/%d/%Y"))


stn_load2 <- emc %>% 
  select(-`Stormflow Volume -cf`, -contains('Field')) %>% 
  gather(Parameter, Value, `d10-Acenaphthene - ng/L`:`TN - mg/L`) %>% 
  filter(!is.na(Value)) %>% 
  separate(Parameter, c('Parameter', 'Units'), sep = ' - ') %>% 
  separate(Value, c("Qualifier", "Result"), "(?<=[<|>|>=]) ?(?=[0-9])") %>% 
  mutate(
    Result = as.numeric(ifelse(is.na(Result), Qualifier, Result)),
    Qualifier = ifelse(grepl('>|<|>=', Qualifier), Qualifier, NA),
    Result = ifelse(!(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli')),
                    ifelse(grepl('<', Qualifier),
                           Result / 2,
                           Result),
                    Result)
  ) %>% 
 mutate(
   Result= ifelse(grepl('e', Qualifier), 
         2.5, Result),
   Result)%>%
    mutate(
      Result = ifelse(Parameter == 'Phosphorus as PO4',
                      Result * .3262,
                      Result),
      Parameter = ifelse(Parameter == 'Phosphorus as PO4',
                         'TP',
                         Parameter)) %>%         
  filter(Parameter != 'Turbidity'& Parameter != 'Calcium, Total' & Parameter != 'SpecificConductivity' & Parameter != 'pH')

str(stn_load2)
  
  
stn_load <- stn_load2 %>%    
  
  left_join(
    .,
    stn_RC_202021b,
    by = c('Station', 'Composite Begin', 'Composite End','Sample Type','Composite Start Time','Composite End Time','HSN', 'Fraction', 'Collected Date')
  ) %>% 
 filter(!is.na(`Stormflow Volume -cf`)) %>% 
  
  #mutate(`Flow Weighted Result` = Result * `Volume Weighting`) %>% 
  #select(Station, FacilityID, `G/C`, Date, `Composite Begin`, `Composite End`, Type, Parameter, Qualifier, Result, Units,`Volume Sampled -cf`, `Stormwater Volume (acre-ft)_RC_M`, area_acres, Cooperative, Rain_Gage_Total, RC_Actual, RC_M, RC_CF, RC_Group) %>% 
mutate(
  Load_RC = `Stormwater Volume (acre-ft)_RC`*Result
) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_Load_RC = mean(Load_RC)) %>% 
  ungroup() %>% 
  mutate(
    PL_RC = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                  (Avg_Load_RC) * 1233481.8511532 * 10,  
                  ifelse(Units == 'mg/L',  #loading in pounds#
                         (Avg_Load_RC) *  (1/453592) * (1233481.8511532),  
                         ifelse(Units == 'ug/L',
                                (Avg_Load_RC) * (1/10^3) * (1/453592) * (1233481.8511532),
                                ifelse(Units == 'ng/L',
                                       (Avg_Load_RC) * (1/10^6) * (1/453592) * (1233481.8511532),
                                       NA
                                )
                         )
                  )
    )) %>%   
mutate(
    Load_M = `Stormwater Volume (acre-ft)_RC_M`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_M = mean(Load_M)) %>% 
  ungroup() %>% 
  mutate(
    PL_M = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                       (Avg_M) * 1233481.8511532 * 10,  
                       ifelse(Units == 'mg/L',  #loading in pounds#
                              (Avg_M) *  (1/453592) * (1233481.8511532),  
                              ifelse(Units == 'ug/L',
                                     (Avg_M) * (1/10^3) * (1/453592) * (1233481.8511532),
                                     ifelse(Units == 'ng/L',
                                            (Avg_M) * (1/10^6) * (1/453592) * (1233481.8511532),
                                            NA
                                     )
                              )
                       )
    )) %>% 
  mutate(
    Load_Annual_Flow = `Stormwater Volume (acre-ft)_RC_Annual`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_Load_Annual = mean(Load_Annual_Flow)) %>% 
  ungroup() %>% 
  mutate(
    PL_AF = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                  (Avg_Load_Annual) * 1233481.8511532 * 10,  
                  ifelse(Units == 'mg/L',  #loading in pounds#
                         (Avg_Load_Annual) *  (1/453592) * (1233481.8511532),  
                         ifelse(Units == 'ug/L',
                                (Avg_Load_Annual) * (1/10^3) * (1/453592) * (1233481.8511532),
                                ifelse(Units == 'ng/L',
                                       (Avg_Load_Annual) * (1/10^6) * (1/453592) * (1233481.8511532),
                                       NA
                                )
                         )
                  )
    )) %>% 
  mutate(
    Load_Annual_FlowCF = `Stormwater Volume (acre-ft)_RC_WMAcf`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_Load_Annualcf = mean(Load_Annual_FlowCF)) %>% 
  ungroup() %>% 
  mutate(
    PL_AFcf = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                   (Avg_Load_Annualcf) * 1233481.8511532 * 10,  
                   ifelse(Units == 'mg/L',  #loading in pounds#
                          (Avg_Load_Annualcf) *  (1/453592) * (1233481.8511532),  
                          ifelse(Units == 'ug/L',
                                 (Avg_Load_Annualcf) * (1/10^3) * (1/453592) * (1233481.8511532),
                                 ifelse(Units == 'ng/L',
                                        (Avg_Load_Annualcf) * (1/10^6) * (1/453592) * (1233481.8511532),
                                        NA
                                 )
                          )
                   )
    )) %>%
mutate( 
  Load_Annual_FlowCF_M = `Stormwater Volume (acre-ft)_RC_WMAcf_M`*Result
) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_Load_Annualcf_M = mean(Load_Annual_FlowCF_M)) %>% 
  ungroup() %>% 
  mutate(
    PL_AFcf_M = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                     (Avg_Load_Annualcf_M) * 1233481.8511532 * 10,  
                     ifelse(Units == 'mg/L',  #loading in pounds#
                            (Avg_Load_Annualcf_M) *  (1/453592) * (1233481.8511532),  
                            ifelse(Units == 'ug/L',
                                   (Avg_Load_Annualcf_M) * (1/10^3) * (1/453592) * (1233481.8511532),
                                   ifelse(Units == 'ng/L',
                                          (Avg_Load_Annualcf_M) * (1/10^6) * (1/453592) * (1233481.8511532),
                                          NA
                                   )
                            )
                     )
    )) 


write_csv(stn_load, paste0(getwd(), '/Output_202021/stn_load.csv'))


#ignore columns with each parameter
str(stn_load)

# Annual loading for print


stn_load_print_GS_M <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_M`, RC_M, RC_Group, PL_M, PL_M) %>% 
  distinct() %>% 
  spread(Parameter, PL_M) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 

stn_load_print_Ann <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_Annual`,  RC_Group, PL_AF) %>% 
  distinct() %>% 
  spread(Parameter,PL_AF) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 
  

stn_load_print_WMAcf_M <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_WMAcf_M`,  RC_Group, PL_AFcf_M) %>% 
  distinct() %>% 
  spread(Parameter,PL_AFcf_M) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 




#filter(SampleType=="NA") %>%
  #write_csv(., paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output/stn_load_print.csv'))

  
  write_csv(stn_load_print_GS_M, paste0(getwd(), '/Output_202021/stn_load_print_GS_M.csv'))
  write_csv(stn_load_print_Ann, paste0(getwd(), '/Output_202021/stn_load_print_Ann.csv'))
  write_csv(stn_load_print_WMAcf_M, paste0(getwd(), '/Output_202021/stn_load_print_WMAcf_M.csv'))


  
  ## Event Mean Concentrations compared to sbpat land use EMC

  landuse_lspc <- landuse_lspc_trib_2022 %>%
    select(FACILITYID, RC_Group, EMC_Group, AREA)
  
stn_emc <- emc %>% 
  select(-`Stormflow Volume -cf`, -contains('Field')) %>% 
  gather(Parameter, Value, `d10-Acenaphthene - ng/L`:`TN - mg/L`) %>% 
  filter(!is.na(Value)) %>% 
  separate(Parameter, c('Parameter', 'Units'), sep = ' - ') %>% 
  separate(Value, c("Qualifier", "Result"), "(?<=[<|>|>=]) ?(?=[0-9])") %>% 
  mutate(
    Result = as.numeric(ifelse(is.na(Result), Qualifier, Result)),
    Qualifier = ifelse(grepl('>|<|>=', Qualifier), Qualifier, NA),
    Result = ifelse(!(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli')),
                    ifelse(grepl('<', Qualifier),
                           Result / 2,
                           Result),
                    Result)
  ) %>%  
  mutate(
    Result = ifelse(Parameter == 'Phosphorus as PO4',
                    Result * .3262,
                    Result),
    Parameter = ifelse(Parameter == 'Phosphorus as PO4',
                       'TP',
                       Parameter)) %>% 
    
  mutate(
    Result = ifelse(Units == 'ug/L',
                    Result / 1000,
                    Result),
    Units = ifelse(Units == 'ug/L',
                   'mg/L',
                   Units)
  ) %>% 
  left_join(., landuse_lspc, by = c('Station' = 'FACILITYID')) %>% 
  filter(Parameter %in% (sbpat_emc_Geosyntec  %>% .$Constituent  %>% unique(.))) %>% #match up parameter names
  filter(!(Parameter == 'Lead, Total' & Fraction == 'Dissolved')) %>% 

  full_join(., sbpat_emc_Geosyntec, by=c('Parameter' = 'Constituent', 'Fraction' = 'Fraction', 'Units' = 'Units', 'EMC_Group'
                                           )) 
 
# EMC modeled for outfall
# Using modeled RC (RC_M) for v2 only
saveRDS(stn_emc, file = 'Output_202021/stn_emc.rds')
write_csv(stn_emc, file = 'Output_202021/stn_emc.csv')


stn_RC_n <-stn_RC_202021b %>%
  select(Station, RC_M, RC_WMAcf_M) %>%
  distinct(Station, RC_M, RC_WMAcf_M, .keep_all = TRUE)

EMC_M <-  #The EMC for each sampled outfall is calculated by area weighting each land use category EMC in each outfall tributary 
  left_join(stn_RC_n,landuse_lspc_trib_2022, by = c("Station" = "FACILITYID")) %>% 
 
    select(AREA, EMC_Group, Station, RC_M, RC_WMAcf_M, RC, LSPC_LU_CODE) %>% 
  group_by(Station, EMC_Group) %>% 
  mutate(
    RC_Model_Outfall = RC_M
  ) %>% 
  mutate(
    SUM_Area = sum(AREA)  #area is already summed by EMC_Group and Facility ID
  ) %>% 
  distinct( .keep_all = TRUE) %>% 
  left_join(
    .,
    sbpat_emc_Geosyntec
  ) %>% 
  filter(!is.na(EMC)) %>% 
  mutate(
    EMC_Model_LU = RC_Model_Outfall * SUM_Area * EMC,   #uses standardized EMC to find specific EMC for outfall)  #units cancel below for area weighting
    EMC_Model_LU_divisor = RC_Model_Outfall * SUM_Area
  ) 
# do this to calculate another way%>%
  #mutate(
    #EMC_Model_LU2 = RC_Model_Outfall*Area*EMC, 
    #EMC_Model_LU_divisor2 = RC_Model_Outfall * SUM_Area
 # )

saveRDS(EMC_M, file = 'Output_202021/EMC_M.rds')
write_csv(EMC_M, path = 'Output_202021/EMC_M.csv')


# EMC modeled for outfall and EMC Correction factor
values <- list() 
values[['EMC_M']] <- 'Output_202021/EMC_M.rds'
EMC_M <-  readRDS(values[["EMC_M"]])


EMC_CF <- EMC_M %>% 
  group_by(Station, Constituent, Fraction) %>% 
  summarise(
    EMC_Model_Outfall = sum(EMC_Model_LU) / sum(EMC_Model_LU_divisor) #sums up for each land-use type
  ) %>% 
  left_join(
    .,
    stn_emc,
    by = c('Station', 'Constituent' = 'Parameter', 'Fraction')
  ) %>% 
  mutate(
    EMC_CF = Result / EMC_Model_Outfall
  ) %>% 
  group_by(Station, Constituent, Fraction, Units) %>% 
  summarise(
    EMC_CF = mean(EMC_CF)
  )

saveRDS(EMC_CF, file = 'Output_202021/EMC_CF.rds')
write_csv(EMC_CF, file = 'Output_202021/EMC_CF.csv') 

values <- list() 
values[['EMC_CF']] <- 'Output_202021/EMC_CF.rds'
EMC_CF <-  readRDS(values[["EMC_CF"]])

# Corrected sbpat EMC and wma EMC  #uses measured EMCs and averages to get an EMC for the entire area
EMC_WMAb <- EMC_M %>% 
  left_join(
    .,
    EMC_CF %>% 
      ungroup() %>% 
      select(Station, Constituent, Fraction, Units, EMC_CF)
  ) %>% 
  mutate(
    EMC_Adj = EMC * EMC_CF
  ) %>% 
  ungroup() %>% 
  mutate(
    EMC_Model_LU = RC_Model_Outfall * AREA * EMC_Adj,
    EMC_Model_LU_divisor = RC_Model_Outfall * AREA
  ) %>% 
  filter(!is.na(EMC_Model_LU)) %>% 
  group_by(EMC_Group, Constituent, Fraction, Units)  
  
saveRDS(EMC_WMAb, file = 'Output_202021/EMC_WMAb.rds')
write_csv(EMC_WMAb, file = 'Output_202021/EMC_WMAb.csv')

  
  EMC_WMA_Ann <- EMC_WMAb_Ann %>%   
  summarise(
    EMC_WMA_LU = sum(EMC_Model_LU) / sum(EMC_Model_LU_divisor)  #overall land use category EMC values for each parameter
  )

saveRDS(EMC_WMA_Ann, file = 'Output_202021/EMC_WMA_Ann.rds')
write_csv(EMC_WMA_Ann, file = 'Output_202021/EMC_WMA_Ann.csv') 



## WMA and overall analysis

# WMA Stormwater Volume (acre-ft)s per jurisdiction  stormwater volume by EMC_Group
# Combine jurisdicton land use with  rc and sbpat emc categories and run-off coefficient  #shows Jurisdiciton, EMC_Group_Area, RC_Group
juris_loada <-landuse_lspc_juris_2022 %>%
  group_by(jurisdicti, EMC_Group, RC) %>%   #LSPC_LU_CODE brings in additional codes for an EMC_Group
  summarise(EMC_Group_Area = sum(acres)) %>%
   mutate(
    Area_Rainfall = 6.7, #average of rainfall event days >0.1" update every year avg from cooperative obs stations
    Juris_LU_Volume = EMC_Group_Area * RC * (Area_Rainfall / 12)  #all-modeled  ##copy and paste output below to notepad and open in Excel - data-frame not being created
  ) %>%
  ungroup()

write_excel_csv(juris_loada, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/juris_loada.csv'), na = '')
  
juris_volume <- juris_loada  %>% 
  group_by(jurisdicti) %>% 
  summarise(
    `Stormwater Volume (acre-ft)` = sum(Juris_LU_Volume)
  ) %>% 
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH')

# WMA pollutant load per jurisdiction  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
juris_load <- juris_loada %>% 
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH') %>%
  full_join(., juris_volume) %>%
  
  left_join(
    .,
    EMC_WMA
  ) %>% 
  mutate(
    LU_Load = EMC_WMA_LU * Juris_LU_Volume
  ) %>% 
  mutate(
    PL_M = ifelse(Constituent %in% c('Fecal coliforms'),
                       (LU_Load) * 1233481.8511532 * 10,
                       ifelse(Units == 'mg/L',
                              (LU_Load) * (1/453592) * (1233481.8511532),
                                            NA
                                     )
                              )
                       )
 
write_excel_csv(juris_load, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/juris_load.csv'), na = '')


# WMA pollutant load per tributary  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
 
landuse_lspc_stn_area<-landuse_lspc_trib_2022 %>%  #see line 235 
  group_by(FACILITYID, EMC_Group, RC) %>%   #LSPC_LU_CODE brings in additional codes for an EMC_Group
  summarise(EMC_Group_Area = sum(AREA)) %>%
  mutate(
    Area_Rainfall = 6.7, #average of co-operative gages
    Trib_LU_Volume = EMC_Group_Area * RC * (Area_Rainfall / 12)  #all-modeled  ##copy and paste output below to notepad and open in Excel - data-frame not being created
  )


Trib_LU_Volume <- landuse_lspc_stn_area %>% 
  group_by(FACILITYID) %>% 
  summarise(
    `Stormwater Volume (acre-ft)` = sum(Trib_LU_Volume)
  )  
 

trib_load <- landuse_lspc_stn_area %>% 
  left_join(
    .,
    EMC_WMA
  ) %>% 
  mutate(
    LU_Load = EMC_WMA_LU * Trib_LU_Volume
  ) %>% 
  mutate(
    PL_M = ifelse(Constituent %in% c('Fecal coliforms'),
                       (LU_Load) * 1233481.8511532 * 10,
                       ifelse(Units == 'mg/L',
                              (LU_Load) * (1/453592) * (1233481.8511532),
                                            NA
                                     )
                              )
                       ) %>%
select(-EMC_WMA_LU,-LU_Load) %>%

unite(Constituent, Constituent:Units, sep =' - ') %>%  
  spread(Constituent, PL_M)

write_csv(trib_load, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/TribLoadsLU_Geosyntec.csv'))


## Summary tables Jurisdiction  

jurisformat<-     #shows distinct EMC_Group and RC_Group and JURISDICTI
  merge(   
  (sbpat_landuse_Geosyntec  %>% select(EMC_Group, RC_Group)),
  (landuse_lspc_juris_2022 %>% ungroup() %>% select(jurisdicti) %>% distinct(jurisdicti)) 
)  %>%
 distinct(jurisdicti, EMC_Group, RC_Group)


jurisformat2<-    #joins to runoff coefficients
  left_join(
    jurisformat,landuse_Geosyntec,
    by=c('RC_Group'='RC_Group')) %>%
            
            ungroup() %>% 
            select(EMC_Group, RC_Group, RC)  %>% 
            distinct()

jurisformat3<-  #joins to get area and volumes     
left_join(
      jurisformat2,
      (juris_load %>% 
         ungroup() %>% 
         select(jurisdicti, EMC_Group, RC, EMC_Group_Area, Juris_LU_Volume))
    ) 

jurisformat3<-
  distinct(jurisformat3, EMC_Group_Area, .keep_all=TRUE )  %>%
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH')

jurisformat4<-  #joins to loads
  left_join(
    jurisformat3, 
    juris_load) %>%
  select(jurisdicti, EMC_Group, RC, EMC_Group_Area, RC_Group, Constituent, Fraction, Units,Juris_LU_Volume, PL_M) %>%
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH') %>%
  filter(!is.na(Constituent)) %>%
  filter(!is.na(Fraction)) %>%
  filter(!is.na(Units)) %>%
  filter(!is.na(PL_M))
   
  
jurisformat5<- jurisformat4 %>%            
       unite(Parameter, Constituent:Units, sep =' - ') %>%  
       spread(Parameter, PL_M) 
  write_csv(jurisformat5, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/JurisLoadsLU.csv'))  #units are in pounds, not mg/L
  
  
 #write.table(., file = 'clipboard', sep = '\t')  ##Table columns print out messed up

## Summary tables Tributary
 
  tribformat<-    #joins RC_group to runoff coefficients
    left_join(
      trib_load,landuse_Geosyntec,
      by=c('RC'='RC')) %>%
    filter(!is.na(RC))
  
 tribformat2<-  #get RC_M     
    left_join(
      tribformat,
      RC_M
    ) %>%
   select(-'NA - NA - NA') %>%
   ungroup()

 str(tribformat2)
 
 #try to organize columns
 
# tribformat2<-tribformat2[, c("FACILITYID", "Area_Rainfall", "Trib_LU_Volume", "EMC_Group", "RC_Group", "EMC_Group_Area", "RC", "RC_M", "Copper, Total - Dissolved - mg/L" :"Zinc, Total - Total - mg/L:")]
 
 write_csv(tribformat2, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/TribLoadsLU_Geosyntec.csv'))
 

 
 tribformat3<-  #get RC_WMA (one RC for the entire WMA)
   left_join(
     tribformat2, 
     RC_WMA, by=c('FACILITYID'='FACILITYID', 'RC_Group'='RC_Group'))
 
  write_csv(tribformat3, paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output_202021/TribLoadsLU_Geosyntec.csv'))
  
  
  
## landuse areas by outfall and by jurisdiction - EMC_Group Areas aren't equivalent by jurisdiction and by outfall
landuse_lspc_stn_202122 %>% 
  group_by(EMC_Group) %>% 
  summarise(Sum = sum(SUM_Area_ACRES.y))

landuse_lspc_juris %>% 
  group_by(EMC_Group) %>% 
  summarise(Sum = sum(EMC_Group_Area))

