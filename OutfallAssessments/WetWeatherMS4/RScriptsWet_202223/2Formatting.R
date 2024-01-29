#   Formatting and intermediate data products                               ####

source(paste0(wd, '/WetWeatherLoadingCalcs/RscriptsWet_202223/1WW_dataimport.R'))

emc <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/emc.csv'))
stn_acres <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_acres.csv'))
stn_near <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Input/Rain/outfall_near_rain_gage.csv'))
dr_rain <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Input/Rain/SOCWMA_coop_202223.csv'), skip = 3, col_names=c('Time', 'TRABUCO', 'SULPHURCREEK', 'ELTORO', 'PALISADES', 'LAGUNABEACH', 'DANAPOINT', 'TUCKER'))

# Combine stations and acreage

EMCacres <- left_join(
  emc,
  stn_acres,
  by = c('Station' = 'FACILITYID')
)  #check table to ensure joined correctly

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
values <- list()
values[['ReportStart']]       <- '2022-10-01'
values[['ReportEnd']]         <- '2023-09-30'

rain_total <- dr_rain %>% 
  select(-c(X9:X13)) %>% 
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

write.csv(rain_total, paste0(wd, '/WetWeatherLoadingCalcs/Output/rain_total.csv'))

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
#select(Station, FacilityID, Date, `G/C`, `Composite Begin`, `Composite End`, `Volume Sampled -cf`, Type) %>%
  #distinct(Station, FacilityID, Date,`Composite Begin`, `Composite End`, `Volume Sampled -cf`, `G/C`, Type) 
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

write.csv(rain_total, paste0(wd, '/WetWeatherLoadingCalcs/Output/rain_total.csv'))
saveRDS(EMCRainTot2, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMCRainTot2.rds'))
write.csv(EMCRainTot2, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMCRainTot2.csv'))


#jurisdictional land-use

landuse_jurisdiction_2023 <- read_csv(paste0(wd, '/juris_lu_summary.csv'))


# Combine jurisdiction land use with Geosyntec rc and sbpat emc categories and run-off coefficient  see line 582
landuse_lspc_juris_2023<-distinct(sbpat_landuse_Geosyntec, LSPC_LU_CODE, .keep_all=TRUE) %>%
  select(LSPC_LU_DESC, LSPC_LU_CODE, RC_Group, EMC_Group
  ) %>%
  left_join(., landuse_Geosyntec) %>%
  left_join(
    .,
    landuse_jurisdiction_2023,
    by = c('LSPC_LU_DESC'='lspc_lu_edit')) 
#%>% 
#ungroup()
write_csv(landuse_lspc_juris_2023, paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_juris_2023.csv'))

SOCWMA_lu<-landuse_lspc_juris_2023 %>%
  group_by(EMC_Group) %>%
  mutate(EMC_groupArea=sum(acres)) %>%
  ungroup() %>%
  select(EMC_Group, EMC_groupArea) %>%
  unique()

write_csv(SOCWMA_lu, paste0(wd, '/WetWeatherLoadingCalcs/Output/SOCWMA_lu.csv'))


# Combine station land use with Geosyntec rc and sbpat emc categories and run-off coefficient
landuse_lspc_trib_2023<-distinct(sbpat_landuse_Geosyntec, LSPC_LU_CODE, .keep_all=TRUE) %>%
  select(LSPC_LU_DESC, LSPC_LU_CODE, RC_Group, EMC_Group
  ) %>%
  left_join(., landuse_Geosyntec) %>%
  left_join(
    .,
    landuse_trib_202223,
    by = c('LSPC_LU_DESC'='LSPC_LU_EDIT')) 
#%>% 
#ungroup()
write_csv(landuse_lspc_trib_2023, paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_trib_2023.csv'))

landuse_lspc_trib_2023 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_trib_2023.csv'))

getwd()

MO<- "MajorOutfalls_2023.csv"
MO <- read_csv(MO) %>% 
  select(FACILITYID, JURISDICTI)

MO_LU <- landuse_lspc_trib_2023 %>% 
  left_join(., MO, by=c('FACILITYID')) %>%
  filter(!is.na(JURISDICTI))

write_csv(MO_LU, paste0(wd, '/WetWeatherLoadingCalcs/Output/MO_LU.csv'))
