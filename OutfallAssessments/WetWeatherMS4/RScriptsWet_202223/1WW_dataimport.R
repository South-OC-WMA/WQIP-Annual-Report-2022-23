#Project Set Up

#library(usethis) 
#usethis::edit_r_environ()

## when the tab opens up in R studio, add this to the 1st line: R_MAX_VSIZE=100Gb (or whatever memory you wish to allocate)

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

wd<-setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments")

#   ____________________________________________________________________________
#   Data Import                                                             ####

##  ..................................WetWeatherDataandSALsAssessment2022-23


# EMC data 
getwd()

emc1<- paste0(wd, '/WetWeatherLoadingCalcs/Input/WetWeatherDataandSALsAssessment2022-23.xlsx')

emc <- read_excel(emc1,'Horizon wide units') %>%
  mutate(`TN - mg/L` = `Nitrate + Nitrite as N - mg/L` + `Nitrogen, Total Kjeldahl - mg/L`) %>%
  filter(!is.na(Station))


emc$Station<-sub("L01-724-4", "L01-724-4 (L01P03)",emc$Station)
emc$Station<-sub("L01-728-5", "L01-728-5 (L01-DP)",emc$Station)
emc$Station<-sub("L02-166-2", "L02-166-2 (L02P25)",emc$Station)
#emc$Station<-sub("J07-9109-4", "J07-9109-4 (J07P02)",emc$Station)
emc$Station<-sub("J01-9992-1", "J01-9992-1 (J01P27)",emc$Station)
emc$Station<-sub("J07P02", "J07-9109-4 (J07P02)",emc$Station) 
emc$Station<-sub("L03P05", "L03-708-11 (L03P05)",emc$Station) 
#emc$Station<-sub("L03-708-11", "L03-708-11 (L03P05)",emc$Station) 
emc$Station<-sub("J03P01in", "J03-9216-2",emc$Station) 
emc$Station<-sub("J03P01in", "J03-9216-2",emc$Station) 


write_csv(emc, paste0(wd, '/WetWeatherLoadingCalcs/Output/emc.csv'))

getwd()
##  ............................................................................
##  Hydrologic Info                                                         ####

# Nearest coop rain gage to swDischargePoint Facility's
stn_near <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Input/Rain/outfall_near_rain_gage.csv'))
#stn_near$FacilityID<-sub("SC06-146-2", "SC06-146-2 (M00P02)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L01-724-4", "L01-724-4 (L01P03)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L01-728-5", "L01-728-5 (L01-DP)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L02-166-2", "L02-166-2 (L02P25)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("L03-708-11", "L03-708-11 (L03P05)",stn_near$FacilityID)
#stn_near$FacilityID<-sub("J07-9109-4", "J07-9109-4 (J07P02)",stn_near$FacilityID)

# Hydstra rainfall data Point format
#dr_rain <- read_csv('Outfall Assessments/WetWeatherLoadingCalcs/Input/Rain/SOCWMA_coop_202122.CSV', skip = 3, col_names = c('Time', 'Trabuco Forestry', 'Sulphur Creek Dam', 'El Toro', 'Palisades Reservoir', 'Laguna Beach', 'Dana Point', 'Upper Aliso')) 
dr_rain <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Input/Rain/SOCWMA_coop_202223.csv'), skip = 3, col_names=c('Time', 'TRABUCO', 'SULPHURCREEK', 'ELTORO', 'PALISADES', 'LAGUNABEACH', 'DANAPOINT', 'TUCKER'))

getwd()
##  ............................................................................
##  Land Use Info                                                           ####

# swDischargePoint tributary acreage 
stn_acres <- arc.open('SPOCDSQL1205.sde/OCEnvRes.OCENVRESUSER.swDischargePoint_Tributary') %>% 
  arc.select() %>% 
  tbl_df() %>% 
  select(GlobalID, FACILITYID, area_acres) %>%
  rename(GlobalID_tribarea=GlobalID)

stn_acres$FACILITYID<-sub("L02-366-1 (L02-P14)", "L02-366-1 (MVL02P14)",stn_acres$FACILITYID)
#stn_acres$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",stn_acres$FACILITYID) area not available


write.csv(stn_acres, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_acres.csv'))
# Land use runoff coefficients (Geosyntec)
landuse_Geosyntec <- data.frame(
  RC_Group = c('Agriculture', 'Commercial', 'Developed Open Space', 'Education', 'Freeway', 'Light Industrial', 'Multi-family Residential', 'Open Space', 'Single Family Residential', 'Streets and Roads', 'Water'),
  RC = c(0.222, 0.486, 0.217, 0.422, 0.450, 0.414, 0.485, 0.078, 0.405, 0.471, 0.797),
  stringsAsFactors = FALSE
)

# sbpat land use emc values 
sbpat_emc_Geosyntec <- read_excel(paste0(wd, '/WetWeatherLoadingCalcs/Input/sbpat_EMC_Geosyntec.xlsx')) %>% 
  gather(EMC_Group, EMC, `agriculture`:WATER) 

 write.csv(sbpat_emc_Geosyntec, paste0(wd, '/WetWeatherLoadingCalcs/Output/sbpat_emc_Geosyntec.csv'))

# Land use codes & runoff coefficient group (SBPAT, Geosyntec)
sbpat_landuse_Geosyntec <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Input/sbpat_landuse_Geosyntec.csv')) %>%
  mutate_each(funs(as.numeric), LSPC_LU_CODE) %>% 
  filter(!is.na(Description))

write.csv(sbpat_landuse_Geosyntec, paste0(wd, '/WetWeatherLoadingCalcs/Output/sbpat_landuse_Geosyntec.csv')
)

R9_Cities <- read_csv(paste0(wd, '/juris_area_summary.csv')) %>%
  select(jurisdicti, acres_juris_soc)


saveRDS(R9_Cities, paste0(wd, '/WetWeatherLoadingCalcs/Output/R9_Cities.rds'))
write_csv(R9_Cities, paste0(wd, '/WetWeatherLoadingCalcs/Output/R9_Cities.csv'))


# Land use by jurisdiction sum_Area_ACRES.x is the area within R9 after clip; Area is total acres (including outside of R9)
# Use codes from 


# swDischargePoint land use data
# land use information for each outfall tributary

landuse_trib_202223 <- read_csv(paste0(wd, '/trib_lu_area.csv'))  %>%
  select(-OBJECTID) %>%
  #%>%  #For area around Oso Reservoir, assume trib areas contribute to area for storm flow calcs, remove the areas only affecting dry weather loading calcs
  filter(FACILITYID!='L03-214-2')%>%
  filter(FACILITYID!='L03-073-3')%>%
  filter(FACILITYID!='L03-141-1')

landuse_trib_202223$FACILITYID<-sub("J01-9224-1", "J01-9224-1 (J01P24)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-9224-2", "J01-9224-2 (J01P25)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("K01-12177-1", "K01-12177-1 (K01P07)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("L01-766-2", "L01-766-2 (L01S06)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("L02-166-3", "L02-166-3 (L02P26)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("L02-366-1 (L02-P14)", "L02-366-1 (MVL02P14)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("L02-622-2", "L02-622-2 (L02P32)",landuse_trib_202223$FACILITYID)
#landuse_trib_202223$FACILITYID<-sub("L03-662-3", "L03-662-3 (L03P16)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("SC10-075-3", "SC10-075-3 (M00S01)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-10041-2", "J01-10041-2 (J03P13)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-9007-1", "J01-9007-1 (J02P05)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-10004-1", "J01-10004-1 (J01P01)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("I01-11343-2", "I01-11343-2 (I02P18)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-9264-1", "J01-9264-1 (J01P06)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("I01-11216-1", "I01-11216-1 (I02P13)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-9066-1", "J01-9066-1 (J01P04)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("J01-9364-3", "J01-9364-3 (J01P21)",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("L01-728-8", "L01-728-8d",landuse_trib_202223$FACILITYID)
landuse_trib_202223$FACILITYID<-sub("SC11-035-1", "SC11-035-1d",landuse_trib_202223$FACILITYID)


#filter(HRU_Composite_LSPC_LU_CODE!=80) #remove areas of water bodies

write.table(landuse_trib_202223, file = 'clipboard-16384', sep = '\t')

write_csv(landuse_trib_202223, paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_trib_202223.csv'))

getwd()