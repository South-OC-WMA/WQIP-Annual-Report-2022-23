##  Determine runoff coefficients                                           ####

source(paste0(wd, '/WetWeatherLoadingCalcs/RscriptsWet_202223/1WW_dataimport.R'))


EMCRainTot2 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/EMCRainTot2.csv'))
landuse_lspc_trib_2022 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_trib_2023.csv'))


                     # Runoff coefficient 'RC' reality  #43560 cf in an acre-foot
stn_RC_202223 <- EMCRainTot2 %>% 
  mutate(
    RC_Event = `Stormflow Volume -cf` / (area_acres * (Rain_Event_Total / 12)*43560)   #- input parameters have been updated Jan 2020
  ) 
  #mutate(  
   # RC_Annual = `AnnualFlow-cf`/ (area_acres * (Rain_Gage_Total/12)*43560) Run if Annual flow data available
#  )
#mutate(
#RC_Actual2 = `Storm Volume`/ (area_acres * (Rain_Event_Total / 12)*43560)  #volume sampled is not the entire storm
#)

# Runoff coefficient 'RC' modeled used for report
RC_M <- landuse_lspc_trib_2023 %>% 
  group_by(FACILITYID) %>%  
  filter(!is.na(RC)) %>%
  summarise(
    RC_M = sum(RC * AREA) / sum(AREA)
  )

write_csv(RC_M, paste0(wd, '/WetWeatherLoadingCalcs/Output/RC_M.csv'))

RC_WMA <- landuse_lspc_trib_2023 %>% 
  group_by(RC_Group, LSPC_LU_DESC, FACILITYID) %>%
  summarise(
    RC_WMA = sum(RC * AREA) / sum(AREA))

#Runoff coefficient use adjusted for comparison



# Combine modeled and actual 'RC', then calculate 'RC' correction factor  #Actual discharge incorrect - input parameters have been updated Jan 2020
stn_RC_202223 <- stn_RC_202223 %>% 
  left_join(
    .,
    RC_M,
    by = c('Station' = 'FACILITYID')
  ) %>% 
  left_join(
    ., RC_WMA, by=c('Station' = 'FACILITYID')
  ) %>%
  mutate(
    RC_CF = (RC_Event/RC_M),  na.rm=TRUE
  )  %>%
  left_join(
    ., landuse_lspc_trib_2023, by=c('Station'= 'FACILITYID', 'RC_Group', 'LSPC_LU_DESC')
  ) %>%
select(-RC) %>%
  mutate (
    RCandCF=(RC_CF*RC_WMA), na.rm=TRUE
  ) %>%
  group_by(Station) %>%
  mutate(RC_Mcf = sum((RCandCF * AREA)/sum(AREA)), na.rm=TRUE) #don't know why RC_Mcf doesn't calculate

write_csv(stn_RC_202223, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223.csv'))

RC <- sbpat_landuse_Geosyntec %>%
  select(RC_Group, RC) %>%
  unique()

# Leave modeled (Geosyntec) RC as-is without correction factors for v2 analysis  sg:how is this different from landuse_rcGeosyntec - if RC_Actual were more accurate, than use the adjusted RC (Geosyntec) with CF applied

#Use RCs created from flow data - run if annual flow data available

#RC_Annual <- stn_RC_202223 %>%  #show RC for each station on SWN
  #select(Station, RC_Annual, area_acres) %>%
 # group_by(Station) %>%  
 # filter(!is.na(RC_Annual)) %>%
  #summarise(
  #  RC_Ann = sum(RC_Annual * area_acres) / sum(area_acres)
#  )

# weight by actual, and then by corrected RC


landuse_wcf<-landuse_lspc_trib_2023 %>% 
  right_join(
    .,
    stn_RC_202223,
    by = c('FACILITYID' = 'Station', 'RC'='RC_WMA', 'LSPC_LU_DESC'='LSPC_LU_DESC', 'RC_Group', 'AREA', 'LSPC_LU_CODE', 'EMC_Group', 'Shape_Area', 'PERCENTAGE')
  ) %>%
  #mutate (
    #RCandCF=RC_CF*RC
 # ) %>%
  #filter(!is.na(RC_Event))



#create corrected WMA RCs 
landuse_wcf2 <- landuse_wcf %>% 
  select(FACILITYID, RCandCF, AREA, RC_Group) %>% 
  filter(!is.na(AREA)) %>%
  group_by(RC_Group) %>%
  filter(!is.na(RCandCF)) %>%
  summarise(
    RC_WMAcf = sum(RCandCF * AREA) / sum(AREA))   #add in row for RC for agriculture from standard RC (0.222)



write_csv(landuse_wcf2, paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_wcf2.csv'))

landuse_agwat <- data.frame(
  RC_Group = c('Agriculture', 'Water'),
  RC_WMAcf = c(0.222, 0.797),     #use the original land-use RC from Geosyntec.  ?use the RC computed each year for Agriculture in landuse_wcf2_noAF
  stringsAsFactors = FALSE
)

landuse_wcf2 <- bind_rows(landuse_agwat, landuse_wcf2)

getwd()

write_csv(landuse_wcf2, paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_wcf2.csv'))


RC_WMAcf_M <- landuse_lspc_trib_2023 %>% 
  select(RC_Group, FACILITYID, AREA) %>% 
  left_join(., landuse_wcf2, 
            by=c('RC_Group' = 'RC_Group')) %>% 
  
  group_by(FACILITYID) %>%
  summarise(
    RC_WMAcf_M = sum(RC_WMAcf * AREA) / sum(AREA))


RC_WMAcf_M2 <- landuse_lspc_trib_2023 %>% 
  select(RC_Group, FACILITYID, AREA) %>% 
  left_join(., landuse_wcf2, 
            by=c('RC_Group' = 'RC_Group'))


write_csv(RC_WMAcf_M, paste0(wd, '/WetWeatherLoadingCalcs/Output/RC_WMAcf_M.csv'))

write_csv(RC_WMAcf_M2, paste0(wd, '/WetWeatherLoadingCalcs/Output/RC_WMAcf_M2.csv'))


stn_RC_202223b<-
  full_join(
    landuse_wcf,
    landuse_wcf2,
    by = c('RC_Group' = 'RC_Group')) %>%
  left_join(., RC_WMAcf_M, by=c('FACILITYID')) %>%
  left_join(., RC_WMAcf_M2, by= c('FACILITYID', 'RC_Group', 'RC_WMAcf', 'AREA'))


stn_RC_202223b<-
  
  left_join(RC_WMAcf_M2, RC_WMAcf_M, by=c('FACILITYID')) %>%
  left_join(landuse_wcf, ., by=c('FACILITYID' = 'FACILITYID', 'RC_Group', 'AREA'))  #check this is correct



# Total stormwater runoff for each station, use RC_M for v2 analysis
stn_RC_202223b <- stn_RC_202223b %>% 
  mutate(`Stormwater Volume (acre-ft)_RC` = RC * AREA*Rain_Gage_Total/12) %>%
  mutate(
    `Stormwater Volume (acre-ft)_RC_M` = RC_M * area_acres * (Rain_Gage_Total/ 12)
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_Event` = RC_Event * area_acres * (Rain_Gage_Total/ 12)
  ) %>%
  #mutate(`Stormwater Volume (acre-ft)_RC_Mcf` = RC_Mcf * area_acres * (Rain_Gage_Total/ 12)
  #) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf` = RC_WMAcf * AREA * (Rain_Gage_Total/ 12)
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf_M` = RC_WMAcf_M * area_acres * (Rain_Gage_Total/ 12))
# Leave modeled (Geosyntec) RC as-is without correction factors for v2 analysis  
write_csv(stn_RC_202223b, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b.csv'))
saveRDS(stn_RC_202223b, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b.rds'))



#use RCs generated without using annual flow (method used in Annual Reports)

#run landuse_wcf without filter at last line

str(landuse_lspc_trib_2023)
str(stn_RC_202223)


landuse_all <-landuse_lspc_trib_2023 %>% 
  right_join(
    .,
    stn_RC_202223,
    by = c('FACILITYID' = 'Station', 'LSPC_LU_DESC', 'LSPC_LU_CODE','EMC_Group', 'RC_Group',  'AREA', 'Shape_Area', 'PERCENTAGE')
  ) 


 RC <- RC
 
stn_RC_202223b_all <-  
  full_join(
    landuse_all,
    landuse_wcf2,
    by = c('RC_Group')) %>%
    left_join(., RC_WMAcf_M, by=c('FACILITYID')) %>%
  left_join(., RC_WMAcf_M2, by= c('FACILITYID', 'RC_Group', 'RC_WMAcf', 'AREA')) %>%
  mutate(`Stormwater Volume (acre-ft)_RC` = RC * AREA*Rain_Gage_Total/12) %>%  #individual land-use
  mutate(`Stormwater Volume (acre-ft)_RC_M` = RC_M * area_acres * (Rain_Gage_Total/ 12)   #total tributary
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_Event` = RC_Event * area_acres * (Rain_Gage_Total/ 12)    #total tributary
  ) %>%
  #mutate(`Stormwater Volume (acre-ft)_RCandCF` = RCandCF * AREA * (Rain_Gage_Total/ 12)    #individual land-use, corrected using annual flow 
  #) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_Mcf` = RC_Mcf * area_acres * (Rain_Gage_Total/ 12)    #total tributary 
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf` = RC_WMAcf * AREA * (Rain_Gage_Total/ 12) #individual land-use RC (generated from stations with annual flow data) applied to all tributaries
  ) %>%
  mutate(`Stormwater Volume (acre-ft)_RC_WMAcf_M` = RC_WMAcf_M * area_acres * (Rain_Gage_Total/ 12)  #total tributary
  ) %>%
  filter(!is.na('FACILITYID'))

write_csv(stn_RC_202223b_all, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b_all.csv'))
saveRDS(stn_RC_202223b_all, paste0(wd,'/WetWeatherLoadingCalcs/Output/stn_RC_202223b_all.rds'))

#needs RC_WMAcf and RC_WMAcf_M




