## WMA and overall analysis

landuse_lspc_juris_2023 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_juris_2023.csv'))


landuse_lspc_trib_2023 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_trib_2023.csv'))


landuse_wcf2 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_wcf2.csv'))

landuse_lspc_stn_area<-landuse_lspc_trib_2023 %>%  
  full_join(., landuse_wcf2) %>% #join in new WMA_RCs
  group_by(FACILITYID, EMC_Group, RC, RC_WMAcf) %>%   #LSPC_LU_CODE brings in additional codes for an EMC_Group
  summarise(EMC_Group_Area = sum(AREA)) %>%
  mutate(
    Area_Rainfall = 28.29, 
    Trib_LU_Volume_RC = EMC_Group_Area * RC * (Area_Rainfall / 12),
    Trib_LU_Volume_RC_WMAcf = EMC_Group_Area * RC_WMAcf * (Area_Rainfall / 12) #all-modeled  ##copy and paste output below to notepad and open in Excel - data-frame not being created
  ) %>%
  group_by(FACILITYID) %>%
  mutate(
    TotalVol_RC = sum(Trib_LU_Volume_RC)
  ) %>%
  mutate(
    TotalVol_RC_WMAcf = sum(Trib_LU_Volume_RC_WMAcf))
    
write_csv(landuse_lspc_stn_area, paste0(wd, '/WetWeatherLoadingCalcs/Output/SWVolumes.csv'))

Trib_LU_Volume <- landuse_lspc_stn_area %>% 
  group_by(FACILITYID) %>% 
  summarise(
    `Stormwater Volume (acre-ft_RC)` = sum(Trib_LU_Volume_RC)
  )  



# WMA Stormwater Volume (acre-ft)s per jurisdiction  stormwater volume by EMC_Group
# Combine jurisdicton land use with  rc and sbpat emc categories and run-off coefficient  #shows Jurisdiciton, EMC_Group_Area, RC_Group
juris_loada <-landuse_lspc_juris_2023 %>%
  group_by(jurisdicti, EMC_Group, RC) %>%   #LSPC_LU_CODE brings in additional codes for an EMC_Group
  summarise(EMC_Group_Area = sum(acres)) %>%
  mutate(
    Area_Rainfall = 28.29, #average of rainfall event days >0.1" update every year avg from cooperative obs stations
    Juris_LU_Volume = EMC_Group_Area * RC * (Area_Rainfall / 12)  #all-modeled  ##copy and paste output below to notepad and open in Excel - data-frame not being created
  ) %>%
  ungroup()

write_excel_csv(juris_loada, paste0(wd, '/WetWeatherLoadingCalcs/Output/juris_loada.csv'))

juris_volume <- juris_loada  %>% 
  group_by(jurisdicti) %>% 
  summarise(
    `Stormwater Volume (acre-ft)` = sum(Juris_LU_Volume)
  ) %>% 
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH')

