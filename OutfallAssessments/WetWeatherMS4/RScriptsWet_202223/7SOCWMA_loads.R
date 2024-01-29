## WMA and overall analysis



landuse_lspc_juris_2023 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_juris_2023.csv'))
SWVolumes <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/SWVolumes.csv'))
EMC_WMA <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMA_RC_WMAcf_noHornoOut.csv'))


# WMA pollutant load per tributary  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot



trib_load <- SWVolumes %>% 
  left_join(
    .,
    EMC_WMA
  ) %>% 
  mutate(
    LU_Load = EMC_WMA_LU * TotalVol_RC
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

write_csv(trib_load, paste0(wd, '/WetWeatherLoadingCalcs/Output/TribLoadsLU_Geosyntec.csv'))

# WMA pollutant load per jurisdiction  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
juris_loada <-landuse_lspc_juris_2023 %>%
  group_by(jurisdicti, EMC_Group, RC) %>%   #LSPC_LU_CODE brings in additional codes for an EMC_Group
  summarise(EMC_Group_Area = sum(acres)) %>%
  mutate(
    Area_Rainfall = 28.29, #average of rainfall event days >0.1" update every year avg from cooperative obs stations
    Juris_LU_Volume = EMC_Group_Area * RC * (Area_Rainfall / 12)  #all-modeled  ##copy and paste output below to notepad and open in Excel - data-frame not being created
  ) %>%
  ungroup()

write_csv(juris_loada, paste0(wd, '/WetWeatherLoadingCalcs/Output/juris_loada.csv'), na = '')

juris_volume <- juris_loada  %>% 
  group_by(jurisdicti) %>% 
  summarise(
    `Stormwater Volume (acre-ft)` = sum(Juris_LU_Volume)
  ) %>% 
  filter(
    jurisdicti != 'IRVINE',
    jurisdicti != 'NEWPORT BEACH')

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

write_csv(juris_load, paste0(wd, '/WetWeatherLoadingCalcs/Output/juris_load.csv'), na = '')


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

write_csv(tribformat2, paste0(wd, '/WetWeatherLoadingCalcs/Output/TribLoadsLU_Geosyntec.csv'))



tribformat3<-  #get RC_WMA (one RC for the entire WMA)
  left_join(
    tribformat2, 
    RC_WMA, by=c('FACILITYID'='FACILITYID', 'RC_Group'='RC_Group'))

write_csv(tribformat3, paste0(wd, '/WetWeatherLoadingCalcs/Output/TribLoadsLU_Geosyntec.csv'))


## Summary tables Jurisdiction  

jurisformat<-     #shows distinct EMC_Group and RC_Group and JURISDICTI
  merge(   
    (sbpat_landuse_Geosyntec  %>% select(EMC_Group, RC_Group)),
    (landuse_lspc_juris_2023 %>% ungroup() %>% select(jurisdicti) %>% distinct(jurisdicti)) 
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
write_csv(jurisformat5, paste0(wd, '/WetWeatherLoadingCalcs/Output/JurisLoadsLU.csv'))  #units are in pounds, not mg/L


#write.table(., file = 'clipboard', sep = '\t')  ##Table columns print out messed up



## landuse areas by outfall and by jurisdiction - EMC_Group Areas aren't equivalent by jurisdiction and by outfall
landuse_lspc_stn_202122 %>% 
  group_by(EMC_Group) %>% 
  summarise(Sum = sum(SUM_Area_ACRES.y))

landuse_lspc_juris %>% 
  group_by(EMC_Group) %>% 
  summarise(Sum = sum(EMC_Group_Area))
