## Event Mean Concentrations compared to sbpat land use EMC

source(paste0(wd, '/WetWeatherLoadingCalcs/RscriptsWet_202223/1WW_dataimport.R'))

landuse_lspc_trib_2023 <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/landuse_lspc_trib_2023.csv'))
emc <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/emc.csv'))
sbpat_emc_Geosyntec <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/sbpat_emc_Geosyntec.csv'))

landuse_lspc <- landuse_lspc_trib_2023 %>%
  select(FACILITYID, LSPC_LU_DESC, RC_Group, EMC_Group, AREA) %>%
  ungroup()

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
saveRDS(stn_emc, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_emc.rds'))
write_csv(stn_emc, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_emc.csv'))


stn_RC_202223b_all <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b_all.csv'))

stn_RC_n <-stn_RC_202223b_all %>%
  select(FACILITYID, LSPC_LU_DESC, RC_Group, EMC_Group, RC, RC_M, RC_Event, RCandCF, RC_Mcf, RC_WMAcf, RC_WMAcf_M) %>%
  distinct(FACILITYID, LSPC_LU_DESC, RC_Group, EMC_Group, RC, RC_M, RC_Event, RCandCF, RC_Mcf, RC_WMAcf, RC_WMAcf_M, .keep_all = TRUE)

EMC_M <-  #The EMC for each sampled outfall is calculated by area weighting each land use category EMC in each outfall tributary 
  left_join(stn_RC_n,landuse_lspc, by = c("FACILITYID", 'LSPC_LU_DESC', 'RC_Group', 'EMC_Group')) %>% 
  
  select(AREA, EMC_Group, RC_Group, FACILITYID, RC, RC_M, RC_Event, RCandCF, RC_Mcf,RC_WMAcf_M, RC_WMAcf, LSPC_LU_DESC) %>% 
  group_by(FACILITYID, RC_Group) %>% 
  mutate(
    RC_Model_Outfall = RC_WMAcf  #toggle land-use RCs for standard and adjusted RC (RC, RC_Annual, RCandCF, RC_WMAcf) and update file name for save
  ) %>% 
  mutate(
    SUM_Area = sum(AREA)  #area is already summed by EMC_Group and Facility ID do this if a single RC is used 
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

saveRDS(EMC_M, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_RC_WMAcf.rds'))  #RC, RC_Annual, RCandCF, RC_WMAcf
write_csv(EMC_M, paste0(wd,  '/WetWeatherLoadingCalcs/Output/EMC_RC_WMAcf.csv'))


# EMC modeled for outfall and EMC Correction factor
values <- list() 
values[['EMC_RC_WMAcf']] <- paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_RC_WMAcf.rds')
EMC_M <-  readRDS(values[["EMC_RC_WMAcf"]])  #update for different RCs

values[['stn_emc']] <- paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_emc.rds')
stn_emc <-  readRDS(values[["stn_emc"]])  #update for different RCs

EMC_ModelOutfall <- EMC_M %>% 
  select(FACILITYID, Constituent, Fraction,EMC_Model_LU, EMC_Model_LU_divisor) %>% 
  group_by(FACILITYID, Constituent, Fraction) %>% 
  unique() %>%
  summarise(
    EMC_Model_Outfall = sum(EMC_Model_LU) / sum(EMC_Model_LU_divisor) #sums up for each land-use type
  )

saveRDS(EMC_ModelOutfall, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_ModelOutfall_RC_WMAcf.rds'))   #RC, RC_Annual, RCandCF, RC_WMAcf
write_csv(EMC_ModelOutfall, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_ModelOutfall_RC_WMAcf.csv'))



EMC_CF <- EMC_M %>% 
  group_by(FACILITYID, Constituent, Fraction) %>% 
  summarise(
    EMC_Model_Outfall = sum(EMC_Model_LU) / sum(EMC_Model_LU_divisor) #sums up for each land-use type
  ) %>% 
  left_join(
    .,
    stn_emc,
    by = c('FACILITYID'='Station', 'Constituent' = 'Parameter', 'Fraction')
  ) %>% 
  mutate(
    EMC_CF = Result / EMC_Model_Outfall
  ) %>% 
  group_by(FACILITYID, Constituent, Fraction, Units) %>% 
  summarise(
    EMC_CF = mean(EMC_CF)
  )

saveRDS(EMC_CF, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_CF_RC_WMAcf.rds'))    #RC, RC_Annual, RCandCF, RC_WMAcf
write_csv(EMC_CF, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_CF_RC_WMAcf.csv'))


values <- list()  
values[['EMC_RC_WMAcf']] <- paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_RC_WMAcf.rds')        #RC, RC_Annual, RCandCF, RC_WMAcf
EMC_M <-  readRDS(values[["EMC_RC_WMAcf"]])  #update for different RCs

values[['EMC_CF_RC_WMAcf']] <- paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_CF_RC_WMAcf.rds')       #RC, RC_Annual, RCandCF, RC_WMAcf
EMC_CF <-  readRDS(values[["EMC_CF_RC_WMAcf"]])

# Corrected sbpat EMC and wma EMC  #uses measured EMCs and averages to get an EMC for the entire area
EMC_WMAb <- EMC_M %>% 
  left_join(
    .,
    EMC_CF %>% 
      ungroup() %>% 
      select(FACILITYID, Constituent, Fraction, Units, EMC_CF)
  ) %>% 
  mutate(
    EMC_Adj = EMC * EMC_CF
  ) %>% 
  ungroup() %>% 
  mutate(
    EMC_Model_LU = RC_Model_Outfall * SUM_Area * EMC_Adj,
    EMC_Model_LU_divisor = RC_Model_Outfall * SUM_Area
  ) %>%  
  filter(!is.na(EMC_Model_LU)) 

EMC_ModelOutfall_adj <- EMC_WMAb %>%
  select(FACILITYID, Constituent, Fraction, Units, RC_Group, EMC_Model_LU, EMC_Model_LU_divisor) %>% 
  unique() %>% 
  group_by(FACILITYID, Constituent, Fraction, Units) %>% 
  mutate(EMC_ModelOutfall_adj=sum(EMC_Model_LU/sum(EMC_Model_LU_divisor)))   #find annual EMC for each outfall

EMC_WMAb <- EMC_ModelOutfall_adj %>%
  left_join(., EMC_WMAb, by=c('FACILITYID', 'Constituent', 'Fraction', 'Units', 'EMC_Model_LU', 'EMC_Model_LU_divisor', 'RC_Group')) 



saveRDS(EMC_WMAb, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMAb_RC_WMAcf.rds'))  #update by type of RC used: RC, RC_Annual, RCandCF, RC_WMAcf
write_csv(EMC_WMAb, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMAb_RC_WMAcf.csv'))

values <- list()  
values[['EMC_WMAb_RC_WMAcf']] <- paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMAb_RC_WMAcf.rds')       #RC, RC_Annual, RCandCF, RC_WMAcf
EMC_WMAb <-  readRDS(values[["EMC_WMAb_RC_WMAcf"]])

EMC_WMA_Ann_noHornoOut <- EMC_WMAb %>% 
  filter(FACILITYID != 'L05-489-3d' & FACILITYID != 'L05-489-3') %>%
  group_by(EMC_Group, Constituent, Fraction, Units) %>%
  summarise(
    EMC_WMA_LU = sum(EMC_Model_LU) / sum(EMC_Model_LU_divisor)  #overall land use category EMC values for each parameter
)
    #May not make sense to do this using RC_Annual and RC_CF?  Would have to add in the standard RCs for water and ag, and the standard EMC for ag.   

saveRDS(EMC_WMA_Ann, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMA_RC_WMAcf.rds')) #update by type of RC used: RC, RC_Annual, RCandCF, RC_WMAcf
write_csv(EMC_WMA_Ann, paste0(wd,'/WetWeatherLoadingCalcs/Output/EMC_WMA_RC_WMAcf.csv'))

saveRDS(EMC_WMA_Ann_noHornoOut, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMA_RC_WMAcf_noHornoOut.rds')) #update by type of RC used: RC, RC_Annual, RCandCF, RC_WMAcf

write_csv(EMC_WMA_Ann_noHornoOut, paste0(wd, '/WetWeatherLoadingCalcs/Output/EMC_WMA_RC_WMAcf_noHornoOut.csv'))
