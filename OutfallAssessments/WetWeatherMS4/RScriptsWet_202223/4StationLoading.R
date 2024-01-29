
source(paste0(wd, '/WetWeatherLoadingCalcs/RscriptsWet_202223/1WW_dataimport.R'))


stn_RC_202223b_all <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b_all.csv'))
emc <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/emc.csv'))


emc <- emc %>%
  mutate(
    `Composite Begin` = as.Date(`Composite Start Time`, format = "%m/%d/%Y")) %>%
  mutate(
    `Composite End` =as.Date(`Composite End Time`, format = "%m/%d/%Y"))

str(emc)

stn_load2 <- emc %>% 
  select(-`Stormflow Volume -cf`, -contains('Field')) %>% 
  gather(Parameter, Value, `2,4'-DDD - ng/L`:`TN - mg/L`) %>% 
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

stn_RC_202223b_all <- read_csv(paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_RC_202223b_all.csv')) %>%
  as_tibble() %>%
  select(LSPC_LU_DESC, LSPC_LU_CODE, RC, RC_Group, RC_M, RC_WMAcf_M, EMC_Group, FACILITYID, AREA, station, Rain_Gage_Total, Rain_Event_Total, 'Stormflow Volume -cf', 'Composite Begin', 'Composite End','Sample Type','HSN', 'Fraction', 'Collected Date', 'Stormwater Volume (acre-ft)_RC','Stormwater Volume (acre-ft)_RC_M','Stormwater Volume (acre-ft)_RCandCF', 'Stormwater Volume (acre-ft)_RC_WMAcf_M') %>%
  filter(!is.na(LSPC_LU_DESC)) 

str(stn_RC_202223b_all)

stn_load <- stn_load2 %>%    
  
  full_join(
    .,
    stn_RC_202223b_all,
    by = c('Station'='FACILITYID', 'Composite Begin', 'Composite End','Sample Type','HSN', 'Fraction', 'Collected Date')
  )  %>% 
  #filter(!is.na(`Stormflow Volume -cf`)) %>% 
  
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
    Load_RC_M = `Stormwater Volume (acre-ft)_RC_M`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_RC_M = mean(Load_RC_M)) %>% 
  ungroup() %>% 
  mutate(
    PL_RC_M = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                  (Avg_RC_M) * 1233481.8511532 * 10,  
                  ifelse(Units == 'mg/L',  #loading in pounds#
                         (Avg_RC_M) *  (1/453592) * (1233481.8511532),  
                         ifelse(Units == 'ug/L',
                                (Avg_RC_M) * (1/10^3) * (1/453592) * (1233481.8511532),
                                ifelse(Units == 'ng/L',
                                       (Avg_RC_M) * (1/10^6) * (1/453592) * (1233481.8511532),
                                       NA
                                )
                         )
                  )
    )) %>% 
  #mutate(
    #Load_RC_Event = `Stormwater Volume (acre-ft)_RC_Event`*Result
 # ) %>% 
  #group_by(Station, Parameter) %>% 
 # mutate(Avg_Load_RC_Event = mean(Load_RC_Event)) %>% 
 # ungroup() %>% 
 # mutate(
  #  PL_RC_Event = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
    #               (Avg_Load_RC_Event) * 1233481.8511532 * 10,  
     #              ifelse(Units == 'mg/L',  #loading in pounds#
      #                    (Avg_Load_RC_Event) *  (1/453592) * (1233481.8511532),  
       #                   ifelse(Units == 'ug/L',
         #                        (Avg_Load_RC_Event) * (1/10^3) * (1/453592) * (1233481.8511532),
          #                       ifelse(Units == 'ng/L',
          #                      #        (Avg_Load_RC_Event) * (1/10^6) * (1/453592) * (1233481.8511532),
                               #         NA
                           #      )
                        #  )
                  # )
   # )) %>% 
  mutate(
    Load_RCandCF = `Stormwater Volume (acre-ft)_RCandCF`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_Load_RCandCF = mean(Load_RCandCF)) %>% 
  ungroup() %>% 
  mutate(
    PL_Load_RCandCF = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                       (Avg_Load_RCandCF) * 1233481.8511532 * 10,  
                       ifelse(Units == 'mg/L',  #loading in pounds#
                              (Avg_Load_RCandCF) *  (1/453592) * (1233481.8511532),  
                              ifelse(Units == 'ug/L',
                                     (Avg_Load_RCandCF) * (1/10^3) * (1/453592) * (1233481.8511532),
                                     ifelse(Units == 'ng/L',
                                            (Avg_Load_RCandCF) * (1/10^6) * (1/453592) * (1233481.8511532),
                                            NA
                                     )
                              )
                       )
    )) %>%
 # mutate(
  #  Load_RC_Mcf = `Stormwater Volume (acre-ft)_RC_Mcf`*Result
 # ) %>% 
 # group_by(Station, Parameter) %>% 
 # mutate(Avg_Load_RC_Mcf = mean(Load_RC_Mcf)) %>% 
 # ungroup() %>% 
#  mutate(
 #   PL_RC_Mcf = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
          #         (Avg_Load_RC_Mcf) * 1233481.8511532 * 10,  
                #   ifelse(Units == 'mg/L',  #loading in pounds#
                  #        (Avg_Load_RC_Mcf) *  (1/453592) * (1233481.8511532),  
                    #      ifelse(Units == 'ug/L',
                       #          (Avg_Load_RC_Mcf) * (1/10^3) * (1/453592) * (1233481.8511532),
                            #     ifelse(Units == 'ng/L',
                             #           (Avg_Load_RC_Mcf) * (1/10^6) * (1/453592) * (1233481.8511532),
                          #              NA
                      #           )
                      #    )
                  # )
  #  )) %>%
  #mutate(
    #Load_RC_WMAcf = `Stormwater Volume (acre-ft)_RC_WMAcf`*Result
 # ) %>% 
 # group_by(Station, Parameter) %>% 
  #mutate(Avg_Load_RC_WMAcf = mean(Load_RC_WMAcf)) %>% 
  #ungroup() %>% 
 # mutate(
  #  PL_RC_WMAcf = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                  # (Avg_Load_RC_WMAcf) * 1233481.8511532 * 10,  
                  # ifelse(Units == 'mg/L',  #loading in pounds#
                          #  (Avg_Load_RC_WMAcf) *  (1/453592) * (1233481.8511532),  
                          #  ifelse(Units == 'ug/L',
                                #   (Avg_Load_RC_WMAcf) * (1/10^3) * (1/453592) * (1233481.8511532),
                                  # ifelse(Units == 'ng/L',
                                        #  (Avg_Load_RC_WMAcf) * (1/10^6) * (1/453592) * (1233481.8511532),
                                       #   NA
                                 #  )
                          #  )
               #      )
    #))  %>%
  mutate( 
    Load_RC_WMAcf_M = `Stormwater Volume (acre-ft)_RC_WMAcf_M`*Result
  ) %>% 
  group_by(Station, Parameter) %>% 
  mutate(Avg_RC_WMAcf_M = mean(Load_RC_WMAcf_M)) %>% 
  ungroup() %>% 
 mutate(
    PL_RC_WMAcf_M = ifelse(Parameter %in% c('Fecal coliforms', 'Total Coliforms', 'Enterococcus', 'E. coli'),  #Unit Converion:  453592 mg in a pound, 1 cubic foot is 62.43 pounds, 43560 cf in an acre-foot, 28.3168 Liters in one cubic-ft, 1233481.8511532 L in one acre-foot
                       (Avg_RC_WMAcf_M) * 1233481.8511532 * 10,  
                       ifelse(Units == 'mg/L',  #loading in pounds#
                              (Avg_RC_WMAcf_M) *  (1/453592) * (1233481.8511532),  
                              ifelse(Units == 'ug/L',
                                     (Avg_RC_WMAcf_M) * (1/10^3) * (1/453592) * (1233481.8511532),
                                     ifelse(Units == 'ng/L',
                                            (Avg_RC_WMAcf_M) * (1/10^6) * (1/453592) * (1233481.8511532),
                                            NA
                                     )
                              )
                       )
    ))  
  
                              
  
write_csv(stn_load, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load.csv'))


#ignore columns with each parameter
str(stn_load)

# Event loading for print


values <- list()
values[['EMCRainTot2']]         <- paste0(wd, '/WetWeatherLoadingCalcs/Output/EMCRainTot2.rds')
EMCRainTot2 <- readRDS(values[["EMCRainTot2"]]) %>%
  as_tibble() %>%
  ungroup() %>%
  select(Station, area_acres, station, Rain_Gage_Total, Rain_Event_Total, HSN, Fraction, 'Sample Type', 'Collected Date', 'Composite Begin', 'Composite End', 'Composite Start Time', 'Composite End Time') %>%
  unique()
  
  
stn_load_print_RC <-stn_load %>% 
  select(Station, HSN, 'Sample Type', 'Composite Begin', 'Composite End', Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC`, RC, RC_Group, PL_RC) %>% 
  distinct() %>% 
  spread(Parameter, PL_RC) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Sample Type', 'HSN')
  ) 


stn_load_print_RC_M <-stn_load %>% 
  select(Station,HSN, 'Sample Type', 'Composite Begin', 'Composite End', Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_M`, RC_M, RC_Group, PL_RC_M) %>% 
  distinct() %>% 
  spread(Parameter, PL_RC_M) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Sample Type', 'HSN')
  ) 




stn_load_print_RC_Event <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_Event`,  RC_Group, PL_RC_Event) %>% 
  distinct() %>% 
  spread(Parameter,PL_RC_Event) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 

stn_load_print_RCandCF <-stn_load %>% 
  select(Station,HSN, 'Sample Type', 'Composite Begin', 'Composite End', Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RCandCF`, RC_CF, RC_Group, PL_Load_RCandCF) %>% 
  distinct() %>% 
  spread(Parameter,PL_Load_RCandCF) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Sample Type', 'HSN')
  ) 


stn_load_print_RC_Mcf <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_Mcf`,  RC_Group, PL_RC_Mcf) %>% 
  distinct() %>% 
  spread(Parameter,PL_RC_Mcf) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 

stn_load_print_WMAcfM <-stn_load %>% 
  select(Station, Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_WMAcf`,  RC_Group, PL_RC_WMAcf) %>% 
  distinct() %>% 
  spread(Parameter,PL_RC_WMAcf) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Stormflow Volume -cf')
  ) 



stn_load_print_WMAcf_M <-stn_load %>% 
  select(Station, HSN, 'Sample Type', 'Composite Begin', 'Composite End', Parameter, Fraction, `Stormflow Volume -cf`,`Stormwater Volume (acre-ft)_RC_WMAcf_M`, RC_WMAcf_M, RC_Group, PL_RC_WMAcf_M) %>% 
  distinct() %>% 
  spread(Parameter,PL_RC_WMAcf_M) %>%  #how to spread with more than 1? 
  right_join(
    EMCRainTot2,
    .,
    by = c('Station', 'Fraction', 'Sample Type', 'HSN')
  ) 



#filter(SampleType=="NA") %>%
#write_csv(., paste0(getwd(), '/A.2 Outfall Assessments/WetWeatherLoadingCalcs/Output/stn_load_print.csv'))


write_csv(stn_load_print_RC, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_RC.csv'))

write_csv(stn_load_print_RC_M, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_RC_M.csv'))

#write_csv(stn_load_print_RC_Event, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_RC_Event.csv'))
write_csv(stn_load_print_RCandCF, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_RCandCF.csv'))
#write_csv(stn_load_print_RC_Mcf, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_RC_Mcf.csv'))
#write_csv(stn_load_print_WMAcfM, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_WMAcfM.csv'))
write_csv(stn_load_print_WMAcf_M, paste0(wd, '/WetWeatherLoadingCalcs/Output/stn_load_print_WMAcf_M.csv'))
