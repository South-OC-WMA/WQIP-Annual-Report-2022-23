library(data.table)
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(xlsx)
library(openxlsx)


wd <- 'C:/Users/givens/Box/OC Environmental Resources/OC Watersheds/Monitoring and Assessment/Water Quality Data/Intermediate Data Products/'

inPath <- paste0(wd,'Data/OUTPUT/')
outPath <- paste0(wd, 'R - Data Workup/')

#Dry Weather Chemistry Outfalls

OutfallDW <- read_csv(paste0(inPath,'Datapull_112223kk.csv')) %>%
  filter(Program == 'SDRDRYODM' & Entry.Set > '2823' & is.na(QA.Type)) 

OutfallDW<- OutfallDW%>%
  mutate(Result = ifelse(!is.na(Qualifier), paste0(Qualifier, Result), Result))

OutfallDW_spread <- OutfallDW %>%
  select(!...1) %>%
  select(Entry.Set,LogNumber,Date,Station,Sample.Type,Filtered,Parameter,Result)

OutfallDW_spread <- OutfallDW_spread %>%
  unique() %>%
spread(Parameter,Result) 
  
write.xlsx(OutfallDW_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Dry_2023.xlsx'), sheetName = 'Outfalls_DW', showNA = FALSE)



#https://stackoverflow.com/questions/73054481/add-one-excel-worksheet-from-one-file-to-another-workbook-in-r
#add a new sheet to Chem data
#Chemdata2023R1and2 <- loadWorkbook("C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2023_draft.xlsx")
Chemdata2023R1and2 <- loadWorkbook("C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/DryWeatherOutfall_Chem/DryWeatherDataNALsAssessment2023_draft.xlsx")
addWorksheet(Chemdata2023R1and2, "Outfalls_DW3")
TargetSheet = grep("^Outfalls_DW3$", names(Chemdata2023R1and2))[[1]]

#open source data, find and read the data tab
Horizon = loadWorkbook(paste0(outPath, 'SDR_WQIP_Outfalls_Dry_2023.xlsx'))
SourceSheet = grep("^Outfalls_DW$", names(Horizon))[[1]]
Sheet2Data = readWorkbook(Horizon, SourceSheet)

#Write the data to the target sheet
writeData(Chemdata2023R1and2, TargetSheet, Sheet2Data)
saveWorkbook(Chemdata2023R1and2, "C:/Users/givens/OneDrive - County of Orange/Shared with Everyone/Outfall/DryWeatherOutfall_Chem/DryWeatherDataNALsAssessment2023_draft.xlsx", overwrite=TRUE)



#Dry Weather Field Data Outfalls
OutfallDWF <- read_excel(paste0(inPath,'SAMPLE_RESULTS-2023-11-21 09 33.xlsx'))

OutfallDWF_spread <- OutfallDWF %>%
  select(project_seq,hsn,collect_date,collection_site,sample_type_desc,analyte_name,result)%>%
  spread(analyte_name,result)

write.xlsx(OutfallDWF_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Dry_FLD.xlsx'), sheetName = 'Outfalls_DW_FLD', showNA = FALSE)

##paste tabs in target sheet
#add a new sheet to Chem data
Chemdata2023R1and2 <- loadWorkbook("C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2023_draft.xlsx")
addWorksheet(Chemdata2023R1and2, "Outfalls_DW_FLD")
TargetSheet = grep("^Outfalls_DW_FLD$", names(Chemdata2023R1and2))[[1]]

#open source data, find and read the data tab
Horizon = loadWorkbook(paste0(outPath, 'SDR_WQIP_Outfalls_Dry_FLD.xlsx'))
SourceSheet = grep("^Outfalls_DW_FLD$", names(Horizon))[[1]]
Sheet2Data = readWorkbook(Horizon, SourceSheet)

#Write the data to the target sheet
writeData(Chemdata2023R1and2, TargetSheet, Sheet2Data)
saveWorkbook(Chemdata2023R1and2, "C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2023_draft.xlsx", overwrite=TRUE)

#Codes
OutfallC <- read_excel(paste0(inPath,'Sample Comments-2023-11-21 09 32.xlsx'))

write.xlsx(OutfallC, paste0(outPath, 'SDR_WQIP_Outfalls_Dry_Codes.xlsx'), sheetName = 'Outfalls_DW_Codes', showNA = FALSE)

##paste tabs in target sheet
#add a new sheet to Chem data
Chemdata2023R1and2 <- loadWorkbook("C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2023_draft.xlsx")
addWorksheet(Chemdata2023R1and2, "Outfalls_DW_Codes")
TargetSheet = grep("^Outfalls_DW_Codes$", names(Chemdata2023R1and2))[[1]]

#open source data, find and read the data tab
Horizon = loadWorkbook(paste0(outPath, 'SDR_WQIP_Outfalls_Dry_Codes.xlsx'))
SourceSheet = grep("^Outfalls_DW_Codes$", names(Horizon))[[1]]
Sheet2Data = readWorkbook(Horizon, SourceSheet)

#Write the data to the target sheet
writeData(Chemdata2023R1and2, TargetSheet, Sheet2Data)
saveWorkbook(Chemdata2023R1and2, "C:/Users/givens/Box/OC Watersheds/Environmental Monitoring/WQIP/Monitoring and Assessment Program Implementation/MS4 Outfall Monitoring/Outfall Field Screening Follow-ups/DryWeatherDataNALsAssessment2023_draft.xlsx", overwrite=TRUE)



#Wet Weather Field Data Outfalls
OutfallWWF <- read.csv(paste0(outPath,'SDR_WQIP_WW_Field.csv'))

OutfallWWF_spread <- OutfallWWF %>%
  select(project_seq,hsn,collect_date,collection_site,sample_type_desc,analyte_name,result)%>%
  spread(analyte_name,result)

write.xlsx(OutfallWWF_spread, paste0(outPath, 'SDR_WQIP_Outfalls_Wet_FLD.xlsx'), sheetName = 'Outfalls_WW_FLD', showNA = FALSE)



