## Import and Format rain data 


source('DryWeatherLoadingCalcs/1projectsetup.R')

getwd()

wd <- 'C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments'

inPath <- paste0(wd,'/DryWeatherLoadingCalcs/Input/Rain/')
outPath <- paste0(wd, '/DryWeatherLoadingCalcs/Output/')

getwd()

#daily total rain, midnight to midnight

#To include dry days for previous years for the continuous flow data, need rain data from 2015
#Rain_Daily <-"OutfallAssessments/DryWeatherLoadingCalcs/Input/Rain/Rain_SOCWMA_COOP_2015-2023.csv"

Rain_Daily <-paste0(inPath, 'Rain_SOCWMA_COOP_2015_2023.csv')

Rain_Daily<-read.csv(Rain_Daily) %>%
  select(c(1:8))


Rain_Daily<-Rain_Daily[-(1:2),]

str(Rain_Daily)


#Format Rain data

rainmax <- Rain_Daily %>%  # 
  gather(key=rain_gage, value=rain_total, 'TRABUCO':'TUCKER') %>%
  filter(!is.na(rain_total)) %>%
  mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
  group_by(date, rain_gage) %>%
  summarize(max_rain_total = max(rain_total)) %>%
  filter(max_rain_total >= 0.1) 

raindaily <- Rain_Daily %>%  #use this object for point rainfall data.  May not need the summarize if already pulled as daily data from Hydstra
  gather(key=rain_gage, value=rain_total, 'TRABUCO':'TUCKER') %>%
  filter(!is.na(rain_total)) %>%
  mutate(date = as.Date(Date, format = '%m/%d/%Y')) %>%
  group_by(date, rain_gage) 
getwd()

saveRDS(rainmax, paste0(outPath, 'Rain_Max.rds'))

saveRDS(raindaily, paste0(outPath, 'Rain_Daily.rds'))

##* Task 2: Determine how many dry weather days at each gage
##  Function Definitions                                                    ####

# did_it_rain, check rainfall dataset for date range,
# expects rainfall dataset to have a column named date, and the rainfall dataset to be named "rainmax"


calc_prior_rain <- function(SD, day_step) {
  
  date_current <- as.Date(SD['date'])
  gage_current <- as.character(SD['rain_gage'])
  
  rained <- rainmax %>%
    
    filter(date >= (date_current - day_step), date <= date_current, rain_gage==gage_current) %>%
    
    summarise(n()) 
  ifelse(as.numeric(rained) > 0,
         return(TRUE),
         return(FALSE))
}


##  ............................................................................
##  Rainfall influenced days                                                  ####
values <- list()
values[['raindaily']]         <- paste0(outPath, 'Rain_Daily.rds')
values[['rainmax']]         <- paste0(outPath, 'Rain_Max.rds')

# import data from import data script
raindaily <- readRDS(values[["raindaily"]]) 

rainmax <- readRDS(values[["rainmax"]])  

str(raindaily)
str(rainmax)

rainmax<-as.data.frame(rainmax)
raindaily<-as.data.frame(raindaily)


#join datasets by date and gage name#  
Rain_join <- left_join(
  raindaily,
  rainmax,
  by = c('date' = 'date', 'rain_gage'='rain_gage')
) %>%
  as_tibble()


## Mark Rainfall within 72 hours (3 days) ##
#Dry Days Defined 72 hours after 0.1 rainfall event is wet, all else is dry#

dry_new <- data.table(Rain_join)
str(dry_new)

dry_new[, 'wet_within_72' := apply(.SD, 1, calc_prior_rain, 3), .SDcols = c('date', 'rain_gage')]

saveRDS(dry_new, paste0(outPath,'dry_new.rds'))

#   Formatting and intermediate data products                               ####

#recreate file in ArcGIS so it is updated with any station changes (Use Near Table Geoprocessing)

stn_near <-paste0(inPath, 'outfall_near_rain_gage.csv')

stn_near <- read.csv(stn_near)


values <- list()
values[['dry_new']] <- paste0(outPath,'dry_new.rds')
  
dry_new <-  readRDS(values[["dry_new"]])

#Combine rain data and station data near co-op gage

RainInf <- left_join(
  dry_new,
  stn_near,
  by=c('rain_gage'='station')) %>% 
 select(c('date', 'rain_gage', 'wet_within_72', 'GlobalID', 'FACILITYID')) %>%
  mutate(Date = as.Date(date, format = '%m/%d/%Y'))

RainInf <- unique(RainInf)  #remove duplicates

# save final product
saveRDS(RainInf, paste0(outPath, 'RainInf.rds'))
write_csv(RainInf, paste0(outPath, 'RainInf.csv'))

RainInfQC<-RainInf %>% 
  select('rain_gage') %>% 
  unique()
      
RainInfQC<-RainInf %>% 
  select('FACILITYID') %>% 
  unique()



str(RainInf)

##  ............................................................................
##  Dry days  ##
values <- list()
values[['RainInf']] <- paste0(outPath, 'RainInf.rds')
RainInf <- readRDS(values[["RainInf"]])

#Count number of dry days#

##use the format function to extract month, years##
dry_days_month <- data.frame(RainInf %>%
                             mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
                             group_by(month, year, rain_gage, FACILITYID) %>%
                             summarise(dry_days = sum(wet_within_72==FALSE)))

dry_days_year<-data.frame(RainInf %>%
                            mutate(year = format(Date, "%Y")) 
)

str(RainInf)

dry_days_year$MonitoringYear<- ifelse(dry_days_year$Date>(as.Date("2022-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2023-10-01", origin="1900-01-01")), "MY2022-23",
  ifelse(dry_days_year$Date>(as.Date("2021-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2022-10-01", origin="1900-01-01")), "MY2021-22",
                                    ifelse(dry_days_year$Date>(as.Date("2020-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2021-10-01", origin="1900-01-01")), "MY2020-21",
                                      ifelse(dry_days_year$Date>(as.Date("2019-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2020-10-01", origin="1900-01-01")), "MY2019-20",
                                             ifelse(dry_days_year$Date>(as.Date("2018-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2019-10-01", origin="1900-01-01")), "MY2018-19",
                                                    ifelse(dry_days_year$Date>(as.Date("2017-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2018-10-01", origin="1900-01-01")), "MY2017-18",
                                                           ifelse(dry_days_year$Date>(as.Date("2016-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2017-10-01", origin="1900-01-01")), "MY2016-17",
                                                                  ifelse(dry_days_year$Date>(as.Date("2015-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2016-10-01", origin="1900-01-01")), "MY2015-16",
                                                                         ifelse(dry_days_year$Date>(as.Date("2014-09-30", origin="1900-01-01")) & dry_days_year$Date<(as.Date("2015-10-01", origin="1900-01-01")), "MY2014-15",  
                                                                                NA)))))))))

dry_days_year_group <- dry_days_year %>%                       
  
  group_by(MonitoringYear, rain_gage, FACILITYID) %>%
  summarise(dry_days = sum(wet_within_72==FALSE)) %>%
  as_tibble()

# save final product
library(here)
library(readr)
write_csv(dry_days_month, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysMonthly.csv'))
write_csv(dry_days_year_group, path = paste0(outPath, 'DryDaysYear.csv'))
write_csv(dry_days_year_group, path = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.csv'))


# Save final dataset for other products
saveRDS(dry_days_month, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysMonthly.rds'))
saveRDS(dry_days_year_group, file = paste0(outPath, 'DryDaysYear.rds'))
saveRDS(dry_days_year_group, file = here('A.2 Outfall Assessments/DryWeatherLoadingCalcs/Output/DryDaysYearGroup.rds'))

