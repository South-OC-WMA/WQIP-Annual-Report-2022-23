#Bring in files from HYCSV and modify files to use in loading calcs
library(dplyr)
library(tidyr)
library(tidyverse)


getwd()
setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/")

wd<-"C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/"

inPath <- paste0(wd,'SWN_raw/')
outPath <- paste0(wd, 'SWN_2023_provisional/')

OutfallWW <- read.csv(paste0(outPath,'SDR_WQIP_Outfalls_NoQC_wet.csv'))

#Group 1

dry_SWNgroup1<-read.csv(paste0(inPath, 'SWN_outfalls1.CSV'))

dry_SWNgroup1Q<-dry_SWNgroup1 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup1Q<-dry_SWNgroup1Q[-(1:2),]

names(dry_SWNgroup1Q)[names(dry_SWNgroup1Q) == 'Time'] <- 'Inst Time'

J01_9264_1 <- dry_SWNgroup1Q %>%
  select(c(1,2))

names(J01_9264_1)[names(J01_9264_1) == 'J01_9264_1.2'] <- 'Flow (cfs)'

J01_9377_1 <- dry_SWNgroup1Q %>%
  select(c(1,3))

names(J01_9377_1)[names(J01_9377_1) == 'J01_9377_1.2'] <- 'Flow (cfs)'


J01_9046_1 <- dry_SWNgroup1Q %>%
  select(c(1,4))

names(J01_9046_1)[names(J01_9046_1) == 'J01_9046_1.2'] <- 'Flow (cfs)'

write.csv(J01_9264_1, paste0(outPath, 'J01-9264-1.csv'))
write.csv(J01_9377_1, paste0(outPath, 'J01-9377-1.csv'))
write.csv(J01_9046_1, paste0(outPath, 'J01-9046-1.csv'))

#Group 2

dry_SWNgroup2<-read.csv(paste0(inPath, 'SWN_outfalls2.CSV'))

dry_SWNgroup2Q<-dry_SWNgroup2 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup2Q<-dry_SWNgroup2Q[-(1:2),]

names(dry_SWNgroup2Q)[names(dry_SWNgroup2Q) == 'Time'] <- 'Inst Time'

J01_9046_2 <- dry_SWNgroup2Q %>%
  select(c(1,2))

names(J01_9046_2)[names(J01_9046_2) == 'J01_9046_2.2'] <- 'Flow (cfs)'

J01_9349_1 <- dry_SWNgroup2Q %>%
  select(c(1,3))

names(J01_9349_1)[names(J01_9349_1) == 'J01_9349_1.2'] <- 'Flow (cfs)'


J01_10004_1 <- dry_SWNgroup2Q %>%
  select(c(1,4))

names(J01_10004_1)[names(J01_10004_1) == 'J01_10004_1.2'] <- 'Flow (cfs)'

write.csv(J01_9046_2, paste0(outPath, 'J01-9046-2.csv'))
write.csv(J01_9349_1, paste0(outPath, 'J01-9349-1.csv'))
write.csv(J01_10004_1, paste0(outPath, 'J01-10004-1.csv'))



#Group 3

dry_SWNgroup3<-read.csv(paste0(inPath, 'SWN_outfalls3.CSV'))

dry_SWNgroup3Q<-dry_SWNgroup3 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup3Q<-dry_SWNgroup3Q[-(1:2),]

names(dry_SWNgroup3Q)[names(dry_SWNgroup3Q) == 'Time'] <- 'Inst Time'

J07_9109_4 <- dry_SWNgroup3Q %>%
  select(c(1,2))

names(J07_9109_4)[names(J07_9109_4) == 'J07_9109_4.2'] <- 'Flow (cfs)'

J07_9110_3 <- dry_SWNgroup3Q %>%
  select(c(1,3))

names(J07_9110_3)[names(J07_9110_3) == 'J07_9110_3.2'] <- 'Flow (cfs)'


J07_9110_2 <- dry_SWNgroup3Q %>%
  select(c(1,4))

names(J07_9110_2)[names(J07_9110_2) == 'J07_9110_2.2'] <- 'Flow (cfs)'


write.csv(J07_9109_4, paste0(outPath, 'J07-9109-4.csv'))
write.csv(J07_9110_3, paste0(outPath, 'J07-9110-3.csv'))
write.csv(J07_9110_2, paste0(outPath, 'J07-9110-2.csv'))

#Group 4

dry_SWNgroup4<-read.csv(paste0(inPath, 'SWN_outfalls4.CSV'))
 
dry_SWNgroup4Q<-dry_SWNgroup4 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup4Q<-dry_SWNgroup4Q[-(1:2),]

names(dry_SWNgroup4Q)[names(dry_SWNgroup4Q) == 'Time'] <- 'Inst Time'

J01_9066_2 <- dry_SWNgroup4Q %>%
  select(c(1,2))

names(J01_9066_2)[names(J01_9066_2) == 'J01_9066_2.2'] <- 'Flow (cfs)'

J01_9066_1 <- dry_SWNgroup4Q %>%
  select(c(1,3))

names(J01_9066_1)[names(J01_9066_1) == 'J01_9066_1.2'] <- 'Flow (cfs)'


J01_9273_1 <- dry_SWNgroup4Q %>%
  select(c(1,4))

names(J01_9273_1)[names(J01_9273_1) == 'J01_9273_1.2'] <- 'Flow (cfs)'

write.csv(J01_9066_2, paste0(outPath, 'J01-9066-2.csv'))
write.csv(J01_9066_1, paste0(outPath, 'J01-9066-1.csv'))
write.csv(J01_9273_1, paste0(outPath, 'J01-9273-1.csv'))

#Group 5

dry_SWNgroup5<-read.csv(paste0(inPath, 'SWN_outfalls5.CSV'))

dry_SWNgroup5Q<-dry_SWNgroup5 %>% 
  select(c(1,4, 7, 9))

dry_SWNgroup5Q<-dry_SWNgroup5Q[-(1:2),]

names(dry_SWNgroup5Q)[names(dry_SWNgroup5Q) == 'Time'] <- 'Inst Time'

J06_10011_1 <- dry_SWNgroup5Q %>%
  select(c(1,2))

names(J06_10011_1)[names(J06_10011_1) == 'J06_10011_1.2'] <- 'Flow (cfs)'

J06_9079_1 <- dry_SWNgroup5Q %>%
  select(c(1,3))

names(J06_9079_1)[names(J06_9079_1) == 'J06_9079_1.2'] <- 'Flow (cfs)'


DAIRYFORK_MOULTON <- dry_SWNgroup5Q %>%
  select(c(1,4))

names(DAIRYFORK_MOULTON)[names(DAIRYFORK_MOULTON) == 'DAIRYFORK_MOULTON.1'] <- 'Flow (cfs)'

write.csv(J06_10011_1, paste0(outPath, 'J06-10011-1.csv'))
write.csv(J06_9079_1, paste0(outPath, 'J06-9079-1.csv'))
write.csv(DAIRYFORK_MOULTON, paste0(outPath, 'J06-9362-1.csv'))

#Group 6

dry_SWNgroup6<-read.csv(paste0(inPath, 'SWN_outfalls6.CSV'))

dry_SWNgroup6Q<-dry_SWNgroup6 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup6Q<-dry_SWNgroup6Q[-(1:2),]

names(dry_SWNgroup6Q)[names(dry_SWNgroup6Q) == 'Time'] <- 'Inst Time'

J05_9800_2 <- dry_SWNgroup6Q %>%
  select(c(1,2))

names(J05_9800_2)[names(J05_9800_2) == 'J05_9800_2.2'] <- 'Flow (cfs)'

J01_9008_1 <- dry_SWNgroup6Q %>%
  select(c(1,3))

names(J01_9008_1)[names(J01_9008_1) == 'J01_9008_1.2'] <- 'Flow (cfs)'


J01_9131_1 <- dry_SWNgroup6Q %>%
  select(c(1,4))

names(J01_9131_1)[names(J01_9131_1) == 'J01_9131_1.2'] <- 'Flow (cfs)'


write.csv(J05_9800_2, paste0(outPath, 'J05-9800-2.csv'))
write.csv(J01_9008_1, paste0(outPath,'J01-9008-1.csv'))
write.csv(J01_9131_1, paste0(outPath, 'J01-9131-1.csv'))


#Group 7

dry_SWNgroup7<-read.csv(paste0(inPath, 'SWN_outfalls7.CSV'))

dry_SWNgroup7Q<-dry_SWNgroup7 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup7Q<-dry_SWNgroup7Q[-(1:2),]

names(dry_SWNgroup7Q)[names(dry_SWNgroup7Q) == 'Time'] <- 'Inst Time'

J01_9992_1 <- dry_SWNgroup7Q %>%
  select(c(1,2))

names(J01_9992_1)[names(J01_9992_1) == 'J01_9992_1.2'] <- 'Flow (cfs)'

J01_10017_1 <- dry_SWNgroup7Q %>%
  select(c(1,3))

names(J01_10017_1)[names(J01_10017_1) == 'J01_10017_1.2'] <- 'Flow (cfs)'


J01_10019_1 <- dry_SWNgroup7Q %>%
  select(c(1,4))

names(J01_10019_1)[names(J01_10019_1) == 'J01_10019_1.2'] <- 'Flow (cfs)'


write.csv(J01_9992_1, paste0(outPath, 'J01-9992-1.csv'))
write.csv(J01_10017_1, paste0(outPath, 'J01-10017-1.csv'))
write.csv(J01_10019_1, paste0(outPath, 'J01-10019-1.csv'))

#Group 8

dry_SWNgroup8<-read.csv(paste0(inPath, 'SWN_outfalls8.CSV'))

dry_SWNgroup8Q<-dry_SWNgroup8 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup8Q<-dry_SWNgroup8Q[-(1:2),]

names(dry_SWNgroup8Q)[names(dry_SWNgroup8Q) == 'Time'] <- 'Inst Time'

J01_9144_4 <- dry_SWNgroup8Q %>%
  select(c(1,2))

names(J01_9144_4)[names(J01_9144_4) == 'J01_9144_4.2'] <- 'Flow (cfs)'

J01_9224_2 <- dry_SWNgroup8Q %>%
  select(c(1,3))

names(J01_9224_2)[names(J01_9224_2) == 'J01_9224_2.2'] <- 'Flow (cfs)'


J01_9224_1 <- dry_SWNgroup8Q %>%
  select(c(1,4))

names(J01_9224_1)[names(J01_9224_1) == 'J01_9224_1.2'] <- 'Flow (cfs)'

write.csv(J01_9144_4, paste0(outPath, 'J01-9144-4.csv'))
write.csv(J01_9224_2, paste0(outPath,'J01-9224-2.csv'))
write.csv(J01_9224_1, paste0(outPath,'J01-9224-1.csv'))

#Group 9

dry_SWNgroup9<-read.csv(paste0(inPath, 'SWN_outfalls9.CSV'))

dry_SWNgroup9Q<-dry_SWNgroup9 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup9Q<-dry_SWNgroup9Q[-(1:2),]

names(dry_SWNgroup9Q)[names(dry_SWNgroup9Q) == 'Time'] <- 'Inst Time'

J01_9144_1 <- dry_SWNgroup9Q %>%
  select(c(1,2))

names(J01_9144_1)[names(J01_9144_1) == 'J01_9144_1.2'] <- 'Flow (cfs)'

J01_9364_3 <- dry_SWNgroup9Q %>%
  select(c(1,3))

names(J01_9364_3)[names(J01_9364_3) == 'J01_9364_3.2'] <- 'Flow (cfs)'


J03_9221_1 <- dry_SWNgroup9Q %>%
  select(c(1,4))

names(J03_9221_1)[names(J03_9221_1) == 'J03_9221_1.2'] <- 'Flow (cfs)'


write.csv(J01_9144_1,paste0(outPath,  'J01-9144-1.csv'))
write.csv(J01_9364_3, paste0(outPath,'J01-9364-3.csv'))
write.csv(J03_9221_1, paste0(outPath, 'J03-9221-1.csv'))

#Group 10

dry_SWNgroup10<-read.csv(paste0(inPath, 'SWN_outfalls10.CSV'))

dry_SWNgroup10Q<-dry_SWNgroup10 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup10Q<-dry_SWNgroup10Q[-(1:2),]

names(dry_SWNgroup10Q)[names(dry_SWNgroup10Q) == 'Time'] <- 'Inst Time'

J03_9368_2 <- dry_SWNgroup10Q %>%
  select(c(1,2))

names(J03_9368_2)[names(J03_9368_2) == 'J03_9368_2.2'] <- 'Flow (cfs)'

J03_9368_1 <- dry_SWNgroup10Q %>%
  select(c(1,3))

names(J03_9368_1)[names(J03_9368_1) == 'J03_9368_1.2'] <- 'Flow (cfs)'


J03_9215_3 <- dry_SWNgroup10Q %>%
  select(c(1,4))

names(J03_9215_3)[names(J03_9215_3) == 'J03_9215_3.2'] <- 'Flow (cfs)'


write.csv(J03_9368_2, paste0(outPath,'J03-9368-2.csv'))
write.csv(J03_9368_1, paste0(outPath,'J03-9368-1.csv'))
write.csv(J03_9215_3, paste0(outPath,'J03-9215-3.csv'))

#Group 11

dry_SWNgroup11<-read.csv(paste0(inPath, 'SWN_outfalls11.CSV'))

dry_SWNgroup11Q<-dry_SWNgroup11 %>% 
  select(c(1,4, 7, 10))

dry_SWNgroup11Q<-dry_SWNgroup11Q[-(1:2),]

names(dry_SWNgroup11Q)[names(dry_SWNgroup11Q) == 'Time'] <- 'Inst Time'

J03_9234_6 <- dry_SWNgroup11Q %>%
  select(c(1,2))

names(J03_9234_6)[names(J03_9234_6) == 'J03_9234_6.2'] <- 'Flow (cfs)'

J03_9216_1 <- dry_SWNgroup11Q %>%
  select(c(1,3))

names(J03_9216_1)[names(J03_9216_1) == 'J03_9216_1.2'] <- 'Flow (cfs)'


J01_9005_1 <- dry_SWNgroup11Q %>%
  select(c(1,4))

names(J01_9005_1)[names(J01_9005_1) == 'J01_9005_1.2'] <- 'Flow (cfs)'


write.csv(J03_9234_6, paste0(outPath, 'J03-9234-6.csv'))
write.csv(J03_9216_1, paste0(outPath,'J03-9216-1.csv'))
write.csv(J01_9005_1, paste0(outPath, 'J01-9005-1.csv'))

#Group 12

dry_SWNgroup12<-read.csv(paste0(inPath, 'SWN_outfalls12.CSV'))

dry_SWNgroup12Q<-dry_SWNgroup12 %>% 
  select(c(1,4,6,9))

dry_SWNgroup12Q<-dry_SWNgroup12Q[-(1:2),]

names(dry_SWNgroup12Q)[names(dry_SWNgroup12Q) == 'Time'] <- 'Inst Time'

J01_9005_3 <- dry_SWNgroup12Q %>%
  select(c(1,2))

names(J01_9005_3)[names(J01_9005_3) == 'J01_9005_3.2'] <- 'Flow (cfs)'

NARCO_LAPAZ <- dry_SWNgroup12Q %>%
  select(c(1,3))

names(NARCO_LAPAZ)[names(NARCO_LAPAZ) == 'NARCO_LAPAZ.1'] <- 'Flow (cfs)'

#names(NARCO_LAPAZ)[names(NARCO_LAPAZ) == 'SULPHUR_LAPAZ.1'] <- 'Flow (cfs)'

J01_9364_3 <- dry_SWNgroup12Q %>%
  select(c(1,4))

names(J01_9364_3)[names(J01_9364_3) == 'J01_9364_3.2'] <- 'Flow (cfs)'


write.csv(J01_9005_3, paste0(outPath, 'J01-9005-3.csv'))
write.csv(NARCO_LAPAZ, paste0(outPath,'NARCO_LAPAZ.csv'))
write.csv(J01_9364_3, paste0(outPath, 'J01-9364-3.csv'))

#Group 13
dry_SWNgroup13<-read.csv(paste0(inPath, 'SWN_outfalls13.CSV'))

dry_SWNgroup13Q<-dry_SWNgroup13 %>% 
  select(c(1,4,7,10))

dry_SWNgroup13Q<-dry_SWNgroup13Q[-(1:2),]

names(dry_SWNgroup13Q)[names(dry_SWNgroup13Q) == 'Time'] <- 'Inst Time'

J01_9082_4 <- dry_SWNgroup13Q %>%
  select(c(1,2))

names(J01_9082_4)[names(J01_9082_4) == 'J01_9082_4.2'] <- 'Flow (cfs)'

J01_9082_3 <- dry_SWNgroup13Q %>%
  select(c(1,3))

names(J01_9082_3)[names(J01_9082_3) == 'J01_9082_3.2'] <- 'Flow (cfs)'

J01_9082_2 <- dry_SWNgroup13Q %>%
  select(c(1,4))

names(J01_9082_2)[names(J01_9082_2) == 'J01_9082_2.2'] <- 'Flow (cfs)'


write.csv(J01_9082_4, paste0(outPath, 'J01-9082-4.csv'))
write.csv(J01_9082_3, paste0(outPath, 'J01-9082-3.csv'))
write.csv(J01_9082_2, paste0(outPath, 'J01-9082-2.csv'))

#Still need J01_9007_1
#Group 14
dry_SWNgroup14<-read.csv(paste0(inPath, 'SWN_outfalls14.CSV'))

dry_SWNgroup14Q<-dry_SWNgroup14 %>% 
  select(c(1,4))

dry_SWNgroup14Q<-dry_SWNgroup14Q[-(1:2),]

names(dry_SWNgroup14Q)[names(dry_SWNgroup14Q) == 'Time'] <- 'Inst Time'

J01_9007_1 <- dry_SWNgroup14Q %>%
  select(c(1,2))

names(J01_9007_1)[names(J01_9007_1) == 'J01_9007_1.2'] <- 'Flow (cfs)'



write.csv(J01_9007_1, paste0(outPath, 'J01-9007-1.csv'))
