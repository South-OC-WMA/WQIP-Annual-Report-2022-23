#Bring in files from HYCSV and modify files to use in loading calcs
library(dplyr)
library(tidyr)
library(tidyverse)


getwd()
setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/")

wd<-"C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/DryWeatherLoadingCalcs/Input/ProvisionalFlow/"

inPath <- paste0(wd,'HachFL900_2023_raw/')
outPath <- paste0(wd, 'HachFL900_2023_provisional/')


#Group 1

J01_9224_2<-read.csv(paste0(inPath, 'J0192242.CSV'))

J01_9224_2<-J01_9224_2[-(1:3),]

names(J01_9224_2)[names(J01_9224_2) == 'Time'] <- 'Inst Time'

J01_9224_2 <- J01_9224_2 %>%
  select(c(1,2))

names(J01_9224_2)[names(J01_9224_2) == 'J01_9224_2'] <- 'Flow (cfs)'


write.csv(J01_9224_2, paste0(outPath, 'J01-9224-2.csv'))


#Group 2

K01P08and09 <-read.csv(paste0(inPath, 'K01P08and09.CSV'))

K01P08and09<-K01P08and09[-(1:3),]

names(K01P08and09)[names(K01P08and09) == 'Time'] <- 'Inst Time'

K01_12058_1 <- K01P08and09 %>%
  select(c(1,2))

names(K01_12058_1)[names(K01_12058_1) == 'K01_12058_1'] <- 'Flow (cfs)'

K01_12058_2 <- K01P08and09 %>%
  select(c(1,3))

names(K01_12058_2)[names(K01_12058_2) == 'K01_12058_2'] <- 'Flow (cfs)'


write.csv(K01_12058_1, paste0(outPath, 'K01-12058-1.csv'))
write.csv(K01_12058_2, paste0(outPath, 'K01-12058-2.csv'))


#Group 3

L03_708_11<-read.csv(paste0(inPath, 'L03_708_11.CSV'))

L03_708_11<-L03_708_11[-(1:3),]

names(L03_708_11)[names(L03_708_11) == 'Time'] <- 'Inst Time'

L03_708_11 <- L03_708_11 %>%
  select(c(1,2))

names(L03_708_11)[names(L03_708_11) == 'L03_708_11'] <- 'Flow (cfs)'


write.csv(L03_708_11, paste0(outPath, 'L03-708-11.csv'))

#Group 4

LC03_11515_1<-read.csv(paste0(inPath, 'LC03_11515_1_MDD.CSV'))

LC03_11515_1<-LC03_11515_1[-(1:4),]

names(LC03_11515_1)[names(LC03_11515_1) == 'Time'] <- 'Inst Time'

LC03_11515_1 <- LC03_11515_1 %>%
  select(c(1,2))

names(LC03_11515_1)[names(LC03_11515_1) == 'LC03_11515_1'] <- 'Flow (cfs)'


write.csv(LC03_11515_1, paste0(outPath, 'LC03-11515-1.csv'))

#Group 5

OCFS1_MDD <-read.csv(paste0(inPath, 'OCFS1_MDD.CSV'))

OCFS1_MDD<-OCFS1_MDD[-(1:4),]

names(OCFS1_MDD)[names(OCFS1_MDD) == 'Time'] <- 'Inst Time'

L02_641_2 <- OCFS1_MDD %>%
  select(c(1,2))

names(L02_641_2)[names(L02_641_2) == 'L02_641_2'] <- 'Flow (cfs)'


L02_246_1 <- OCFS1_MDD %>%
  select(c(1,3))

names(L02_246_1)[names(L02_246_1) == 'L02_246_1'] <- 'Flow (cfs)'

L02_374_1 <- OCFS1_MDD %>%
  select(c(1,4))

names(L02_374_1)[names(L02_374_1) == 'L02_374_1'] <- 'Flow (cfs)'

write.csv(L02_641_2, paste0(outPath, 'L02-641-2.csv'))
write.csv(L02_246_1, paste0(outPath, 'L02-246-1.csv'))
write.csv(L02_374_1, paste0(outPath, 'L02-374-1.csv'))

#Group 6

L04_672_1<-read.csv(paste0(inPath, 'L04_672_1.CSV'))

L04_672_1<-L04_672_1[-(1:4),]

names(L04_672_1)[names(L04_672_1) == 'Time'] <- 'Inst Time'

L04_672_1 <- L04_672_1 %>%
  select(c(1,3))

names(L04_672_1)[names(L04_672_1) == 'L04_672_1.1'] <- 'Flow (cfs)'


write.csv(L04_672_1, paste0(outPath, 'L04_672_1.csv'))
