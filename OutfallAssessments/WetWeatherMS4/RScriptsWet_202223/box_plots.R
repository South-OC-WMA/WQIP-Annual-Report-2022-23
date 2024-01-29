#load libraries
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(here)
library(tibble)
library(tidyverse)
library(readxl)
library(purrr)
library(rqdatatable)
library(lubridate)
library(readxl)
library(readr)
library(hms)
library(EnvStats)
library(DescTools)
library(ggplot2)
#library(psyche)

# Install latest version:
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

#library(arcgisbinding)
#arc.check_product()


getwd()
wd<-setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments/WetWeatherLoadingCalcs/")

dataset <- paste0(wd, '/Input/Dataset_vertical_Wet2023.xlsx')
dataset <- read_excel(dataset, sheet = 'Dataset_vertical')

cadmium <- dataset %>%
  filter(Parameter == 'Cadmium (Total Cd)') 

  ggplot(data=cadmium, mapping=aes(x=`Monitoring Year`, y=Result)) + 
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot() +labs(y= "Cd (ug/L)", x = "Monitoring Year") +
    geom_hline(yintercept=3, linetype="dashed", color = "orange", size = 2) + theme(text=element_text(size=18))

 pb <- dataset %>%
    filter(Parameter == 'Lead (Total Pb)') 
  
  ggplot(data=pb, mapping=aes(x=`Monitoring Year`, y=Result)) + 
    stat_boxplot(geom = "errorbar", width = 0.2) + 
    geom_boxplot() +labs(y= "Lead (ug/L)", x = "Monitoring Year") +
    geom_hline(yintercept=250, linetype="dashed", color = "orange", size = 2) + theme(text=element_text(size=18))
  
 
  zn <- dataset %>%
    filter(Parameter == 'Zinc (Total Zn)') 
  
  ggplot(data=pb, mapping=aes(x=`Monitoring Year`, y=Result)) + 
    stat_boxplot(geom = "errorbar", width = 0.2) + 
    geom_boxplot() +labs(y= "Zinc (ug/L)", x = "Monitoring Year") +
    geom_hline(yintercept=976, linetype="dashed", color = "orange", size = 2) + coord_trans(y = "log10") + theme(text=element_text(size=18))
  
  
  
  
  
  NN <- dataset %>%
    filter(Parameter == 'Nitrate & Nitrite as N (Total)a' | Parameter == 'Nitrate+Nitrite Nitrogen') 
  
  ggplot(data=NN , mapping=aes(x=`Monitoring Year`, y=Result)) + 
    stat_boxplot(geom = "errorbar", width = 0.2) + 
    geom_boxplot() +labs(y= "Nitrate + Nitrite as N (mg/L)", x = "Monitoring Year") +
  geom_hline(yintercept=2.6, linetype="dashed", color = "orange", size = 2) + theme(text=element_text(size=18))

  
 


  TP <- dataset %>%
    filter(Parameter == 'Phosphorus (Total P)') 
  
  ggplot(data=TP , mapping=aes(x=`Monitoring Year`, y=Result)) + 
    stat_boxplot(geom = "errorbar", width = 0.2) + 
    geom_boxplot() +labs(y= "Total Phosphorus as P (mg/L)", x = "Monitoring Year") + coord_trans(y = "log10") +
    geom_hline(yintercept=1.46, linetype="dashed", color = "orange", size = 2) + theme(text=element_text(size=18))
  





