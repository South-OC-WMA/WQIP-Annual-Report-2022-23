
#   ____________________________________________________________________________
#   Project Setup                                                           ####

##  ............................................................................
##  Libraries                                                               ####
library(tidyverse)
library(lubridate)
library(data.table)
library(magrittr)
library(readxl)
library(dplyr)
library(here)
library(tidyr)
library(readr)
library(hms)
library(arcgisbinding) #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary") #install bridge manually from GitHub:  https://github.com/R-ArcGIS/r-bridge/releases/tag/v1.0.1.232#
arc.check_product()

##  ............................................................................
##  Common Values                                                           ####
values <- list()
values[['AnalysisRunDate']]   <- Sys.Date()
values[['AnalysisStartDate']] <- as.Date('07/01/2004', format = c('%m/%d/%Y'))
values[['ReportStart']]       <- '2021-10-01'
values[['ReportEnd']]         <- '2022-09-30'




