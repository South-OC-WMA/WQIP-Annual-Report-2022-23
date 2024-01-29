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
#library(psyche)

# Install latest version:
install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")

library(arcgisbinding)
arc.check_product()


getwd()
wd<-setwd("C:/Users/givens/Box/WQIP-Annual-Report-2022-23/OutfallAssessments")

