## This file runs all requirements and script files needed to put together all scripted sections of ReqtoCheckSTAT and currently assumes computer running the script has access to the OPA share drive. 

# Load required packages
library(devtools) #### for source_url function in Main.R
library(tidyr) # Contracts
library(plyr)  # May still be used
library(dplyr) # All
library(lubridate) # All
library(xlsx) # Contracts
library(stringr)# Contracts and payments narrative
library(reshape2)# Contracts and procurement
library(zoo)# All (for as.yearqtr class)
library(ggplot2)# All
library(bizdays)# For business day processing
library(scales) # Not sure if this is needed anymore
library(RCurl) # Not sure this is needed anymore
library(data.table)#### setnames function (not sure if needed)
library(readxl) ### Reading in Adjustments.xlsx
library(RODBC) ### Needed for SQL queries

### Set reporting period, as well as first and last date of analysis quarter
r_period<-readline("Enter reporting period in yyyy Q# format:")
#r_period<-as.yearqtr(as.Date(Sys.Date())-1) # uncomment if you would rather set r_period automatically based on last complete quarter
last<-as.Date(as.yearqtr(r_period)+0.25)-1 ### last date of reporting quarter
first<-as.Date(as.yearqtr(r_period)) ## first date of reporting quarter
next_day<-as.Date(as.yearqtr(r_period)+0.25) ### day after reporting quarter (for calculating age)


source_url("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") #### Load OPA theme
source_url("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R")#### Load calendar for business day calculations (Need to manually add City holidays for each year in repo)

### Load component scripts
source("O:/Projects/ReqtoCheckStat/Buyspeed_Queries.R",echo=TRUE)
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Reqs2.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Procurement.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Bids_RFPs_DBEs.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_Reqs.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_PO2.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Payments2.R")
