## This file runs all requirements and script files needed to put together all scripted sections of ReqtoCheckSTAT and currently assumes computer running the script has access to the OPA share drive. 

# Load required packages
library(devtools) #### for source_url function in Main.R
library(tidyr) # Contracts
library(plyr)  # All
library(dplyr) # All
library(lubridate) # All
library(xlsx) # Contracts
library(stringr)# Contracts and payments
library(reshape2)# Contracts and procurement
library(zoo)# All
library(ggplot2)# All
library(bizdays)# Payments and Procurement
library(scales)
library(RCurl)
library(data.table)#### setnames function
library(readxl)
library(RODBC)

### Set reporting period, as well as first and last date of analysis quarter
r_period<-readline("Enter reporting period in yyyy Q# format:")
#r_period<-as.yearqtr(as.Date(Sys.Date())-1) # uncomment if you would rather set r_period automatically based on last complete quarter
last<-as.Date(as.yearqtr(r_period)+0.25)-1
first<-as.Date(as.yearqtr(r_period))

#####  
Days<-function(df,FirstDt,EndDt,digits=1){
  arguments<-as.list(match.call())  
  
  ### Identify args as dataframe columns
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  
  ### Calculate days betweens first date and end date, rounding to the specified number of decimal places, with a default of 0
  round(difftime(as.POSIXct(EndDt),as.POSIXct(FirstDt),units="days"),digits)
}



ReqDays<-function(df,FirstDt,EndDt,digits=0){
  arguments<-as.list(match.call())  
  
  ### Identify args as dataframe columns
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  
  ### Calculate days betweens first date and end date, rounding to the specified number of decimal places, with a default of 0
  round(as.POSIXct(EndDt,"%m/%d/%Y %H:%M")-as.POSIXct(FirstDt,"%m/%d/%Y %H:%M"),digits)/86400
}

## Create function for days since contract creation for open contracts at end of the reporting period, rounded to whole numbers
Age<-function(df,StartDt,QtrEnd){
  arguments<-as.list(match.call())  
  StartDt<-eval(arguments$StartDt,df)
  as.Date(as.vector(QtrEnd),format="%Y-%m-%d")
  round((strptime(QtrEnd,"%Y-%m-%d")-strptime(StartDt,"%Y-%m-%d"))/86400,digits=0)
}

source_url("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") #### Load OPA theme
source_url("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R")#### Load calendar for business day calculations (Need to manually add City holidays for each year in repo)

### Load component scripts
source("O:/Projects/ReqtoCheckStat/Buyspeed_Queries.R",echo=TRUE)
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Reqs2.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_Reqs.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Procurement.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Bids_RFPs_DBEs.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_PO2.R")
source_url("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Payments.R")
