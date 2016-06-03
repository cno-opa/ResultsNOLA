## This file runs all requirements and script files needed to put together all scripted sections of ReqtoCheckSTAT and currently assumes computer running the script has access to the OPA share drive. 

# Load required packages
library(tidyr) # Contracts
library(plyr)  # All
library(dplyr) # All
library(lubridate) # All
library(xlsx) # Contracts
library(gdata)# Contracts
library(stringr)# Contracts and payments
library(reshape2)# Contracts and procurement
library(zoo)# All
library(ggplot2)# All
library(gdata) # Payments
library(bizdays)# Payments and Procurement
library(scales)
library(RCurl)
library(data.table)#### setnames function
library(readxl)
library(RODBC)
library(ggvis)

### Set first and last date of analysis quarter
r_period<-as.yearqtr(as.Date(Sys.Date())-1)
last<-as.Date(as.yearqtr(Sys.Date())) - 1
first<-as.Date(as.yearqtr(last))
  
### Function for reading R files directly from github.com
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}


### Create function to calculate days between two dates, with rounding defaulted to the whole number
# Days<-function(df,FirstDt,EndDt,digits=0){
#   arguments<-as.list(match.call())  
#   
#   ### Identify args as dataframe columns
#   EndDt<-eval(arguments$EndDt,df)
#   FirstDt<-eval(arguments$FirstDt,df)
#   
#   ### Calculate days betweens first date and end date, rounding to the specified number of decimal places, with a default of 0
#   round((strptime(EndDt,"%Y-%m-%d")-strptime(FirstDt,"%Y-%m-%d"))/86400,digits)
# }

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


source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") #### Load OPA theme
#source_https("https://raw.githubusercontent.com/cno-opa/stat-template/master/R/lib/utils.R") 
source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R")#### Load calendar for business day calculations
#source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/Multiplot%20function.R") #### Load function allowing for mulitiple graphs in one plot output
#source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/lm_equation.R") #### Load script for displaying Linear regression equation and R-squared on lm plots

### Load component scripts
source("O:/Projects/ReqtoCheckSTAT-scripts/Buyspeed_Queries.R",echo=TRUE)
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Reqs2.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_Reqs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Procurement.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Bids_RFPs_DBEs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_PO2.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Payments.R")
