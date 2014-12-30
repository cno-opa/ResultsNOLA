##load relevant packages
library(dplyr)
library(ggplot2)
library(plyr)
library(scales) ## to scale dates into month-year format in graphs
library(lubridate)

## read, clean, and format
headings<-c("Dept","Reqnumber","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")
Reqs<-read.csv("Procurement Req Processing.csv",col.names=headings,stringsAsFactors=FALSE)
names(Reqs)=headings
Reqs<-Reqs[-c(1,2,3),]
Reqs$FinanceDate<-as.Date(Reqs$FinanceDate,"%m/%d/%Y")
Reqs$POdate<-as.Date(Reqs$POdate,"%m/%d/%Y")

## set up business day calculation
networkdays <- function(start, end, holidays) 
  { 
    dates <- seq(as.Date(start), as.Date(end), by="day")   
      if(missing(holidays))holidays <- 0 else holidays <- length(holidays) 
    sum(as.numeric(format(dates, "%w") > 1)) - holidays 
    } 

holidays<-c("2014-01-01","2014-01-20","2014-03-04","2014-04-18","2014-05-26","2014-07-04","2014-09-01","2014-11-27","2014-11-28","2014-12-25","2014-12-26")

mutate(Reqs,businessdays)