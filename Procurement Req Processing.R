### This script "WILL BE" to generate the graph of business days for the Purchasing Bureau to convert requisitions into purchase orders, 
## with a target of 4 business days.

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

## set up business day calculation (WORK IN PROGRESS)
