.libPaths("C:/Rpackages")

## Load required packages
library(gdata)
library(stringr)
library(lubridate)
library(bizdays)

## Read in needed files
GP<-read.csv("Great Plains.csv") 
GreatPlains<-read.xls("Great Plains 1-1 to 3-16.xlsx",sheet="Source") 
Holidays<-read.xls("Holidays.xlsx",sheet=1)

## Parse Purchase Order numbers to be consistent with BuySpeed
GP$Vendor<-gsub("\\/.*","",x=GP$Vendor)
GP$Purchase.Order.Number<-gsub("\\:.*","",x=GP$Purchase.Order.Number)

## Convert relevant columns into POSIXct
GP$Check.Date<-as.POSIXct(GP$Check.Date,format="%m/%d/%Y")
GP$Invoice.Date<-as.POSIXct(GP$Invoice.Date,format="%m/%d/%Y")
GP$Accounts.Payable.Date<-as.POSIXct(GP$Accounts.Payable.Date,format="%m/%d/%Y")
GP$Stamp.Date<-as.POSIXct(GP$Stamp.Date,format="%m/%d/%Y")
Holidays<-as.vector(Holidays)


