# Load required packages
library(tidyr) # Contracts
library(plyr)  # All
library(dplyr) # All
library(lubridate) # All
library(xlsx) # Contracts
library(gdata)# Contracts
library(stringr)# Contracts and payments
library(r2excel)# Contracts (not sure if needed)
library(reshape2)# Contracts and procurement
library(zoo)# All
library(ggplot2)# All
library(gdata) # Payments
library(bizdays)# Payments and Procurement

# Set up calendar to enable business day calculations
NOLA_calendar<-Calendar(holidays=c("2012-1-2",  "2012-1-16",  "2012-2-21",  "2012-4-6",	"2012-5-28",	"2012-7-4",	"2012-9-3",	"2012-11-22",	"2012-11-23",	"2012-12-25",	"2013-1-1",	"2013-1-21",	"2013-2-12",	"2013-3-29",	"2013-5-27",	"2013-7-4",	"2013-9-2",	"2013-11-28",	"2013-11-29",	"2013-12-25",	"2014-1-1",	"2014-1-20",	"2014-3-4",	"2014-4-18",	"2014-5-26",	"2014-7-4",	"2014-9-1",	"2014-11-27",	"2014-11-28",	"2014-12-25",	"2015-1-1",	"2015-1-19",	"2015-2-17",	"2015-4-3",	"2015-5-25",	"2015-7-3",	"2015-9-7",	"2015-11-26",	"2015-11-27",	"2015-12-25"),start.date="2012-1-1",end.date="2015-12-31",name="NOLA_calendar",weekdays=c("saturday","sunday"))