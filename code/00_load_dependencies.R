library(devtools) #### for source_url function in Main.R
library(tidyr) # Contracts
library(plyr)  # May still be used
library(dplyr) # All
library(lubridate) # All
# library(xlsx) # Contracts
library(stringr)# Contracts and payments narrative
library(reshape2)# Contracts and procurement
library(zoo)# All (for as.yearqtr class)
library(ggplot2)# All
library(bizdays)# For business day processing
# library(scales) # Not sure if this is needed anymore
# library(RCurl) # Not sure this is needed anymore
# library(data.table)#### setnames function (not sure if needed)
library(readxl) ### Reading in Adjustments.xlsx
library(RODBC) ### Needed for SQL queries
library(feather)
library(readr)

# source_url("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") 

source('code/functions/query_and_feather.R')


holiday_list <- read_csv('data/holiday_list.csv')

holiday_list <- holiday_list %>%
  mutate(holidays = mdy(holidays))

NOLA_calendar<-create.calendar(holidays= holiday_list$holidays,
                               start.date="2011-01-1",end.date="2017-12-31",
                               name="NOLA_calendar",
                               weekdays=c("saturday","sunday"))


r_period<-as.yearqtr(as.Date(Sys.Date())-1)

last<- as.Date(as.yearqtr(r_period)+0.25)-1
