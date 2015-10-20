## This file runs all requirements and script files needed to put together all scripted sections of ReqtoCheckSTAT and currently assumes computer running the script has access to the OPA share drive. 

.libPaths("C:/Rpackages")

### Function for reading R files directly from github.com
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}


### Create function to calculate days between two dates, with rounding defaulted to the whole number
Days<-function(df,FirstDt,EndDt,digits=0){
  arguments<-as.list(match.call())  
  
  ### Identify args as dataframe columns
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  
  ### Calculate days betweens first date and end date, rounding to the specified number of decimal places, with a default of 0
  round((strptime(EndDt,"%Y-%m-%d")-strptime(FirstDt,"%Y-%m-%d"))/86400,digits)
}

ReqDays<-function(df,FirstDt,EndDt,digits=0){
  arguments<-as.list(match.call())  
  
  ### Identify args as dataframe columns
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  
  ### Calculate days betweens first date and end date, rounding to the specified number of decimal places, with a default of 0
  round(as.POSIXct(EndDt,"%m/%d/%Y %H:%M")-as.POSIXct(FirstDt,"%m/%d/%Y %H:%M"),digits)/86400
}

## Create function for days in current stage for open contracts at end of the reporting period, rounded to whole numbers
Age<-function(df,PrevDt,QtrEnd){
  arguments<-as.list(match.call())  
  PrevDt<-eval(arguments$PrevDt,df)
  as.Date(as.vector(QtrEnd),format="%Y-%m-%d")
  round((strptime(QtrEnd,"%Y-%m-%d")-strptime(PrevDt,"%Y-%m-%d"))/86400,digits=0)
}

## Create function for days since contract creation for open contracts at end of the reporting period, rounded to whole numbers
TotalAge<-function(df,StartDt,QtrEnd){
  arguments<-as.list(match.call())  
  StartDt<-eval(arguments$StartDt,df)
  as.Date(as.vector(QtrEnd),format="%Y-%m-%d")
  round((strptime(QtrEnd,"%Y-%m-%d")-strptime(StartDt,"%Y-%m-%d"))/86400,digits=0)
}

### Function to add regression equation and r-squared to scatterplot
lm_eqn = function(m) {    
   l <- list(a = format(coef(m)[1], digits = 2),       
             b = format(abs(coef(m)[2]), digits = 2),       
             r2 = format(summary(m)$r.squared, digits = 3));
   if (coef(m)[2] >= 0)  {     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)   } 
   else {     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)       }
   as.character(as.expression(eq));
}

source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") # Load OPA theme
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Requirements.R") # Load required packages
source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R")# Load calendar for business day calculations

### Load component scripts
#source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Reqs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Procurement.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Bids_RFPs_DBEs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_POs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Gen_Fund_Payments.R")