.libPaths("C:/Rpackages")

## Download OPA theme, as well as required packages from OPA github account
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Requirements.R")

## Create function for days between invoice date and check date, rounded to whole numbers
PayDays<-function(df,InvoiceDt,CheckDt){
  arguments<-as.list(match.call())  
  CheckDt<-eval(arguments$CheckDt,df)
  InvoiceDt<-eval(arguments$InvoiceDt,df)
  round((strptime(CheckDt,"%Y-%m-%d")-strptime(InvoiceDt,"%Y-%m-%d"))/86400,digits=0)
}

## Read in needed files
GP<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Great Plains master.csv") 
Dep_code<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Dept Codebook.csv")

## Parse Purchase Order numbers to be consistent with BuySpeed
GP$Vendor<-gsub("\\/.*","",x=GP$Vendor)
GP$Purchase.Order.Number<-gsub("\\:.*","",x=GP$Purchase.Order.Number)

# Trim white space from beginning of department column
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
GP$Department<-trim(GP$Department)
GP$Account.String<-trim(GP$Account.String)

#
GP$Department<-tolower(GP$Department)

## Convert relevant columns into POSIXct
GP$Check.Date<-as.Date(GP$Check.Date,format="%m/%d/%Y")
GP$Invoice.Date<-as.Date(GP$Invoice.Date,format="%m/%d/%Y")
GP$Accounts.Payable.Date<-as.Date(GP$Accounts.Payable.Date,format="%m/%d/%Y")
GP$Stamp.Date<-as.Date(GP$Stamp.Date,format="%m/%d/%Y")

# 
GP$Accounts.Payable.Date<-ifelse(GP$Accounts.Payable.Date>GP$Check.Date,GP$Check.Date,GP$Accounts.Payable.Date);class(GP$Accounts.Payable.Date)<-"Date"

## set up calendar for calculation for Account
NOLA_calendar<-Calendar(holidays=c("2012-1-2",  "2012-1-16",  "2012-2-21",	"2012-4-6",	"2012-5-28",	"2012-7-4",	"2012-9-3",	"2012-11-22",	"2012-11-23",	"2012-12-25",	"2013-1-1",	"2013-1-21",	"2013-2-12",	"2013-3-29",	"2013-5-27",	"2013-7-4",	"2013-9-2",	"2013-11-28",	"2013-11-29",	"2013-12-25",	"2014-1-1",	"2014-1-20",	"2014-3-4",	"2014-4-18",	"2014-5-26",	"2014-7-4",	"2014-9-1",	"2014-11-27",	"2014-11-28",	"2014-12-25",	"2015-1-1",	"2015-1-19",	"2015-2-17",	"2015-4-3",	"2015-5-25",	"2015-7-3",	"2015-9-7",	"2015-11-26",	"2015-11-27",	"2015-12-25"),start.date="2012-1-1",end.date="2015-12-31",name="NOLA_calendar",weekdays=c("saturday","sunday"))

# Calculate business days to process checks by Accounts Payable
GP$APDays<-bizdays(GP$Accounts.Payable.Date,GP$Check.Date,NOLA_calendar)

# Calculate total days from invoice date (or stamp date, if applicable) to check date
GP$TotalDays<-ifelse(is.na(GP$Stamp.Date),PayDays(GP,Invoice.Date,Check.Date),PayDays(GP,Stamp.Date,Check.Date))

# Segment payments into quarters
GP$Period<-as.yearqtr(GP$Account.String,format="%m/%d/%Y") 

# Recode account string to generate department variable
GP$Account_recode<-substr(GP$Account.String,1,4)
GP$Account_recode2<-substr(GP$Account.String,1,9)
GP$Dept<-ifelse(startsWith(GP$Account_recode,"0200"),"Aviation",
                ifelse(startsWith(GP$Account_recode,"0691"),"Library",
                       ifelse(startsWith(GP$Account_recode,"0203"),"Parks & Parkways",
                              ifelse(startsWith(GP$Account_recode,"0205"),"NORDC",
                                     ifelse(startsWith(GP$Account_recode,"0138"),"OCD",
                                            ifelse(startsWith(GP$Account_recode,"0139"),"Mayor's Office - Other",NA))))))
                


GP<-select(GP,Check=Check..,PO=Purchase.Order.Number,Account.String,Account_recode,Check.Date:Vendor,Amount:Period)


