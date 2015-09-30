Set library path to writable directory, if needed
#.libPaths("C:/Rpackages")

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
source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R") # Calendar for business day calculation

## Read in needed files
GP<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Great Plains master.csv") 
#Dep_code<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Dept Codebook.csv")

## Parse Purchase Order numbers to be consistent with BuySpeed
GP$Vendor<-gsub("\\/.*","",x=GP$Vendor)
GP$Purchase.Order.Number<-gsub("\\:.*","",x=GP$Purchase.Order.Number)

## Trim white space from beginning of department column
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
GP$Department<-trim(GP$Department)
GP$Account.String<-trim(GP$Account.String)

##
GP$Department<-tolower(GP$Department)

## Convert relevant columns into POSIXct
GP$Check.Date<-as.Date(GP$Check.Date,format="%m/%d/%Y")
GP$Invoice.Date<-as.Date(GP$Invoice.Date,format="%m/%d/%Y")
GP$Accounts.Payable.Date<-as.Date(GP$Accounts.Payable.Date,format="%m/%d/%Y")
GP$Stamp.Date<-as.Date(GP$Stamp.Date,format="%m/%d/%Y")

##
GP$Accounts.Payable.Date<-ifelse(GP$Accounts.Payable.Date>GP$Check.Date,GP$Check.Date,GP$Accounts.Payable.Date);class(GP$Accounts.Payable.Date)<-"Date"

## Calculate business days to process checks by Accounts Payable
GP$AP<-bizdays(GP$Accounts.Payable.Date,GP$Check.Date,NOLA_calendar)

## Calculate total days from invoice date (or stamp date, if applicable) to check date
GP$TotalDays<-ifelse(is.na(GP$Stamp.Date),Days(GP,Invoice.Date,Check.Date),Days(GP,Stamp.Date,Check.Date))
GP$TotalDays<-ifelse(GP$TotalDays<0,0,GP$TotalDays) ### Adjust any payments with a check date prior to invoice date to 0 days

## Segment payments into quarters
GP$Qtr<-as.yearqtr(GP$Check.Date,format="%Y-%m-%d") 

## Recode account string to generate department variable
GP$Account_recode<-substr(GP$Account.String,1,4)
GP$Account_recode2<-substr(GP$Account.String,1,9)
GP$Dept<-ifelse(startsWith(GP$Account_recode,"0200"),"Aviation",
                ifelse(startsWith(GP$Account_recode,"0691"),"Library",
                       ifelse(startsWith(GP$Account_recode,"0203"),"Parks & Parkways",
                              ifelse(startsWith(GP$Account_recode,"0205"),"NORDC",
                                     ifelse(startsWith(GP$Account_recode,"0138"),"OCD",
                                            ifelse(startsWith(GP$Account_recode,"0139"),"Mayor's Office - Other",NA))))))
                

##
GP<-select(GP,Check=Check..,PO=Purchase.Order.Number,Account.String,Account_recode,Check.Date:Vendor,Amount:Period)

## Create distribution bins for AP business days and calendar days from invoice to check
GP$APUnder7<-ifelse(GP$AP<=7,1,0)
    GP$AP7_14<-ifelse(GP$AP>7 & GP$AP<=14,1,0)
        GP$APOver14<-ifelse(GP$AP>14,1,0)
GP$TotalUnder30<-ifelse(GP$TotalDays<=30,1,0)
    GP$Total31_60<-ifelse(GP$TotalDays>30 & GP$TotalDays<=60,1,0)
        GP$Total61_90<-ifelse(GP$TotalDays>60 & GP$TotalDays<=90,1,0)    
            GP$Total91_120<-ifelse(GP$TotalDays>90 & GP$TotalDays<=120,1,0)
                GP$TotalOver120<-ifelse(GP$TotalDays>120,1,0)


## Plot business days to process by Accounts Payable
AP_Days<-aggregate(data=GP,AP~Qtr,FUN=mean)
AP_Dayplot<-ggplot(AP_Days,aes(x=factor(Qtr),y=AP))
AP_Dayplot<-AP_Dayplot+geom_bar(stat="identity",fill="steelblue")
AP_Dayplot<-AP_Dayplot+ggtitle("Average Business Days to Process Payments by Accounts Payable")
AP_Dayplot<-AP_Dayplot+xlab("Quarters")
AP_Dayplot<-AP_Dayplot+ylab("Business Days")
AP_Dayplot<-AP_Dayplot+geom_text(aes(y=AP,ymax=AP+1,label=round(AP,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
AP_Dayplot<-AP_Dayplot+geom_hline(aes(yintercept=7,colour="#FF0000"),linetype=2,size=1)
print(AP_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/AP Days.png")

## Plot the distribution percentages of business days to process by quarter
APdist<-select(GP,Qtr,APUnder7,AP7_14,APOver14)
APdist<-aggregate(cbind(APdist$APUnder7,APdist$AP7_14,APdist$APOver14)~Qtr,data=APdist,FUN=sum);colnames(APdist)[grepl("V1", colnames(APdist))] <- "Under7";colnames(APdist)[grepl("V2", colnames(APdist))] <- "Between7_14";colnames(APdist)[grepl("V3", colnames(APdist))] <- "Over14"
#POdist<-subset(POdist,Qtr>"2012 Q4")
APdist$Total<-APdist$Under7+APdist$Between7_14+APdist$Over14
APdist$Under7<-round(APdist$Under7/APdist$Total,3)
APdist$Between7_14<-round(APdist$Between7_14/APdist$Total,3)
APdist$Over14<-round(APdist$Over14/APdist$Total,3)
APdist<-select(APdist,Qtr,Under7,Between7_14,Over14)
APdist<-melt(APdist,id.vars="Qtr")
APdist$position<-ifelse(APdist$variable=="Under7",.6,
                        ifelse(APdist$variable=="Between7_14",.8,.97)) ## Manually set height of data labels to appear at 95% and 75% on y axis
APDist_plot<-ggplot(APdist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Business Days to Process Payments by Accounts Payable")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=3.5)+
  scale_fill_manual(values=c(darkBlue,lightBlue,red),name=" ",labels=c("<=7 Business Days","7-14 Business Days",">14 Business Days"))
print(APDist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/AP Distribution.png")

## Plot average days from invoice date to check date
Check_Days<-aggregate(data=GP,TotalDays~Qtr,FUN=mean)
Check_Dayplot<-ggplot(Check_Days,aes(x=factor(Qtr),y=TotalDays))
Check_Dayplot<-Check_Dayplot+geom_bar(stat="identity",fill="steelblue")
Check_Dayplot<-Check_Dayplot+ggtitle("Average Days from Invoice to Check")
Check_Dayplot<-Check_Dayplot+xlab("Quarters")
Check_Dayplot<-Check_Dayplot+ylab("Days")
Check_Dayplot<-Check_Dayplot+geom_text(aes(y=TotalDays,ymax=TotalDays+1,label=round(TotalDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Check_Dayplot<-Check_Dayplot+geom_hline(aes(yintercept=45,colour="#FF0000"),linetype=2,size=1)
print(Check_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Invoice to Check Days.png")


## Plot the distribution percentages of days from invoice to check
Checkdist<-select(GP,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)
Checkdist<-aggregate(cbind(Checkdist$TotalUnder30,Checkdist$Total31_60,Checkdist$Total61_90,Checkdist$Total91_120,Checkdist$TotalOver120)~Qtr,data=Checkdist,FUN=sum)
    colnames(Checkdist)[grepl("V1", colnames(Checkdist))] <- "TotalUnder30"
        colnames(Checkdist)[grepl("V2", colnames(Checkdist))] <- "Total31_60"
            colnames(Checkdist)[grepl("V3", colnames(Checkdist))] <- "Total61_90"
                colnames(Checkdist)[grepl("V4", colnames(Checkdist))] <- "Total91_120"
                   colnames(Checkdist)[grepl("V5", colnames(Checkdist))] <- "TotalOver120"
#Checkdist<-subset(Checkdist,Qtr>"2012 Q4")
Checkdist$Total<-Checkdist$TotalUnder30+Checkdist$Total31_60+Checkdist$Total61_90+Checkdist$Total91_120+Checkdist$TotalOver120
Checkdist$TotalUnder30<-round(Checkdist$TotalUnder30/Checkdist$Total,3)
Checkdist$Total31_60<-round(Checkdist$Total31_60/Checkdist$Total,3)
Checkdist$Total61_90<-round(Checkdist$Total61_90/Checkdist$Total,3)
Checkdist$Total91_120<-round(Checkdist$Total91_120/Checkdist$Total,3)
Checkdist$TotalOver120<-round(Checkdist$TotalOver120/Checkdist$Total,3)
Checkdist<-select(Checkdist,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)

##Define data label positions for distribution
undermaxcheck<-max(Checkdist$TotalUnder30); 
    min31_60<-min(Checkdist$Total31_60)+undermaxcheck;
         min61_90<-min(Checkdist$Total61_90)+min31_60;
            min91_120<-min(Checkdist$Total91_120)+min61_90;
                overmin<-min(Checkdist$TotalOver120)+min91_120

##Melt function to swing distribution categories and values into two respective columns
Checkdist<-melt(Checkdist,id.vars="Qtr")

Checkdist$position<-ifelse(Checkdist$variable=="TotalUnder30",Checkdist$value-.05,
                     ifelse(Checkdist$variable=="Total31_60",sum(undermaxcheck, min31_60)/2,
                         ifelse(Checkdist$variable=="Total61_90",sum(min31_60,min61_90)/2,
                           ifelse(Checkdist$variable=="Total91_120",sum(min61_90,min91_120)/2,sum(min91_120,overmin)/2)))) ## Set height of data labels to appear at 95% and 75% on y axis


Checkdist$position<-ifelse(Checkdist$variable=="TotalUnder30",Checkdist$value-.02,
                           ifelse(Checkdist$variable=="Total31_60",min31_60-.02,
                                  ifelse(Checkdist$variable=="Total61_90",min61_90-.02,
                                         ifelse(Checkdist$variable=="Total91_120",min91_120-.02,overmin-.02)))) ## Set height of data labels to appear at 95% and 75% on y axis

CheckDist_plot<-ggplot(Checkdist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Days from Invoice to Check")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
  scale_fill_manual(values=c("339900","green",lightBlue,darkBlue,red) ,name=" ",labels=c("<=4 Business Days",">4 Business Days"))
print(CheckDist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payment Distribution.png")

## Export cleaned data set
read.csv(GP,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Great Plains.csv")