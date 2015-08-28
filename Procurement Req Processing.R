### This script to generate the graph of business days for the Purchasing Bureau to convert requisitions into purchase orders, 
## with a target of 4 business days.

##load relevant packages
library(plyr)
library(ggplot2)
library(dplyr)
library(scales) ## to scale dates into month-year format in graphs
library(lubridate)
library(bizdays)

## read, clean, and format
headings<-c("Dept","Reqnumber","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")
Reqs<-read.csv("ProcurementReqProcessing.csv",col.names=headings,stringsAsFactors=FALSE,skip=3)

Reqs$Cost<-as.numeric(sub("\\$","",Reqs$Cost))

## Clean out errors
Reqs2<-filter(Reqs,Vendor == "Independent Stationers" & Cost == 30.48)
Reqmatch<-Reqs$Reqnumber %in% Reqs2$Reqnumber
Reqs<-Reqs[!Reqs$Reqnumber %in% test$Reqnumber,]

## names(Reqs)=headings
## Reqs<-Reqs[-c(1,2,3),]
Reqs$FinanceDate<-as.Date(Reqs$FinanceDate,"%m/%d/%Y")
Reqs$POdate<-as.Date(Reqs$POdate,"%m/%d/%Y")
Reqs<-filter(Reqs,POdate>FinanceDate)
Reqs$Qtr<-as.yearqtr(Reqs$POdate,format="%m/%d/%Y")


## set up business day calculation 
Calendar(holidays=c("2012-1-2",  "2012-1-16",	"2012-2-21",	"2012-4-6",	"2012-5-28",	"2012-7-4",	"2012-9-3",	"2012-11-22",	"2012-11-23",	"2012-12-25",	"2013-1-1",	"2013-1-21",	"2013-2-12",	"2013-3-29",	"2013-5-27",	"2013-7-4",	"2013-9-2",	"2013-11-28",	"2013-11-29",	"2013-12-25",	"2014-1-1",	"2014-1-20",	"2014-3-4",	"2014-4-18",	"2014-5-26",	"2014-7-4",	"2014-9-1",	"2014-11-27",	"2014-11-28",	"2014-12-25",	"2015-1-1",	"2015-1-19",	"2015-2-17",	"2015-4-3",	"2015-5-25",	"2015-7-3",	"2015-9-7",	"2015-11-26",	"2015-11-27",	"2015-12-25"),start.date="2012-1-1",end.date="2015-12-31",name="NOLA_calendar",weekdays=c("saturday","sunday"))
Reqs$WorkingDays<-bizdays(Reqs$FinanceDate,Reqs$POdate,NOLA_calendar)

Days2PO<-ddply(Reqs,"Qtr",summarise,PO=mean(WorkingDays))
Purchasing<-ggplot(Days2PO,aes(x=factor(Qtr),y=PO))
Purchasing<-Purchasing+geom_bar(stat="identity",fill="steelblue")
Purchasing<-Purchasing+ggtitle("Average Business Days to Process Purchase Orders")
Purchasing<-Purchasing+xlab("Quarters")
Purchasing<-Purchasing+ylab("Business Days")
Purchasing<-Purchasing+geom_text(aes(y=PO,ymax=PO+1,label=round(PO,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Purchasing<-Purchasing+geom_hline(aes(yintercept=4,colour="red"),linetype=2)
print(Purchasing)
ggsave("./Output/Days to Execute.png")

write.csv(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Purchase Orders.csv")
