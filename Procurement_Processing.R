### This script to generate the graph of business days for the Purchasing Bureau to convert requisitions into purchase orders, 
## with a target of 4 business days.

##load relevant packages
library(plyr)
library(ggplot2)
library(dplyr)
library(scales) ## to scale dates into month-year format in graphs
library(lubridate)
library(bizdays)
library(zoo)
library(reshape2)

## read, clean, and format
headings<-c("Dept","Req","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")
Reqs<-read.csv("ProcurementReqProcessing.csv",col.names=headings,stringsAsFactors=FALSE,skip=3)
ReqStatus<-select(read.csv("Req Status.csv",skip=3),Req=REQ_NBR,Status=STATUS)
#Category<-select(read.csv("PObyCategory.csv",skip=3),Req=REQ_NBR,Descr=DESC_LONG)

# Standardize req number variables in two data sets
Reqs<-merge(Reqs,ReqStatus,by="Req",all.x=TRUE)
#Reqs<-merge(Reqs,Category,by="Req",all.x=TRUE)

## Convert dollar amount column to numeric class for readability
Reqs$Cost<-as.numeric(sub("\\$","",Reqs$Cost))

## Clean out errors
Exclude<-Reqs[Reqs$Vendor=="Independent Stationers" & Reqs$Cost==30.48|Reqs$Vendor=="Independent Stationers" & Reqs$Cost==0|Reqs$Vendor=="FASTENAL COMPANY" & Reqs$Cost==0 | Reqs$Vendor=="Independent Stationers" & Reqs$Cost==30.44 | Reqs$Vendor=="Independent Stationers" & Reqs$Cost==34.80 | Reqs$Vendor=="Independent Stationers" & Reqs$Cost==53.88 | Reqs$Vendor=="Grainger, Inc." & Reqs$Cost==99.09,]
Reqs<-anti_join(Reqs,Exclude,by="Req")


## names(Reqs)=headings
## Reqs<-Reqs[-c(1,2,3),]
Reqs$FinanceDate<-as.Date(Reqs$FinanceDate,"%m/%d/%Y")
Reqs$POdate<-as.Date(Reqs$POdate,"%m/%d/%Y")
Reqs<-filter(Reqs,POdate>FinanceDate)
Reqs$Qtr<-as.yearqtr(Reqs$POdate,format="%m/%d/%Y")


## set up business day calculation 
NOLA_calendar<-Calendar(holidays=c("2011-1-3","2011-1-17","2011-12-25","2011-3-8","2011-4-22","2011-9-5","2011-5-30","2011-7-","2012-1-2", "2012-1-16",	"2012-2-21",	"2012-4-6",	"2012-5-28",	"2012-7-4",	"2012-9-3",	"2012-11-22",	"2012-11-23",	"2012-12-25",	"2013-1-1",	"2013-1-21",	"2013-2-12",	"2013-3-29",	"2013-5-27",	"2013-7-4",	"2013-9-2",	"2013-11-28",	"2013-11-29",	"2013-12-25",	"2014-1-1",	"2014-1-20",	"2014-3-4",	"2014-4-18",	"2014-5-26",	"2014-7-4",	"2014-9-1",	"2014-11-27",	"2014-11-28",	"2014-12-25",	"2015-1-1",	"2015-1-19",	"2015-2-17",	"2015-4-3",	"2015-5-25",	"2015-7-3",	"2015-9-7",	"2015-11-26",	"2015-11-27",	"2015-12-25"),start.date="2012-1-1",end.date="2015-12-31",name="NOLA_calendar",weekdays=c("saturday","sunday"))
Reqs$WorkingDays<-bizdays(Reqs$FinanceDate,Reqs$POdate,NOLA_calendar)

## Create distribution bins for business days to process
Reqs$Under4<-ifelse(Reqs$WorkingDays<=4,1,0)
Reqs$Over4<-ifelse(Reqs$WorkingDays<=4,0,1)

## Plot the business days to process by quarter
Days2PO<-aggregate(data=Reqs,WorkingDays~Qtr,FUN=mean)
Days2PO<-subset(Days2PO,Qtr>"2012 Q4")
Purchasing<-ggplot(Days2PO,aes(x=factor(Qtr),y=WorkingDays))
Purchasing<-Purchasing+geom_bar(stat="identity",fill="steelblue")
Purchasing<-Purchasing+ggtitle("Average Business Days to Process Purchase Orders")
Purchasing<-Purchasing+xlab("Quarters")
Purchasing<-Purchasing+ylab("Business Days")
Purchasing<-Purchasing+geom_text(aes(y=WorkingDays,ymax=WorkingDays+1,label=round(WorkingDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Purchasing<-Purchasing+geom_hline(aes(yintercept=4,colour="#FF0000"),linetype=2,size=1)
print(Purchasing)
ggsave("./Slides/Days to PO.png")

## Plot the distribution percentages of business days to process by quarter
POdist<-select(Reqs,Qtr,Under4,Over4)
POdist<-aggregate(cbind(POdist$Under4,POdist$Over4)~Qtr,data=POdist,FUN=sum);colnames(POdist)[grepl("V1", colnames(POdist))] <- "Under4";colnames(POdist)[grepl("V2", colnames(POdist))] <- "Over4"
POdist<-subset(POdist,Qtr>"2012 Q4")
  POdist$Total<-POdist$Under4+POdist$Over4
      POdist$Under_4<-round(POdist$Under4/POdist$Total,3)
          POdist$Over_4<-round(POdist$Over4/POdist$Total,3)
POdist<-select(POdist,Qtr,Under_4,Over_4)
POdist<-melt(POdist,id.vars="Qtr")
POdist$position<-ifelse(POdist$variable=="Over_4",.95,.75) ## Manually set height of data labels to appear at 95% and 75% on y axis
#POdist<-ddply(POdist,.(Qtr),transform,position=cumsum(value)-0.5)
Dist_plot<-ggplot(POdist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
    scale_y_continuous(labels = percent_format())+
       ggtitle("Distribution of Business Days to Process Purchase Orders")+
          xlab("Quarters")+ylab("Percent")+
             geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
                 scale_fill_manual(values=c(lightBlue,red),name=" ",labels=c("<=4 Business Days",">4 Business Days"))
print(Dist_plot)
ggsave("./Slides/PO Distribution.png")

## Export the data
write.csv(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Purchase Orders.csv")
