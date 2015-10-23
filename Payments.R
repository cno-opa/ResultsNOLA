## This script analyzes the processing of general fund, as well as capital and grant payments, defined as those payments made through Great Plains and AFIN, respectively.

## Read in needed files
GP<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/Great Plains master.csv") ### "General Fund" data collected from Great Plains
AFIN<-select(read.csv("O:/Projects/ReqtoCheckStat/Query Files/AFIN master.csv"),Check,Fund,PV,Department,Vendor,InvoiceDate,StampDate,APDate,CheckDate,Amount) ### "Capital/Grant Fund" data collected from AFIN
Orgs<-select(read.csv("O:/Projects/ReqtoCheckStat/Query Files/Org Codes.csv"),Org_code=Segment.ID,Dept2=Description)
#Dep_code<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Dept Codebook.csv")

## Great Plains data cleaning

## Parse Great Plains Purchase Order numbers to be consistent with BuySpeed
GP$Vendor<-gsub("\\/.*","",x=GP$Vendor)
GP$Purchase.Order.Number<-gsub("\\:.*","",x=GP$Purchase.Order.Number)

## Trim white space from beginning of Great Plains department column, and convert to lower-case
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
GP$Department<-trim(GP$Department)
GP$Account.String<-trim(GP$Account.String)
GP$Department<-tolower(GP$Department)

## Convert relevant columns into date format 
GP$Check.Date<-as.Date(GP$Check.Date,format="%m/%d/%Y")
GP$Invoice.Date<-as.Date(GP$Invoice.Date,format="%m/%d/%Y")
GP$Accounts.Payable.Date<-as.Date(GP$Accounts.Payable.Date,format="%m/%d/%Y")
GP$Stamp.Date<-as.Date(GP$Stamp.Date,format="%m/%d/%Y")
AFIN$CheckDate<-as.Date(AFIN$CheckDate,format="%m/%d/%Y")
AFIN$InvoiceDate<-as.Date(AFIN$InvoiceDate,format="%m/%d/%Y")
AFIN$APDate<-as.Date(AFIN$APDate,format="%m/%d/%Y")
AFIN$StampDate<-as.Date(AFIN$StampDate,format="%m/%d/%Y")

## Adjust 
GP$Accounts.Payable.Date<-ifelse(GP$Accounts.Payable.Date>GP$Check.Date,GP$Check.Date,GP$Accounts.Payable.Date);class(GP$Accounts.Payable.Date)<-"Date"
AFIN$APDate<-ifelse(AFIN$APDate>AFIN$CheckDate,AFIN$CheckDate,AFIN$APDate);class(AFIN$APDate)<-"Date"

## Calculate business days to process checks by Accounts Payable
GP$AP<-bizdays(GP$Accounts.Payable.Date,GP$Check.Date,NOLA_calendar)
GP$AP<-GP$AP+1
AFIN$AP<-bizdays(AFIN$APDate,AFIN$CheckDate,NOLA_calendar)
AFIN$AP<-AFIN$AP+1

## Calculate total days from invoice date (or stamp date, if applicable) to check date
GP$TotalDays<-ifelse(is.na(GP$Stamp.Date),Days(GP,Invoice.Date,Check.Date),Days(GP,Stamp.Date,Check.Date))
GP$TotalDays<-ifelse(GP$TotalDays<0,0,GP$TotalDays) ### Adjust any payments with a check date prior to invoice date to 0 days
AFIN$TotalDays<-ifelse(is.na(AFIN$StampDate),Days(AFIN,InvoiceDate,CheckDate),Days(AFIN,StampDate,CheckDate))
AFIN$TotalDays<-ifelse(AFIN$TotalDays<0,0,AFIN$TotalDays) ### Adjust any payments with a check date prior to invoice date to 0 days

## Segment payments into quarters
GP$Qtr<-as.yearqtr(GP$Check.Date,format="%Y-%m-%d") 
AFIN$Qtr<-as.yearqtr(AFIN$CheckDate,format="%Y-%m-%d")

## Recode account string to generate department variable.  NEED TO FINISH DEPT RE-CODE.
GP$Org_code<-substr(GP$Account.String,11,14)
GP$Org_code<-trim(GP$Org_code)
GP<-merge(GP,Orgs,by="Org_code",all.x=TRUE)

#GP$Account_recode2<-substr(GP$Account.String,1,9)
#GP$Dept<-ifelse(startsWith(GP$Account_recode,"0200"),"Aviation",
           #     ifelse(startsWith(GP$Account_recode,"0691"),"Library",
                     #  ifelse(startsWith(GP$Account_recode,"0203"),"Parks & Parkways",
                          #    ifelse(startsWith(GP$Account_recode,"0205"),"NORDC",
                              #       ifelse(startsWith(GP$Account_recode,"0138"),"OCD",
                                   #         ifelse(startsWith(GP$Account_recode,"0139"),"Mayor's Office - Other",NA))))))
                

##
GP<-select(GP,Check=Check..,PO=Purchase.Order.Number,Account.String,Org_code,Dept2,Department,Invoice.Date,Stamp.Date,Accounts.Payable.Date,Check.Date,Vendor,Amount,AP,TotalDays,Qtr)

## Create distribution bins for Accounts Payable business days and calendar days from invoice to check
GP$APUnder7<-ifelse(GP$AP<=7,1,0)
    GP$AP7_14<-ifelse(GP$AP>7 & GP$AP<=14,1,0)
        GP$APOver14<-ifelse(GP$AP>14,1,0)
GP$TotalUnder30<-ifelse(GP$TotalDays<=30,1,0)
    GP$Total31_60<-ifelse(GP$TotalDays>30 & GP$TotalDays<=60,1,0)
        GP$Total61_90<-ifelse(GP$TotalDays>60 & GP$TotalDays<=90,1,0)    
            GP$Total91_120<-ifelse(GP$TotalDays>90 & GP$TotalDays<=120,1,0)
                GP$TotalOver120<-ifelse(GP$TotalDays>120,1,0)
AFIN$APUnder7<-ifelse(AFIN$AP<=7,1,0)
    AFIN$AP7_14<-ifelse(AFIN$AP>7 & AFIN$AP<=14,1,0)
        AFIN$APOver14<-ifelse(AFIN$AP>14,1,0)
AFIN$TotalUnder30<-ifelse(AFIN$TotalDays<=30,1,0)
    AFIN$Total31_60<-ifelse(AFIN$TotalDays>30 & AFIN$TotalDays<=60,1,0)
        AFIN$Total61_90<-ifelse(AFIN$TotalDays>60 & AFIN$TotalDays<=90,1,0)    
            AFIN$Total91_120<-ifelse(AFIN$TotalDays>90 & AFIN$TotalDays<=120,1,0)
                AFIN$TotalOver120<-ifelse(AFIN$TotalDays>120,1,0)


## Plot business days to process by Accounts Payable
GF_AP_Days<-aggregate(data=GP,AP~Qtr,FUN=mean)
GF_AP_Dayplot<-ggplot(GF_AP_Days,aes(x=factor(Qtr),y=AP))
GF_AP_Dayplot<-GF_AP_Dayplot+geom_bar(stat="identity",fill="steelblue")
GF_AP_Dayplot<-GF_AP_Dayplot+ggtitle("Average Business Days to Process Payments by Accounts Payable")
GF_AP_Dayplot<-GF_AP_Dayplot+xlab("Quarters")
GF_AP_Dayplot<-GF_AP_Dayplot+ylab("Business Days")
GF_AP_Dayplot<-GF_AP_Dayplot+geom_text(aes(y=AP,ymax=AP+1,label=round(AP,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
GF_AP_Dayplot<-GF_AP_Dayplot+geom_hline(aes(yintercept=7,colour="#FF0000"),linetype=2,size=1)
print(GF_AP_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund AP Days.png")

## Plot business days to process by Accounts Payable
CG_AP_Days<-aggregate(data=AFIN,AP~Qtr,FUN=mean)
CG_AP_Dayplot<-ggplot(CG_AP_Days,aes(x=factor(Qtr),y=AP))
CG_AP_Dayplot<-CG_AP_Dayplot+geom_bar(stat="identity",fill="steelblue")
CG_AP_Dayplot<-CG_AP_Dayplot+ggtitle("Average Business Days to Process Payments by Accounts Payable")
CG_AP_Dayplot<-CG_AP_Dayplot+xlab("Quarters")
CG_AP_Dayplot<-CG_AP_Dayplot+ylab("Business Days")
CG_AP_Dayplot<-CG_AP_Dayplot+geom_text(aes(y=AP,ymax=AP+1,label=round(AP,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
CG_AP_Dayplot<-CG_AP_Dayplot+geom_hline(aes(yintercept=7,colour="#FF0000"),linetype=2,size=1)
print(CG_AP_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/Capital-Grant AP Days.png")

## Plot the distribution percentages of business days to process by quarter
GF_APdist<-select(GP,Qtr,APUnder7,AP7_14,APOver14)
GF_APdist<-aggregate(cbind(GF_APdist$APUnder7,GF_APdist$AP7_14,GF_APdist$APOver14)~Qtr,data=GF_APdist,FUN=sum);colnames(GF_APdist)[grepl("V1", colnames(GF_APdist))] <- "Under7";colnames(GF_APdist)[grepl("V2", colnames(GF_APdist))] <- "Between7_14";colnames(GF_APdist)[grepl("V3", colnames(GF_APdist))] <- "Over14"
#POdist<-subset(POdist,Qtr>"2012 Q4")
GF_APdist$Total<-GF_APdist$Under7+GF_APdist$Between7_14+GF_APdist$Over14
GF_APdist$Under7<-round(GF_APdist$Under7/GF_APdist$Total,3)
GF_APdist$Between7_14<-round(GF_APdist$Between7_14/GF_APdist$Total,3)
GF_APdist$Over14<-round(GF_APdist$Over14/GF_APdist$Total,3)
GF_APdist<-select(GF_APdist,Qtr,Under7,Between7_14,Over14)
GF_APdist<-melt(GF_APdist,id.vars="Qtr")
GF_APdist$position<-ifelse(GF_APdist$variable=="Under7",.6,
                        ifelse(GF_APdist$variable=="Between7_14",.8,.97)) ## Manually set height of data labels to appear at 95% and 75% on y axis
GF_APdist_plot<-ggplot(GF_APdist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Business Days to Process Payments by Accounts Payable")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=3.5)+
  scale_fill_manual(values=c(darkBlue,lightBlue,red),name=" ",labels=c("<=7 Business Days","7-14 Business Days",">14 Business Days"))
print(GF_APdist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund AP Distribution.png")


## Plot average days from invoice date to check date
GF_Check_Days<-aggregate(data=GP,TotalDays~Qtr,FUN=mean)
GF_Check_Dayplot<-ggplot(GF_Check_Days,aes(x=factor(Qtr),y=TotalDays))
GF_Check_Dayplot<-GF_Check_Dayplot+geom_bar(stat="identity",fill="steelblue")
GF_Check_Dayplot<-GF_Check_Dayplot+ggtitle("Average Days from Invoice to GF_Check")
GF_Check_Dayplot<-GF_Check_Dayplot+xlab("Quarters")
GF_Check_Dayplot<-GF_Check_Dayplot+ylab("Days")
GF_Check_Dayplot<-GF_Check_Dayplot+geom_text(aes(y=TotalDays,ymax=TotalDays+1,label=round(TotalDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
GF_Check_Dayplot<-GF_Check_Dayplot+geom_hline(aes(yintercept=45,colour="#FF0000"),linetype=2,size=1)
print(GF_Check_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/GP Invoice to Check Days.png")

## Plot average days from invoice date to check date
CG_Check<-aggregate(data=AFIN,TotalDays~Qtr,FUN=mean)
CG_Checkplot<-ggplot(CG_Check,aes(x=factor(Qtr),y=TotalDays))
CG_Checkplot<-CG_Checkplot+geom_bar(stat="identity",fill="steelblue")
CG_Checkplot<-CG_Checkplot+ggtitle("Average Days from Invoice to Check - Capital/Grant Payments")
CG_Checkplot<-CG_Checkplot+xlab("Quarters")
CG_Checkplot<-CG_Checkplot+ylab("Days")
CG_Checkplot<-CG_Checkplot+geom_text(aes(y=TotalDays,ymax=TotalDays+1,label=round(TotalDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
CG_Checkplot<-CG_Checkplot+geom_hline(aes(yintercept=45,colour="#FF0000"),linetype=2,size=1)
print(CG_Checkplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/AFIN Invoice to Check Days.png")

## Plot the distribution percentages of days from invoice to check
GF_dist<-select(GP,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)
GF_dist<-aggregate(cbind(GF_dist$TotalUnder30,GF_dist$Total31_60,GF_dist$Total61_90,GF_dist$Total91_120,GF_dist$TotalOver120)~Qtr,data=GF_dist,FUN=sum)
    colnames(GF_dist)[grepl("V1", colnames(GF_dist))] <- "TotalUnder30"
        colnames(GF_dist)[grepl("V2", colnames(GF_dist))] <- "Total31_60"
            colnames(GF_dist)[grepl("V3", colnames(GF_dist))] <- "Total61_90"
                colnames(GF_dist)[grepl("V4", colnames(GF_dist))] <- "Total91_120"
                   colnames(GF_dist)[grepl("V5", colnames(GF_dist))] <- "TotalOver120"
#GF_dist<-subset(GF_dist,Qtr>"2012 Q4")
GF_dist$Total<-GF_dist$TotalUnder30+GF_dist$Total31_60+GF_dist$Total61_90+GF_dist$Total91_120+GF_dist$TotalOver120
GF_dist$TotalUnder30<-round(GF_dist$TotalUnder30/GF_dist$Total,3)
GF_dist$Total31_60<-round(GF_dist$Total31_60/GF_dist$Total,3)
GF_dist$Total61_90<-round(GF_dist$Total61_90/GF_dist$Total,3)
GF_dist$Total91_120<-round(GF_dist$Total91_120/GF_dist$Total,3)
GF_dist$TotalOver120<-round(GF_dist$TotalOver120/GF_dist$Total,3)
GF_dist<-select(GF_dist,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)

##Define data label positions for distribution
undermaxcheck<-max(GF_dist$TotalUnder30); 
    min31_60<-min(GF_dist$Total31_60)+undermaxcheck;
         min61_90<-min(GF_dist$Total61_90)+min31_60;
            min91_120<-min(GF_dist$Total91_120)+min61_90;
                overmin<-min(GF_dist$TotalOver120)+min91_120

##Melt function to swing distribution categories and values into two respective columns
GF_dist<-melt(GF_dist,id.vars="Qtr")

GF_dist$position<-ifelse(GF_dist$variable=="TotalUnder30",GF_dist$value-.05,
                     ifelse(GF_dist$variable=="Total31_60",sum(undermaxcheck, min31_60)/2,
                         ifelse(GF_dist$variable=="Total61_90",sum(min31_60,min61_90)/2,
                           ifelse(GF_dist$variable=="Total91_120",sum(min61_90,min91_120)/2,sum(min91_120,overmin)/2)))) ## Set height of data labels to appear at 95% and 75% on y axis


GF_dist$position<-ifelse(GF_dist$variable=="TotalUnder30",GF_dist$value-.02,
                           ifelse(GF_dist$variable=="Total31_60",min31_60-.02,
                                  ifelse(GF_dist$variable=="Total61_90",min61_90-.02,
                                         ifelse(GF_dist$variable=="Total91_120",min91_120-.02,overmin-.02)))) ## Set height of data labels to appear at 95% and 75% on y axis

GF_dist_plot<-ggplot(GF_dist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Days from Invoice to Check")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
  scale_fill_manual(values=c("339900","green",lightBlue,darkBlue,red) ,name=" ",labels=c("<=4 Business Days",">4 Business Days"))
print(GF_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund Payment Distribution.png")

## Export cleaned data set
write.csv(GP,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Great Plains.csv")
write.csv(AFIN,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/AFIN.csv")
