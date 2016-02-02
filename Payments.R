### This script analyzes the processing of general fund, as well as capital and grant payments, defined as those payments made through Great Plains and AFIN, respectively.


### Import data
GP<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/Great Plains master.csv") ### "General Fund" data collected from Great Plains
AFIN<-select(read.csv("O:/Projects/ReqtoCheckStat/Query Files/AFIN master.csv"),Check,Fund,PV,Department,Vendor,InvoiceDate,StampDate,APDate,CheckDate,Amount) ### "Capital/Grant Fund" data collected from AFIN
#Orgs<-select(read.csv("O:/Projects/ReqtoCheckStat/Query Files/Org Codes.csv"),Org_code=Segment.ID,Org_desc=Description)
#Agencies<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Agency Codes.csv"),Agency_code=Segment.ID,Agency_desc=Description)


### Data cleaning

#### Parse Great Plains Purchase Order numbers to be consistent with BuySpeed
GP$Vendor<-gsub("\\/.*","",x=GP$Vendor)
GP$Purchase.Order.Number<-gsub("\\:.*","",x=GP$Purchase.Order.Number)

#### Trim white space from beginning of Great Plains department column, and convert to lower-case
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
GP$Department<-trim(GP$Department)
GP$Account.String<-trim(GP$Account.String)
GP$Department<-tolower(GP$Department)

#### Convert relevant columns into date format
GP$Check.Date<-as.Date(GP$Check.Date,format="%m/%d/%Y")
GP$Invoice.Date<-as.Date(GP$Invoice.Date,format="%m/%d/%Y")
GP$Accounts.Payable.Date<-as.Date(GP$Accounts.Payable.Date,format="%m/%d/%Y")
GP$Stamp.Date<-as.Date(GP$Stamp.Date,format="%m/%d/%Y")
AFIN$CheckDate<-as.Date(AFIN$CheckDate,format="%m/%d/%Y")
AFIN$InvoiceDate<-as.Date(AFIN$InvoiceDate,format="%m/%d/%Y")
AFIN$APDate<-as.Date(AFIN$APDate,format="%m/%d/%Y")
AFIN$StampDate<-as.Date(AFIN$StampDate,format="%m/%d/%Y")

#### Code accounts payable date to same as check date, if missing
GP$Accounts.Payable.Date<-ifelse(GP$Accounts.Payable.Date>GP$Check.Date,GP$Check.Date,GP$Accounts.Payable.Date);class(GP$Accounts.Payable.Date)<-"Date"
AFIN$APDate<-ifelse(AFIN$APDate>AFIN$CheckDate,AFIN$CheckDate,AFIN$APDate);class(AFIN$APDate)<-"Date"

#### Calculate business days to process checks by Accounts Payable
GP$AP<-bizdays(GP$Accounts.Payable.Date,GP$Check.Date,NOLA_calendar)
GP$AP<-GP$AP+1
AFIN$AP<-bizdays(AFIN$APDate,AFIN$CheckDate,NOLA_calendar)
AFIN$AP<-AFIN$AP+1

#### Calculate total days from invoice date (or stamp date, if applicable) to check date
GP$TotalDays<-ifelse(is.na(GP$Stamp.Date),Days(GP,Invoice.Date,Check.Date),Days(GP,Stamp.Date,Check.Date))
GP$TotalDays<-ifelse(GP$TotalDays<0,0,GP$TotalDays) ### Adjust any payments with a check date prior to invoice date to 0 days
AFIN$TotalDays<-ifelse(is.na(AFIN$StampDate),Days(AFIN,InvoiceDate,CheckDate),Days(AFIN,StampDate,CheckDate))
AFIN$TotalDays<-ifelse(AFIN$TotalDays<0,0,AFIN$TotalDays) ### Adjust any payments with a check date prior to invoice date to 0 days

#### Segment payments into quarters
GP$Qtr<-as.yearqtr(GP$Check.Date,format="%Y-%m-%d")
AFIN$Qtr<-as.yearqtr(AFIN$CheckDate,format="%Y-%m-%d")


#### Recode account string to generate department variable

##### Extract agency and org codes from the five-segment accounting string for each transaction
GP$Agency_code<-substr(GP$Account.String,6,9); GP$Agency_code<-ifelse(startsWith(GP$Agency_code,"0"),substr(GP$Agency_code,2,4),GP$Agency_code)
GP$Org_code<-substr(GP$Account.String,11,14)

##### Trim extraneous characters and white space from agency and org codes in master dataset, as well as codebook datasets
#Agencies$Agency_code<-trim(Agencies$Agency_code)
#Orgs$Org_codes<-trim(Orgs$Org_code)
#GP[c("Agency_code","Org_code")]<-lapply(GP[c("Agency_code","Org_code")], trim)

##### 
#GP$Agency<-ifelse(GP$Agency_code %in% Agencies$Agency_code, as.character(Agencies$Agency_desc),NA)
#GP$Org<-ifelse(GP$Org_code %in% Orgs$Org_code, as.character(Orgs$Org_desc),NA)
#GP$Dept<-paste(as.character(GP$Agency)," - ",as.character(GP$Org))

               
#### Select desired columns
GP<-select(GP,Check=Check..,PO=Purchase.Order.Number,
    Account.String,Org_code,Dept2,Department,Invoice.Date,Stamp.Date,
    Accounts.Payable.Date,Check.Date,Vendor,Amount,AP,TotalDays,Qtr)

#### Create distribution bins for Accounts Payable business days and calendar days from invoice to check
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
AFIN$TotalUnder45<-ifelse(AFIN$TotalDays<=45,1,0)
    AFIN$Total46_90<-ifelse(AFIN$TotalDays>45 & AFIN$TotalDays<=90,1,0)
        AFIN$Total91_135<-ifelse(AFIN$TotalDays>90 & AFIN$TotalDays<=135,1,0)
          AFIN$TotalOver135<-ifelse(AFIN$TotalDays>135,1,0)



### Plotting: Accounts Payable business days

#### Plot business days to process by Accounts Payable
GF_AP_Days<-aggregate(data=GP,AP~Qtr,FUN=mean)
GF_AP_Dayplot<-ggplot(GF_AP_Days,aes(x=factor(Qtr),y=AP))+
    geom_bar(stat="identity",fill="steelblue")+
      ggtitle("Business Days to Process General Fund Payments by Accounts Payable")+
        xlab("Quarters")+ylab("Business Days")+
          geom_text(aes(y=AP,ymax=AP+1,label=round(AP,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
            geom_hline(aes(yintercept=7,colour="#FF0000"),linetype=2,size=1)+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(GF_AP_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund AP Days.png")

#### Plot business days to process by Accounts Payable
CG_AP_Days<-aggregate(data=AFIN,AP~Qtr,FUN=mean)
CG_AP_Dayplot<-ggplot(CG_AP_Days,aes(x=factor(Qtr),y=AP))+
    geom_bar(stat="identity",fill="steelblue")+
      ggtitle("Business Days to Process Capital/Grant Payments by Accounts Payable")+
        xlab("Quarters")+ylab("Business Days")+
          geom_text(aes(y=AP,ymax=AP+1,label=round(AP,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
            geom_hline(aes(yintercept=7,colour="#FF0000"),linetype=2,size=1)+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(CG_AP_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/Capital-Grant AP Days.png")


### Plotting: Accounts Payable distributions

#### Plot the distribution percentages of business days to process general fund payments by quarter
GF_APdist<-select(GP,Qtr,APUnder7,AP7_14,APOver14)
GF_APdist<-aggregate(cbind(GF_APdist$APUnder7,GF_APdist$AP7_14,GF_APdist$APOver14)~Qtr,data=GF_APdist,FUN=sum);colnames(GF_APdist)[grepl("V1", colnames(GF_APdist))] <- "Under7";colnames(GF_APdist)[grepl("V2", colnames(GF_APdist))] <- "Between7_14";colnames(GF_APdist)[grepl("V3", colnames(GF_APdist))] <- "Over14"
GF_APdist$Total<-GF_APdist$Under7+GF_APdist$Between7_14+GF_APdist$Over14
GF_APdist$Under7<-round(GF_APdist$Under7/GF_APdist$Total,3)
GF_APdist$Between7_14<-round(GF_APdist$Between7_14/GF_APdist$Total,3)
GF_APdist$Over14<-round(GF_APdist$Over14/GF_APdist$Total,3)
GF_APdist<-select(GF_APdist,Qtr,Under7,Between7_14,Over14)
GF_APdist<-melt(GF_APdist,id.vars="Qtr")
GF_APdist_plot<-ggplot(GF_APdist,aes(x = factor(Qtr), y = value,fill = variable)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Business Days to Process \nPayments by Accounts Payable")+
  xlab("Quarters")+ylab("Percent")+
  scale_fill_manual(values=c(darkBlue,lightBlue,red),name=" ",labels=c("<=7 Business Days","7-14 Business Days",">14 Business Days"))+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(GF_APdist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund AP Distribution.png")

#### Plot the distribution percentages of business days to process general fund payments by quarter
CG_APdist<-select(GP,Qtr,APUnder7,AP7_14,APOver14)
CG_APdist<-aggregate(cbind(CG_APdist$APUnder7,CG_APdist$AP7_14,CG_APdist$APOver14)~Qtr,data=CG_APdist,FUN=sum);colnames(CG_APdist)[grepl("V1", colnames(CG_APdist))] <- "Under7";colnames(CG_APdist)[grepl("V2", colnames(CG_APdist))] <- "Between7_14";colnames(CG_APdist)[grepl("V3", colnames(CG_APdist))] <- "Over14"
CG_APdist$Total<-CG_APdist$Under7+CG_APdist$Between7_14+CG_APdist$Over14
CG_APdist$Under7<-round(CG_APdist$Under7/CG_APdist$Total,3)
CG_APdist$Between7_14<-round(CG_APdist$Between7_14/CG_APdist$Total,3)
CG_APdist$Over14<-round(CG_APdist$Over14/CG_APdist$Total,3)
CG_APdist<-select(CG_APdist,Qtr,Under7,Between7_14,Over14)
CG_APdist<-melt(CG_APdist,id.vars="Qtr")
CG_APdist_plot<-ggplot(CG_APdist,aes(x = factor(Qtr), y = value,fill = variable)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Business Days to Process n\ Payments by Accounts Payable")+
  xlab("Quarters")+ylab("Percent")+
  scale_fill_manual(values=c(darkBlue,lightBlue,red),name=" ",labels=c("<=7 Business Days","7-14 Business Days",">14 Business Days"))+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(CG_APdist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund AP Distribution.png")


### Plotting: Days from Invoice to Check

#### Plot average days from invoice date to check date
GF_Check_Days<-aggregate(data=GP,TotalDays~Qtr,FUN=mean)
GF_Check_Dayplot<-ggplot(GF_Check_Days,aes(x=factor(Qtr),y=TotalDays))+
  geom_bar(stat="identity",fill="steelblue")+
    ggtitle("Days from Invoice to Check - General Fund")+
      xlab("Quarters")+ylab("Days")+
        geom_text(aes(y=TotalDays,ymax=TotalDays+1,label=round(TotalDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
          geom_hline(aes(yintercept=30,colour="#FF0000"),linetype=2,size=1)+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(GF_Check_Dayplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/GP Invoice to Check Days.png")

#### Plot average days from invoice date to check date for Capital/Grant payments
CG_Check<-aggregate(data=AFIN,TotalDays~Qtr,FUN=mean)
CG_Checkplot<-ggplot(CG_Check,aes(x=factor(Qtr),y=TotalDays))+
  geom_bar(stat="identity",fill="steelblue")+
    ggtitle("Days from Invoice to Check - Capital/Grant")+
      xlab("Quarters")+ylab("Days")+
        geom_text(aes(y=TotalDays,ymax=TotalDays+1,label=round(TotalDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
          geom_hline(aes(yintercept=45,colour="#FF0000"),linetype=2,size=1)+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(CG_Checkplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/AFIN Invoice to Check Days.png")


### Plotting: Days from Invoice to Check distributions

#### Plot the distribution percentages of days from invoice to check
GF_dist<-select(GP,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)
GF_dist<-aggregate(cbind(GF_dist$TotalUnder30,GF_dist$Total31_60,GF_dist$Total61_90,GF_dist$Total91_120,GF_dist$TotalOver120)~Qtr,data=GF_dist,FUN=sum)
    colnames(GF_dist)[grepl("V1", colnames(GF_dist))] <- "TotalUnder30"
        colnames(GF_dist)[grepl("V2", colnames(GF_dist))] <- "Total31_60"
            colnames(GF_dist)[grepl("V3", colnames(GF_dist))] <- "Total61_90"
                colnames(GF_dist)[grepl("V4", colnames(GF_dist))] <- "Total91_120"
                   colnames(GF_dist)[grepl("V5", colnames(GF_dist))] <- "TotalOver120"
GF_dist$Total<-GF_dist$TotalUnder30+GF_dist$Total31_60+GF_dist$Total61_90+GF_dist$Total91_120+GF_dist$TotalOver120
GF_dist$TotalUnder30<-round(GF_dist$TotalUnder30/GF_dist$Total,3)
GF_dist$Total31_60<-round(GF_dist$Total31_60/GF_dist$Total,3)
GF_dist$Total61_90<-round(GF_dist$Total61_90/GF_dist$Total,3)
GF_dist$Total91_120<-round(GF_dist$Total91_120/GF_dist$Total,3)
GF_dist$TotalOver120<-round(GF_dist$TotalOver120/GF_dist$Total,3)
GF_dist<-select(GF_dist,Qtr,TotalUnder30,Total31_60,Total61_90,Total91_120,TotalOver120)
GF_dist<-melt(GF_dist,id.vars="Qtr")
GF_dist_plot<-ggplot(GF_dist,aes(x = factor(Qtr), y = value,fill = variable)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Days from Invoice to Check \n - General Fund")+
  xlab("Quarters")+ylab("Percent")+
  scale_fill_manual(values=c("339900","green",lightBlue,darkBlue,red) ,name=" ",labels=c("<=30","31-60","61-90","91-120",">120"))+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(GF_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/General Fund Payment Distribution.png")

#### Plot the distribution percentages of days from invoice to check
CG_dist<-select(AFIN,Qtr,TotalUnder45,Total46_90,Total91_135,TotalOver135)
CG_dist<-aggregate(cbind(CG_dist$TotalUnder45,CG_dist$Total46_90,CG_dist$Total91_135,CG_dist$TotalOver135)~Qtr,data=CG_dist,FUN=sum)
  colnames(CG_dist)[grepl("V1", colnames(CG_dist))] <- "TotalUnder45"
    colnames(CG_dist)[grepl("V2", colnames(CG_dist))] <- "Total46_90"
      colnames(CG_dist)[grepl("V3", colnames(CG_dist))] <- "Total91_135"
          colnames(CG_dist)[grepl("V4", colnames(CG_dist))] <- "TotalOver135"
CG_dist$Total<-CG_dist$TotalUnder45+CG_dist$Total46_90+CG_dist$Total91_135+CG_dist$TotalOver135
CG_dist$TotalUnder45<-round(CG_dist$TotalUnder45/CG_dist$Total,3)
CG_dist$Total46_90<-round(CG_dist$Total46_90/CG_dist$Total,3)
CG_dist$Total91_135<-round(CG_dist$Total91_135/CG_dist$Total,3)
CG_dist$TotalOver135<-round(CG_dist$TotalOver135/CG_dist$Total,3)
CG_dist<-select(CG_dist,Qtr,TotalUnder45,Total46_90,Total91_135,TotalOver135)
CG_dist<-melt(CG_dist,id.vars="Qtr")
CG_dist_plot<-ggplot(CG_dist,aes(x = factor(Qtr), y = value,fill = variable)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(labels = percent_format())+
  ggtitle("Days from Invoice to Check \n - Capital/Grants")+
  xlab("Quarters")+ylab("Percent")+
  scale_fill_manual(values=c("339900","green",lightBlue,red) ,name=" ",labels=c("<=45","46-90","91-135",">135"))+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(CG_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Payments/Capital-Grant Payment Distribution.png")


### Export cleaned datasets
write.csv(GP,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Payments/Great Plains.csv")
write.csv(AFIN,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Payments/AFIN.csv")
