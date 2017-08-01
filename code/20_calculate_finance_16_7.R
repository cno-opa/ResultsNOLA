headings<-c("Dept","Req","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")
POs1<-read.csv("data/source/ProcurementReqProcessing.csv",col.names=headings,stringsAsFactors=FALSE,skip=3)
ReqStatus<-select(read.csv("data/source/RequsitionStatus.csv",skip=3),Req=REQ_NBR,Status=STATUS)
#Category<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/PObyCategory.csv",skip=3),Req=REQ_NBR,Descr=DESC_LONG)


### Data cleaning

#### Standardize req number variables in two data sets
POs<-merge(POs1,ReqStatus,by="Req",all.x=TRUE)
#Reqs<-merge(Reqs,Category,by="Req",all.x=TRUE)

#### Convert dollar amount column to numeric class for readability
POs$Cost<-as.numeric(sub("\\$","",POs$Cost))

#### Clean out purchase orders that have been cancelled, as well as punch-outs that have created errors.
Exclude<-POs[POs$Vendor=="Independent Stationers" & POs$Cost==30.48|POs$Vendor=="Independent Stationers" & POs$Cost==0|POs$Vendor=="FASTENAL COMPANY" & POs$Cost==0 | POs$Vendor=="Independent Stationers" & POs$Cost==30.44 | POs$Vendor=="Independent Stationers" & POs$Cost==34.80 | POs$Vendor=="Independent Stationers" & POs$Cost==53.88 | POs$Vendor=="Grainger, Inc." & POs$Cost==99.09|POs$Cost==0,]
POs<-anti_join(POs,Exclude,by="Req")

#### Format date and quarter columns as needed
POs$FinanceDate<-as.Date(POs$FinanceDate,"%m/%d/%Y")
POs$POdate<-as.Date(POs$POdate,"%m/%d/%Y")
#POs<-filter(POs,POdate>FinanceDate)
POs$Qtr<-as.yearqtr(POs$POdate,format="%m/%d/%Y")

#### Calculate business days (this relies on NOLA_calendar read from github)
POs$WorkingDays<-bizdays(POs$FinanceDate,POs$POdate,NOLA_calendar)
POs$WorkingDays<-POs$WorkingDays+1 ##### Adjust calculation up one day, as bizdays function calculates 1 less day than Excel's parallel formula, networkdays 

#### Create distribution bins for business days to process
POs$Under4<-ifelse(POs$WorkingDays<=4,1,0)
POs$Over4<-ifelse(POs$WorkingDays<=4,0,1)


### Plotting

#### Plot the business days to process by quarter
Days2PO<-cbind(aggregate(data=POs,WorkingDays~Qtr,FUN=mean),select(aggregate(data=POs,Req~Qtr,FUN=length),-Qtr,Count=Req))                    
# Purchasing<-ggplot(Days2PO,aes(x=factor(Qtr),y=WorkingDays))+
