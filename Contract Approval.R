require(tidyr)
require (plyr)  
require(dplyr)
require(lubridate)

## Read in needed files
contractPOapproval<-read.csv("Contract Approval Sequence POs.csv",skip=3) ## Report pulled from ECMS
contractPOstatus<-read.csv("Contract PO Status.csv",skip=3) ## Report pulled from ECMS
contractReqstatus<-read.csv("Contract Req Status.csv",skip=3) ## Report pulled from ECMS
contractReqapproval<-read.csv("Contract Approval Sequence Reqs.csv",skip=3)  ##Report pulled from ECMS
Adjustments<-read.csv("Contract Adjustments.csv") ## List compiled by OPA
LawExec<-read.csv("Law and Executive Counsel Log.csv") ## List compiled by Law and Executive Counsel
LawMaster<-read.csv("Law Master.csv") ##List compiled by Law


## Extract desired variable columns from each file and merge into master dataset
contractPOapproval1<-select(contractPOapproval,AltID=ALTERNATE_ID,PO=PO_NBR,AttorneyReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,ApproverType=PO_APPROVER_TYPE,Approver=PO_APPROVER)
contractPOstatus1<-select(contractPOstatus,PO=PO_NBR3,Req=REQ_NBR3,AltID=ALTERNATE_ID3,POdate=PO_DATE3,ReqApp=Max_Date3,Description=SHORT_DESC3,Dept=DESC_TEXT3,Vendor=NAME3,Requestor=REQUESTOR_ID3,POStatus=Status3)
contractReqstatus1<-select(contractReqstatus,Req=REQ_NBR,Description=SHORT_DESC,ReqStatus=STATUS)
contractReqapproval1<-select(contractReqapproval,Req=REQ_NBR,ReqApprove=APPROVAL_DATE,ReqApprover=REQ_APPROVER)
LawMaster<-select(LawMaster,PO=PO.Number,AltID=AltID,Govt=Govt,Type=Type.of.K,TimeOnly=TimeOnly,LawStatus=Status,Ordinance=Ordinance)
contracts<-merge(contractPOapproval1,contractPOstatus1,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,contractReqstatus1,by=c("Req"),all=TRUE)
contracts<-merge(contracts,LawMaster,by=c("PO","AltID"),all=TRUE)

## Need to rename PO and Req description columns 



## Remove contracts that have already been executed, but haven't been approved and uploaded to ECMS
Opencontracts<-subset(contracts,POStatus=="Ready for Approval"|POStatus=="In Progress"|ReqStatus=="Ready for Purchasing")
Adjust1<-Opencontracts$PO %in% Adjustments$PO
Opencontracts2<-Opencontracts[!Adjust1,]  

## Need a section on data cleaning closed contracts
Closedcontracts<-subset(contracts,POStatus=="Sent"|POStatus=="Canceled"|POStatus=="Ready to Send")

## Recode approver column into appropriate categories
Opencontracts$Approver<-paste(Opencontracts$Approver,Opencontracts$ApproverType,sep="_")

Opencontracts$Approver[Opencontracts$Approver=="CRHDIETZ_P"|Opencontracts$Approver=="CJPMEYER_P"|Opencontracts$Approver=="CMJMANZELLA_A"|Opencontracts$Approver=="CLCSETTLEMYER_A"]<-"CityAttorney"
Opencontracts$Approver[Opencontracts$Approver=="CJECHRISTOPHER_A"|Opencontracts$Approver=="CAKOPPLIN_P"]<-"CAO"
Opencontracts$Approver[Opencontracts$Approver=="CTDOATES_A"|Opencontracts$Approver=="CJPMEYER_A"|Opencontracts$Approver=="CSCWELLMAN_P"]<-"SentVendor"
Opencontracts$Approver[Opencontracts$Approver=="CTDOATES_P"|Opencontracts$Approver=="CSCWELLMAN_A"]<-"FinalLaw"
Opencontracts$Approver[Opencontracts$Approver=="CONTRACTCLERK_A"|Opencontracts$Approver=="CSTSEPCICH_P"|Opencontracts$Approver=="CVCHONORE"|Opencontracts$Approver=="CEFPUGH_P"|Opencontracts$Approver=="CNSFOSTER_P"|Opencontracts$Approver=="CMJAVERILL_A"|Opencontracts$Approver=="CMESTRICKLAND_A"|Opencontracts$Approver=="CMJAVERILL_A"]<-"Executed"
Opencontracts$Approver[Opencontracts$Approver=="NA"]<-"Not Assigned; Still at Req Stage"
  
## Fake it til you make it
Opencontracts$ApprovalDate<-as.Date(Opencontracts$ApprovalDate,"%m/%d/%Y")
Opencontracts$AttorneyReview<-as.Date(Opencontracts$AttorneyReview,"%m/%d/%Y")
Opencontracts$POdate<-as.Date(Opencontracts$POdate,"%m/%d/%Y")
Opencontracts<-arrange(Opencontracts,desc(ApprovalDate))


## 
ReadyforLaw<-filter(Opencontracts,ReqStatus=="Ready for Purchasing")
AttorneyReview<-filter(Opencontracts,POStatus=="In Progress")
CityAttorney<-filter(Opencontracts,Approver=="CityAttorney")
CAO<-filter(Opencontracts,Approver=="CAO")
SentVendor<-filter(Opencontracts,Approver=="SentVendor")
FinalLaw<-filter(Opencontracts,Approver=="FinalLaw")
Executed<-filter(Opencontracts,Approver=="Executed")

## Pivot the approval dates to form one row for each contract
CityAttorney<-reshape(CityAttorney,timevar="Approver",idvar=c("AltID","PO","POdate","AttorneyReview"),direction="wide")
CAO<-reshape(CAO,timevar="Approver",idvar=c("AltID","PO","POdate","AttorneyReview"),direction="wide")
SentVendor<-reshape(SentVendor,timevar="Approver",idvar=c("AltID","PO","POdate","AttorneyReview"),direction="wide")
FinalLaw<-reshape(FinalLaw,timevar="Approver",idvar=c("AltID","PO","POdate","AttorneyReview"),direction="wide")
Executed<-reshape(Executed,timevar="Approver",idvar=c("AltID","PO","POdate","AttorneyReview"),direction="wide")

## 
CityAttorney<-select(CityAttorney,PO,AltID,Req=Req.CityAttorney,PO_Description=Description.y.CityAttorney,Req_Description=Description.x.CityAttorney,Vendor=Vendor.CityAttorney,Dept=Dept.CityAttorney,ReqStatus=ReqStatus.CityAttorney,POStatus=POStatus.CityAttorney,Type=Type.CityAttorney,Ordinance=Ordinance.CityAttorney,ContractDate=POdate,AttorneyReview=AttorneyReview,CityAttorney=ApprovalDate.CityAttorney)
CAO<-select(CAO,PO,AltID,CAO=ApprovalDate.CAO)
SentVendor<-select(SentVendor,PO,AltID,SentVendor=ApprovalDate.SentVendor)
FinalLaw<-select(FinalLaw,PO,AltID,FinalLaw=ApprovalDate.FinalLaw)
Executed<-select(Executed,PO,AltID,Executed=ApprovalDate.Executed)


Open<-merge(CityAttorney,CAO,by=c("PO","AltID"))
  Open<-merge(Open,SentVendor,by=c("PO","AltID"))
    Open$BackfromVendor<-
    Open<-merge(Open,FinalLaw,by=c("PO","AltID"))
      Open<-merge(Open,Executed,by=c("PO","AltID"))

## End Date of Analysis Period
Date<-as.data.frame(Sys.Date())
colnames(Date)<-c("Date")

## Calculate Age of Open Contracts at Each Stage
AttorneyReview<-Date$Date - Open$AttorneyReview
Open$Attorney_Age<-ifelse(is.na(AttorneyReview),Date$Date-Open$ContractDate,NA)

CityAttorney<-Date$Date - Open$CityAttorney
Open$Attorney_Age<-ifelse(is.na(CityAttorney),Date$Date-Open$AttorneyReview,NA)

CAO<-Date$Date - Open$CAO
Open$CAO_Age<-ifelse(is.na(CAO),Date$Date-Open$AttorneyReview,NA)

Ordinance<-Date$Date-Open$SentVendor
Open$Ordinance_Age<-ifelse(Open$Ordinance=="Yes" & is.na(SentVendor),Date$Date-Open$CAO,NA)

SentVendor<-Date$Date - Open$SentVendor
Open$SentVendor_Age<-ifelse(is.na(SentVendor),Date$Date-Open$CAO,NA)

## Pending; need to add Executed also
BackFromVendor<-Date$Date-Open$BackfromVendor

FinalLaw<-Date$Date - Open$FinalLaw
Open$FinalLaw_Age<-ifelse(is.na(FinalLaw) & is.na(Open$Ordinance_Age),Date$Date-Open$SentVendor,NA)

## Write CSV's
write.csv(Open,"C:/Users/Vic/Documents/ReqtoCheckSTAT/Query Files/Test/Opentest.csv")