require(tidyr)
require (plyr)  
require(dplyr)
require(lubridate)
require(xlsx)
require(gdata)
require(stringr)

## Read in needed files
contractPOapproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Approval Sequence POs.csv",skip=3) ## Report pulled from ECMS
contractPOstatus<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract PO Status.csv",skip=3) ## Report pulled from ECMS
contractReqstatus<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Req Status.csv",skip=3) ## Report pulled from ECMS
contractReqapproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Approval Sequence Reqs.csv",skip=3)  ##Report pulled from ECMS
Adjustments<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Adjustments.csv",na.strings="") ## List compiled by OPA
LawExec<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/LawExec.csv",na.strings="") ## List compiled by Law and Executive Counsel
LawMaster<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master_hist.csv",strip.white=TRUE,na.strings="") ##Lists compiled by Law
Law2<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master.csv",strip.white=TRUE,na.strings=""),PO.Number,K.Number,Govt=Govtal.Entity,Type.of.K,TimeOnly=Time..Only)
LawMaster<-merge(LawMaster,Law2,by=c("PO.Number","K.Number","Govt","Type.of.K","TimeOnly"),all=TRUE)
LawMaster<-LawMaster[!is.na(LawMaster$K.Number),]

## Filter out unnecessary rows from Law-Exec log, and re-code invalid dates to 2015.
LawExec<-LawExec[!is.na(LawExec$Description),]
LawExec$Date.Signed.by.MAY<-as.Date(LawExec$Date.Signed.by.MAY,"%m/%d/%Y")
LawExec$Date.Signed.by.MAY<-ifelse(grepl('6$',LawExec$Date.Signed.by.MAY),LawExec$Date.Signed.by.MAY-365,LawExec$Date.Signed.by.MAY)
class(LawExec$Date.Signed.by.MAY)<-"Date"
LawExec$Date.Received.by.Law<-as.Date(LawExec$Date.Received.by.Law,"%m/%d/%Y")
LawExec$Date.Received.by.Law<-ifelse(substr(LawExec$Date.Received.by.Law,4,4)=="2",LawExec$Date.Received.by.Law+1096,LawExec$Date.Received.by.Law)
class(LawExec$Date.Received.by.Law)<-"Date"

##Clean Alternate ID number column to match ECMS formatting conventions of "k##-###"/"mk##-###"
LawExec$K.Number<-tolower(LawExec$K.Number)
LawExec$K.Number<-ifelse(grepl("^\\d",LawExec$K.Number),paste("k",LawExec$K.Number,sep=""),paste(LawExec$K.Number))
LawExec$K.Number<-ifelse(startsWith(LawExec$K.Number,"m"),paste(substring(LawExec$K.Number,1,1),"k",substring(LawExec$K.Number,2,8),sep=""),LawExec$K.Number)
LawMaster$K.Number<-tolower(LawMaster$K.Number)
LawMaster$K.Number<-str_trim(LawMaster$K.Number,side="both")
LawMaster$K.Number<-ifelse(grepl("^\\d",LawMaster$K.Number),paste("k",LawMaster$K.Number,sep=""),paste(LawMaster$K.Number))
LawMaster$K.Number<-ifelse(startsWith(LawMaster$K.Number,"m"),paste(substring(LawMaster$K.Number,1,1),"k",substring(LawMaster$K.Number,2,8),sep=""),LawMaster$K.Number)

## Extract desired variable columns from each file and merge into master dataset
LawMaster<-merge(LawMaster,Law2),by=c("PO.Number","K.Number","Govt","Type.of.K","TimeOnly"),all=TRUE) 
contractPOapproval1<-select(contractPOapproval,AltID=ALTERNATE_ID,PO=PO_NBR,AttorneyReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,ApproverType=PO_APPROVER_TYPE,Approver=PO_APPROVER)
contractPOstatus1<-select(contractPOstatus,PO=PO_NBR3,Req=REQ_NBR3,AltID=ALTERNATE_ID3,POdate=PO_DATE3,ReqApp=Max_Date3,Description=SHORT_DESC3,Dept=DESC_TEXT3,Vendor=NAME3,Requestor=REQUESTOR_ID3,POStatus=Status3)
contractReqstatus1<-select(contractReqstatus,Req=REQ_NBR,Description=SHORT_DESC,ReqStatus=STATUS)
contractReqapproval1<-select(contractReqapproval,Req=REQ_NBR,ReqApprove=APPROVAL_DATE,ReqApprover=REQ_APPROVER)
LawExec$Date.Signed.by.MAY<-as.Date(LawExec$Date.Signed.by.MAY,"%m/%d/%Y")
Adjustments$SignDate<-as.Date(Adjustments$SignDate,"%m/%d/%Y")
LawExec<-select(LawExec,PO=PO.Number,AltID=K.Number,BackFromVendor=Date.Received.by.Law,DownForSignature=Date.Received.by.EX,AdjustedSignDate=Date.Signed.by.MAY)
LawMaster<-select(LawMaster,PO=PO.Number,AltID=K.Number,Govt=Govt,Type=Type.of.K,TimeOnly=TimeOnly,LawStatus=Status)

##
Adjust<-select(Adjustments,PO,AltID,AdjustedSignDate=SignDate,Ordinance,BackFromVendor)
LawExec$BackFromVendor<-as.Date(LawExec$BackFromVendor,"%m/%d/%Y")
Adjust$AdjustedSignDate<-as.Date(Adjust$AdjustedSignDate,"%m/%d/%Y")
Adjust$BackFromVendor<-as.Date(Adjust$BackFromVendor,"%m/%d/%Y")
Adjusted<-merge(LawExec,Adjust,by=c("PO","AltID","AdjustedSignDate","BackFromVendor"),all=TRUE)


## Merge files into consolidated contract list; 
contracts<-merge(contractPOapproval1,contractPOstatus1,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,contractReqstatus1,by=c("Req"),all=TRUE)
contracts<-merge(contracts,LawMaster,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,Adjusted,by=c("PO","AltID"),all=TRUE)

contracts<-select(contracts,PO,AltID,Req,ApproverType,Approver,Req_Description=Description.x,PO_Description=Description.y,Dept,Vendor,Requestor,ReqStatus,POStatus,Govt,Type,TimeOnly,LawStatus,Ordinance,ReqApp,POdate,AttorneyReview,ApprovalDate,BackFromVendor,AdjustedSignDate)

## Convert remaining date columns to "Date" class
contracts$ApprovalDate<-as.Date(contracts$ApprovalDate,"%m/%d/%Y")
contracts$AttorneyReview<-as.Date(contracts$AttorneyReview,"%m/%d/%Y")
contracts$POdate<-as.Date(contracts$POdate,"%m/%d/%Y")
contracts$BackFromVendor<-as.Date(contracts$BackFromVendor,"%m/%d/%Y")
contracts$ReqApp<-as.Date(contracts$ReqApp,"%m/%d/%Y")

## Remove contracts from the Open list that have already been executed, but haven't been approved in ECMS; subset adjustments
Opencontracts<-subset(contracts,POStatus=="Ready for Approval"|POStatus=="In Progress"|ReqStatus=="Ready for Purchasing")
Adjust1<-Opencontracts$PO %in% Adjustments$PO
Opencontracts<-Opencontracts[!Adjust1,]  
AdjustClosed<-Opencontracts[Adjust1,]

## Need a section on data cleaning closed contracts
Closedcontracts<-subset(contracts,POStatus=="Sent"|POStatus=="Canceled"|POStatus=="Ready to Send")
Closedcontracts<-merge(AdjustClosed,Closedcontracts,by=c("PO","AltID","Req","ApproverType","Approver","Req_Description","PO_Description","Dept","Vendor","Requestor","Govt","Type","TimeOnly","LawStatus","ReqStatus","POStatus","Ordinance","ReqApp","POdate","AttorneyReview","ApprovalDate","BackFromVendor"),all=TRUE)

## Recode approver column for open contracts into appropriate categories
Opencontracts$Approver<-paste(Opencontracts$Approver,Opencontracts$ApproverType,sep="_")

## Sort approver column;
Opencontracts$Approver[Opencontracts$Approver=="CRHDIETZ_P"|Opencontracts$Approver=="CJPMEYER_P"|Opencontracts$Approver=="CMJMANZELLA_A"|Opencontracts$Approver=="CLCSETTLEMYER_A"]<-"CityAttorney"
Opencontracts$Approver[Opencontracts$Approver=="CJECHRISTOPHER_A"|Opencontracts$Approver=="CAKOPPLIN_P"]<-"CAO"
Opencontracts$Approver[Opencontracts$Approver=="CTDOATES_A"|Opencontracts$Approver=="CJPMEYER_A"|Opencontracts$Approver=="CSCWELLMAN_P"]<-"SentVendor"
Opencontracts$Approver[Opencontracts$Approver=="CTDOATES_P"|Opencontracts$Approver=="CSCWELLMAN_A"]<-"FinalLaw"
Opencontracts$Approver[Opencontracts$Approver=="CONTRACTCLERK_A"|Opencontracts$Approver=="CSTSEPCICH_P"|Opencontracts$Approver=="CVCHONORE"|Opencontracts$Approver=="CEFPUGH_P"|Opencontracts$Approver=="CNSFOSTER_P"|Opencontracts$Approver=="CMJAVERILL_A"|Opencontracts$Approver=="CMESTRICKLAND_A"|Opencontracts$Approver=="CMJAVERILL_A"]<-"Executed"
Opencontracts$Approver[Opencontracts$Approver=="NA"]<-"Not Assigned; Still at Req Stage"

## Sort the contracts lists by ApprovalDate to prepare to drop one of the two rows
Opencontracts<-arrange(Opencontracts,desc(ApprovalDate))
Closedcontracts<-arrange(Closedcontracts,desc(ApprovalDate))

##

## 
ReadyForLaw<-filter(Opencontracts,ReqStatus=="Ready for Purchasing")
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

## Subset columns needed for Open list
ReadyForLaw<-select(ReadyForLaw,PO,AltID,Req=Req,PO_Description,Req_Description,Vendor=Vendor,Dept=Dept,ReqStatus=ReqStatus,POStatus=POStatus,Type=Type,Ordinance=Ordinance,ContractDate=POdate,AttorneyReview=AttorneyReview)
AttorneyReview<-select(AttorneyReview,PO,AltID,Req,PO_Description,Req_Description,Vendor,Dept,ReqStatus,POStatus,Type,Ordinance,ContractDate=POdate,AttorneyReview)
CityAttorney<-select(CityAttorney,PO,AltID,Req=Req.CityAttorney,PO_Description=PO_Description.CityAttorney,Req_Description=Req_Description.CityAttorney,Vendor=Vendor.CityAttorney,Dept=Dept.CityAttorney,ReqStatus=ReqStatus.CityAttorney,POStatus=POStatus.CityAttorney,Type=Type.CityAttorney,Ordinance=Ordinance.CityAttorney,ContractDate=POdate,AttorneyReview=AttorneyReview,CityAttorney=ApprovalDate.CityAttorney,BackFromVendor=BackFromVendor.CityAttorney)
CAO<-select(CAO,PO,AltID,CAO=ApprovalDate.CAO)
SentVendor<-select(SentVendor,PO,AltID,SentVendor=ApprovalDate.SentVendor)
FinalLaw<-select(FinalLaw,PO,AltID,FinalLaw=ApprovalDate.FinalLaw)
ExecutiveSignature<-select(Executed,PO,AltID,ExecutiveSignature=ApprovalDate.Executed)

## Merge open contract lists
Open<-merge(CityAttorney,ReadyForLaw,by=c("PO","AltID","Req","PO_Description","Req_Description","Vendor","Dept","ReqStatus","POStatus","Type","Ordinance","ContractDate","AttorneyReview"),all=TRUE)
  Open<-merge(Open,AttorneyReview,by=c("PO","AltID","Req","PO_Description","Req_Description","Vendor","Dept","ReqStatus","POStatus","Type","Ordinance","ContractDate","AttorneyReview"),all=TRUE)
    Open<-merge(Open,CAO,by=c("PO","AltID"),all=TRUE)
      Open<-merge(Open,SentVendor,by=c("PO","AltID"),all=TRUE)
        Open<-merge(Open,FinalLaw,by=c("PO","AltID"),all=TRUE)
          Open<-merge(Open,ExecutiveSignature,by=c("PO","AltID"),all=TRUE)
            Open<-Open[,c(1:14,16,17,15,18,19)]

## End Date of Analysis Period
Date<-as.data.frame(Sys.Date())
colnames(Date)<-c("Date")

## Calculate Age of Open Contracts at Each Stage
AttorneyReviewopen<-Date$Date - Open$AttorneyReview
Open$Attorney_Age<-ifelse(is.na(AttorneyReviewopen),Date$Date-Open$ContractDate,NA)

CityAttorneyopen<-Date$Date - Open$CityAttorney
Open$Attorney_Age<-ifelse(is.na(CityAttorneyopen),Date$Date-Open$AttorneyReview,NA)

CAOopen<-Date$Date - Open$CAO
Open$CAO_Age<-ifelse(is.na(CAOopen),Date$Date-Open$AttorneyReview,NA)

Ordinanceopen<-Date$Date-Open$SentVendor
Open$Ordinance_Age<-ifelse(Open$Ordinance=="Yes" & is.na(Ordinanceopen),Date$Date-Open$CAO,NA)

SentVendoropen<-Date$Date - Open$SentVendor
Open$SentVendor_Age<-ifelse(is.na(SentVendoropen),Date$Date-Open$CAO,NA)

## Calculate days awaiting vendor  
FinalLawopen<-Date$Date - Open$FinalLaw
BackFromVendoropen<-Date$Date-Open$BackFromVendor
Open$VendorReturn_Age<-ifelse(is.na(FinalLawopen) & is.na(BackFromVendoropen),Date$Date-Open$SentVendor,NA)

Open$ExecutiveSignature_Age<-ifelse(is.na(Open$FinalLaw) & is.na(BackFromVendoropen),NA,Date$Date-Open$FinalLaw)

## Sort the open list with the oldest contracts at the top
Open<-arrange(Open,ContractDate)

## Filter to get list of contracts awaiting Ordinance or to be sent to vendor
Ord_SendVendor<-filter(Open,!is.na(CAO) & is.na(SentVendor) & is.na(BackFromVendor))

## Filter to get list of contracts awaiting vendor signature
AwaitingVendor<-filter(Open,!is.na(SentVendor) & is.na(BackFromVendor) & is.na(FinalLaw))


## Write CSV's
write.csv(Open,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Open Contracts.csv")
write.csv(Closedcontracts,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Closed Contracts.csv")

## Create "snapshot" generally requested by the Law Department several times per month
write.xlsx(Ord_SendVendor,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Snapshot.xlsx",sheetName="Ordinance-ReadytoSend",showNA=FALSE) ##Write excel file of first of contracts that are either awaiting Ordinance or ready to send to the vendor.
write.xlsx(AwaitingVendor,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Snapshot.xlsx",sheetName="AwaitingVendor",append=TRUE,showNA=FALSE) ##Add new tab to the snapshot of the contracts currently awaiting vendor signature.
