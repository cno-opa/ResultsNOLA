.libPaths("C:/Rpackages")

library(tidyr)
library(plyr)  
library(dplyr)
library(lubridate)
library(xlsx)
library(gdata)
library(stringr)
library(r2excel)
library(reshape2)
library(zoo)
library(ggplot2)

## Create function for days between dates in the contracting process among closed contracts, rounded to whole numbers
ContractDays<-function(df,FirstDt,EndDt){
  arguments<-as.list(match.call())  
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  round((strptime(EndDt,"%Y-%m-%d")-strptime(FirstDt,"%Y-%m-%d"))/86400,digits=0)
}

## Create function for days in current stage for open contracts at end of the reporting period, rounded to whole numbers
StageAge<-function(df,PrevDt,QtrEnd){
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

QtrEnd<-as.Date("2015-9-30",format="%Y-%m-%d")

## Read in needed files
contractPOapproval<-read.csv("Contract Approval Sequence POs.csv",skip=3) ## Report pulled from ECMS
contractPOstatus<-read.csv("Contract PO Status.csv",skip=3) ## Report pulled from ECMS
contractReqstatus<-read.csv("Contract Req Status.csv",skip=3) ## Report pulled from ECMS
contractReqapproval<-read.csv("Contract Approval Sequence Reqs.csv",skip=3)  ##Report pulled from ECMS
LawExec<-read.csv("Law and Executive Counsel Log.csv",na.strings="") ## List compiled by Law and Executive Counsel
Adjustments<-read.xlsx2("Adjustments.xlsx",sheetIndex=1,colClasses=c("character","character","Date","Date","numeric","Date","character","Date","character"))
BackFromVendor<-read.xlsx2("Adjustments.xlsx",sheetIndex=2,colClasses=c("character","character","Date","Date","numeric","Date","character","Date","character"))
Ordinance<-read.xlsx2("Adjustments.xlsx",sheetIndex=3,colClasses=c("character","character","Date","Date","numeric","Date","character","Date","character")) 

#
BackFromVendor<-select(BackFromVendor,PO:BackFromVendor)
class(BackFromVendor$BackFromVendor)<-"Date"
class(Ordinance$OrdinanceDate)<-"Date"
Adjustments$CancelDate<-as.Date(Adjustments$CancelDate,"%m/%d/%Y")
Ordinance<-select(Ordinance,PO,AltID,Ordinance,OrdinanceDate)
Adjustments<-select(Adjustments,PO,AltID,AdjustedSignDate,Closed,CancelDate)
Adjustments<-merge(Adjustments,BackFromVendor,by=c("PO","AltID"),all=TRUE)
Adjustments<-merge(Adjustments,Ordinance,by=c("PO","AltID"),all=TRUE)

## Read in, and merge Law Master files
LawMaster<-read.csv("Law Master_hist.csv",strip.white=TRUE,na.strings="") ##Lists compiled by Law
Law2<-select(read.csv("Law Master.csv",strip.white=TRUE,na.strings=""),PO.Number,K.Number,Govt=Govtal.Entity,Type.of.K)
LawMaster<-merge(LawMaster,Law2,by=c("PO.Number","K.Number","Govt","Type.of.K"),all=TRUE)
rm(Law2)
LawMaster<-LawMaster[!is.na(LawMaster$K.Number),]

## Filter out unnecessary rows from Law-Exec log, and re-code invalid dates to 2015.
LawExec<-LawExec[!is.na(LawExec$Description),]
LawExec$Date.Signed.by.MAY<-as.Date(LawExec$Date.Signed.by.MAY,"%m/%d/%Y")
class(LawExec$Date.Signed.by.MAY)<-"Date"
LawExec$Date.Received.by.Law<-as.Date(LawExec$Date.Received.by.Law,"%m/%d/%Y")
LawExec$Date.Received.by.Law<-ifelse(substr(LawExec$Date.Received.by.Law,"ECMS_Back From Vendor","ECMS_Back From Vendor")=="2",LawExec$Date.Received.by.Law+1096,LawExec$Date.Received.by.Law)
class(LawExec$Date.Received.by.Law)<-"Date"


##Clean Alternate ID number column to match ECMS formatting conventions of "k##-###"/"mk##-###"
LawExec$K.Number<-tolower(LawExec$K.Number)
LawExec$K.Number<-ifelse(grepl("^\\d", LawExec$K.Number),paste("k",LawExec$K.Number,sep=""),paste(LawExec$K.Number))
LawExec$K.Number<-ifelse(startsWith(LawExec$K.Number,"m"),paste(substring(LawExec$K.Number,1,1),"k",substring(LawExec$K.Number,2,8),sep=""),LawExec$K.Number)
LawMaster$K.Number<-tolower(LawMaster$K.Number)
LawMaster$K.Number<-str_trim(LawMaster$K.Number,side="both")
LawMaster$K.Number<-ifelse(grepl("^\\d",LawMaster$K.Number),paste("k",LawMaster$K.Number,sep=""),paste(LawMaster$K.Number))
LawMaster$K.Number<-ifelse(startsWith(LawMaster$K.Number,"m"),paste(substring(LawMaster$K.Number,1,1),"k",substring(LawMaster$K.Number,2,8),sep=""),LawMaster$K.Number)
Adjustments$AltID<-ifelse(grepl("^\\d",Adjustments$AltID),paste("k",Adjustments$AltID,sep=""),paste(Adjustments$AltID)) ## Just added 9-8 to reconcile adjustments and Law Master contracts
Adjustments$AltID<-ifelse(startsWith(Adjustments$AltID,"m"),paste(substring(Adjustments$AltID,1,1),"k",substring(Adjustments$AltID,2,8),sep=""),Adjustments$AltID) ## Just added 9-8 to reconcile adjustments and Law Master contracts


## Extract desired variable rows and columns from each file
contractPOapproval<-select(contractPOapproval,AltID=ALTERNATE_ID,PO=PO_NBR,ContractDate=PO_DATE,LegalReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,Approver=PO_APPROVER,Stage=ORDER_SEQUENCE)
contractPOstatus<-select(contractPOstatus,PO=PO_NBR3,Req=REQ_NBR3,AltID=ALTERNATE_ID3,Description=SHORT_DESC3,Dept=DESC_TEXT3,Vendor=NAME3,Requestor=REQUESTOR_ID3,Purchaser=PURCHASER3,ContractDate=PO_DATE3,POStatus=Status3,ReqComplete=Max_Date3)
contractReqstatus<-select(contractReqstatus,Req=REQ_NBR,Description=SHORT_DESC,ReqStatus=STATUS)
contractReqapproval<-select(contractReqapproval,Req=REQ_NBR,ReqApprove=APPROVAL_DATE,ReqApprover=REQ_APPROVER)
LawExec$Date.Signed.by.MAY<-as.Date(LawExec$Date.Signed.by.MAY,"%m/%d/%Y")
LawExec<-select(LawExec,PO=PO.Number,AltID=K.Number,BackFromVendor=Date.Received.by.Law,DownForSignature=Date.Received.by.EX,AdjustedSignDate=Date.Signed.by.MAY)
LawMaster<-select(LawMaster,PO=PO.Number,AltID=K.Number,Admin_ContractDate=Date.In.Law,Govt=Govt,Type=Type.of.K,LawStatus=Status)

## Code relevant columns into "Date" class
LawExec$BackFromVendor<-as.Date(LawExec$BackFromVendor,"%m/%d/%Y")
LawMaster$Admin_ContractDate<-as.Date(LawMaster$Admin_ContractDate,"%m/%d/%Y")
Adjustments<-merge(LawExec,Adjustments,by=c("PO","AltID","BackFromVendor","AdjustedSignDate"),all=TRUE)

## Merge files into consolidated contract list; 
contracts<-merge(contractPOapproval,contractPOstatus,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,contractReqstatus,by=c("Req"),all=TRUE)
contracts<-merge(contracts,LawMaster,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,Adjustments,by=c("PO","AltID"),all=TRUE)

## Re-Code Order Sequence to appropriate appproval stage category
contracts$Stage<-ifelse(contracts$Stage==1,"Deputy City Attorney",ifelse(contracts$Stage==2,"CAO",ifelse(contracts$Stage==3,"Sent to Vendor",ifelse(contracts$Stage==4,"ECMS_Back From Vendor",ifelse(contracts$Stage==5,"Executive Signature","Error")))))
contracts$Stage<-ifelse(contracts$Stage=="Error" & contracts$Approver=="CENBECK" & contracts$Stage==4 |contracts$Stage=="Error" & contracts$Approver=="CNSFOSTER" & contracts$Stage==4 |contracts$Stage=="Error" & contracts$Approver=="CRFCORTIZAS" & contracts$Stage==4 |contracts$Stage=="Error" & contracts$Approver=="CSTSEPCICH" & contracts$Stage==4 |contracts$Stage=="Error" & contracts$Approver=="CVCHONORE" & contracts$Stage==4 ,"Executive Signature",contracts$Stage)
contracts<-select(contracts,PO,AltID,Req,Description.x,Description.y,Dept,Vendor,Requestor,Purchaser,ReqStatus,POStatus,Govt,Type,Ordinance,Admin_ContractDate,CancelDate,Closed,ReqComplete,ContractDate.x,ContractDate.y,LegalReview,ApprovalDate,Stage,OrdinanceDate,BackFromVendor,DownForSignature,AdjustedSignDate)

## Convert remaining date columns to "Date" class
contracts$ContractDate.y<-as.Date(contracts$ContractDate.y,"%m/%d/%Y")
contracts$ContractDate.x<-as.Date(contracts$ContractDate.x,"%m/%d/%Y")
contracts$ApprovalDate<-as.Date(contracts$ApprovalDate,"%m/%d/%Y")
contracts$LegalReview<-as.Date(contracts$LegalReview,"%m/%d/%Y")
contracts$DownForSignature<-as.Date(contracts$DownForSignature,"%m/%d/%Y")
contracts$ReqComplete<-as.Date(contracts$ReqComplete,"%m/%d/%Y")

## Consolidate contract description and contract date columns
contracts$Description<-ifelse(!is.na(contracts$Description.x),as.character(contracts$Description.x),as.character(contracts$Description.y))
contracts<-select(contracts,-Description.x,-Description.y)
contracts$ContractDate<-ifelse(is.na(contracts$ContractDate.x),as.Date(contracts$ContractDate.y,"%m/%d/%Y"),as.Date(contracts$ContractDate.x,"%m/%d/%Y"));class(contracts$ContractDate)<-"Date"
contracts<-select(contracts,-ContractDate.x,-ContractDate.y)
contracts<-contracts[,c(1:5,24,6:7,10:14,8:9,15,16,25,17:23)]

SystemError<-filter(contracts,Stage=="Error")

## ## Sort the contracts list by ApprovalDate to prepare to drop one of the two rows for each stage
contracts<-arrange(contracts,desc(ApprovalDate))

## Filter the list down by stage
Outliers<-filter(contracts,is.na(ReqStatus))
ReadyForLaw<-filter(contracts,ReqStatus=="Ready for Purchasing")
LegalReview<-filter(contracts,POStatus=="In Progress")
DepAttorney<-filter(contracts,Stage=="Deputy City Attorney")%>%
  select(PO,AltID,ApprovalDate) 
CAO<-filter(contracts,Stage=="CAO")%>%
  select(PO,ApprovalDate)
SentVendor<-filter(contracts,Stage=="Sent to Vendor")%>%
  select(PO,ApprovalDate)
ECMS_BackFromVendor<-filter(contracts,Stage=="ECMS_Back From Vendor")%>%
  select(PO,ApprovalDate)
ExecutiveSignature<-filter(contracts,Stage=="Executive Signature")%>%
  select(PO,ApprovalDate)

## Pivot the approval dates to form one row for each contract
DepAttorney<-ddply(DepAttorney,"PO",summarize,DepAttorney=first(ApprovalDate))
CAO<-ddply(CAO,"PO",summarize,CAO=first(ApprovalDate))
SentVendor<-ddply(SentVendor,"PO",summarize,SentVendor=first(ApprovalDate))
ECMS_BackFromVendor<-ddply(ECMS_BackFromVendor,"PO",summarize,ECMS_BackFromVendor=first(ApprovalDate))
ExecutiveSignature<-ddply(ExecutiveSignature,"PO",summarize,ExecutiveSignature=first(ApprovalDate))

## Merge back into full PO dataset
contractMaster<-join(DepAttorney,contracts,by="PO","left",match="first")
  contractMaster<-merge(contractMaster,CAO,by="PO",all=TRUE)
    contractMaster<-merge(contractMaster,SentVendor,by="PO",all=TRUE)
      contractMaster<-merge(contractMaster,ECMS_BackFromVendor,by="PO",all=TRUE)
        contractMaster<-merge(contractMaster,ExecutiveSignature,by="PO",all=TRUE)
            contractMaster<-select(contractMaster,PO,DepAttorney,AltID:AdjustedSignDate,CAO:ExecutiveSignature)
contractpipeline<-rbind(ReadyForLaw,LegalReview)
contractpipeline<-rbind(contractpipeline,Outliers)
  contractpipeline$DepAttorney<-"NA"
  contractpipeline$CAO<-"NA"
  contractpipeline$SentVendor<-"NA"
  contractpipeline$ECMS_BackFromVendor<-"NA"
  contractpipeline$ExecutiveSignature<-"NA"
      contractpipeline<-select(contractpipeline,PO,DepAttorney,AltID:AdjustedSignDate,CAO:ExecutiveSignature)
          contractMaster<-join(contractMaster,contractpipeline,by="PO","full",match="first")

## Reconcile any date adjustments that may have fallen out of list during "match='first'"
#Reconcile<-select(contracts,PO,AltID,CancelDate,AdjustedSignDate,OrdinanceDate,BackFromVendor,Admin_ContractDate)
contractMaster<-merge(contractMaster,Adjustments,by=c("PO"),all=TRUE)
names(contractMaster)[names(contractMaster)=="Closed.x"]<-"Closed"
names(contractMaster)[names(contractMaster)=="OrdinanceDate.x"]<-"OrdinanceDate"
names(contractMaster)[names(contractMaster)=="BackFromVendor.x"]<-"BackFromVendor"
names(contractMaster)[names(contractMaster)=="DownForSignature.x"]<-"DownForSignature"
names(contractMaster)[names(contractMaster)=="Ordinance.x"]<-"Ordinance"
names(contractMaster)[names(contractMaster)=="CancelDate.x"]<-"CancelDate"
names(contractMaster)[names(contractMaster)=="AdjustedSignDate.x"]<-"AdjustedSignDate"
names(contractMaster)[names(contractMaster)=="AltID.x"]<-"AltID"

contractMaster$Closed<-ifelse(is.na(contractMaster$Closed),contractMaster$Closed.y,contractMaster$Closed)
contractMaster$Ordinance<-ifelse(is.na(contractMaster$Ordinance),contractMaster$Ordinance.y,contractMaster$Ordinance)
contractMaster$CancelDate<-ifelse(is.na(contractMaster$CancelDate),as.Date(contractMaster$CancelDate.y,format="%m/%d/%Y"),as.Date(contractMaster$CancelDate,format="%m/%d/%Y"));class(contractMaster$CancelDate)<-"Date"
contractMaster$OrdinanceDate<-ifelse(is.na(contractMaster$OrdinanceDate),as.Date(contractMaster$OrdinanceDate.y,format="%m/%d/%Y"),as.Date(contractMaster$OrdinanceDate,format="%m/%d/%Y"));class(contractMaster$OrdinanceDate)<-"Date"
contractMaster$BackFromVendor<-ifelse(is.na(contractMaster$BackFromVendor),as.Date(contractMaster$BackFromVendor.y,format="%m/%d/%Y"),as.Date(contractMaster$BackFromVendor,format="%m/%d/%Y"));class(contractMaster$BackFromVendor)<-"Date"
contractMaster$AdjustedSignDate<-ifelse(is.na(contractMaster$AdjustedSignDate),as.Date(contractMaster$AdjustedSignDate.y,format="%m/%d/%Y"),as.Date(contractMaster$AdjustedSignDate,format="%m/%d/%Y"));class(contractMaster$AdjustedSignDate)<-"Date"
contractMaster$DownForSignature<-ifelse(is.na(contractMaster$DownForSignature),as.Date(contractMaster$DownForSignature.y,format="%m/%d/%Y"),as.Date(contractMaster$DownForSignature,format="%m/%d/%Y"));class(contractMaster$DownForSignature)<-"Date"
contractMaster<-select(contractMaster,-AltID.y,-Closed.y,-Ordinance.y,-CancelDate.y,-OrdinanceDate.y,-BackFromVendor.y,-AdjustedSignDate.y,-DownForSignature.y)

## Select and arrange desired columns in dataset
contractMaster<-select(contractMaster,PO,Req,AltID,Dept:CancelDate,Closed,ReqStatus,POStatus,ReqComplete:LegalReview,Dep_CityAttorney=DepAttorney,CAO,OrdinanceDate,SentVendor,BackFromVendor,ECMS_BackFromVendor,DownForSignature,AdjustedSignDate,ExecutiveSignature) 
contractMaster$Type<-tolower(contractMaster$Type)
contractMaster$AltID<-tolower(contractMaster$AltID)

# 
contractMaster$Ordinance<-ifelse(is.na(contractMaster$Ordinance),"Unknown",ifelse(contractMaster$Ordinance=="Yes","Yes","No"))
contractMaster$Type<-gsub("[[:punct:]]","",contractMaster$Type)

## Categorize contracts into different types for analyzing differentials in days to execute
contractMaster$Group<-ifelse(startsWith(contractMaster$AltID,"m"),"Manual",
                             ifelse(contractMaster$Ordinance=="Yes","Ordinance", 
                                 ifelse(grepl("time",contractMaster$Type),"Time Only",
                                      ifelse(contractMaster$Dept=="AVIATION","Aviation",
                                          ifelse(is.na(contractMaster$Govt),"Other","Intergovernmental")))))   
                                                                         
contractMaster$Type2<-ifelse(grepl("cea",contractMaster$Type),"CEA",
                           ifelse(grepl("psa under 15k",contractMaster$Type),"PSA Under $15k",
                                ifelse(grepl("psa",contractMaster$Type),"PSA Over $15k",
                                    ifelse(grepl("grant",contractMaster$Type),"Grant",
                                        ifelse(grepl("bid",contractMaster$Type),"Bid","Other")))))

## Create reconciliation variables to make days between stages easier to calculate
contractMaster$CAO_Ord<-ifelse(is.na(contractMaster$OrdinanceDate),as.Date(contractMaster$CAO,format="%m/%d/%Y"),as.Date(contractMaster$OrdinanceDate,format="%m/%d/%Y")); class(contractMaster$CAO_Ord)<-"Date"
contractMaster$VendorReconciled<-ifelse(is.na(contractMaster$BackFromVendor),as.Date(contractMaster$ECMS_BackFromVendor,format="%m/%d/%Y"),as.Date(contractMaster$BackFromVendor,format="%m/%d/%Y")); class(contractMaster$VendorReconciled)<-"Date"
contractMaster$ReadyforExec<-ifelse(is.na(contractMaster$DownForSignature),as.Date(contractMaster$VendorReconciled,format="%m/%d/%Y"),as.Date(contractMaster$DownForSignature,format="%m/%d/%Y")); class(contractMaster$ReadyforExec)<-"Date"
contractMaster$SignDate<-ifelse(is.na(contractMaster$AdjustedSignDate),as.Date(contractMaster$ExecutiveSignature,format="%m/%d/%Y"),as.Date(contractMaster$AdjustedSignDate,format="%m/%d/%Y")); class(contractMaster$SignDate)<-"Date"
contractMaster$ContractDate<-ifelse(is.na(contractMaster$ContractDate),as.Date(contractMaster$Admin_ContractDate,format="%m/%d/%Y"),as.Date(contractMaster$ContractDate,format="%m/%d/%Y")); class(contractMaster$ContractDate)<-"Date"

## Create variable for the average days from contract creation to execution (Days to Execute)
contractMaster$Days_to_Execute<-ContractDays(contractMaster,ContractDate,SignDate)
contractMaster$Days_to_Execute<-ifelse(contractMaster$Days_to_Execute<0,0,contractMaster$Days_to_Execute)

## Calculate days per stage
contractMaster$LegalReview_Days<-ContractDays(contractMaster,ContractDate,LegalReview )
contractMaster$DepAttorney_Days<-ContractDays(contractMaster,LegalReview,Dep_CityAttorney)
contractMaster$CAO_Days<-ContractDays(contractMaster,Dep_CityAttorney,CAO)
contractMaster$Ordinance_Days<-strptime(contractMaster$OrdinanceDate,format="%Y-%m-%d")-strptime(contractMaster$CAO,format="%Y-%m-%d") ##ContractDays doesn't work with this variable
contractMaster$SentVendor_Days<-ContractDays(contractMaster,CAO_Ord,SentVendor)
contractMaster$DaysWithVendor<-ContractDays(contractMaster,SentVendor,VendorReconciled)
contractMaster$DownForSignature_Days<-ContractDays(contractMaster,VendorReconciled,DownForSignature)
contractMaster$ExecutiveSignature_Days<-ContractDays(contractMaster,ReadyforExec,SignDate)

## Re-calculate any negative days to zero days
contractMaster$Ordinance_Days<-ifelse(contractMaster$Ordinance_Days<0,NA,contractMaster$Ordinance_Days)
contractMaster$SentVendor_Days<-ifelse(contractMaster$SentVendor_Days<0,0,contractMaster$SentVendor_Days)
contractMaster$DaysWithVendor<-ifelse(contractMaster$DaysWithVendor<0,0,contractMaster$DaysWithVendor)
contractMaster$ExecutiveSignature_Days<-ifelse(contractMaster$ExecutiveSignature_Days<0,0,contractMaster$ExecutiveSignature_Days)

## Clean days in stage for manual contracts so there is only a "days to execute" calculation for those contracts
contractMaster$LegalReview_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$LegalReview_Days)
contractMaster$DepAttorney_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$DepAttorney_Days)
contractMaster$CAO_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$CAO_Days)
contractMaster$Ordinance_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$Ordinance_Days)
contractMaster$SentVendor_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$SentVendor_Days)
contractMaster$DaysWithVendor<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$DaysWithVendor)
contractMaster$DownForSignature_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$DownForSignature_Days)
contractMaster$ExecutiveSignature_Days<-ifelse(startsWith(contractMaster$AltID,"m"),NA,contractMaster$ExecutiveSignature_Days)

## Create a variable to segment data into quarters
contractMaster$Last_Qtr<-as.yearqtr(contractMaster$SignDate,format="%m/%d/%Y")                                    
contractMaster$Last_Qtr2<-ifelse(contractMaster$Purchaser=="CECMS",as.yearqtr(contractMaster$ContractDate,format="%Y-%m-%d"),NA);class(contractMaster$Last_Qtr2)<-"yearqtr"
contractMaster$Last_Qtr3<-ifelse(!is.na(contractMaster$CancelDate),as.yearqtr(contractMaster$CancelDate,format="%Y-%m-%d"),NA);class(contractMaster$Last_Qtr3)<-"yearqtr"
contractMaster$Last_Qtr4<-ifelse(contractMaster$POStatus=="Canceled" & is.na(contractMaster$CancelDate),as.yearqtr(contractMaster$ContractDate,format="%Y-%m-%d"),NA);class(contractMaster$Last_Qtr4)<-"yearqtr"
contractMaster$Last_Qtr<-ifelse(!is.na(contractMaster$Last_Qtr),contractMaster$Last_Qtr,
                                ifelse(!is.na(contractMaster$Last_Qtr2),contractMaster$Last_Qtr2,
                                  ifelse(!is.na(contractMaster$Last_Qtr3),contractMaster$Last_Qtr3,
                                    ifelse(!is.na(contractMaster$Last_Qtr4),contractMaster$Last_Qtr4,NA))));class(contractMaster$Last_Qtr)<-"yearqtr"
contractMaster<-select(contractMaster,-Last_Qtr2,-Last_Qtr3,-Last_Qtr4)
contractMaster$First_Qtr<-as.yearqtr(contractMaster$ContractDate,format="%Y-%m-%d") ## Find quarter that a contract was created
contractMaster$Qtr_Start<-as.Date(as.yearqtr(contractMaster$First_Qtr,format="%Y-%m-%d")) ##Find start date of quarter that a contract was created in ECMS.
contractMaster$Qtr_End<-as.Date(as.yearqtr(contractMaster$Last_Qtr), frac = 1 )  ## Find end date of quarter that a contract was signed, if applicable
contractMaster$Qtr_End<-ifelse(!is.na(contractMaster$CancelDate),as.Date( as.yearqtr(contractMaster$CancelDate), frac = 1 ),contractMaster$Qtr_End);class(contractMaster$Qtr_End)<-"Date"

## Assign contracts that haven't been drafted yet (without a contract date) as having a starting quarter and date in the current reporting period
#contractMaster$First_Qtr<-ifelse(is.na(contractMaster$ContractDate),
          #  as.yearqtr(MaxQtr,format="%Y-%m-%d"),
          #  as.yearqtr(contractMaster$ContractDate,format="%Y-%m-%d"))
           #     class(contractMaster$First_Qtr)<-"yearqtr"
#contractMaster$Qtr_Start<-ifelse(is.na(contractMaster$ContractDate),as.Date(as.yearqtr(MaxQtr,format="%Y-%m-%d")),as.Date(as.yearqtr(contractMaster$ContractDate,format="%Y-%m-%d")))
 # class(contractMaster$Qtr_Start)<-"Date"
contractMaster$First_Qtr2<-ifelse(!is.na(contractMaster$CancelDate) & is.na(contractMaster$First_Qtr),as.yearqtr(contractMaster$CancelDate,format="%Y-%m-%d"),NA)
contractMaster$First_Qtr3<-ifelse(!is.na(contractMaster$SignDate) & is.na(contractMaster$First_Qtr),as.yearqtr(contractMaster$SignDate,format="%Y-%m-%d"),NA)
contractMaster$First_Qtr<-ifelse(!is.na(contractMaster$First_Qtr),contractMaster$First_Qtr,
                                 ifelse(!is.na(contractMaster$First_Qtr2),contractMaster$First_Qtr2,
                                        ifelse(!is.na(contractMaster$First_Qtr3),contractMaster$First_Qtr3,NA)));class(contractMaster$First_Qtr)<-"yearqtr"
df_max<-select(contractMaster,ContractDate,First_Qtr)%>%
  subset(!is.na(ContractDate))  ## create dummy dataframe
MaxQtr<-max(df_max$First_Qtr)  ## Determine last quarter in reporting period
#contractMaster$First_Qtr<-ifelse(!is.na(contractMaster$First_Qtr),contractMaster$First_Qtr,
                             #    ifelse(!is.na(contractMaster$SignDate),as.yearqtr(contractMaster$SignDate,format="%Y-%m-%d"),
                                  #      ifelse(!is.na(contractMaster$CancelDate),as.yearqtr(contractMaster$CancelDate,format="%Y-%m-%d"),NA)));class(contractMaster$First_Qtr)<-"yearqtr"
contractMaster$Qtr_Start<-ifelse(is.na(contractMaster$First_Qtr),as.Date(as.yearqtr(MaxQtr,format="%Y-%m-%d")),as.Date(as.yearqtr(contractMaster$First_Qtr,format="%Y-%m-%d")));class(contractMaster$Qtr_Start)<-"Date"
contractMaster<-subset(contractMaster,!is.na(contractMaster$First_Qtr))
contractMaster<-select(contractMaster,-First_Qtr2,-First_Qtr3)
  
## Re-code POStatus column so contracts that don't have a POStatus will be coded as "Not Processed."
contractMaster$POStatus<-ifelse(is.na(contractMaster$POStatus),"Not Processed",
                                ifelse(contractMaster$POStatus=="Ready for Approval","Ready for Approval",
                                       ifelse(contractMaster$POStatus=="In Progress","In Progress",
                                              ifelse(contractMaster$POStatus=="Sent","Sent",
                                                     ifelse(contractMaster$POStatus=="Canceled","Canceled","Ready to Send")))))

Anti-join outlier contracts that have been closed, but appear to be open 
#exclude<-contractMaster[is.na(contractMaster$Last_Qtr) & contractMaster$POStatus=="Sent"|contractMaster$POStatus=="Canceled" & is.na(contractMaster$Last_Qtr)|is.na(contractMaster$ContractDate),]
exclude<-subset(contractMaster,is.na(contractMaster$Last_Qtr) & contractMaster$First_Qtr<"2015 Q1")
contractMaster<-anti_join(contractMaster,exclude,by="PO")

## Convert columns that lost class back to appropriate class
class(contractMaster$Last_Qtr)<-"yearqtr"; class(contractMaster$First_Qtr)<-"yearqtr"
   class(contractMaster$Dep_CityAttorney)<-"Date";class(contractMaster$CAO)<-"Date"
            class(contractMaster$SentVendor)<-"Date";class(contractMaster$ECMS_BackFromVendor)<-"Date"
                class(contractMaster$VendorReconciled)<-"Date";class(contractMaster$ReadyforExec)<-"Date";                    
                      class(contractMaster$ExecutiveSignature)<-"Date"; class(contractMaster$CAO_Ord)<-"Date"
                            class(contractMaster$SignDate)<-"Date"

# Remove duplicated contracts from contractMaster
contractMaster<-contractMaster[order(contractMaster$DownForSignature,decreasing=TRUE),]
contractMaster<-contractMaster[!duplicated(contractMaster$PO),]

## Create summary for plotting the chart of incoming, outgoing, and net contracts (currently only net)
Qtr_First_summary<-ddply(contractMaster,"First_Qtr",summarise,n=n())
Qtr_End_summary<-ddply(contractMaster,"Last_Qtr",summarise,n=n())
Qtr_First_summary<-rename(Qtr_First_summary,Qtr=First_Qtr,Opened=n)
Qtr_End_summary<-rename(Qtr_End_summary,Qtr=Last_Qtr,Closed=n)
summary_merge<-merge(Qtr_First_summary,Qtr_End_summary,by="Qtr",all=TRUE)
#summary_merge<-subset(summary_merge,Qtr>"2012 Q4")
#summary_merge$Closed<-ifelse(summary_merge$Qtr=="2012 Q4",0,summary_merge$Closed) ## Coerce 2012 Q4 closed to 0
#summary_merge$Opened<-ifelse(summary_merge$Qtr=="2012 Q4",112,summary_merge$Opened)  ## Coerce 2012 Q4 open contracts to 126, per historical analysis
summary_merge$cumulative_opened<-cumsum(summary_merge$Opened)
summary_merge$cumulative_closed<-cumsum(summary_merge$Closed)
summary_merge$cumulative_net<-summary_merge$cumulative_opened-summary_merge$cumulative_closed
summary_merge<-subset(summary_merge,Qtr>"2012 Q3")
#summary_merge$qtr_net<-summary_merge$Opened-summary_merge$Closed
#summary_merge$qtr_net<-mutate(summary_merge,Opened-Closed)
#summary<-subset(summary_merge,!is.na(Qtr) & Qtr>"2012 Q2")
openplot<-ggplot(summary_merge,aes(x=factor(Qtr),y=cumulative_net,group=1))
openplot<-openplot+geom_bar(stat="identity",fill="purple",size=0.6)+ggtitle("Contracts in Queue at the End of Quarter")+ylab("Number of Contracts")+xlab("Quarters")
openplot<-openplot+geom_text(aes(y=cumulative_net,ymax=cumulative_net+1,label=cumulative_net),vjust=-.25)
print(openplot) ## Create chart of open contracts at end of each quarter
ggsave("./Slides/Open Contracts.png")
summary_melt<-select(summary_merge,Qtr,Opened,Closed)
summary_melt<-subset(summary_melt,Qtr>"2012 Q4")
summary_melt<-melt(summary_melt,id.vars="Qtr")
Open_closedplot<-ggplot(summary_melt,aes(x=factor(Qtr),y=value))+
  geom_bar(width=0.8,aes(fill=variable),position="dodge",stat="identity") ## Create chart of open and closed contracts by quarter
    Open_closedplot<-Open_closedplot+ggtitle("Contracts Opened and Closed by Quarter")+xlab("Quarters")+ylab("Number of Contracts")
      Open_closedplot<-Open_closedplot+geom_text(aes(y=value,ymax=value,label=value),position=position_dodge(width=0.7),size=4)
print(Open_closedplot)
ggsave("./Slides/Opened and Closed Contracts.png")

## Subset contract master list into separate list of contracts that have been closed and those currently open
Opencontracts<-subset(contractMaster,is.na(Last_Qtr))
Closedcontracts<-subset(contractMaster, POStatus=="Canceled"|POStatus=="Ready to Send"|POStatus=="Sent"|Closed==1|Days_to_Execute>=0)
#Closedcontracts<-contractMaster[!contractMaster$PO %in% Closedcontracts$PO,]

## Subset Closed contract data frame for contracts signed later than Q2 2012, and closed contract data frame for historical contracts that should be removed from list
Closedcontracts<-subset(Closedcontracts,Last_Qtr>"2012 Q2")
#Opencontracts<-subset(Opencontracts,is.na(ContractDate)|ContractDate>as.Date("2013-12-31"))

##Calculate total age, as well as days in each stage except for Exec Counsel for open contracts
Opencontracts$AttorneyReview_Age<-ifelse(is.na(Opencontracts$LegalReview) & !is.na(Opencontracts$ContractDate),StageAge(Opencontracts,ContractDate,QtrEnd),NA)
Opencontracts$DepAttorney_Age<-ifelse(is.na(Opencontracts$Dep_CityAttorney) & !is.na(Opencontracts$LegalReview),StageAge(Opencontracts,LegalReview,QtrEnd),NA)
Opencontracts$CAO_Age<-ifelse(is.na(Opencontracts$CAO) & !is.na(Opencontracts$Dep_CityAttorney),StageAge(Opencontracts,Dep_CityAttorney,QtrEnd),NA)
Opencontracts$OpenOrd<-ifelse(Opencontracts$Ordinance=="Yes","Yes",NA)
Opencontracts$Ordinance_Age<-ifelse(!is.na(Opencontracts$OpenOrd) & is.na(Opencontracts$Ordinance_Days),strptime(QtrEnd,format="%Y-%m-%d")-strptime(contractMaster$CAO,format="%Y-%m-%d"),NA);Opencontracts<-select(Opencontracts,-OpenOrd)
Opencontracts$SendVendor_Age<-ifelse(is.na(Opencontracts$SentVendor) & !is.na(Opencontracts$CAO_Ord),StageAge(Opencontracts,CAO_Ord,QtrEnd),NA)
Opencontracts$AwaitingVendor_Age<-ifelse(is.na(Opencontracts$VendorReconciled) & !is.na(Opencontracts$SentVendor),StageAge(Opencontracts,SentVendor,QtrEnd),NA)
Opencontracts$BringToExec_Age<-ifelse(is.na(Opencontracts$DownForSignature) & Opencontracts$VendorReconciled>as.Date("2014-12-31","%Y-%m-%d"),StageAge(Opencontracts,VendorReconciled,QtrEnd),NA)

## Determine which contracts are with Exec Counsel and calculate age
Opencontracts$AgeReconcile<-mutate(Opencontracts,AgeReconcile=AttorneyReview_Age+DepAttorney_Age+CAO_Age+Ordinance_Age+SendVendor_Age+AwaitingVendor_Age+BringToExec_Age)
Opencontracts$ExecutiveSignature_Age<-ifelse(is.na(Opencontracts$AgeReconcile),StageAge(Opencontracts,ReadyforExec,QtrEnd),NA)
#Opencontracts$ExecutiveSignature_Age<-ifelse(is.na(Opencontracts$AttorneyReview_Age) & is.na(Opencontracts$DepAttorney_Age) & is.na(Opencontracts$CAO) & is.na(Opencontracts$Ordinance_Age) & is.na(Opencontracts$SendVendor_Age) & is.na(Opencontracts$AwaitingVendor_Age) & is.na(Opencontracts$BringToExec_Age), StageAge(Opencontracts, ReadyforExec, QtrEnd), NA)
Opencontracts<-select(Opencontracts,-AgeReconcile)
## Calculate average age of contracts in queue, from contract date to end of Qtr
Opencontracts$TotalAge<-TotalAge(Opencontracts,ContractDate,QtrEnd)

## Remove days-in-stage calculation columns, as they only apply to closed contracts
Opencontracts<-select(Opencontracts,-ends_with("Days"),-starts_with("Days"))

## Plot days in stage for executed contracts
Stages<-subset(Closedcontracts,Last_Qtr>"2012 Q4")
Stages<-melt(select(Stages,PO,Last_Qtr,LegalReview_Days:ExecutiveSignature_Days),id.var=c("PO","Last_Qtr"),variable.name="Stage",value.name="Days_in_Stage")
Stages$Days_in_Stage<-as.numeric(Stages$Days_in_Stage)
Stages<- aggregate(Days_in_Stage ~ Last_Qtr + Stage, data = Stages, mean)
levels(Stages$Stage)[levels(Stages$Stage)=="LegalReview_Days"]<-"Legal Review"
levels(Stages$Stage)[levels(Stages$Stage)=="DepAttorney_Days"]<-"Dep. City Attorney"
levels(Stages$Stage)[levels(Stages$Stage)=="CAO_Days"]<-"CAO"
levels(Stages$Stage)[levels(Stages$Stage)=="Ordinance_Days"]<-"Ordinance"
levels(Stages$Stage)[levels(Stages$Stage)=="SentVendor_Days"]<-"Sent Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="DaysWithVendor"]<-"Days with Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="DownForSignature_Days"]<-"Down to Exec"
levels(Stages$Stage)[levels(Stages$Stage)=="ExecutiveSignature_Days"]<-"Executed"
Stage_plot<-ggplot(Stages,aes(x=factor(Last_Qtr),y=Days_in_Stage,group=1))
Stage_plot<-Stage_plot+geom_line(stat="identity",fill="purple",size=0.6)
Stage_plot<-Stage_plot+facet_grid(facets=.~Stage)
Stage_plot<-Stage_plot+ggtitle("Average Days per Stage for Executed Contracts")
Stage_plot<-Stage_plot+xlab("Quarters")
Stage_plot<-Stage_plot+ylab("Days")
Stage_plot<-Stage_plot+theme(strip.text.x=element_text(size=8))
print(Stage_plot)
ggsave("./Slides/Closed Contracts by Stage.png")

## Plot Days to Execute
Days2Execute<-ddply(subset(Closedcontracts,!is.na(Days_to_Execute),Last_Qtr>"2012 Q2"),"Last_Qtr",summarise,Sign=mean(Days_to_Execute))
Execution<-ggplot(Days2Execute,aes(x=factor(Last_Qtr),y=Sign))
Execution<-Execution+geom_bar(stat="identity",fill="steelblue")
Execution<-Execution+ggtitle("Average Days to Execute Contracts by Quarter")
Execution<-Execution+xlab("Quarters")
Execution<-Execution+ylab("Days")
Execution<-Execution+geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Execution<-Execution+geom_hline(aes(yintercept=30),colour="#FF0000",linetype=2,size=1.2)+theme(legend.position="top",legend.text=)
print(Execution)
ggsave("./Slides/Days to Execute.png")

## Plot days in stage for executed contracts
Stage_Ages<-select(Opencontracts,PO:AltID,AttorneyReview_Age:ExecutiveSignature_Age)
Stage_Ages<-melt(select(Stage_Ages,PO,Last_Qtr,AttorneyReview_Age:ExecutiveSignature_Age),id.var=c("PO","Last_Qtr"),variable.name="Stage",value.name="Age_in_Stage")
Stage_Ages$Days_in_Stage<-as.numeric(Stage_Ages$Age_in_Stage)
Stages<- aggregate(Age_in_Stage ~ Last_Qtr + Stage, data = Stages, mean)
levels(Stages$Stage)[levels(Stages$Stage)=="AttorneyReview_Age"]<-"Legal Review"
levels(Stages$Stage)[levels(Stages$Stage)=="DepAttorney_Age"]<-"Dep. City Attorney"
levels(Stages$Stage)[levels(Stages$Stage)=="CAO_Age"]<-"CAO"
levels(Stages$Stage)[levels(Stages$Stage)=="Ordinance_Age"]<-"Ordinance"
levels(Stages$Stage)[levels(Stages$Stage)=="SendVendor_Age"]<-"Sent Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="AwaitingVendor_Age"]<-"Days with Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="BringToExec_Age"]<-"Down to Exec"
levels(Stages$Stage)[levels(Stages$Stage)=="ExecutiveSignature_Days"]<-"Executed"
Stage_plot<-ggplot(Stages,aes(x=factor(Last_Qtr),y=Days_in_Stage,group=1))
Stage_plot<-Stage_plot+geom_line(stat="identity",fill="purple",size=0.6)
Stage_plot<-Stage_plot+facet_grid(facets=.~Stage)
Stage_plot<-Stage_plot+ggtitle("Average Days per Stage for Executed Contracts")
Stage_plot<-Stage_plot+xlab("Quarters")
Stage_plot<-Stage_plot+ylab("Days")
Stage_plot<-Stage_plot+theme(strip.text.x=element_text(size=8))
print(Stage_plot)
ggsave("./Slides/Closed Contracts by Stage.png")

## Plot days to execute broken down by process
Execute_Process<-aggregate(Days_to_Execute~Last_Qtr+Group,data=Closedcontracts,mean)
ExecuteProcess_Plot<-ggplot(Execute_Process,aes(x=factor(Last_Qtr),y=Days_to_Execute,group=Group,color=factor(Group)))
ExecuteProcess_Plot<-ExecuteProcess_Plot+geom_line(stat="identity",size=1.25)
ExecuteProcess_Plot<-ExecuteProcess_Plot+text(c(2,2),c(37,35),labels=c("Manual","Ordinance","Time Only","Aviation","CEA","PSA","Grant","Bid"))
print(ExecuteProcess_Plot)

## Plot days to execute broken down by process
Execute_Type<-aggregate(Days_to_Execute~Last_Qtr+Type2,data=Closedcontracts,mean)
ExecuteType_Plot<-ggplot(Execute_Type,aes(x=factor(Last_Qtr),y=Days_to_Execute,group=Type2,color=factor(Type2)))
ExecuteType_Plot<-ExecuteType_Plot+geom_line(stat="identity",size=1.25)
print(ExecuteType_Plot)

## Subset for contracts requiring ordinance and Plot 
OrdinanceSign<-subset(Closedcontracts,Ordinance=="Yes")
OrdExecute<-ddply(OrdinanceSign,"Last_Qtr",summarise,OrdSign=mean(Days_to_Execute))
OrdExecution<-ggplot(OrdExecute,aes(x=factor(Last_Qtr),y=OrdSign))
OrdExecution<-OrdExecution+geom_bar(stat="identity",fill="steelblue")
OrdExecution<-OrdExecution+ggtitle("Days to Execute by Quarter")
OrdExecution<-OrdExecution+xlab("Quarters")
OrdExecution<-OrdExecution+ylab("Days")
OrdExecution<-OrdExecution+geom_text(aes(y=OrdSign,ymax=OrdSign+10,label=round(OrdSign,1)),position=position_dodge(width=0.9),vjust=-.5)
print(OrdExecution)
ggsave("./Slides/Ordinance Execute.png")


## Generate spreadsheets of the underlying output data
write.xlsx2(contractMaster,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract Approval Master.xlsx",showNA=FALSE)
write.csv(contractMaster,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract Approval Master.csv")
write.csv(Closedcontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Closed Contracts.csv")
write.csv(Opencontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Open Contracts.csv")
write.csv(LawExec,"O:/Projects/ReqtoCheckStat/Query Files/Output/LawExec.csv")
write.csv(Adjustments,"O:/Projects/ReqtoCheckStat/Query Files/Output/Adjust.csv")
write.csv(OrdinanceSign,"O:/Projects/ReqtoCheckStat/Query Files/Output/OrdSign.csv")
write.csv(contracts,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/contracts.csv")
write.csv(Ordinance,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Ordinances.csv")

localdir<-"C:/Users/vbspencer/Desktop/Query Files/Output/contract master.csv"
localdir2<-"C:/Users/vbspencer/Desktop/Query Files/Output/contracts.csv"
write.csv(contracts,localdir2)
write.csv(contractMaster,localdir)

workdircontracts<-"O:/Projects/ReqtoCheckSTAT/Query Files/Output/"
write.xlsx2(contractMaster,"Output/Contract Approval Master.xlsx",showNA=FALSE)
