.libPaths("C:/Rpackages")

library(tidyr)
library (plyr)  
library(dplyr)
library(lubridate)
library(xlsx)
library(gdata)
library(stringr)
library(r2excel)
library(reshape2)
library(zoo)
library(ggplot2)

## Read in needed files
contractPOapproval<-read.csv("Contract Approval Sequence POs.csv",skip=3) ## Report pulled from ECMS
contractPOstatus<-read.csv("Contract PO Status.csv",skip=3) ## Report pulled from ECMS
contractReqstatus<-read.csv("Contract Req Status.csv",skip=3) ## Report pulled from ECMS
contractReqapproval<-read.csv("Contract Approval Sequence Reqs.csv",skip=3)  ##Report pulled from ECMS
LawExec<-read.csv("Law and Executive Counsel Log.csv",na.strings="") ## List compiled by Law and Executive Counsel
Adjustments<-read.xlsx2("Adjustments.xlsx",sheetIndex=1,colClasses=c("character","character","Date","Date","numeric","numeric","character","Date","character"))
BackFromVendor<-read.xlsx2("Adjustments.xlsx",sheetIndex=2,colClasses=c("character","character","Date","Date","numeric","numeric","character","Date","character"))
Ordinance<-read.xlsx2("Adjustments.xlsx",sheetIndex=3,colClasses=c("character","character","Date","Date","numeric","numeric","character","Date","character")) 

#
BackFromVendor<-select(BackFromVendor,PO:BackFromVendor)
class(BackFromVendor$BackFromVendor)<-"Date"
Ordinance<-select(Ordinance,PO,AltID,Ordinance,OrdinanceDate)
class(Ordinance$OrdinanceDate)<-"Date"
Adjustments<-select(Adjustments,PO,AltID,AdjustedSignDate,Closed)
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
LawExec$Date.Received.by.Law<-ifelse(substr(LawExec$Date.Received.by.Law,4,4)=="2",LawExec$Date.Received.by.Law+1096,LawExec$Date.Received.by.Law)
class(LawExec$Date.Received.by.Law)<-"Date"

##Clean Alternate ID number column to match ECMS formatting conventions of "k##-###"/"mk##-###"
LawExec$K.Number<-tolower(LawExec$K.Number)
LawExec$K.Number<-ifelse(grepl("^\\d", LawExec$K.Number),paste("k",LawExec$K.Number,sep=""),paste(LawExec$K.Number))
LawExec$K.Number<-ifelse(startsWith(LawExec$K.Number,"m"),paste(substring(LawExec$K.Number,1,1),"k",substring(LawExec$K.Number,2,8),sep=""),LawExec$K.Number)
LawMaster$K.Number<-tolower(LawMaster$K.Number)
LawMaster$K.Number<-str_trim(LawMaster$K.Number,side="both")
LawMaster$K.Number<-ifelse(grepl("^\\d",LawMaster$K.Number),paste("k",LawMaster$K.Number,sep=""),paste(LawMaster$K.Number))
LawMaster$K.Number<-ifelse(startsWith(LawMaster$K.Number,"m"),paste(substring(LawMaster$K.Number,1,1),"k",substring(LawMaster$K.Number,2,8),sep=""),LawMaster$K.Number)

## Extract desired variable rows and columns from each file
contractPOapproval<-select(contractPOapproval,AltID=ALTERNATE_ID,PO=PO_NBR,ContractDate=PO_DATE,LegalReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,Approver=PO_APPROVER,Stage=ORDER_SEQUENCE)
contractPOstatus<-select(contractPOstatus,PO=PO_NBR3,Req=REQ_NBR3,AltID=ALTERNATE_ID3,Description=SHORT_DESC3,Dept=DESC_TEXT3,Vendor=NAME3,Requestor=REQUESTOR_ID3,POStatus=Status3,ReqComplete=Max_Date3)
contractReqstatus<-select(contractReqstatus,Req=REQ_NBR,Description=SHORT_DESC,ReqStatus=STATUS)
contractReqapproval<-select(contractReqapproval,Req=REQ_NBR,ReqApprove=APPROVAL_DATE,ReqApprover=REQ_APPROVER)
LawExec$Date.Signed.by.MAY<-as.Date(LawExec$Date.Signed.by.MAY,"%m/%d/%Y")
LawExec<-select(LawExec,PO=PO.Number,AltID=K.Number,BackFromVendor=Date.Received.by.Law,DownForSignature=Date.Received.by.EX,AdjustedSignDate=Date.Signed.by.MAY)
LawMaster<-select(LawMaster,PO=PO.Number,AltID=K.Number,Govt=Govt,Type=Type.of.K,LawStatus=Status)

## Code relevant columns into "Date" class
LawExec$BackFromVendor<-as.Date(LawExec$BackFromVendor,"%m/%d/%Y")
Adjustments<-merge(LawExec,Adjustments,by=c("PO","AltID","BackFromVendor","AdjustedSignDate"),all=TRUE)

## Merge files into consolidated contract list; 
contracts<-merge(contractPOapproval,contractPOstatus,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,contractReqstatus,by=c("Req"),all=TRUE)
contracts<-merge(contracts,LawMaster,by=c("PO","AltID"),all=TRUE)
contracts<-merge(contracts,Adjustments,by=c("PO","AltID"),all=TRUE)

## Re-Code Order Sequence to appropriate appproval stage category
contracts$Stage<-ifelse(contracts$Stage==1,"Deputy City Attorney",ifelse(contracts$Stage==2,"CAO",ifelse(contracts$Stage==3,"Sent to Vendor",ifelse(contracts$Stage==4,"ECMS_Back From Vendor",ifelse(contracts$Stage==5,"Executive Signature","Error")))))
contracts<-select(contracts,PO,AltID,Req,Description.x,Description.y,Dept,Vendor,Requestor,ReqStatus,POStatus,Govt,Type,Ordinance,Closed,ReqComplete,ContractDate,LegalReview,ApprovalDate,Stage,OrdinanceDate,BackFromVendor,DownForSignature,AdjustedSignDate)

## Convert remaining date columns to "Date" class
contracts$ContractDate<-as.Date(contracts$ContractDate,"%m/%d/%Y")
contracts$ApprovalDate<-as.Date(contracts$ApprovalDate,"%m/%d/%Y")
contracts$LegalReview<-as.Date(contracts$LegalReview,"%m/%d/%Y")
contracts$ContractDate<-as.Date(contracts$ContractDate,"%m/%d/%Y")
contracts$DownForSignature<-as.Date(contracts$DownForSignature,"%m/%d/%Y")

## Consolidate contract description column
contracts$Description<-ifelse(!is.na(contracts$Description.x),as.character(contracts$Description.x),as.character(contracts$Description.y))
contracts<-select(contracts,-Description.x,-Description.y)
contracts<-contracts[,c(1:5,22,6,9:11,7:8,12:21)]

SystemError<-filter(contracts,Stage=="Error")

## ## Sort the contracts list by ApprovalDate to prepare to drop one of the two rows for each stage
contracts<-arrange(contracts,desc(ApprovalDate))

## Filter the list down by stage
ReadyForLaw<-filter(contracts,ReqStatus=="Ready for Purchasing")
LegalReview<-filter(contracts,POStatus=="In Progress")
DepAttorney<-filter(contracts,Stage=="Deputy City Attorney")%>%
  select(PO,ApprovalDate) 
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
  contractpipeline$DepAttorney<-"NA"
  contractpipeline$CAO<-"NA"
  contractpipeline$SentVendor<-"NA"
  contractpipeline$ECMS_BackFromVendor<-"NA"
  contractpipeline$ExecutiveSignature<-"NA"
      contractpipeline<-select(contractpipeline,PO,DepAttorney,AltID:AdjustedSignDate,CAO:ExecutiveSignature)
          contractMaster<-join(contractMaster,contractpipeline,by="PO","full",match="first")

## Select and arrange desired columns in dataset
contractMaster<-select(contractMaster,PO,Req,AltID,Dept:Ordinance,Closed,ReqStatus,POStatus,ReqComplete:LegalReview,Dep_CityAttorney=DepAttorney,CAO,OrdinanceDate,SentVendor,BackFromVendor,ECMS_BackFromVendor,DownForSignature,AdjustedSignDate,ExecutiveSignature) 
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

## Create variable for the average days from contract creation to execution (Days to Execute)
contractMaster$Days_to_Execute<-ContractDays(Closedcontracts,ContractDate,SignDate)
contractMaster$Days_to_Execute<-ifelse(contractMaster$Days_to_Execute<0,0,contractMaster$Days_to_Execute)

## Subset contract master list into separate list of contracts that have been closed and those currently open
Closedcontracts<-subset(contractMaster, POStatus=="Canceled"|POStatus=="Ready to Send"|POStatus=="Sent"|Closed==1|Days_to_Execute>=0)
OpenReconcile<-contractMaster$PO %in% Closedcontracts$PO
Opencontracts<-contractMaster[!contractMaster$PO %in% Closedcontracts$PO,]

## Create function for days between dates in the contracting process, rounded to whole numbers
ContractDays<-function(df,FirstDt,EndDt){
  arguments<-as.list(match.call())  
  EndDt<-eval(arguments$EndDt,df)
  FirstDt<-eval(arguments$FirstDt,df)
  round((strptime(EndDt,"%Y-%m-%d")-strptime(FirstDt,"%Y-%m-%d"))/86400,digits=0)
}


## Calculate days per stage
contractMaster$LegalReview_Days<-ContractDays(Closedcontracts,ContractDate,LegalReview )
contractMaster$AttorneyReview_Days<-ContractDays(Closedcontracts,LegalReview,Dep_CityAttorney)
contractMaster$CAO_Days<-ContractDays(Closedcontracts,Dep_CityAttorney,CAO)
contractMaster$Ordinance_Days<-ifelse(contractMaster$Ordinance=="Yes",ContractDays(Closedcontracts,CAO,OrdinanceDate),"NA")
contractMaster$SentVendor_Days<-ContractDays(Closedcontracts,CAO_Ord,SentVendor)
contractMaster$DaysWithVendor<-ContractDays(Closedcontracts,SentVendor,VendorReconciled)
contractMaster$DownForSignature_Days<-ContractDays(Closedcontracts,VendorReconciled,DownForSignature)
contractMaster$ExecutiveSignature_Days<-ContractDays(Closedcontracts,ReadyforExec,SignDate)

## Re-calculate any negative days to zero days
contractMaster$Ordinance_Days<-ifelse(contractMaster$Ordinance_Days<0,0,contractMaster$Ordinance_Days)
contractMaster$SentVendor_Days<-ifelse(contractMaster$SentVendor_Days<0,0,contractMaster$SentVendor_Days)
contractMaster$DaysWithVendor<-ifelse(contractMaster$DaysWithVendor<0,0,contractMaster$DaysWithVendor)
contractMaster$ExecutiveSignature_Days<-ifelse(contractMaster$ExecutiveSignature_Days<0,0,contractMaster$ExecutiveSignature_Days)

## Create a variable to segment data into quarters.
contractMaster$Qtr<-as.yearqtr(contractMaster$SignDate,format="%m/%d/%Y")
Closedcontracts<-subset(Closedcontracts,Qtr>"2012 Q2")

## Plot days in stage for executed contracts
Stages<-melt(select(Closedcontracts,PO,Qtr,LegalReview_Days:ExecutiveSignature_Days),id.var=c("PO","Qtr"),variable.name="Stage",value.name="Days_in_Stage")
Stages$Days_in_Stage<-as.numeric(Stages$Days_in_Stage)
Stages<- aggregate(Days_in_Stage ~ Qtr + Stage, data = Stages, mean)
Stage_plot<-ggplot(Stages,aes(x=factor(Qtr),y=Days_in_Stage,group=1))
Stage_plot<-Stage_plot+geom_line(stat="identity",fill="purple",size=0.6)
Stage_plot<-Stage_plot+facet_grid(facets=.~Stage)
Stage_plot<-Stage_plot+ggtitle("Average Days per Stage for Executed Contracts")
Stage_plot<-Stage_plot+xlab("Quarters")
Stage_plot<-Stage_plot+ylab("Days")
Stage_plot<-Stage_plot+theme(strip.text.x=element_text(size=8))
print(Stage_plot)
ggsave("./Output/Closed Contracts by Stage.png")

## Plot Days to Execute
Days2Execute<-ddply(Closedcontracts,"Qtr",summarise,Sign=mean(Days_to_Execute))
Execution<-ggplot(Days2Execute,aes(x=factor(Qtr),y=Sign))
Execution<-Execution+geom_bar(stat="identity",fill="steelblue")
Execution<-Execution+ggtitle("Days to Execute by Quarter")
Execution<-Execution+xlab("Quarters")
Execution<-Execution+ylab("Days")
Execution<-Execution+geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Execution<-Execution+geom_hline(aes(yintercept=30),colour="#FF0000",linetype=2,size=1.2)
print(Execution)
ggsave("./Output/Days to Execute.png")

## Plot days to execute broken down by process
Execute_Process<-aggregate(Days_to_Execute~Qtr+Group,data=Closedcontracts,mean)
ExecuteProcess_Plot<-ggplot(Execute_Process,aes(x=factor(Qtr),y=Days_to_Execute,group=Group,color=factor(Group)))
ExecuteProcess_Plot<-ExecuteProcess_Plot+geom_line(stat="identity",size=1.25)
ExecuteProcess_Plot<-ExecuteProcess_Plot+text(c(2,2),c(37,35),labels=c("Manual","Ordinance","Time Only","Aviation","CEA","PSA","Grant","Bid"))
print(ExecuteProcess_Plot)

## Plot days to execute broken down by process
Execute_Type<-aggregate(Days_to_Execute~Qtr+Type2,data=Closedcontracts,mean)
ExecuteType_Plot<-ggplot(Execute_Type,aes(x=factor(Qtr),y=Days_to_Execute,group=Type2,color=factor(Type2)))
ExecuteType_Plot<-ExecuteType_Plot+geom_line(stat="identity",size=1.25)
print(ExecuteType_Plot)

## Subset for contracts requiring ordinance and Plot 
OrdinanceSign<-subset(Closedcontracts,Ordinance=="Yes")
OrdExecute<-ddply(OrdinanceSign,"Qtr",summarise,OrdSign=mean(Days_to_Execute))
OrdExecution<-ggplot(OrdExecute,aes(x=factor(Qtr),y=OrdSign))
OrdExecution<-OrdExecution+geom_bar(stat="identity",fill="steelblue")
OrdExecution<-OrdExecution+ggtitle("Days to Execute by Quarter")
OrdExecution<-OrdExecution+xlab("Quarters")
OrdExecution<-OrdExecution+ylab("Days")
OrdExecution<-OrdExecution+geom_text(aes(y=OrdSign,ymax=OrdSign+10,label=round(OrdSign,1)),position=position_dodge(width=0.9),vjust=-.5)
print(OrdExecution)
ggsave("./Output/Ordinance Execute.png")



## Generate spreadsheets of the underlying output data
write.xlsx2(contractMaster,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract Approval Master.xlsx",showNA=FALSE)
write.csv(Closedcontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Closed Contracts.csv")
write.csv(Opencontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Open Contracts.csv")
write.csv(LawExec,"O:/Projects/ReqtoCheckStat/Query Files/Output/LawExec.csv")
write.csv(Adjustments,"O:/Projects/ReqtoCheckStat/Query Files/Output/Adjust.csv")
write.csv(OrdinanceSign,"O:/Projects/ReqtoCheckStat/Query Files/Output/OrdSign.csv")
write.csv(contracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contracts.csv")
write.csv(contractpipeline,"O:/Projects/ReqtoCheckStat/Query Files/Output/contractpipeline.csv")

