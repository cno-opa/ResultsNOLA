


## Read needed files
contractPOapproval<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Approval Sequence POs.csv",skip=3)
                    ,AltID=ALTERNATE_ID,PO=PO_NBR,ContractDate=PO_DATE,LegalReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,Approver=PO_APPROVER,Stage=ORDER_SEQUENCE) ## Report pulled from ECMS
contractPOstatus<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract PO Status.csv",skip=3)
                    ,PO=PO_NBR3,Req=REQ_NBR3,AltID=ALTERNATE_ID3,ContractDate=PO_DATE3,Description=SHORT_DESC3,Dept=DESC_TEXT3,Vendor=NAME3,Requestor=REQUESTOR_ID3,Purchaser=PURCHASER3,POStatus=Status3,ReqComplete=Max_Date3) ## Report pulled from ECMS
contractReqstatus<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Req Status.csv",skip=3)
                    ,Req=REQ_NBR,Description=SHORT_DESC,ReqStatus=STATUS) ## Report pulled from ECMS
contractReqapproval<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Contract Approval Sequence Reqs.csv",skip=3)
                    ,Req=REQ_NBR,ReqApprove=APPROVAL_DATE,ReqApprover=REQ_APPROVER)##Report pulled from ECMS
LawExec<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law and Executive Counsel Log.csv",na.strings="")
                          ,PO=PO.Number,AltID=K.Number,BackFromVendor=Date.Received.by.Law,DownForSignature=Date.Received.by.EX,AdjustedSignDate=Date.Signed.by.MAY) ## List compiled by Law and Executive Counsel
Adjustments<-read.xlsx2("O:/Projects/ReqtoCheckSTAT/Query Files/Adjustments.xlsx",sheetIndex=1,colClasses=c("character","character","Date","Date","numeric","Date","character","Date","character"))

## Read in, and merge Law Master files
LawMaster<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master_hist.csv",strip.white=TRUE),PO=PO.Number,AltID=K.Number,Admin_ContractDate=Date.In.Law,Dept=Department,Vendor,Govt,Type=Type.of.K,LawStatus=Status) ##Lists compiled by Law
Law2<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master.csv",strip.white=TRUE),PO=PO.Number,AltID=K.Number,Admin_ContractDate=Date.In.Law,Dept=Department,Vendor,Govt=Govtal.Entity,Type=Type.of.K,LawStatus=Cancelled)
LawMaster<-rbind(LawMaster,Law2)

## Filter out unnecessary rows from Law-Exec log, and re-code invalid dates to 2015.
LawExec<-LawExec[!is.na(LawExec$PO),]

## Code other Law and Exec columns 
LawExec$AdjustedSignDate<-as.Date(LawExec$AdjustedSignDate,"%m/%d/%Y")
LawExec$BackFromVendor<-as.Date(LawExec$BackFromVendor,"%m/%d/%Y")
LawExec$DownForSignature<-as.Date(LawExec$DownForSignature,"%m/%d/%Y")
LawMaster$Admin_ContractDate<-as.Date(LawMaster$Admin_ContractDate,"%m/%d/%Y")

##Clean Alternate ID number column to match ECMS formatting conventions of "k##-###"/"mk##-###"
Adjustments$AltID<-tolower(Adjustments$AltID)
Adjustments$AltID<-str_trim(Adjustments$AltID,side="both")
Adjustments$PO<-str_trim(Adjustments$PO,side="both")
#Adjustments$AltID<-ifelse(grepl("^\\d",Adjustments$AltID),paste("k",Adjustments$AltID,sep=""),paste(Adjustments$AltID)) ## Just added 9-8 to reconcile adjustments and Law Master contracts
#Adjustments$AltID<-ifelse(startsWith(Adjustments$AltID,"m"),paste(substring(Adjustments$AltID,1,1),"k",substring(Adjustments$AltID,2,8),sep=""),Adjustments$AltID) ## Just added 9-8 to reconcile adjustments and Law Master contracts

LawExec$AltID<-tolower(LawExec$AltID)
LawExec$AltID<-str_trim(LawExec$AltID,side="both")
LawExec$PO<-str_trim(LawExec$PO,side="both")
LawExec$AltID<-ifelse(grepl("^\\d", LawExec$AltID),paste("k",LawExec$AltID,sep=""),paste(LawExec$AltID))
LawExec$AltID<-ifelse(startsWith(LawExec$AltID,"m"),paste(substring(LawExec$AltID,1,1),"k",substring(LawExec$AltID,2,8),sep=""),LawExec$AltID)

contractPOstatus$AltID<-str_trim(contractPOstatus$AltID,side="both")
contractPOstatus$AltID<-tolower(contractPOstatus$AltID)

contractPOapproval$AltID<-tolower(contractPOapproval$AltID)
contractPOapproval$AltID<-str_trim(contractPOapproval$AltID,side="both")

LawMaster$AltID<-tolower(LawMaster$AltID)
LawMaster$AltID<-str_trim(LawMaster$AltID,side="both")
LawMaster$AltID<-ifelse(grepl("^\\d",LawMaster$AltID),paste("k",LawMaster$AltID,sep=""),paste(LawMaster$AltID))
LawMaster$AltID<-ifelse(startsWith(LawMaster$AltID,"m"),paste(substring(LawMaster$AltID,1,1),"k",substring(LawMaster$AltID,2,8),sep=""),LawMaster$AltID)

## Remove duplicated contracts from ECMS contractPOstatus report
contractPOstatus<-contractPOstatus[!duplicated(contractPOstatus),]

#LawExec<-select(LawExec,-PO)
Adjustments<-select(Adjustments,-POStatus,-Closed,-Ordinance)
Adjustments<-merge(LawExec,Adjustments,by=c("AltID"),all=TRUE)
Adjustments$PO<-ifelse(!is.na(Adjustments$PO.x),as.character(Adjustments$PO.x,"%m/%d/%Y"),as.character(Adjustments$PO.y,"%m/%d/%Y"))
Adjustments$BackFromVendor<-ifelse(!is.na(Adjustments$BackFromVendor.x),as.Date(Adjustments$BackFromVendor.x,"%m/%d/%Y"),as.Date(Adjustments$BackFromVendor.y,"%m/%d/%Y"));class(Adjustments$BackFromVendor)<-"Date"
Adjustments$AdjustedSignDate<-ifelse(!is.na(Adjustments$AdjustedSignDate.x),as.Date(Adjustments$AdjustedSignDate.x,"%m/%d/%Y"),as.Date(Adjustments$AdjustedSignDate.y,"%m/%d/%Y"));class(Adjustments$AdjustedSignDate)<-"Date"
Adjustments<-select(Adjustments,PO,AltID,OrdinanceDate,BackFromVendor,DownForSignature,AdjustedSignDate,CancelDate)

# Code blanks in ID columns to NA in Law Master list
LawMaster$PO[LawMaster$PO==""]<-NA
LawMaster$AltID[LawMaster$AltID==""]<-NA
LawMaster$Govt[LawMaster$Govt==""]<-NA
LawMaster$Type[LawMaster$Type==""]<-NA
LawMaster$LawStatus[LawMaster$LawStatus==""]<-NA

##Code blanks in ID columns to NA in Law Master list
Adjustments$PO[Adjustments$PO==""]<-NA
Adjustments$AltID[Adjustments$AltID==""]<-NA

## Re-Code Order Sequence to appropriate appproval stage category
contractPOapproval$Stage<-ifelse(contractPOapproval$Stage==1,"DepAttorney",ifelse(contractPOapproval$Stage==2,"CAO",ifelse(contractPOapproval$Stage==3,"SentVendor",ifelse(contractPOapproval$Stage==4,"ECMS_BackFromVendor",ifelse(contractPOapproval$Stage==5,"ExecutiveSignature","Error")))))
contractPOapproval$Stage<-ifelse(contractPOapproval$Stage=="Error" & contractPOapproval$Approver=="CENBECK" & contractPOapproval$Stage==4 |contractPOapproval$Stage=="Error" & contractPOapproval$Approver=="CNSFOSTER" & contractPOapproval$Stage==4 |contractPOapproval$Stage=="Error" & contractPOapproval$Approver=="CRFCORTIZAS" & contractPOapproval$Stage==4 |contractPOapproval$Stage=="Error" & contractPOapproval$Approver=="CSTSEPCICH" & contractPOapproval$Stage==4 |contractPOapproval$Stage=="Error" & contractPOapproval$Approver=="CVCHONORE" & contractPOapproval$Stage==4 ,"ExecutiveSignature",contractPOapproval$Stage)

# Convert relevant columns of ECMS contract PO reports to dates
contractPOapproval$ApprovalDate<-as.Date(contractPOapproval$ApprovalDate,"%m/%d/%Y")
contractPOapproval$LegalReview<-as.Date(contractPOapproval$LegalReview,"%m/%d/%Y")
contractPOapproval$ContractDate<-as.Date(contractPOapproval$ContractDate,"%m/%d/%Y")
contractPOstatus$ContractDate<-as.Date(contractPOstatus$ContractDate,"%m/%d/%Y")
contractPOstatus$ReqComplete<-as.Date(contractPOstatus$ReqComplete,"%m/%d/%Y")

# 
contractPOapproval<-filter(contractPOapproval,!is.na(ApprovalDate))
contractPOapproval<-select(contractPOapproval,-Approver)

## Pivot ECMS approvals into separate columns by stage
contractPOapproval<-spread(contractPOapproval,Stage,ApprovalDate)

## Select desired columns
contractPOapproval<-select(contractPOapproval,PO,AltID,PO:LegalReview,DepAttorney,CAO,SentVendor,ECMS_BackFromVendor,ExecutiveSignature,Error)

## Convert stage column approvals to date
contractPOapproval[ , 5:10] <- lapply(contractPOapproval[ , 5:10], as.Date)


## Merge ECMS system files into consolidated contract list; 
contracts<-merge(contractPOapproval,contractPOstatus,by="PO",all=TRUE)
      contracts$AltID<-ifelse(!is.na(contracts$AltID.x),as.character(contracts$AltID.x,"%m/%d/%Y"),as.character(contracts$AltID.y,"%m/%d/%Y"))   
      contracts$ContractDate<-ifelse(!is.na(contracts$ContractDate.x),as.Date(contracts$ContractDate.x,"%m/%d/%Y"),as.Date(contracts$ContractDate.y,"%m/%d/%Y"));class(contracts$ContractDate)<-"Date"  
          contracts<-select(contracts,-ContractDate.x,-ContractDate.y,-AltID.x,-AltID.y)
contracts<-merge(contracts,contractReqstatus,by=c("Req"),all=TRUE)
      contracts$Description<-ifelse(!is.na(contracts$Description.x),as.character(contracts$Description.x),as.character(contracts$Description.y))
          contracts<-select(contracts,-Description.x,-Description.y)

## Merge consolidated list with Law master list
Master1<-filter(LawMaster,!is.na(PO))
      contracts<-merge(contracts,Master1,by="PO",all=TRUE)
            contracts$Dept<-ifelse(!is.na(contracts$Dept.x),as.character(contracts$Dept.x),as.character(contracts$Dept.y))
            contracts$Vendor<-ifelse(!is.na(contracts$Vendor.x),as.character(contracts$Vendor.x),as.character(contracts$Vendor.y))
            contracts$AltID<-ifelse(!is.na(contracts$AltID.x),as.character(contracts$AltID.x),as.character(contracts$AltID.y))
                contracts<-select(contracts,-Dept.y,-Vendor.y,-Dept.x,-Vendor.x,-AltID.x,-AltID.y)
Master2<-filter(LawMaster,!is.na(AltID) & is.na(PO))
      contracts<-merge(contracts,Master2,by="AltID",all=TRUE)
            contracts$PO<-ifelse(!is.na(contracts$PO.x),as.character(contracts$PO.x),as.character(contracts$PO.y))
            contracts$Govt<-ifelse(!is.na(contracts$Govt.x),as.character(contracts$Govt.x),as.character(contracts$Govt.y))
            contracts$Type<-ifelse(!is.na(contracts$Type.x),as.character(contracts$Type.x),as.character(contracts$Type.y))
            contracts$LawStatus<-ifelse(!is.na(contracts$LawStatus.x),as.character(contracts$LawStatus.x),as.character(contracts$LawStatus.y))
            contracts$Admin_ContractDate<-ifelse(!is.na(contracts$Admin_ContractDate.x),as.Date(contracts$Admin_ContractDate.x,"%m/%d/%Y"),as.Date(contracts$Admin_ContractDate.y,"%m/%d/%Y"));class(contracts$Admin_ContractDate)<-"Date"
            contracts$LawStatus<-ifelse(!is.na(contracts$LawStatus.x),as.character(contracts$LawStatus.x),as.character(contracts$LawStatus.y)) 
            contracts$Dept<-ifelse(!is.na(contracts$Dept.x),as.character(contracts$Dept.x),as.character(contracts$Dept.y))
            contracts$Vendor<-ifelse(!is.na(contracts$Vendor.x),as.character(contracts$Vendor.x),as.character(contracts$Vendor.y))
contracts<-select(contracts,-Govt.x,-Govt.y,-Type.x,-Type.y,-Admin_ContractDate.y,-Admin_ContractDate.x,-LawStatus.y,-LawStatus.x,-Dept.x,-Dept.y,-Vendor.y,-Vendor.x,-PO.x,-PO.y)

## Merge with adjustment dataset
Adjust1<-filter(Adjustments,!is.na(PO))
               contracts<-merge(contracts,Adjust1,by="PO",all=TRUE)
              contracts$AltID<-ifelse(!is.na(contracts$AltID.x),as.character(contracts$AltID.x),as.character(contracts$AltID.y))
                    contracts<-select(contracts,-AltID.x,-AltID.y)
Adjust2<-filter(Adjustments,!is.na(AltID) & is.na(PO))
      contracts<-merge(contracts,Adjust2,by="AltID",all=TRUE)
          contracts$PO<-ifelse(!is.na(contracts$PO.x),as.character(contracts$PO.x),as.character(contracts$PO.y))          
          #contracts$Closed<-ifelse(!is.na(contracts$Closed.x),as.character(contracts$Closed.x),as.character(contracts$Closed.y))
          #contracts$Ordinance<-ifelse(!is.na(contracts$Ordinance.x),as.character(contracts$Ordinance.x),as.character(contracts$Ordinance.y))
          contracts$OrdinanceDate<-ifelse(!is.na(contracts$OrdinanceDate.x),as.Date(contracts$OrdinanceDate.x,"%m/$d/%Y"),as.Date(contracts$OrdinanceDate.y,"%m/$d/%Y"));class(contracts$OrdinanceDate)<-"Date"
          contracts$BackFromVendor<-ifelse(!is.na(contracts$BackFromVendor.x),as.Date(contracts$BackFromVendor.x,"%m/$d/%Y"),as.Date(contracts$BackFromVendor.y,"%m/$d/%Y"));class(contracts$BackFromVendor)<-"Date"
          contracts$DownForSignature<-ifelse(!is.na(contracts$DownForSignature.x),as.Date(contracts$DownForSignature.x,"%m/$d/%Y"),as.Date(contracts$DownForSignature.y,"%m/$d/%Y"));class(contracts$DownForSignature)<-"Date"
          contracts$AdjustedSignDate<-ifelse(!is.na(contracts$AdjustedSignDate.x),as.Date(contracts$AdjustedSignDate.x,"%m/$d/%Y"),as.Date(contracts$AdjustedSignDate.y,"%m/$d/%Y"));class(contracts$AdjustedSignDate)<-"Date"
          contracts$CancelDate<-ifelse(!is.na(contracts$CancelDate.x),as.Date(contracts$CancelDate.x,"%m/$d/%Y"),as.Date(contracts$CancelDate.y,"%m/$d/%Y"));class(contracts$CancelDate)<-"Date"
contracts<-select(contracts,-OrdinanceDate.x,-OrdinanceDate.y,-BackFromVendor.x,-BackFromVendor.y,-DownForSignature.x,-DownForSignature.y,-AdjustedSignDate.x,-AdjustedSignDate.y,-CancelDate.x,-CancelDate.y,-PO.x,-PO.y)

## Filter out contracts that haven't made it to Law yet
contracts<-contracts[contracts$ReqStatus=="Gone to PO"|contracts$ReqStatus=="Ready for Purchasing"|is.na(contracts$ReqStatus),]

## Create date reconciliation columns
contracts$CAO_Ord<-ifelse(is.na(contracts$OrdinanceDate),as.Date(contracts$CAO,format="%m/%d/%Y"),as.Date(contracts$OrdinanceDate,format="%m/%d/%Y")); class(contracts$CAO_Ord)<-"Date"
contracts$VendorReconciled<-ifelse(is.na(contracts$BackFromVendor),as.Date(contracts$ECMS_BackFromVendor,format="%m/%d/%Y"),as.Date(contracts$BackFromVendor,format="%m/%d/%Y")); class(contracts$VendorReconciled)<-"Date"
contracts$ReadyforExec<-ifelse(is.na(contracts$DownForSignature),as.Date(contracts$VendorReconciled,format="%m/%d/%Y"),as.Date(contracts$DownForSignature,format="%m/%d/%Y")); class(contracts$ReadyforExec)<-"Date"
contracts$SignDate<-ifelse(is.na(contracts$AdjustedSignDate),as.Date(contracts$ExecutiveSignature,format="%m/%d/%Y"),as.Date(contracts$AdjustedSignDate,format="%m/%d/%Y")); class(contracts$SignDate)<-"Date"

## Select and arrange desired columns in dataset
contracts<-select(contracts,PO,Req,AltID,Dept:Vendor,Description:LawStatus,POStatus,ReqStatus,Requestor:Purchaser,Error
                  ,ReqComplete,Admin_ContractDate,ContractDate,LegalReview:CAO,OrdinanceDate,SentVendor,BackFromVendor,
                  ECMS_BackFromVendor,DownForSignature,AdjustedSignDate,ExecutiveSignature,CancelDate:SignDate)

# Clean contract type language to prepare for re-coding
contracts$Type<-tolower(contracts$Type)
contracts$Type<-gsub("[[:punct:]]","",contracts$Type)

# Re-code type
contracts$Type2<-ifelse(grepl("cea",contracts$Type),"CEA",
                        ifelse(grepl("psa under 15k",contracts$Type),"PSA Under $15k",
                               ifelse(grepl("psa",contracts$Type),"PSA Over $15k",
                                      ifelse(grepl("grant",contracts$Type),"Grant",
                                             ifelse(grepl("bid",contracts$Type),"Bid","Other")))))

#contracts$Ordinance<-ifelse(is.na(contracts$Ordinance),"Unknown",ifelse(contracts$Ordinance=="Yes","Yes","No"))

## Create variable for the average days from contract creation to execution (Days to Execute)
contracts$Days_to_Execute<-Days(contracts,PO,SignDate)
contracts$Days_to_Execute<-ifelse(contracts$Days_to_Execute<0,0,contracts$Days_to_Execute)

## Calculate days per stage, recalculating appropriate calculations to 0 or NA, where needed.
contracts$LegalReview_Days<-Days(contracts,PO,LegalReview)
contracts$DepAttorney_Days<-Days(contracts,LegalReview,DepAttorney)
contracts$CAO_Days<-Days(contracts,DepAttorney,CAO)
contracts$Ordinance_Days<-strptime(contracts$OrdinanceDate,format="%Y-%m-%d")-strptime(contracts$CAO,format="%Y-%m-%d")
                          contracts$Ordinance_Days<-ifelse(contracts$Ordinance_Days<0,NA,contracts$Ordinance_Days)##Days function doesn't work with this variable
contracts$SentVendor_Days<-Days(contracts,CAO_Ord,SentVendor)
                          contracts$SentVendor_Days<-ifelse(contracts$SentVendor_Days<0,0,contracts$SentVendor_Days)
contracts$DaysWithVendor<-Days(contracts,SentVendor,VendorReconciled)
                          contracts$DaysWithVendor<-ifelse(contracts$DaysWithVendor<0,0,contracts$DaysWithVendor)
contracts$DownForSignature_Days<-Days(contracts,VendorReconciled,DownForSignature)
                          contracts$DownForSignature<-ifelse(contracts$BackFromVendor>"2014-12-31",contracts$BackFromVendor,NA)
contracts$ExecutiveSignature_Days<-Days(contracts,ReadyforExec,SignDate)
                          contracts$ExecutiveSignature_Days<-ifelse(contracts$ExecutiveSignature_Days<0,0,contracts$ExecutiveSignature_Days)

## Remove contracts that have been orphaned by deficient historical data
Exclude<-subset(subset(contracts,ReqStatus=="Gone to PO" & is.na(ContractDate) & is.na(Admin_ContractDate)))
contracts<-anti_join(contracts,Exclude,by="Req") 
  
## Clean days in stage for manual contracts so there is only a "days to execute" calculation for those contracts
contracts$LegalReview_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$LegalReview_Days)
contracts$DepAttorney_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$DepAttorney_Days)
contracts$CAO_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$CAO_Days)
contracts$Ordinance_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$Ordinance_Days)
contracts$SentVendor_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$SentVendor_Days)
contracts$DaysWithVendor<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$DaysWithVendor)
contracts$DownForSignature_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$DownForSignature_Days)
contracts$ExecutiveSignature_Days<-ifelse(startsWith(contracts$AltID,"m"),NA,contracts$ExecutiveSignature_Days)

## Create a variable to segment data into quarters
contracts$Last_Qtr<-as.yearqtr(contracts$SignDate,format="%m/%d/%Y")    
contracts$Last_Qtr2<-ifelse(contracts$Purchaser=="CECMS",as.yearqtr(contracts$ContractDate,format="%Y-%m-%d"),NA);class(contracts$Last_Qtr2)<-"yearqtr"
contracts$Last_Qtr3<-ifelse(!is.na(contracts$CancelDate),as.yearqtr(contracts$CancelDate,format="%Y-%m-%d"),NA);class(contracts$Last_Qtr3)<-"yearqtr"
contracts$Last_Qtr4<-ifelse(contracts$POStatus=="Canceled" & is.na(contracts$CancelDate),as.yearqtr(contracts$ContractDate,format="%Y-%m-%d"),NA);class(contracts$Last_Qtr4)<-"yearqtr"
contracts$Last_Qtr<-ifelse(!is.na(contracts$Last_Qtr),contracts$Last_Qtr,
                                ifelse(!is.na(contracts$Last_Qtr2),contracts$Last_Qtr2,
                                       ifelse(!is.na(contracts$Last_Qtr3),contracts$Last_Qtr3,
                                              ifelse(!is.na(contracts$Last_Qtr4),contracts$Last_Qtr4,NA))));class(contracts$Last_Qtr)<-"yearqtr"
contracts$Last_Qtr<-ifelse(is.na(contracts$Last_Qtr) & contracts$POStatus=="Sent",as.yearqtr(contracts$ContractDate,format="%Y-%m-%d"),contracts$Last_Qtr);class(contracts$Last_Qtr)<-"yearqtr"

contracts<-select(contracts,-Last_Qtr2,-Last_Qtr3,-Last_Qtr4)
contracts$First_Qtr<-as.yearqtr(contracts$ReqComplete,format="%Y-%m-%d") ## Find quarter that a contract was created
contracts$Qtr_Start<-as.Date(as.yearqtr(contracts$First_Qtr,format="%Y-%m-%d")) ##Find start date of quarter that a contract was created in ECMS.
contracts$Qtr_End<-as.Date(as.yearqtr(contracts$Last_Qtr), frac = 1 )  ## Find end date of quarter that a contract was signed, if applicable
contracts$Qtr_End<-ifelse(!is.na(contracts$CancelDate),as.Date( as.yearqtr(contracts$CancelDate), frac = 1 ),contracts$Qtr_End);class(contracts$Qtr_End)<-"Date"
