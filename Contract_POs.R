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
                          ,PO=PO.Number,AltID=K.Number,BackFromVendor=Date.Received.by.Law,DownForSignature=Date.Received.by.EX,AdjustedSignDate=Date.Signed.by.MAY,Vendor,Dept=Description) ## List compiled by Law and Executive Counsel
Adjustments<-read.xlsx2("O:/Projects/ReqtoCheckSTAT/Query Files/Adjustments.xlsx",sheetIndex=1,colClasses=c("character","character","Date","Date","numeric","Date","character","Date","character",na.strings=""))

## Read in, and merge Law Master files
LawMaster<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master_hist.csv",strip.white=TRUE),PO=PO.Number,AltID=K.Number,Admin_ContractDate=Date.In.Law,Dept=Department,Vendor,Govt,Type=Type.of.K,LawStatus=Status) ##Lists compiled by Law
Law2<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Law Master.csv",strip.white=TRUE),PO=PO.Number,AltID=K.Number,Admin_ContractDate=Date.In.Law,Dept=Department,Vendor,Govt=Govtal.Entity,Type=Type.of.K,LawStatus=Cancelled)
LawMaster<-rbind(LawMaster,Law2)

## Filter out unnecessary rows from Law-Exec log, and re-code invalid dates to 2015.
LawExec<-LawExec[!is.na(LawExec$PO),]

## Code othe relevant Law and Exec columns to dates
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

## Clean up the alternate ID variable in the Law-Executive Counsel log 
LawExec$AltID<-tolower(LawExec$AltID)
LawExec$AltID<-str_trim(LawExec$AltID,side="both")
LawExec$PO<-str_trim(LawExec$PO,side="both")
LawExec$AltID<-ifelse(grepl("^\\d", LawExec$AltID),paste("k",LawExec$AltID,sep=""),paste(LawExec$AltID))
LawExec$AltID<-ifelse(startsWith(LawExec$AltID,"m"),paste(substring(LawExec$AltID,1,1),"k",substring(LawExec$AltID,2,8),sep=""),LawExec$AltID)

## Clean up the alternate ID variable in the ECMS contract PO status report
contractPOstatus$AltID<-str_trim(contractPOstatus$AltID,side="both")
contractPOstatus$AltID<-tolower(contractPOstatus$AltID)

## Clean up the alternate ID variable in the ECMS contract PO approval report
contractPOapproval$AltID<-tolower(contractPOapproval$AltID)
contractPOapproval$AltID<-str_trim(contractPOapproval$AltID,side="both")

## Clean up the alternate ID variable in the Law Master list
LawMaster$AltID<-tolower(LawMaster$AltID)
LawMaster$AltID<-str_trim(LawMaster$AltID,side="both")
LawMaster$AltID<-ifelse(grepl("^\\d",LawMaster$AltID),paste("k",LawMaster$AltID,sep=""),paste(LawMaster$AltID))
LawMaster$AltID<-ifelse(startsWith(LawMaster$AltID,"m"),paste(substring(LawMaster$AltID,1,1),"k",substring(LawMaster$AltID,2,8),sep=""),LawMaster$AltID)

## Remove duplicated contracts from ECMS contractPOstatus report
contractPOstatus<-contractPOstatus[!duplicated(contractPOstatus),]

#LawExec<-select(LawExec,-PO)
Adjustments<-select(Adjustments,-POStatus,-Closed,Ordinance)
Adjustments<-merge(Adjustments,LawExec,by=c("AltID"),all=TRUE)
Adjustments$PO<-ifelse(!is.na(Adjustments$PO.x),as.character(Adjustments$PO.x,"%m/%d/%Y"),as.character(Adjustments$PO.y,"%m/%d/%Y"))
Adjustments$BackFromVendor<-ifelse(!is.na(Adjustments$BackFromVendor.x),as.Date(Adjustments$BackFromVendor.x,"%m/%d/%Y"),as.Date(Adjustments$BackFromVendor.y,"%m/%d/%Y"));class(Adjustments$BackFromVendor)<-"Date"
Adjustments$AdjustedSignDate<-ifelse(!is.na(Adjustments$AdjustedSignDate.x),as.Date(Adjustments$AdjustedSignDate.x,"%m/%d/%Y"),as.Date(Adjustments$AdjustedSignDate.y,"%m/%d/%Y"));class(Adjustments$AdjustedSignDate)<-"Date"
Adjustments<-select(Adjustments,PO,AltID,Vendor,Dept,Ordinance,OrdinanceDate,BackFromVendor,DownForSignature,AdjustedSignDate,CancelDate)

# Code blanks in ID columns to NA in Law Master list
LawMaster$Govt[LawMaster$Govt==""]<-NA
LawMaster$Type[LawMaster$Type==""]<-NA
LawMaster$LawStatus[LawMaster$LawStatus==""]<-NA

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
                contracts$PO<-as.character(contracts$PO)

## Merge consolidated contract list with requisition status list to capture contracts that haven't made it to contract date yet.                  
contracts<-merge(contracts,contractReqstatus,by=c("Req"),all=TRUE)
              contracts$Description<-ifelse(!is.na(contracts$Description.x),as.character(contracts$Description.x),as.character(contracts$Description.y))
                  contracts<-select(contracts,-Description.x,-Description.y)
              
## Merge consolidated list with Law master list
contracts<-merge(contracts,LawMaster,by="PO",all=TRUE)
            contracts$Dept<-ifelse(!is.na(contracts$Dept.x),as.character(contracts$Dept.x),as.character(contracts$Dept.y))
            contracts$Vendor<-ifelse(!is.na(contracts$Vendor.x),as.character(contracts$Vendor.x),as.character(contracts$Vendor.y))
            contracts$AltID<-ifelse(!is.na(contracts$AltID.x),as.character(contracts$AltID.x),as.character(contracts$AltID.y))
                contracts<-select(contracts,-Dept.y,-Vendor.y,-Dept.x,-Vendor.x,-AltID.x,-AltID.y)

contracts<-merge(contracts,Adjustments,by="PO",all=TRUE)
#Adjust1<-filter(Adjustments,!is.na(PO))
            #   contracts<-merge(contracts,Adjust1,by="PO",all=TRUE)
                  contracts$AltID<-ifelse(!is.na(contracts$AltID.x),as.character(contracts$AltID.x),as.character(contracts$AltID.y))
                  contracts$Vendor<-ifelse(!is.na(contracts$Vendor.x),as.character(contracts$Vendor.x),as.character(contracts$Vendor.y))
                  contracts$Dept<-ifelse(!is.na(contracts$Dept.x),as.character(contracts$Dept.x),as.character(contracts$Dept.y))
                      contracts<-select(contracts,-AltID.x,-AltID.y,-Vendor.x,-Vendor.y,-Dept.x,-Dept.y)

## Filter out contracts that haven't made it to Law yet
contracts<-contracts[contracts$ReqStatus=="Gone to PO"|contracts$ReqStatus=="Ready for Purchasing"|is.na(contracts$ReqStatus),]

## Create date reconciliation columns
contracts$ContractDate<-ifelse(is.na(contracts$ContractDate),as.Date(contracts$Admin_ContractDate,format="%m/%d/%Y"),as.Date(contracts$ContractDate,format="%m/%d/%Y")); class(contracts$ContractDate)<-"Date"
contracts$CAO_Ord<-ifelse(is.na(contracts$OrdinanceDate),as.Date(contracts$CAO,format="%m/%d/%Y"),as.Date(contracts$OrdinanceDate,format="%m/%d/%Y")); class(contracts$CAO_Ord)<-"Date"
contracts$VendorReconciled<-ifelse(is.na(contracts$BackFromVendor),as.Date(contracts$ECMS_BackFromVendor,format="%m/%d/%Y"),as.Date(contracts$BackFromVendor,format="%m/%d/%Y")); class(contracts$VendorReconciled)<-"Date"
contracts$ReadyforExec<-ifelse(is.na(contracts$DownForSignature),as.Date(contracts$VendorReconciled,format="%m/%d/%Y"),as.Date(contracts$DownForSignature,format="%m/%d/%Y")); class(contracts$ReadyforExec)<-"Date"
contracts$SignDate<-ifelse(is.na(contracts$AdjustedSignDate),as.Date(contracts$ExecutiveSignature,format="%m/%d/%Y"),as.Date(contracts$AdjustedSignDate,format="%m/%d/%Y")); class(contracts$SignDate)<-"Date"

## Select and arrange desired columns in dataset
contracts<-select(contracts,PO,Req,AltID,Dept:Vendor,Description:LawStatus,POStatus,ReqStatus,Requestor:Purchaser,Error
                  ,ReqComplete,ContractDate,LegalReview:CAO,Ordinance,OrdinanceDate,SentVendor,BackFromVendor,
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

## Create variable for the average days from contract creation to execution (Days to Execute)
contracts$Days_to_Execute<-Days(contracts,ContractDate,SignDate)
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
Exclude1<-subset(contracts,ReqStatus=="Gone to PO" & is.na(ContractDate) & is.na(Admin_ContractDate))
Exclude2<-subset(contracts,ReqStatus=="" & is.na(ContractDate) & is.na(Admin_ContractDate))
Exclude<-rbind(Exclude1,Exclude2)
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

## Create variable for quarter that a contract was closed
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

## Create variable for quarter that a contract was opened
contracts$First_Qtr<-as.yearqtr(contracts$ReqComplete,format="%Y-%m-%d") ## Find quarter that a contract was created
      contracts$First_Qtr<-ifelse(is.na(contracts$First_Qtr),as.yearqtr(contracts$ContractDate,format="%Y-%m-%d"),contracts$First_Qtr);class(contracts$First_Qtr)<-"yearqtr"

## Remove contracts that a start date cannot be verified
contracts<-subset(contracts,!is.na(First_Qtr))

## Calculate first date of first quarter and last date of last quarter for each contract where applicable (NOT SURE IF NEEDED YET)
contracts$Qtr_Start<-as.Date(as.yearqtr(contracts$First_Qtr,format="%Y-%m-%d")) ##Find start date of quarter that a contract was created in ECMS.
contracts$Qtr_End<-as.Date(as.yearqtr(contracts$Last_Qtr), frac = 1 )  ## Find end date of quarter that a contract was signed, if applicable
contracts$Qtr_End<-ifelse(!is.na(contracts$CancelDate),as.Date( as.yearqtr(contracts$CancelDate), frac = 1 ),contracts$Qtr_End);class(contracts$Qtr_End)<-"Date"

## Create plot of the contracts opened, closed, and open at the end of each quarter
Qtr_First_summary<-ddply(contracts,"First_Qtr",summarise,n=n())
Qtr_End_summary<-ddply(contracts,"Last_Qtr",summarise,n=n())
Qtr_First_summary<-rename(Qtr_First_summary,Qtr=First_Qtr,Opened=n)
Qtr_End_summary<-rename(Qtr_End_summary,Qtr=Last_Qtr,Closed=n)
summary_merge<-merge(Qtr_First_summary,Qtr_End_summary,by="Qtr",all=TRUE)
  summary_merge$cumulative_opened<-cumsum(summary_merge$Opened)
  summary_merge$cumulative_closed<-cumsum(summary_merge$Closed)
  summary_merge$Open_EndofQtr<-summary_merge$cumulative_opened-summary_merge$cumulative_closed
summary_melt<-select(summary_merge,Qtr,Opened,Closed,Open_EndofQtr)
summary_melt<-subset(summary_melt,Qtr>"2012 Q4")
summary_melt<-melt(summary_melt,id.vars="Qtr")
Open_closedplot<-ggplot(summary_melt,aes(x=factor(Qtr),y=value,fill=variable))+
  geom_bar(subset=.(variable=="Opened"|variable=="Closed"),width=0.8,aes(fill=variable),position="dodge",stat="identity")+
  ggtitle("Contracts Opened,Closed, and in Queue by Quarter")+xlab("Quarters")+ylab("Number of Contracts")+
  geom_line(subset=.(variable=="Open_EndofQtr"),aes(fill=variable,group=variable),size=1)+
  geom_text(aes(y=value,ymax=value,label=value),position=position_dodge(width=0.7),size=4)
print(Open_closedplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Opened_Closed_In Queue_Contracts.png")

## Subset contract list into separate lists of contracts that have been closed and those currently open
Opencontracts<-subset(contracts,is.na(Last_Qtr))
Closedcontracts<-subset(contracts,!is.na(Last_Qtr))

## Remove days-in-stage calculation columns, as they only apply to closed contracts
Opencontracts<-select(Opencontracts,-ends_with("Days"),-starts_with("Days"))

## Calculate average age of contracts in queue, from contract date to end of reporting period
r_period<-max(contracts$Qtr_End[!is.na(contracts$Qtr_End)])
Opencontracts$TotalAge<-Age(Opencontracts,ContractDate,r_period)

##Calculate total age, as well as days in each stage except for Exec Counsel for open contracts
Opencontracts$AttorneyReview_Age<-ifelse(is.na(Opencontracts$LegalReview) & !is.na(Opencontracts$ContractDate),Days(Opencontracts,ContractDate,r_period),NA)
Opencontracts$DepAttorney_Age<-ifelse(is.na(Opencontracts$DepAttorney) & !is.na(Opencontracts$LegalReview),Days(Opencontracts,LegalReview,r_period),NA)
Opencontracts$CAO_Age<-ifelse(is.na(Opencontracts$CAO) & !is.na(Opencontracts$DepAttorney),Days(Opencontracts,DepAttorney,r_period),NA)
Opencontracts$OpenOrd<-ifelse(Opencontracts$Ordinance=="Yes","Yes",NA)
              Opencontracts$Ordinance_Age<-ifelse(!is.na(Opencontracts$OpenOrd) & is.na(Opencontracts$OrdinanceDate),strptime(r_period,format="%Y-%m-%d")-strptime(Opencontracts$CAO,format="%Y-%m-%d"),NA)
Opencontracts$SendVendor_Age<-ifelse(is.na(Opencontracts$SentVendor) & !is.na(Opencontracts$CAO_Ord) & is.na(Opencontracts$OpenOrd),strptime(r_period,format="%Y-%m-%d")-strptime(Opencontracts$CAO_Ord,format="%Y-%m-%d"),NA);Opencontracts<-select(Opencontracts,-OpenOrd)
Opencontracts$AwaitingVendor_Age<-ifelse(is.na(Opencontracts$BackFromVendor) & !is.na(Opencontracts$SentVendor),strptime(r_period,format="%Y-%m-%d")-strptime(Opencontracts$SentVendor,format="%Y-%m-%d"),NA)
Opencontracts$BringToExec_Age<-ifelse(is.na(Opencontracts$DownForSignature) & Opencontracts$BackFromVendor>as.Date("2014-12-31","%Y-%m-%d"),Days(Opencontracts,VendorReconciled,r_period),NA)

## Determine which contracts are with Exec Counsel and calculate age
Opencontracts$ExecutiveSignature_Age<-ifelse(is.na(Opencontracts$ExecutiveSignature),Days(Opencontracts,DownForSignature,r_period),NA)

## Plot days in stage for executed contracts
Stages<-subset(Closedcontracts,Last_Qtr>"2012 Q4")
Stages<-melt(select(Stages,PO,Last_Qtr,LegalReview_Days:ExecutiveSignature_Days),id.var=c("PO","Last_Qtr"),variable.name="Stage",value.name="Days_in_Stage")
Stages$Days_in_Stage<-as.numeric(Stages$Days_in_Stage)
Stages<- aggregate(Days_in_Stage ~ Last_Qtr + Stage, data = Stages, mean)
levels(Stages$Stage)[levels(Stages$Stage)=="LegalReview_Days"]<-"Legal Review"
levels(Stages$Stage)[levels(Stages$Stage)=="DepAttorney_Days"]<-"Dep. City Attorney"
levels(Stages$Stage)[levels(Stages$Stage)=="CAO_Days"]<-"CAO"
levels(Stages$Stage)[levels(Stages$Stage)=="Ordinance_Days"]<-"Ordinance*"
levels(Stages$Stage)[levels(Stages$Stage)=="SentVendor_Days"]<-"Sent Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="DaysWithVendor"]<-"Days with Vendor"
levels(Stages$Stage)[levels(Stages$Stage)=="DownForSignature_Days"]<-"Down to Exec"
levels(Stages$Stage)[levels(Stages$Stage)=="ExecutiveSignature_Days"]<-"Executed"
Stage_plot<-ggplot(Stages,aes(x=factor(Last_Qtr),y=Days_in_Stage,group=1))
Stage_plot<-Stage_plot+geom_bar(stat="identity",fill=darkBlue,size=0.6)
Stage_plot<-Stage_plot+facet_grid(facets=.~Stage)
Stage_plot<-Stage_plot+ggtitle("Average Days per Stage for Executed Contracts 2013-Present")
Stage_plot<-Stage_plot+xlab("Quarters")
Stage_plot<-Stage_plot+ylab("Days")
Stage_plot<-Stage_plot+theme(strip.text.x=element_text(size=8),axis.text.x=element_blank())
print(Stage_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Closed Contracts by Stage.png")

## Plot Days to Execute
Days2Execute<-ddply(subset(Closedcontracts,!is.na(Days_to_Execute)),"Last_Qtr",summarise,Sign=mean(Days_to_Execute))
Days2Execute<-subset(Days2Execute,Last_Qtr>"2012 Q4")
Execution<-ggplot(Days2Execute,aes(x=factor(Last_Qtr),y=Sign))
Execution<-Execution+geom_bar(stat="identity",fill="steelblue")
Execution<-Execution+ggtitle("Average Days to Execute Contracts by Quarter")
Execution<-Execution+xlab("Quarters")
Execution<-Execution+ylab("Days")
Execution<-Execution+geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Execution<-Execution+geom_hline(aes(yintercept=30),colour="#FF0000",linetype=2,size=1.2)+theme(legend.position="Top")
print(Execution)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Days to Execute.png")

## Create distribution bins for days to execute
Closedcontracts$Under30<-ifelse(Closedcontracts$Days_to_Execute<=30,1,0)
Closedcontracts$Between31_60<-ifelse(Closedcontracts$Days_to_Execute>30 & Closedcontracts$Days_to_Execute<=60,1,0)
Closedcontracts$Between61_90<-ifelse(Closedcontracts$Days_to_Execute>60 & Closedcontracts$Days_to_Execute<=90,1,0)
Closedcontracts$Between91_120<-ifelse(Closedcontracts$Days_to_Execute>90 & Closedcontracts$Days_to_Execute<=120,1,0)
Closedcontracts$Over120<-ifelse(Closedcontracts$Days_to_Execute>120,1,0)

## Plot the distribution percentages of business days to process by quarter
Contract_PO_dist<-select(Closedcontracts,Last_Qtr,Under30,Between31_60,Between61_90,Between91_120,Over120)
Contract_PO_dist<-aggregate(cbind(Closedcontracts$Under30,Closedcontracts$Between31_60,Closedcontracts$Between61_90,Closedcontracts$Between91_120,Closedcontracts$Over120)~Last_Qtr,data=Contract_PO_dist,FUN=sum);colnames(Contract_PO_dist)[grepl("V1", colnames(Contract_PO_dist))] <- "Under30";colnames(Contract_PO_dist)[grepl("V2", colnames(Contract_PO_dist))] <- "Between31_60";colnames(Contract_PO_dist)[grepl("V3", colnames(Contract_PO_dist))] <- "Between61_90";colnames(Contract_PO_dist)[grepl("V4", colnames(Contract_PO_dist))] <- "Between91_120";colnames(Contract_PO_dist)[grepl("V5", colnames(Contract_PO_dist))] <- "Over120"
Contract_PO_dist<-subset(Contract_PO_dist,Last_Qtr>"2012 Q4")
Contract_PO_dist$Total<-Contract_PO_dist$Under30+Contract_PO_dist$Between31_60+Contract_PO_dist$Between61_90+Contract_PO_dist$Between91_120+Contract_PO_dist$Over120
Contract_PO_dist$Under30<-round(Contract_PO_dist$Under30/Contract_PO_dist$Total,3)
Contract_PO_dist$Between31_60<-round(Contract_PO_dist$Between31_60/Contract_PO_dist$Total,3)
Contract_PO_dist$Between61_90<-round(Contract_PO_dist$Between61_90/Contract_PO_dist$Total,3)
Contract_PO_dist$Between91_120<-round(Contract_PO_dist$Between91_120/Contract_PO_dist$Total,3)
Contract_PO_dist$Over120<-round(Contract_PO_dist$Over120/Contract_PO_dist$Total,3)
Contract_PO_dist<-select(Contract_PO_dist,Last_Qtr,Under30,Between31_60,Between61_90,Between91_120,Over120)
undermaxPO<-max(POdist$Under_4)
Contract_PO_dist<-melt(Contract_PO_dist,id.vars="Last_Qtr")
Contract_PO_dist$position<-ifelse(Contract_PO_dist$variable=="Under_4",undermaxPO-.10,1-((1-undermaxPO)/2)) # calculate height of data labels
Contract_PO_Dist_plot<-ggplot(Contract_PO_dist,aes(x = factor(Last_Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Days to Execute Contracts")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
  scale_fill_manual(values=c("dark green","339900",lightBlue,darkBlue,red),name=" ",labels=c("<=30 Days","31-60 Days","61-90 Days","91-120 Days","Over 120 Days"))
print(Contract_PO_Dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Contract PO Distribution.png")

## Create scatterplot of open contracts per attorney
#Attorneys1<-aggregate(AttorneyReview_Age ~ Purchaser,data = Opencontracts,length)
#Attorneys2<-aggregate(AttorneyReview_Age ~ Purchaser,data = Opencontracts,mean)
#Attorneys<-merge(Attorneys1,Attorneys2,by="Purchaser")
 #   colnames(Attorneys)[colnames(Attorneys) == "AttorneyReview_Age.x"] <- "Count"
   # colnames(Attorneys)[colnames(Attorneys) == "AttorneyReview_Age.y"] <- "Age"
  #      Attorneys$Attorneys<-ifelse(Attorneys$Purchaser=="CJPMEYER","Meyer",
                   #             ifelse(Attorneys$Purchaser=="CSCWELLMAN","Wellman",
                                  #     ifelse(Attorneys$Purchaser=="CTDOATES","Oates",
                                  #        ifelse(Attorneys$Purchaser=="CAJBECNEL","Becnel",
                                            #     ifelse(Attorneys$Purchaser=="CASZELLER","Zeller",
                                           #             ifelse(Attorneys$Purchaser=="CCCDYER","Dyer",
                                                            #   ifelse(Attorneys$Purchaser=="CMJMANZELLA","Manzella",
                                                               #       ifelse(Attorneys$Purchaser=="CTRACYT","Tyler",NA))))))))
#Attorney_plot<-ggplot(Attorneys,aes(x=Age,y=Count))+
#  geom_point(shape=1)+
 #  ggtitle("Average Number of Contracts by Days Awaiting Attorney Review per Attorney")+
 #   xlab("Days Awaiting Attorney Review")+ylab("Number of Contracts")+
 #   geom_text(aes(label=Attorneys))+
 #  print(Attorney_plot)
#ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Attorney Review.png")

## Create scatterplot of contracts awaiting vendor by department
Vendor1<-aggregate(AwaitingVendor_Age ~ Dept,data = Opencontracts,length)
Vendor2<-aggregate(AwaitingVendor_Age ~ Dept,data = Opencontracts,mean)
Vendor<-merge(Vendor1,Vendor2,by="Dept")  ## Right now, manually re-code Dept to Dept2
colnames(Vendor)[colnames(Vendor) == "AwaitingVendor_Age.x"] <- "Count"
colnames(Vendor)[colnames(Vendor) == "AwaitingVendor_Age.y"] <- "Age"
Vendor_plot<-ggplot(Vendor,aes(x=Count,y=Age))+
  geom_point(shape=1)+
  ggtitle("Contracts Awaiting Vendor per Dept")+
  xlab("Number of Contracts")+ylab("Days Awaiting Vendor Signature")+
  geom_text(aes(label=Dept2))
print(Vendor_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Awaiting Vendor.png")

## Plot days to execute broken down by process
Execute_Type<-aggregate(Days_to_Execute~Last_Qtr+Type2,data=Closedcontracts,mean)
Execute_Type<-subset(Execute_Type,Last_Qtr>"2012 Q4")
ExecuteType_Plot<-ggplot(Execute_Type,aes(x=factor(Last_Qtr),y=Days_to_Execute,group=Type2,color=factor(Type2)))
ExecuteType_Plot<-ExecuteType_Plot+geom_line(stat="identity",size=1.25)
print(ExecuteType_Plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Execute Type.png")

## Create Law KPI calculation, table and chart
Closedcontracts$KPI_LawDays<-Days(Closedcontracts,ContractDate,DepAttorney)
Closedcontracts$LawUnder30<-ifelse(Closedcontracts$KPI_LawDays<=30,1,0)
Closedcontracts$LawOver30<-ifelse(Closedcontracts$KPI_LawDays>30,1,0)
LawKPI<-select(Closedcontracts,Last_Qtr,LawUnder30,LawOver30)
LawKPI<-aggregate(cbind(LawKPI$LawUnder30,LawKPI$LawOver30)~Last_Qtr,data=LawKPI,FUN=sum);colnames(LawKPI)[grepl("V1", colnames(LawKPI))] <- "Under30";colnames(LawKPI)[grepl("V2", colnames(LawKPI))] <- "Over30"
LawKPI<-subset(LawKPI,Last_Qtr>"2012 Q4")
  LawKPI$Total<-LawKPI $Under30+LawKPI$Over30
      LawKPI$Under_30<-round(LawKPI$Under30/LawKPI$Total,3)
          LawKPI$Over_30<-round(LawKPI$Over30/LawKPI$Total,3)
  LawKPI_dist<-select(LawKPI,Last_Qtr,Under_30,Over_30)   
    undermaxLawKPI<-max(LawKPI_dist$Under_30)
LawKPI_dist<-melt(LawKPI_dist,id.vars="Last_Qtr")
LawKPI_dist$position<-ifelse(LawKPI_dist$variable=="Under_30",undermaxLawKPI-.10,1-((1-undermaxLawKPI)/2)) # calculate height of data labels
LawKPI_dist_plot<-ggplot(LawKPI_dist,aes(x = factor(Last_Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
    scale_y_continuous(labels = percent_format())+
       ggtitle("Distribution of Days to Draft, Review, and Approve by Law Dept")+
          xlab("Quarters")+ylab("Percent")+
             geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
                 scale_fill_manual(values=c(lightBlue,red),name=" ",labels=c("<=30 Days",">30 Days"))+
                      geom_hline(aes(yintercept=.85,colour="#FF0000"),linetype=2,size=1)
print(LawKPI_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Contract POs/Law Distribution.png")



## Write spreadsheets for relevant data frames
write.csv(contracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/Contracts.csv",na="")
write.csv(Closedcontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/Closed Contracts.csv",na="")
write.csv(Opencontracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/Open Contracts.csv",na="")
write.csv(LawKPI,"O:/Projects/ReqtoCheckStat/Query Files/KPIs/Law KPI.csv")
