## This script analyzess contract processing once a contract has made it to the Law Dept for processing.
## Before running this script, first run Buyspeed_Queries.R.  if SQL access not available, run Alternative_Queries.R.

##### Load OPA and Law data to supplement BuySpeed data
adjust<-select(read_excel("O:/Projects/ReqtoCheckStat/Query Files/Adjustments.xlsx",1),-AltID)
#Law<-filter(read_excel("O:/Projects/ReqtoCheckStat/Query Files/Law Master.xlsx",1),!is.na(K))


## Data cleaning

#### Narrative
##### Create data frame of comments and notes put in by ECMS users for contracts
narrative<-select(filter(po_routing,!is.na(Comments)),-ApprovalDate,-Ord)
narrative<-inner_join(narrative,po_notes,by="PO")

##### Detect comments and notes in ECMS denoting contracts which require a City Council Ordinance
po_comments<-narrative[str_detect(tolower(narrative$Comments),"ordinance"),]
con_notes<-narrative[str_detect(tolower(narrative$Note),"ordinance"),]
po_comments<-po_comments[!duplicated(po_comments$PO),] # De-duplicate contracts
con_notes<-con_notes[!duplicated(con_notes$PO),] # De-duplicate contracts
con_notes<-con_notes[!(con_notes$PO %in% po_comments$PO),] # Drop any rows from notes that appear in comments dataframe
ordinance<-rbind(po_comments,con_notes)


#### Law
##### Trim whitespace from AltID
#Law$AltID<-str_trim(Law$AltID)


### System data cleaning

#### Contract requisitions 
##### Subset contract reqs that were created during the analysis period
# law_reqs<-req_routing[!duplicated(req_routing$Req),]
# law_reqs<-law_reqs[as.Date(law_reqs$ReqComplete)>=first,]
# law_reqs<-select(law_reqs,-Req)
# 
# ##### Code contract req status column
# ##### De-duplicate reqs waiting for Law processing
# req_status<-group_by(req_status_dates,Req)%>%
#   mutate(current_status=max(ReqStatusDate))%>%
#   filter(current_status==ReqStatusDate)%>%
#   filter(MAJOR_STATUS_CODE=="1RRP"|MAJOR_STATUS_CODE=="1RGP")%>%
#   select(Req,ReqStatusDate,MAJOR_STATUS_CODE)
# 
# ##### Create and code a contract req status column
# req_status[,"ContractReqStatus"]<-NA
# req_status$ContractReqStatus<-ifelse(req_status$MAJOR_STATUS_CODE=="1RRP","ReadyForLaw","ConvertedtoPO")
# 
# ##### Pivot into specific pre-law status columns
# req_status<-select(req_status,-MAJOR_STATUS_CODE)%>%
#   spread(ContractReqStatus,ReqStatusDate)
# 
# ##### Merge contract reqs 
# law_reqs$Req<-as.factor(law_reqs$Req)
# law_reqs<-left_join(law_reqs,req_status,by="Req")

##### Create data frame of rows with no approval dates
# pre_law_pos<-select(po_routing,PO,ApprovalDate) %>%
#   filter(is.na(ApprovalDate))%>%
#   select(PO)

##### Create data frame of dates attorneys completed legal review
legal_review<-select(po_routing,PO,ReqComplete)
legal_review<-legal_review[!duplicated(legal_review$PO),]

##### Clean contract routing dataset to prepare to pivot
con_routing<-select(po_routing,-Comments,-ReqComplete) %>%
        filter(!is.na(ApprovalDate) & Ord<6)

##### Filter pre_law list for contracts that have no approvals and/or path
#pre_law_pos$PO<-ifelse(pre_law_pos$PO %in% po_routing$PO,NA,pre_law$PO)
#pre_law_pos<-filter(pre_law_pos,!is.na(pre_law_pos$PO))
#pre_law_pos<-data.frame(unique(pre_law_pos$PO))
#pre_law_pos[,2:6]<-NA; names(pre_law_pos)<-c("PO","DepAttorney","CAO","SentVendor","ECMS_BackFromVendor","Publication")


##### Create stage column to code approval stages based on the order number
con_routing[,"Stage"]<-NA 


for (i in 1:nrow(con_routing)){
  
  if (con_routing$Ord[i]==1){
    
    con_routing$Stage[i]<-"DepAttorney"
    
  } else if (con_routing$Ord[i]==2){
    
    con_routing$Stage[i]<-"CAO"
    
  } else if (con_routing$Ord[i]==3){
    
    con_routing$Stage[i]<-"SentVendor"
    
  } else if (con_routing$Ord[i]==4){
    
    con_routing$Stage[i]<-"ECMS_BackFromVendor"
    
  } else {
    
    con_routing$Stage[i]<-"Publication"
    
  }
}

#####
con_routing<-select(con_routing,-Ord,-Approver)

##### Pivot approval column into separate columns for each stage
con_routing<-select(spread(con_routing,Stage,ApprovalDate),PO,DepAttorney,CAO,SentVendor,ECMS_BackFromVendor,Publication)

##### Elminate cancelled contracts from header dataframe
po_header<-po_header[po_header$POStatus!="3PCA"& po_header$POStatus!="3PCR",]

##### Merge into master file
#contracts<-rbind(contracts,pre_law_pos)
contracts<-left_join(po_header,legal_review,by="PO")
contracts<-left_join(contracts,con_routing, by="PO")
contracts<-left_join(contracts,po_item,by="PO")
contracts<-left_join(contracts,vendor,by="Vendor_code")
contracts<-left_join(contracts,approval_paths,by="Dept_code")
contracts<-left_join(contracts,adjust,by="PO")

### Eliminate contracts that have been cancelled, but are not reflected in system
contracts<-filter(contracts,is.na(Cancelled))
needs_cancel<-filter(contracts,!is.na(Cancelled))

### Eliminate contracts created after the end of the quarter
contracts<-contracts[as.Date(contracts$ContractDate)<=last,]

### Code additional ordinance contracts identified through ECMS notes and comments
contracts$Ordinance<-ifelse(!is.na(contracts$Ordinance),
                            contracts$Ordinance,
                            ifelse(contracts$PO %in% ordinance$PO & is.na(contracts$Ordinance),1,NA))

##### Create consolidated start date column to account for contracts with an inaccurate contrat date in ECMS
contracts[,"Start_Date"]<-NA
contracts$Start_Date<-as.POSIXct(contracts$Start_Date)

for (i in 1:nrow(contracts)){
  
  if (!is.na(contracts$Manual_Date[i])){
    
    contracts$Start_Date[i]<-as.POSIXct(contracts$Manual_Date[i])
    
} else {
  
  contracts$Start_Date[i]<-as.POSIXct(contracts$ContractDate[i])
  
}

}

##### Create consolidated close date column to indiicate the date a contract was signed and/or closed in ECMS
contracts[,"Close_Date"]<-NA 
contracts$Close_Date<-as.POSIXct(contracts$Close_Date)

for (i in 1:nrow(contracts)){
  
  if (!is.na(contracts$AdjustedSignDate[i])){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$AdjustedSignDate[i])
    
  } else if (!is.na(contracts$Publication[i])){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$Publication[i])
    
  } else if ( (is.na(contracts$Publication[i])) & (contracts$POStatus[i]=="3PS") ){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$StatusDate[i])
    
  } else if ( (is.na(contracts$Publication[i])) & (contracts$POStatus[i]=="3PRS") ){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$StatusDate[i])
    
  } else {
    
    contracts$Close_Date[i]<-NA
    
  }

}

##### Create column for quarter contracts were created and closed
contracts$Start_Qtr<-as.yearqtr(contracts$Start_Date)
contracts$Close_Qtr<-as.yearqtr(contracts$Close_Date)

##### Create dataframes of contracts that have been signed, as well as those that were open at the end of the quarter
open_contracts<-contracts[is.na(contracts$Close_Date),]
closed_contracts<-contracts[!is.na(contracts$Close_Date),]


## Closed contracts cleaning
##### Create days to execute column
closed_contracts$Execute_Days<-as.integer(difftime(as.POSIXct(closed_contracts$Close_Date),as.POSIXct(closed_contracts$Start_Date),units="days"))

##### Calculate days per approval stage 
closed_contracts$Legal_Days<-as.integer(difftime(as.POSIXct(closed_contracts$ReqComplete),as.POSIXct(closed_contracts$ContractDate),units="days"))

closed_contracts$Dep_Attorney_Days<-as.integer(difftime(as.POSIXct(closed_contracts$DepAttorney),as.POSIXct(closed_contracts$ReqComplete),units="days"))

closed_contracts$CAO_Days<-as.integer(difftime(as.POSIXct(closed_contracts$CAO),as.POSIXct(closed_contracts$DepAttorney),units="days"))

closed_contracts$Sent_Vendor_Days<-ifelse(is.na(closed_contracts$Ordinance)
                    ,as.integer(difftime(as.POSIXct(closed_contracts$SentVendor),as.POSIXct(closed_contracts$CAO),units="days"))
                    ,NA)

closed_contracts$Awaiting_Vendor_Days<-ifelse(!is.na(closed_contracts$BackFromVendor)
                    ,as.integer(difftime(as.POSIXct(closed_contracts$BackFromVendor),as.POSIXct(closed_contracts$SentVendor),units="days"))
                    ,as.integer(difftime(as.POSIXct(closed_contracts$ECMS_BackFromVendor),as.POSIXct(closed_contracts$SentVendor),units="days")))

closed_contracts$Executive_Signature_Days<-as.integer(difftime(as.POSIXct(closed_contracts$Close_Date),as.POSIXct(closed_contracts$ECMS_BackFromVendor),units="days"))

closed_contracts$Publication_Days<-ifelse(!is.na(closed_contracts$AdjustedSignDate) & !is.na(closed_contracts$Publication)
                                          ,as.integer(difftime(as.POSIXct(closed_contracts$Publication),as.POSIXct(closed_contracts$Close_Date),units="days"))
                                          ,NA)


### Export data to spreadsheets
write.csv(contracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/contracts.csv")
write.xlsx(select(closed_contracts,PO,AltID,Dept,Vendor,Description,),
           "O:/Projects/"

