require(tidyr)
require (plyr)  
require(dplyr)
require(lubridate)

## Read in needed files 
ReqApproval<-read.csv("Req Approval Sequence.csv",skip=3)
ReqStatus<-read.csv("Req Status.csv",skip=3)

## Merge files and select desired columns
Reqs<-merge(ReqApproval,ReqStatus,by=c("REQ_NBR"),all.x=TRUE)
Reqs$Requestor<-paste(Reqs$FIRST_NAME,"",Reqs$LAST_NAME)  ## Concatenate Requestor's first and last names
Reqs<-select(Reqs,Req=REQ_NBR,CreationDate=ENTERED_DATE.x,SubmissionDate=REQ_REQ_APP_DATE,ApproverType=REQ_APPROVER_TYPE,Approver=REQ_APPROVER,Dept=DESC_TEXT,Description=SHORT_DESC,ReqType=DESCRIPTION,Requestor,EstimatedCost=EST_COST,STATUS,ApprovalDate=APPROVAL_DATE)

## Filter list to exclude requisitions that were cancelled or returned to the department without approval
Reqs<-filter(Reqs,STATUS!="Canceled" & STATUS!="Returned")

## Code Approval Levels
Reqs$ApproverLevel<-ifelse(Reqs$Dept=="FINANCE DIRECTORS OFFICE" & Reqs$Approver=="FIRFB01","Finance",ifelse(Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAMFM01"|Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAOAHE01","Budget","Department"))
  
## Convert relevant columns to Dates
Reqs$CreationDate<-as.Date(Reqs$CreationDate,"%m/%d/%Y")
Reqs$SubmissionDate<-as.Date(Reqs$SubmissionDate,"%m/%d/%Y")
Reqs$ApprovalDate<-as.Date(Reqs$ApprovalDate,"%m/%d/%Y")

##Pivot the data
Reqs1<-select(Reqs,Req,SubmissionDate,ApprovalDate,ApproverLevel)
Reqs1<-reshape(Reqs1,idvar=c("Req","SubmissionDate"),timevar="ApproverLevel",drop=c("CreationDate","ApproverType","Approver","Dept","Description","ReqType","Requestor","EstimatedCost","STATUS"),direction="wide")

## Calculate Days to Approve
Reqs1$Budget_Days<- Reqs1$ApprovalDate.Budget - Reqs1$ApprovalDate.Department
Reqs1$Finance_Days<- Reqs1$ApprovalDate.Finance - Reqs1$ApprovalDate.Budget

## Create CSVs
write.csv(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest.csv")
