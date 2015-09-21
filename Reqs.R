.libPaths("C:/Rpackages")

## Download OPA theme, as well as required packages from OPA github account
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Requirements.R")

## Read in needed files 
ReqApproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Approval Sequence.csv",skip=3)
ReqStatus<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Status.csv",skip=3)

## Merge files and select desired columns
Reqs<-merge(ReqApproval,ReqStatus,by=c("REQ_NBR"),all.x=TRUE)
Reqs$Requestor<-paste(Reqs$FIRST_NAME,"",Reqs$LAST_NAME)  ## Concatenate Requestor's first and last names
Reqs<-select(Reqs,Req=REQ_NBR,CreationDate=ENTERED_DATE.x,SubmissionDate=REQ_REQ_APP_DATE,ApproverType=REQ_APPROVER_TYPE,Approver=REQ_APPROVER,Dept=DESC_TEXT,Description=SHORT_DESC,ReqType=DESCRIPTION,Requestor,EstimatedCost=EST_COST,STATUS,ApprovalDate=APPROVAL_DATE)

## Remove unnecessary objects from environment to clear memory *TEMPORARY*
rm(ReqApproval)
rm(ReqStatus)

## Filter list to exclude requisitions that were cancelled or returned to the department without approval
Reqs<-filter(Reqs,STATUS!="Canceled" & STATUS!="Returned" & STATUS!="In Progress")

## Eliminate approvals that are neither department-level, nor Budget or Finance
Reqs<-Reqs[Reqs$Approver!="ISTWN01",]  ## ITI at-large approval

## Code Approval Levels
Reqs$ApproverLevel<-ifelse(Reqs$Dept=="FINANCE DIRECTORS OFFICE" & Reqs$Approver=="FIRFB01","Finance",ifelse(Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAMFM01"|Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAOAHE01","Budget","Department"))

### Convert relevant date columns to POSIXct format to parse days to issuance 
Reqs$SubmissionDate<-as.POSIXct(Reqs$SubmissionDate,format="%m/%d/%Y %H:%M")
Reqs$ApprovalDate<-as.POSIXct(Reqs$ApprovalDate,format="%m/%d/%Y %H:%M")

Reqs<-arrange(Reqs,desc(ApprovalDate))

##Pivot the data
Reqs<-select(Reqs,Req,Approver,CreationDate,SubmissionDate,ApprovalDate,ApproverLevel)
Reqs1<-reshape(Reqs,idvar=c("Req","CreationDate","SubmissionDate"),timevar="ApproverLevel",direction="wide")  
Reqs1<-select(Reqs1,Req,CreationDate,SubmissionDate,Department=ApprovalDate.Department,Budget=ApprovalDate.Budget,Finance=ApprovalDate.Finance)
Reqs1<-arrange(Reqs1,CreationDate,SubmissionDate,Department,Budget,Finance)
Reqs2<-merge(Reqs1,FirstDept,by="Req",all.x=TRUE)

## 
FirstDept<-aggregate(ApprovalDate~Req,Reqs,min)

## Calculate Days to Approve
Reqs1$Budget_Days<- Reqs1$Budget - Reqs1$Department  ##Converts to seconds
Reqs1$Budget_Days<-Reqs1$Budget_Days/86400
Reqs1$Finance_Days<- Reqs1$Finance - Reqs1$Budget ##Converts to seconds
Reqs1$Finance_Days<-Reqs1$Budget_Days/86400

## Create CSVs
write.csv(Reqs1,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest2.csv")
write.csv(FirstDept,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest1.csv")
write.xlsx(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest.xlsx",showNA=FALSE)
