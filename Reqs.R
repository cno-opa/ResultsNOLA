### This script analyzes the intial stage of the process, in which the Budget and Finance offices approve requisitons.

## Read in needed files 
ReqApproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Approval.csv",skip=3)
ReqStatus<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Status.csv",skip=3)

### Merge files and select desired columns
Reqs<-merge(ReqApproval,ReqStatus,by=c("REQ_NBR"),all.x=TRUE)
Reqs$Requestor<-paste(Reqs$FIRST_NAME,"",Reqs$LAST_NAME)  ## Concatenate Requestor's first and last names
Reqs<-select(Reqs,Req=REQ_NBR,Order=ORDER_SEQUENCE,CreationDate=ENTERED_DATE.x,SubmissionDate=REQ_REQ_APP_DATE,ApproverType=REQ_APPROVER_TYPE,Approver=REQ_APPROVER,Dept=DESC_TEXT,Description=SHORT_DESC,ReqType=DESCRIPTION,Requestor,EstimatedCost=EST_COST,STATUS,ApprovalDate=APPROVAL_DATE)

### Filter list to exclude requisitions that were cancelled or returned to the department without approval
Reqs<-filter(Reqs,STATUS!="Canceled" & STATUS!="Returned" & STATUS!="In Progress")

### Clean dataset 
Reqs<-Reqs %>%
  group_by(Req) %>% ## Group reqs by Req number
    top_n(3) ### Filter to only include the highest three approval levels (one level each representing the requesting department, Budget, and Finance)

### Code Approval Levels
#Reqs$ApproverLevel<-ifelse(Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Order==2 & Reqs$Approver!="CAOBDF01" & Reqs$Approver!="CAOAHE01" & Reqs$Approver!="CAOCMG01" |Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAOAHE01" & Reqs$Order==3,"Budget",
  #                         ifelse(Reqs$Dept=="FINANCE DIRECTORS OFFICE" & Reqs$Order>=2,"Finance","Department"))

Reqs$ApproverLevel<-ifelse(Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Order==2 & Reqs$Approver!="CAOBDF01" & Reqs$Approver!="CAOAHE01" & Reqs$Approver!="CAOCMG01" |Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAOAHE01" & Reqs$Order==3,"Budget",
                           ifelse(Reqs$Dept=="FINANCE DIRECTORS OFFICE" & Reqs$Order>=2 & Reqs$Approver!="FIAML01","Finance","Department"))

##Select the relevant columns
Reqs<-select(Reqs,-Dept,-Approver,-ApproverType,-Order)

## Remove department and extract latest department approval, prior to merging back with rest of dataset
Dept<-filter(Reqs,ApproverLevel=="Department")%>%
  select(Req,ApprovalDate)####NEED TO FILTER BY MAX APPROVALDATE, then merge 

Reqs<-filter(Reqs,ApproverLevel!="Department")

Reqs<-spread(Reqs,ApproverLevel,ApprovalDate) 

### Convert relevant date columns to POSIXct format to parse days to issuance 
Reqs$SubmissionDate<-as.POSIXct(Reqs$SubmissionDate,format="%m/%d/%Y %H:%M")
Reqs$Department<-as.POSIXct(Reqs$Department,format="%m/%d/%Y %H:%M")
Reqs$Budget<-as.POSIXct(Reqs$Budget,format="%m/%d/%Y %H:%M")
Reqs$Finance<-as.POSIXct(Reqs$Finance,format="%m/%d/%Y %H:%M")

Reqs1<-dcast(Reqs,Req~ApproverLevel,value.var="ApprovalDate")
Reqs1<-reshape(Reqs,idvar=c("Req","ReqType","Description","EstimatedCost","SubmissionDate"),timevar="ApproverLevel",direction="wide")  

Reqs1<-melt(Reqs,id.var=c("Req","CreationDate","SubmissionDate","Approver","ApproverLevel"))
Reqs1<-select(Reqs1,-variable)
Reqs2<-dcast(Reqs1,Req~ApproverLevel+ApprovalDate)

#Reqs1<-reshape(Reqs,idvar=c("Req","CreationDate","SubmissionDate"),timevar="ApproverLevel",direction="wide")  
#Reqs1<-select(Reqs1,Req,CreationDate,SubmissionDate,Department=ApprovalDate.Department,Budget=ApprovalDate.Budget,Finance=ApprovalDate.Finance)
#Reqs1<-arrange(Reqs1,CreationDate,SubmissionDate,Department,Budget,Finance)
Reqs2<-merge(Reqs1,FirstDept,by="Req",all.x=TRUE)

## 
FirstDept<-aggregate(ApprovalDate~Req,Reqs,min)

## Calculate Days to Approve
Reqs1$Budget_Days<- Days(Reqs,Budget,Department,2)  
Reqs1$Finance_Days<- Days(Reqs,Finance,Budget,2)

## Create CSVs
write.csv(Reqs1,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqs.csv")
write.csv(FirstDept,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest1.csv")
write.xlsx(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest.xlsx",showNA=FALSE)

Error: Duplicate identifiers for rows 
(56182, 56183) 15116502 bud  Approver level CAOAHE01 is order 3 and 4
(19509, 19510) 1279454 Fin   FIAML01=Order2, FIRFB01=Order4
(20601, 20602), 1280755 Fin  FIAML01=Order2, FIRFB01=Order4
(27603, 27604), 1388359 Fin  FIAML01=Order2, FIRFB01=Order4
(30341, 30342), 1390870 Fin
(49083, 49084), 14109997 Fin
(54499, 54500) 15115021 Fin
