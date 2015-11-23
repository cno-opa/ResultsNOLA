### This script analyzes the intial stage of the process, in which the Budget and Finance offices approve requisitons.

#### Read in needed files
ReqApproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Approval.csv",skip=3)
ReqStatus<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Status.csv",skip=3),-ENTERED_DATE)

#### Merge files and select desired columns
Reqs<-merge(ReqApproval,ReqStatus,by=c("REQ_NBR"),all.x=TRUE)

###$ Concatenate Requestor's first and last names
Reqs$Requestor<-paste(Reqs$FIRST_NAME,"",Reqs$LAST_NAME)

#### Select desired columns
Reqs<-select(Reqs,Req=REQ_NBR,Order=ORDER_SEQUENCE,ReqDate=ENTERED_DATE,SubmissionDate=REQ_REQ_APP_DATE,ApproverType=REQ_APPROVER_TYPE,Location=LOCATION_NBR_REF,Approver=REQ_APPROVER,Dept=DESC_TEXT,Description=SHORT_DESC,ReqType=DESCRIPTION,Requestor,Cost=EST_COST,STATUS,ApprovalDate=APPROVAL_DATE)

#### Filter list to exclude requisitions that were cancelled or returned to the department without approval
Reqs<-filter(Reqs,STATUS!="Canceled" & STATUS!="Returned" & STATUS!="In Progress")

#### Code approval levels
Reqs$ApproverLevel<-ifelse(Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Order==2 & Reqs$Approver!="CAOBDF01"|Reqs$Dept=="CAO BUDGET & OPERATIONS" & Reqs$Approver=="CAOAHE01" & Reqs$Order>=2|Reqs$Dept=="CAO BUDGET & OPERATIONS"& Reqs$Approver=="CAMFM01" & Reqs$Order>=2|Reqs$Approver=="CAOTMB01" & Reqs$Order>=2,"Budget",
                           ifelse(Reqs$Dept=="FINANCE DIRECTORS OFFICE" & Reqs$Order>=2 & Reqs$Approver!="FIAML01"|Reqs$Approver=="FIRFB01","Finance","Department"))

#### Clean dataset
Reqs<-Reqs %>%
  group_by(Req) %>% ## Group reqs by Req number
  #arrange(desc(Order))
  top_n(3,Order) ### Filter to only include the highest three approval levels (one level each representing the requesting department, Budget, and Finance)

#### Select the relevant columns
Reqs<-select(Reqs,-Dept,-Approver,-ApproverType,-Order)

#### Convert dataset back to data frame
class(Reqs)<-"data.frame"

#### Convert relevant columns to POSIXct, to make it possible to compute days to approve, down to the minute.
Reqs$ReqDate<-as.POSIXct(Reqs$ReqDate,format="%m/%d/%Y %H:%M")
Reqs$SubmissionDate<-as.POSIXct(Reqs$SubmissionDate,format="%m/%d/%Y %H:%M")
Reqs$ApprovalDate<-as.POSIXct(Reqs$ApprovalDate,format="%m/%d/%Y %H:%M")

#### Remove department and extract latest department approval, prior to merging back with rest of dataset
Dept<-filter(Reqs,ApproverLevel=="Department")%>%
  select(Req,ApprovalDate)%>%
    group_by(Req)%>%
      top_n(1)

#### Re-name department approval column to "Department"
colnames(Dept)[2]<-"Department"

#### Filter master data set to only include Budget and Finance approvals, and then pivot those approval times into new Budget and Finance columns
Reqs<-filter(Reqs,ApproverLevel!="Department")%>%
        spread(ApproverLevel,ApprovalDate)

#### Merge master data set with department approval data set
Reqs<-merge(Reqs,Dept,by="Req",all=TRUE)%>%
  select(Req:STATUS,Department,Budget,Finance)

#### Calculate days to approve by stage
Reqs$Budget_Days<-ReqDays(Reqs,Department,Budget,2)
Reqs$Finance_Days<- ReqDays(Reqs,Budget,Finance,2)



#### Create csv's
write.csv(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqs/Reqs.csv")
write.csv(FirstDept,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest1.csv")
write.xlsx(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest.xlsx",showNA=FALSE)
