### This script analyzes the intial stage of the process, in which the Budget and Finance offices approve requisitons.

#### Read in needed files
ReqApproval<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Approval.csv",skip=3)
ReqStatus<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Status.csv",skip=3),-ENTERED_DATE)

#### Merge files and select desired columns
Reqs<-merge(ReqApproval,ReqStatus,by=c("REQ_NBR"),all.x=TRUE)

###$ Concatenate Requestor's first and last names
Reqs$Requestor<-paste(Reqs$FIRST_NAME,"",Reqs$LAST_NAME)

#### Select desired columns
Reqs<-select(Reqs,Req=REQ_NBR,Order=ORDER_SEQUENCE,ReqDate=ENTERED_DATE,SubmissionDate=REQ_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,ApproverType=REQ_APPROVER_TYPE,Location=LOCATION_NBR_REF,Approver=REQ_APPROVER,Dept=DESC_TEXT,Description=SHORT_DESC,ReqType=DESCRIPTION,Requestor,Cost=EST_COST,STATUS)

#### Filter list to exclude requisitions that were cancelled or returned to the department without approval
Reqs<-filter(Reqs,STATUS!="Canceled" & STATUS!="Returned" & STATUS!="In Progress")

#### Convert relevant columns to dates
Reqs[,3:5]<-lapply(Reqs[,3:5], function(x) as.POSIXct(x,format="%m/%d/%Y %H:%M"))

#### Filter for reqs 
Reqs<-getTwoYears(Reqs,ReqDate,"dec 2015")

#### Cut up and for loop through each "Order" level for each req number to code 
#### whether the row approval is at the Finance, Budget, or Departmental level
reqs <- Reqs %>% mutate(stage = NA)

reqs_with_stages <- data.frame()

uniquereqs <- unique(reqs$Req)

for(i in 1:length(uniquereqs)) {
  
  slice <- reqs %>% filter(Req == uniquereqs[i])
  last_stage <- max(slice$Order)
  
  for(i in 1:length(slice$Order)){
    
    if(slice$Order[i] == last_stage) {
      slice$stage[i] <- "Finance"
    } else if(slice$Order[i] == last_stage - 1) {
      slice$stage[i] <- "Budget"
    } else if (slice$Order[i] == last_stage - 2){
      slice$stage[i] <- "Department"
    } else {
      slice$stage[i]<-"NA"
    }
  }
  
  reqs_with_stages <- rbind(reqs_with_stages, slice)
  
}

####
Reqs<-reqs_with_stages
  
#### Select the relevant columns
Reqs<-select(Reqs,-Approver,-ApproverType,-Order,-Location,ApproverLevel=stage,-Dept)

#### Filter out approvals prior to the final departmental approval
Reqs<-Reqs[Reqs$ApproverLevel!="NA",]

#### Pivot approval times into Department, Budget, and Finance columns
Reqs<-spread(Reqs,ApproverLevel,ApprovalDate)

#### Merge master data set with department approval data set
#Reqs<-merge(Reqs,Dept,by="Req",all=TRUE)%>%
 # select(Req:STATUS,Department,Budget,Finance)

#### Calculate days to approve by stage
Reqs$Budget_Days<-ReqDays(Reqs,Department,Budget,2)
Reqs$Finance_Days<- ReqDays(Reqs,Budget,Finance,2)

#### Code each req into quarters based on the date of final approval by Finance
Reqs$Qtr<-as.yearqtr(Reqs$Finance)

#### Create csv's
write.csv(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqs/Reqs.csv")
write.csv(FirstDept,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest1.csv")
write.xlsx(Reqs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Reqtest.xlsx",showNA=FALSE)
