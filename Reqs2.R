### This script analyzes the intial stage of the process, in which the Budget and Finance offices approve requisitons.

### Data cleaning

#### Drop rows with no approval date
Reqs<-BS_routing[!is.na(BS_routing$ApprovalDate),]

#### Subset data for reqs from 2011 to current reporting period
Reqs<-filter(Reqs,Request_Date>"2010-12-31")
Reqs<-Reqs[as.Date(Reqs$Request_Date)<=last,]

#### Clean path ID column to identify if the approver is at the departmental, Budget, or Finance level
Reqs<-Reqs[!is.na(Reqs$Path_ID),]
Reqs$Path_ID<-ifelse(Reqs$Path_ID=="BUDGET","BUDGET",
                     ifelse(Reqs$Path_ID=="FINANCE","Finance","Dept"))

#### Save request dates as separate data frame
Request<-select(Reqs,Req,Request_Date)
Request<-Request[!duplicated(Request$Req),]

#### Departmental subset cleaning
##### Create separate dataframe of department approvals and take only the latest approval date
Dept_reqs<-Reqs[Reqs$Path_ID=="Dept",]
Dept_reqs<-arrange(Dept_reqs,desc(ApprovalDate))
Dept_reqs<-Dept_reqs[!duplicated(Dept_reqs$Req),]
Dept_reqs<-select(Dept_reqs,-Approver,-Ord,-Path_ID)

#### Re-name approval date column for departmental data frame
Dept_reqs


#### Budget and Finance cleaning
##### Remove departmental approvals from main dataset
Reqs<-Reqs[Reqs$Path_ID!="Dept",]
Reqs<-select(Reqs,-Ord,-Approver,-Request_Date)
Reqs<-spread(Reqs,Path_ID,ApprovalDate)


#### Re-join department data frame with Budget and Finance approvals
Reqs<-left_join(Reqs,Request,by="Req")
Reqs<-full_join(Reqs,Dept_reqs,by="Req")

#### Consolidate reques date column
Reqs$Request_Date<-ifelse(is.na(Reqs$Request_Date.x),as.POSIXct(Reqs$Request_Date.y,"%Y-%m-%d %H:%M:%S"), as.POSIXct(Reqs$Request_Date.x,"%Y-%m-%d %H:%M:%S"))


