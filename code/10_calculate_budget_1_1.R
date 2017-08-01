BS_routing <- loadDataset("BS_routing",  refresh_feather = FALSE) 

BS_item <- loadDataset("BS_item",  refresh_feather = FALSE) %>%
  mutate(Req = as.integer(as.character(Req)))

BS <- loadDataset("BS",  refresh_feather = FALSE)  %>%
  mutate(Req = as.integer(as.character(Req)))

vendor <-  loadDataset("vendor",  refresh_feather = FALSE)
approval_paths <-  loadDataset("approval_paths",  refresh_feather = FALSE)



Reqs<-BS_routing [!is.na(BS_routing$ApprovalDate),]

#### Subset data for reqs from 2011 to current reporting period
Reqs<-filter(Reqs,Request_Date>"2010-12-31")
Reqs<-Reqs[as.Date(Reqs$Request_Date)<=last,]

#### Clean path ID column to identify if the approver is at the departmental, Budget, or Finance level
Reqs<-Reqs[!is.na(Reqs$Path_ID),]
Reqs$Path_ID<-ifelse(Reqs$Path_ID=="BUDGET","Budget",
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
names(Dept_reqs)[names(Dept_reqs)=="ApprovalDate"]<-"Department"


#### Budget and Finance cleaning
##### Remove departmental approvals from main dataset
Reqs<-Reqs[Reqs$Path_ID!="Dept",]
Reqs<-select(Reqs,-Ord,-Approver,-Request_Date)
Reqs<-spread(Reqs,Path_ID,ApprovalDate)



#### Master dataset cleaning
##### Re-join department data frame with Budget and Finance approvals
Reqs<-left_join(Reqs,Request,by="Req")
Reqs<-full_join(Reqs,Dept_reqs,by="Req")

##### Consolidate request date column into one
Reqs[,"Request_Date"]<-NULL


Reqs <- Reqs %>%
  mutate(Request_Date.x = as.POSIXct(Request_Date.x),
         Request_Date.y = as.POSIXct(Request_Date.y),
         Request_Date = if_else(is.na(Request_Date.x) == TRUE, 
                                Request_Date.y, 
                                Request_Date.x))


##### Drop .x and .y request columns, and rearrange column order
Reqs <- select(Reqs,Req,Request_Date,Department,Budget,Finance)

#####Select needed columns from Buyspeed item table
Req_item<-select(BS_item,Req,Bid,PO,Vendor_code)
Req_item<-Req_item[!duplicated(Req_item$Req),]

##### Merge cleaned dataset with other needed Buyspeed tables
Reqs<-left_join(Reqs,BS,by="Req") %>%
  left_join(Req_item,by="Req") %>%
  left_join(vendor,by="Vendor_code") %>%
  left_join(approval_paths,by="Dept_code")


##### Remove cancelled requisitions, as those returned to department for resubmission from dataset
Reqs<-Reqs[Reqs$ReqStatus!="1RC"| Reqs$ReqStatus!="1RR",]

##### Code each req into quarters based on the date of final approval by Finance
Reqs$First_Qtr<-as.yearqtr(Reqs$Request_Date)
Reqs$Close_Qtr<-as.yearqtr(Reqs$Finance)

##### Calculate days per stage
Reqs$Budget_Days<-ifelse(!is.na(Reqs$Department),
                         round(as.numeric(difftime(as.POSIXct(Reqs$Budget),as.POSIXct(Reqs$Department),units="days")),2),
                         round(as.numeric(difftime(as.POSIXct(Reqs$Budget),as.POSIXct(Reqs$Request_Date),units="days")),2))
Reqs$Finance_Days<-round(as.numeric(difftime(as.POSIXct(Reqs$Finance),as.POSIXct(Reqs$Budget),units="days")),2)

##### Calculate total days from final departmental approval to Finance approval
Reqs$Total_Days<-ifelse(!is.na(Reqs$Department),
                        round(as.numeric(difftime(as.POSIXct(Reqs$Finance),as.POSIXct(Reqs$Department),units="days")),2),
                        round(as.numeric(difftime(as.POSIXct(Reqs$Finance),as.POSIXct(Reqs$Request_Date),units="days")),2))

#### Aggregate quarterly counts and averages for days to Budget and Finance approval
Budget_KPI<-cbind(aggregate(data=Reqs,Budget_Days~Close_Qtr,FUN=mean),select(aggregate(data=Reqs,Budget_Days~Close_Qtr,FUN=length),-Close_Qtr,Count=Budget_Days))                    
