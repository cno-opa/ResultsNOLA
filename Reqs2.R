### This script analyzes the intial stage of the process, in which the Budget and Finance offices approve requisitons.

### Data cleaning


#### Drop rows with no approval date from routing table
Reqs<-BS_routing[!is.na(BS_routing$ApprovalDate),]

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
Reqs[,"Request_Date"]<-NA
Reqs$Request_Date<-as.POSIXct(Reqs$Request_Date)

for (i in 1:nrow(Reqs)){
  
if (!is.na(Reqs$Request_Date.x[i])){
  
  Reqs$Request_Date[i]<-as.POSIXct(Reqs$Request_Date.x[i])
  
} else {
  
  Reqs$Request_Date[i]<-as.POSIXct(Reqs$Request_Date.y[i])
  
}

}

##### Drop .x and .y request columns, and rearrange column order
Reqs<-select(Reqs,Req,Request_Date,Department,Budget,Finance)

#####Select needed columns from Buyspeed item table
Req_item<-select(BS_item,Req,Bid,PO,Vendor_code)
Req_item<-Req_item[!duplicated(Req_item$Req),]

##### Merge cleaned dataset with other needed Buyspeed tables
Reqs<-left_join(Reqs,BS,by="Req") #### REQ table
Reqs<-left_join(Reqs,Req_item,by="Req") #### REQ_ITEM table
Reqs<-left_join(Reqs,vendor,by="Vendor_code") #### Vendor table
Reqs<-left_join(Reqs,approval_paths,by="Dept_code")


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
Finance_Days<-cbind(aggregate(data=Reqs,Finance_Days~Close_Qtr,FUN=mean),select(aggregate(data=Reqs,Finance_Days~Close_Qtr,FUN=length),-Close_Qtr,Count=Finance_Days))                    

#### Aggregate total days to approval
Days2Req<-select(cbind(Budget_KPI,select(Finance_Days,-Close_Qtr,-Count)),Close_Qtr,Count,Budget_Days,Finance_Days)



### Plotting
#### Drop incomplete quarters (assumption of at least 2,500 per quarter)
Days2Req<-filter(Days2Req,Days2Req$Count>2000)

#### Plot days to approval requisitions
Days2Req<-select(Days2Req,Close_Qtr,Budget_Days,Finance_Days)

Days2Req<-melt(Days2Req,id.vars="Close_Qtr")

Days2Req_plot<-ggplot(Days2Req,aes(x = factor(Close_Qtr), y = value,fill = variable)) +
  geom_bar(position = "stack",stat = "identity") +
  ggtitle("Days to Approve General Fund Requisitions")+
  xlab("Quarters")+ylab("Days")+
  #geom_hline(aes(yintercept=2,colour="#FF0000"),linetype=2,size=1)+
  scale_fill_manual(values=c(darkBlue,lightBlue) ,name=" ",labels=c("Budget","Finance"))+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
print(Days2Req_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Reqs/Days to Req.png")


##### Write out datasets
write.csv(Budget_KPI,"O:/Projects/STAT KPIs/ReqtoCheckSTAT/Budget Reqs.csv") ### Budget KPIs
write.csv(Reqs,"O:/Projects/ReqtoCheckStat/Query Files/Output/Reqs/Reqs.csv")
