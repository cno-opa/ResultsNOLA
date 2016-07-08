## This script analyzes processing times on contract requisitions

#### Load contract requisition routing table where approval date is not NA
contract_reqs<-con_req_routing[!is.na(con_req_routing$ApprovalDate),]


contract_req_status<-select(req_status_dates,Req,ReqStatus=MAJOR_STATUS_CODE,ReqStatusDate)%>%
                     arrange(desc(ReqStatusDate))%>%
                     select(Req,ReqStatus)


#### Data cleaning     

contract_req_status<-contract_req_status[!duplicated(contract_req_status$Req),]


contract_reqs$Req<-as.factor(contract_reqs$Req)

#### Code approvals based on approver level (NOTE: This is currently dependent on approver name recognition, 
#### so approvers may need to be added over time)
contract_reqs$Level<-ifelse(!str_detect(tolower(contract_reqs$Path_ID),"citywid"),
                            
                            "Department",
                            
                            ifelse(str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="PBBAJ01"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CMKZAMORA",
                            
                            "Purchasing",
                            
                            ifelse(str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CASMATTHEWS"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CKFGROSS" | str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CRSPENCER",
                                   
                            "OSD",
                            
                            ifelse(str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CNSFOSTER"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CBBGARIEPY" | str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CDAMUSE",
                              
                             "Finance",
                             
                             ifelse(str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CGBARTHOLOMEW"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CLMHUDSON"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CRWHAGMANN",
                                                                             
                             "Civil_Service",
                             
                             ifelse(str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CJECHRISTOPHER"| str_detect(tolower(contract_reqs$Path_ID),"citywid") & contract_reqs$Approver=="CAKOPPLIN",
                                   
                             "CAO",NA)))))) 

#### Remove approver column, as it will no longer be needed, and would complicate join process later on
contract_reqs<-select(contract_reqs,-Approver)

dept_con_reqs<-filter(contract_reqs,Level=="Department")%>%
               select(Req,Request_Date,Department=ApprovalDate)%>%
               arrange(desc(Department))

dept_con_reqs<-dept_con_reqs[!duplicated(dept_con_reqs$Req),]
               
#### Select columns needed to pivot approval dates into respective columns based on approval level
con_reqapprovals<-filter(contract_reqs,Level!="Department")

#### Remove rows with errors in the approval path
exclude<-con_reqapprovals[con_reqapprovals$Ord>=8|con_reqapprovals$Level!="CAO" & con_reqapprovals$Ord==7,]

#### Remove reqs with errors from dataset
con_reqapprovals<-anti_join(con_reqapprovals,exclude,by="Req")
con_reqapprovals<-con_reqapprovals[con_reqapprovals$Req!="1271201",]

#### Pivot approval dates into separtate columns based on approver level
con_reqapprovals<-select(con_reqapprovals,Req,ApprovalDate,Level)%>%
                  spread(Level,ApprovalDate)


#### Merge datasets
contract_reqs<-full_join(dept_con_reqs,con_reqapprovals,by="Req") # Join department and city-wide approvals into master dataset                          
contract_reqs<-left_join(contract_reqs,contract_req_status,by="Req")

### Remove cancelled requisitions from dataset
contract_reqs<-contract_reqs[contract_reqs$ReqStatus!="1RC"& contract_reqs$ReqStatus!="1RR",]
 
### Arrange dataframe columns
contract_reqs<-select(contract_reqs,Req,ReqStatus,Request_Date,Department,Purchasing,OSD,Finance,Civil_Service,CAO)


### Approval day calculations
contract_reqs$Purchasing_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Purchasing),as.POSIXct(contract_reqs$Department),units="days")),2)
contract_reqs$OSD_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$OSD),as.POSIXct(contract_reqs$Purchasing),units="days")),2)
contract_reqs$Finance_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Finance),as.POSIXct(contract_reqs$OSD),units="days")),2)
contract_reqs$Civil_Service_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Civil_Service),as.POSIXct(contract_reqs$Finance),units="days")),2)
contract_reqs$CAO_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$CAO),as.POSIXct(contract_reqs$Civil_Service),units="days")),2)

#### Calculate total days from departmental approval to 
contract_reqs$Total_Days<-ifelse(is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGP" | is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGB",
                                 round(as.numeric(difftime(as.POSIXct(contract_reqs$Civil_Service),as.POSIXct(contract_reqs$Department),units="days")),2),
                                 round(as.numeric(difftime(as.POSIXct(contract_reqs$CAO),as.POSIXct(contract_reqs$Department),units="days")),2))

#### Remove reqs that were requested after the reporting period
contract_reqs<-contract_reqs[as.Date(contract_reqs$Department)<=last,]

#### Remove rows with no req number
contract_reqs<-contract_reqs[!is.na(contract_reqs$Req),]

#### Code start and close quarter for each req
contract_reqs$Start_Qtr<-as.yearqtr(contract_reqs$Request_Date)
contract_reqs$Close_Qtr<-ifelse(is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGP" | is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGB",
                                as.yearqtr(contract_reqs$Civil_Service),
                                as.yearqtr(contract_reqs$CAO))
contract_reqs$Close_Qtr<-as.yearqtr(contract_reqs$Close_Qtr)  ### Coerce Close_Qtr to proper format

### Plotting

##### Contract req days to process
conReq_days<-ddply(subset(contract_reqs,!is.na(Total_Days)),"Close_Qtr",summarise,Sign=mean(Total_Days))
conReq_days<-subset(conReq_days,Close_Qtr>"2012 Q2")
conReq_days_plot<-ggplot(conReq_days,aes(x=factor(Close_Qtr),y=Sign))+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Average Days to Execute Contracts by Quarter \n - Non-Ordinance")+
  xlab("Quarters")+ylab("Days")+
  geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
  geom_hline(aes(yintercept=5),colour="#FF0000",linetype=2,size=1.2)+theme(legend.position="Top")+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
print(conReq_days_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract Reqs/Days to approve.png")

##### Approval days per stage
conReq_Stages<-subset(contract_reqs,Close_Qtr>"2012 Q4")
conReq_Stages<-melt(select(conReq_Stages,Req,Close_Qtr,Purchasing_Days:CAO_Days),id.var=c("Req","Close_Qtr"),variable.name="Stage",value.name="Days_in_Stage")
conReq_Stages$Days_in_Stage<-as.numeric(conReq_Stages$Days_in_Stage)
conReq_Stages<- aggregate(Days_in_Stage ~ Close_Qtr + Stage, data = conReq_Stages, mean)
levels(conReq_Stages$Stage)[levels(conReq_Stages$Stage)=="Purchasing_Days"]<-"Puchasing"
levels(conReq_Stages$Stage)[levels(conReq_Stages$Stage)=="OSD_Days"]<-"OSD"
levels(conReq_Stages$Stage)[levels(conReq_Stages$Stage)=="Finance_Days"]<-"Finance"
levels(conReq_Stages$Stage)[levels(conReq_Stages$Stage)=="Civil_Service_Days"]<-"Civil Service"
levels(conReq_Stages$Stage)[levels(conReq_Stages$Stage)=="CAODays"]<-"CAO"
conReq_Stage_plot<-ggplot(conReq_Stages,aes(x=factor(Close_Qtr),label=ifelse(Close_Qtr==r_period,Days_in_Stage,""),y=Days_in_Stage,group=1))+
  geom_bar(stat="identity",fill=darkBlue,size=0.6)+
  facet_grid(facets=.~Stage)+
  ggtitle("Average Days to Approve Contract Requisitions")+
  xlab("Quarters")+ylab("Days")+
  theme(strip.text.x=element_text(size=8),
        axis.text.x=element_text(angle=90,hjust=0.25,size=9),plot.title=element_text(size=13,face="bold",vjust=1))
print(conReq_Stage_plot)
ggsave("O:/Projects/ReqtoCheckSTAT/Query Files/Slides/Contract Reqs/Days by stage.png")

                               