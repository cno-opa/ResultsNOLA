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


### Approval time calculations
contract_reqs$Purchasing_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Purchasing),as.POSIXct(contract_reqs$Department),units="days")),2)
contract_reqs$OSD_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$OSD),as.POSIXct(contract_reqs$Purchasing),units="days")),2)
contract_reqs$Finance_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Finance),as.POSIXct(contract_reqs$OSD),units="days")),2)
contract_reqs$Civil_Service_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$Civil_Service),as.POSIXct(contract_reqs$Finance),units="days")),2)
contract_reqs$CAO_Days<-round(as.numeric(difftime(as.POSIXct(contract_reqs$CAO),as.POSIXct(contract_reqs$Civil_Service),units="days")),2)

### Calculate total days
contract_reqs$Total_Days<-ifelse(is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGP" | is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGB",
                                 round(as.numeric(difftime(as.POSIXct(contract_reqs$Civil_Service),as.POSIXct(contract_reqs$Department),units="days")),2),
                                 round(as.numeric(difftime(as.POSIXct(contract_reqs$CAO),as.POSIXct(contract_reqs$Department),units="days")),2))

#### Remove reqs that were requested after the reporting period
contract_reqs<-contract_reqs[as.Date(contract_reqs$Department)<=last,]

#### Remove rows with no req number
contract_reqs<-contract_reqs[!is.na(contract_reqs$Req),]

### Code start and close quarter for each req
contract_reqs$Start_Qtr<-as.yearqtr(contract_reqs$Request_Date)
contract_reqs$Close_Qtr<-ifelse(is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGP" | is.na(contract_reqs$CAO) & contract_reqs$ReqStatus=="1RGB",
                                as.yearqtr(contract_reqs$Civil_Service),
                                as.yearqtr(contract_reqs$CAO))

                               