## This script analyzess contract processing once a contract has made it to the Law Dept for processing.
## Before running this script, first run Buyspeed_Queries.R.  if SQL access not available, run Alternative_Queries.R.

##### Load OPA and Law data to supplement BuySpeed data
adjust<-select(read_excel("O:/Projects/ReqtoCheckStat/Query Files/Adjustments.xlsx",1),-AltID)
#Law<-filter(read_excel("O:/Projects/ReqtoCheckStat/Query Files/Law Master.xlsx",1),!is.na(K))


## Data cleaning

#### Narrative
##### Create data frame of comments and notes put in by ECMS users for contracts
narrative<-select(filter(po_routing,!is.na(Comments)),-ApprovalDate,-Ord)
narrative<-inner_join(narrative,po_notes,by="PO")

##### Detect comments and notes in ECMS denoting contracts which require a City Council Ordinance
po_comments<-narrative[str_detect(tolower(narrative$Comments),"ordinance"),]
con_notes<-narrative[str_detect(tolower(narrative$Note),"ordinance"),]
po_comments<-po_comments[!duplicated(po_comments$PO),] # De-duplicate contracts
con_notes<-con_notes[!duplicated(con_notes$PO),] # De-duplicate contracts
con_notes<-con_notes[!(con_notes$PO %in% po_comments$PO),] # Drop any rows from notes that appear in comments dataframe
ord_narrative<-rbind(po_comments,con_notes)

##### Detect comments and notes in ECMS denoting contracts which have been manually routed via Expediency form
manual_comments<-narrative[str_detect(tolower(narrative$Comments),"expediency")| str_detect(tolower(narrative$Comments),"manual"),]
manual_comments<-manual_comments[!duplicated(manual_comments$PO),]
manual_notes<-narrative[str_detect(tolower(narrative$Note),"expediency")| str_detect(tolower(narrative$Note),"manual"),]
manual_notes<-manual_notes[!duplicated(manual_notes$PO),]
manual<-rbind(manual_comments,manual_notes)

##### Detect comments and notes indicating that a contract has been executed
exec_comments<-narrative[narrative$Approver=="CONTRACTCLERK"|narrative$Approver=="CMESTRICKLAND"|narrative$Approver=="CMJAVERILL",]
exec_comments<-exec_comments[!is.na(exec_comments$Comments),]
exec_comments<-exec_comments[exec_comments$Comments!="",]


#### Approval data cleaning
##### Create data frame of dates attorneys completed legal review
legal_review<-select(po_routing,PO,LegalReview)
legal_review<-legal_review[!duplicated(legal_review$PO),]

##### Clean contract routing dataset to prepare to pivot
con_routing<-select(po_routing,-Comments,-LegalReview) %>%
        filter(!is.na(ApprovalDate) & Ord<6)

##### Create stage column to code approval stages based on the order number
con_routing[,"Stage"]<-NA 


for (i in 1:nrow(con_routing)){
  
  if (con_routing$Ord[i]==1){
    
    con_routing$Stage[i]<-"DepAttorney"
    
  } else if (con_routing$Ord[i]==2){
    
    con_routing$Stage[i]<-"CAO"
    
  } else if (con_routing$Ord[i]==3){
    
    con_routing$Stage[i]<-"SentVendor"
    
  } else if (con_routing$Ord[i]==4){
    
    con_routing$Stage[i]<-"ECMS_BackFromVendor"
    
  } else {
    
    con_routing$Stage[i]<-"Executive"
    
  }
}

#####
con_routing<-select(con_routing,-Ord,-Approver)

##### Pivot approval column into separate columns for each stage
con_routing<-select(spread(con_routing,Stage,ApprovalDate),PO,DepAttorney,CAO,SentVendor,ECMS_BackFromVendor,Executive)

##### Elminate cancelled contracts from header dataframe
po_header_con<-po_header_con[po_header_con$POStatus!="3PCA"& po_header_con$POStatus!="3PCR",]

##### Merge into master file
#contracts<-rbind(contracts,pre_law_pos)
contracts<-left_join(po_header_con,legal_review,by="PO")
contracts<-left_join(contracts,con_routing, by="PO")
contracts<-left_join(contracts,po_item,by="PO")
contracts<-left_join(contracts,vendor,by="Vendor_code")
contracts<-left_join(contracts,approval_paths,by="Dept_code")
contracts<-left_join(contracts,adjust,by="PO")

### Eliminate contracts that have been cancelled, but are not reflected in system
needs_cancel<-filter(contracts,!is.na(Cancelled))
contracts<-filter(contracts,is.na(Cancelled))


### Eliminate contracts created after the end of the quarter
contracts<-contracts[as.Date(contracts$ContractDate)<=last,]

### Code additional ordinance contracts identified through ECMS notes and comments
contracts$Ordinance<-ifelse(!is.na(contracts$Ordinance),
                            contracts$Ordinance,
                            ifelse(contracts$PO %in% ord_narrative$PO & is.na(contracts$Ordinance),1,NA))

### Code additional manual contracts identified through ECMS notes and comments
contracts$Manual<-ifelse(startsWith(tolower(contracts$AltID),'m'),1,
                         ifelse(contracts$PO %in% manual$PO,1,0))

#### Categorize contracts by type
contracts$Type2<-ifelse(str_detect(tolower(contracts$Type),"cea"),"CEA",
                        ifelse(str_detect(tolower(contracts$Type),"psa") & str_detect(tolower(contracts$Type),"15"),"PSA < $15k",
                               ifelse(str_detect(tolower(contracts$Type),"psa") & !str_detect(tolower(contracts$Type),"15"),"PSA >= $15k",
                                      ifelse(str_detect(tolower(contracts$Type),"grant"),"Grant",
                                             ifelse(str_detect(tolower(contracts$Type),"bid"),"Bid","Other")))))

##### Create consolidated start date column to account for contracts with an inaccurate contract date in ECMS
contracts[,"Start_Date"]<-NA
contracts$Start_Date<-as.POSIXct(contracts$Start_Date)

for (i in 1:nrow(contracts)){
  
  if (!is.na(contracts$Manual_Date[i])){
    
    contracts$Start_Date[i]<-as.POSIXct(contracts$Manual_Date[i])
    
} else {
  
  contracts$Start_Date[i]<-as.POSIXct(contracts$ContractDate[i])
  
}

}

##### Create consolidated close date column to indiicate the date a contract was signed and/or closed in ECMS
contracts[,"Close_Date"]<-NA 
contracts$Close_Date<-as.POSIXct(contracts$Close_Date)

for (i in 1:nrow(contracts)){
  
  if (!is.na(contracts$AdjustedSignDate[i])){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$AdjustedSignDate[i])
    
  } else if (!is.na(contracts$Executive[i])){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$Executive[i])
    
  } else if ( (is.na(contracts$Executive[i])) & (contracts$POStatus[i]=="3PS") ){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$StatusDate[i])
    
  } else if ( (is.na(contracts$Executive[i])) & (contracts$POStatus[i]=="3PRS") ){
    
    contracts$Close_Date[i]<-as.POSIXct(contracts$StatusDate[i])
    
  } else {
    
    contracts$Close_Date[i]<-NA
    
  }

}

##### Create column for quarter contracts were created and closed
contracts$Start_Qtr<-as.yearqtr(contracts$Start_Date)
contracts$Close_Qtr<-as.yearqtr(contracts$Close_Date)


## Closed contracts cleaning
##### Create days to execute column
contracts$Execute_Days<-round(as.numeric(difftime(as.POSIXct(contracts$Close_Date),as.POSIXct(contracts$Start_Date),units="days")),1)

##### Calculate days per approval stage 
contracts$Legal_Days<-round(as.numeric(difftime(as.POSIXct(contracts$LegalReview),as.POSIXct(contracts$ContractDate),units="days")),1)

contracts$Dep_Attorney_Days<-round(as.numeric(difftime(as.POSIXct(contracts$DepAttorney),as.POSIXct(contracts$LegalReview),units="days")),1)

contracts$CAO_Days<-round(as.numeric(difftime(as.POSIXct(contracts$CAO),as.POSIXct(contracts$DepAttorney),units="days")),1)

contracts$Sent_Vendor_Days<-round(as.numeric(difftime(as.POSIXct(contracts$SentVendor),as.POSIXct(contracts$CAO),units="days")),1)

contracts$Awaiting_Vendor_Days<-round(as.numeric(difftime(as.POSIXct(contracts$ECMS_BackFromVendor),as.POSIXct(contracts$SentVendor),units="days")),1)

contracts$Executive_Signature_Days<-round(as.numeric(difftime(as.POSIXct(contracts$Close_Date),as.POSIXct(contracts$ECMS_BackFromVendor),units="days")),1)

contracts$Executive_Days<-ifelse(!is.na(contracts$AdjustedSignDate) & !is.na(contracts$Executive)
                                          ,round(as.numeric(difftime(as.POSIXct(contracts$Executive),as.POSIXct(contracts$Close_Date),units="days")),1)
                                          ,NA)



#### Parse out contracts requiring a City Council Ordinance as a separate dataset 
ordinance<-contracts[contracts$Ordinance==1,]
contracts<-anti_join(contracts,ordinance,by="PO")


### Plotting

#### Bar charts of days to excute
##### Non-Ordinance days to execute
Days2Execute<-ddply(subset(contracts,!is.na(Execute_Days)),"Close_Qtr",summarise,Sign=mean(Execute_Days))
Days2Execute<-subset(Days2Execute,Close_Qtr>"2012 Q2")
Execution<-ggplot(Days2Execute,aes(x=factor(Close_Qtr),y=Sign))+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Average Days to Execute Contracts by Quarter \n - Non-Ordinance")+
  xlab("Quarters")+ylab("Days")+
  geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
  geom_hline(aes(yintercept=30),colour="#FF0000",linetype=2,size=1.2)+theme(legend.position="Top")+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
print(Execution)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract POs/Days to Execute - Non-Ordinance.png")

##### Ordinance days to execute
Ord_Days2Execute<-ddply(subset(ordinance,!is.na(Execute_Days)),"Close_Qtr",summarise,Sign=mean(Execute_Days))
Ord_Days2Execute<-subset(Ord_Days2Execute,Close_Qtr>"2012 Q2")
Ord_Execution<-ggplot(Ord_Days2Execute,aes(x=factor(Close_Qtr),y=Sign))+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Average Days to Execute Contracts by Quarter \n - Ordinance")+
  xlab("Quarters")+ylab("Days")+
  geom_text(aes(y=Sign,ymax=Sign+1,label=round(Sign,1)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
print(Ord_Execution)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract POs/Days to Execute - Ordinance.png")


#### Stacked bars of days to execute
##### Create bins of day distributions
contracts$Under30<-ifelse(contracts$Execute_Days<=30,1,0)
contracts$Between31_60<-ifelse(contracts$Execute_Days>30 & contracts$Execute_Days<=60,1,0)
contracts$Between61_90<-ifelse(contracts$Execute_Days>60 & contracts$Execute_Days<=90,1,0)
contracts$Between91_120<-ifelse(contracts$Execute_Days>90 & contracts$Execute_Days<=120,1,0)
contracts$Over120<-ifelse(contracts$Execute_Days>120,1,0)
##### Aggregation and melting
con_PO_dist<-select(contracts,Close_Qtr,Under30,Between31_60,Between61_90,Between91_120,Over120)
con_PO_dist<-aggregate(cbind(con_PO_dist$Under30,con_PO_dist$Between31_60,con_PO_dist$Between61_90,con_PO_dist$Between91_120,con_PO_dist$Over120)~Close_Qtr,data=con_PO_dist,FUN=sum);colnames(con_PO_dist)[grepl("V1", colnames(con_PO_dist))] <- "<=30";colnames(con_PO_dist)[grepl("V2", colnames(con_PO_dist))] <- "31-60";colnames(con_PO_dist)[grepl("V3", colnames(con_PO_dist))] <- "61-90";colnames(con_PO_dist)[grepl("V4", colnames(con_PO_dist))] <- "91-120";colnames(con_PO_dist)[grepl("V5", colnames(con_PO_dist))] <- ">120"
con_PO_dist<-subset(con_PO_dist,Close_Qtr>"2012 Q2")
con_PO_dist<-melt(con_PO_dist,id.vars="Close_Qtr",variable.name="Days")
con_PO_dist<-con_PO_dist %>% group_by(Close_Qtr, Days)%>% 
  dplyr::summarise(value = sum(value))%>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
##### Generate plot
Execute_dist_plot<-ggplot(con_PO_dist, aes(x=factor(Close_Qtr),y=percent, fill=Days)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.01, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="black",size=2) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=c("#009900","#FFFFCC","#FFCC99","#FF9999","#FF3300"))+
  labs(y="Distribution", x="Quarter")+
  ggtitle("Distribution of Days to Execute (Non-Ordinance)")+
  theme(plot.title=element_text(size=12,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,size=7))
print(Execute_dist_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract POs/Execute Distribution.png")



#### Law KPI calculation
contracts$KPI_LawDays<-as.numeric(difftime(as.POSIXct(contracts$DepAttorney),as.POSIXct(contracts$ContractDate),units="days"))
contracts$LawUnder14<-ifelse(contracts$KPI_LawDays<=14,1,0)
contracts$Law14_28<-ifelse(contracts$KPI_LawDays>14 & contracts$KPI_LawDays<29,1,0)
contracts$LawOver28<-ifelse(contracts$KPI_LawDays>=29,1,0)
LawKPI<-select(contracts,Close_Qtr,LawUnder14,Law14_28,LawOver28)
LawKPI<-aggregate(cbind(LawKPI$LawUnder14,LawKPI$Law14_28,LawKPI$LawOver28)~Close_Qtr,data=LawKPI,FUN=sum);colnames(LawKPI)[grepl("V1", colnames(LawKPI))] <- "Under14";colnames(LawKPI)[grepl("V2", colnames(LawKPI))] <- "Between14_28";colnames(LawKPI)[grepl("V3", colnames(LawKPI))] <- "Over28"
LawKPI<-subset(LawKPI,Close_Qtr>"2012 Q2")
LawKPI<-melt(LawKPI,id.vars="Close_Qtr",variable.name="Days")
LawKPI<-LawKPI %>% group_by(Close_Qtr, Days) %>% 
  summarise(value = sum(value)) %>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
##### Generate plot
Law_plot<-ggplot(LawKPI, aes(x=factor(Close_Qtr),y=percent, fill=Days)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.01, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="black",size=2) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=c("#009900","#FFCC99","#FF9999"))+
  labs(y="Distribution", x="Quarter")+
  ggtitle("Days to Review and Approve by Law Dept")+
  theme(plot.title=element_text(size=12,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))   
print(Law_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract POs/Law Distribution.png")


#### Days to execute by type
Execute_Type<-aggregate(Execute_Days~Close_Qtr+Type2,data=contracts,mean)
Execute_Type<-getOneYear(Execute_Type,Close_Qtr,first) ### Take last 5 quarters using custom "getOneYear" function
Execute_Type$Execute_Days<-round(Execute_Type$Execute_Days,1)
print(ggplot(Execute_Type, aes(x=factor(Close_Qtr), y=Execute_Days, group=Type2, colour=Type2)) +
  geom_line() +
  geom_hline(aes(yintercept=30,colour="#FF0000"),linetype=2,size=1)+
  ggtitle("Days to Execute by Contract Type")+
  labs(y="Days",x="Quarter")+
  geom_text(aes(label=ifelse(Close_Qtr==r_period,Type2,""),size=2,show_guide=FALSE))+
  theme(legend.position="none",plot.title=element_text(size=10,face="bold",vjust=1)))
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Contract POs/Execute Type.png")


#### Create subsets of data for sending to Law and Executive Counsel
##### Create dataframes of contracts that have been signed, as well as those that were open at the end of the quarter
open_contracts<-contracts[is.na(contracts$Close_Qtr)|contracts$Close_Qtr>as.yearqtr(last),]
closed_contracts<-contracts[contracts$Close_Qtr<=as.yearqtr(last),]

##### Executive Counsel queue to send 
Exec_queue<-select(contracts[is.na(contracts$Executive) & contracts$POStatus=="3PRA" & !is.na(contracts$ECMS_BackFromVendor),]
                   , PO, AltID, Dept, Vendor, Description, ECMS_BackFromVendor) 


### Export data to spreadsheets
write.csv(Exec_queue,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/Exec counsel queue.csv")
write.csv(LawKPI,"O:/Projects/ReqtoCheckStat/Query Files/Output/Law KPI.csv")
write.csv(contracts,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/contracts.csv",na="")
write.csv(narrative,"O:/Projects/ReqtoCheckStat/Query Files/Output/Contract POs/narrative.csv")
