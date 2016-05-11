## This script analyzes the Purchasing Bureau's performance on the conversion of requisitions into purchase orders

### Read, clean, and format data files
headings<-c("Dept","Req","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")
POs<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/ProcurementReqProcessing.csv",col.names=headings,stringsAsFactors=FALSE,skip=3)
ReqStatus<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Req Status.csv",skip=3),Req=REQ_NBR,Status=STATUS)
#Category<-select(read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/PObyCategory.csv",skip=3),Req=REQ_NBR,Descr=DESC_LONG)


### Data cleaning

#### Standardize req number variables in two data sets
POs<-merge(POs,ReqStatus,by="Req",all.x=TRUE)
#Reqs<-merge(Reqs,Category,by="Req",all.x=TRUE)

#### Convert dollar amount column to numeric class for readability
POs$Cost<-as.numeric(sub("\\$","",POs$Cost))

#### Clean out purchase orders that have been cancelled, as well as punch-outs that have created errors.
Exclude<-POs[POs$Vendor=="Independent Stationers" & POs$Cost==30.48|POs$Vendor=="Independent Stationers" & POs$Cost==0|POs$Vendor=="FASTENAL COMPANY" & POs$Cost==0 | POs$Vendor=="Independent Stationers" & POs$Cost==30.44 | POs$Vendor=="Independent Stationers" & POs$Cost==34.80 | POs$Vendor=="Independent Stationers" & POs$Cost==53.88 | POs$Vendor=="Grainger, Inc." & POs$Cost==99.09|POs$Cost==0,]
POs<-anti_join(POs,Exclude,by="Req")

#### Format date and quarter columns as needed
POs$FinanceDate<-as.Date(POs$FinanceDate,"%m/%d/%Y")
POs$POdate<-as.Date(POs$POdate,"%m/%d/%Y")
#POs<-filter(POs,POdate>FinanceDate)
POs$Qtr<-as.yearqtr(POs$POdate,format="%m/%d/%Y")

#### Calculate business days (this relies on NOLA_calendar read from github)
POs$WorkingDays<-bizdays(POs$FinanceDate,POs$POdate,NOLA_calendar)
POs$WorkingDays<-POs$WorkingDays+1 ##### Adjust calculation up one day, as bizdays function calculates 1 less day than Excel's parallel formula, networkdays 

#### Create distribution bins for business days to process
POs$Under4<-ifelse(POs$WorkingDays<=4,1,0)
POs$Over4<-ifelse(POs$WorkingDays<=4,0,1)


### Plotting

#### Plot the business days to process by quarter
Days2PO<-cbind(aggregate(data=POs,WorkingDays~Qtr,FUN=mean),select(aggregate(data=POs,Req~Qtr,FUN=length),-Qtr,Count=Req))                    
Purchasing<-ggplot(Days2PO,aes(x=factor(Qtr),y=WorkingDays))+
    geom_bar(stat="identity",fill="steelblue")+
      ggtitle("Average Business Days to Process Purchase Orders")+
          xlab("Quarters")+ylab("Business Days")+
              geom_text(aes(y=WorkingDays,ymax=WorkingDays,label=round(WorkingDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
                geom_hline(aes(yintercept=4,colour="#FF0000"),linetype=2,size=1)+
                  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))   
print(Purchasing)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Procurement/Days to PO.png")

#### Plot the distribution percentages of business days to process by quarter
POdist<-select(POs,Qtr,Under4,Over4)
POdist<-aggregate(cbind(POdist$Under4,POdist$Over4)~Qtr,data=POdist,FUN=sum);colnames(POdist)[grepl("V1", colnames(POdist))] <- "<=4";colnames(POdist)[grepl("V2", colnames(POdist))] <- ">4"
POdist<-subset(POdist,Qtr>"2012 Q4")
POdist<-melt(POdist,id.vars="Qtr",variable.name="Days")
POdist<-POdist %>% group_by(Qtr, Days) %>% 
  summarise(value = sum(value)) %>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
##### Generate plot
PO_dist_plot<-ggplot(POdist, aes(x=factor(Qtr),y=percent, fill=Days)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.01, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="black",size=4) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=c("#009900","#FFFFCC"))+
  labs(y="Days", x="Quarter")+
  ggtitle("Distribution of Business Days to \n Process Purchase Orders")+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))   
print(PO_dist_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Procurement/PO Distribution.png")

#### Plot business days to process by Buyer
r_period<-max(POs$Qtr)
POs$Buyer2<-ifelse(POs$Buyer=="Bernice Ealy","Ealy",
                    ifelse(POs$Buyer=="Kai Wells","Wells",
                           ifelse(POs$Buyer=="Burma Jackson","Jackson",
                                  ifelse(POs$Buyer=="Stephanie Warren","Warren",
                                         ifelse(POs$Buyer=="Thersa C Kendrick","Kendrick",NA)))))
Buyers<-aggregate(WorkingDays~Qtr+Buyer2,data=POs,mean);Buyers$WorkingDays<-round(Buyers$WorkingDays,2)
Buyers<-subset(Buyers,Qtr>"2012 Q4")
Buyer_Plot<-ggplot(Buyers,aes(x=factor(Qtr),y=WorkingDays,group=Buyer2,color=factor(Buyer2)))+
    geom_line(stat="identity",size=1)+
      geom_hline(aes(yintercept=4,colour="#FF0000"),linetype=2,size=1)+
        ggtitle("Business Days to Process POs per Buyer")+
          ylab("Business Days")+xlab("Quarter")+
            geom_text(aes(label=ifelse(Qtr==r_period,Buyer2,""),show_guide=FALSE))+
                theme(legend.position="none",plot.title=element_text(size=13,face="bold",vjust=1))              
print(Buyer_Plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Procurement/Buyer Plot.png")

#### Export datasets to respective output folder
write.csv(POs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Procurement/POs.csv")
write.csv(Buyers,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Procurement/Buyers.csv")

## Export KPI data table to STAT KPI folder
write.csv(Days2PO,"O:/Projects/STAT KPIs/ReqtoCheckSTAT/Days to PO.csv")
