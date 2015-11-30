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
Days2PO<-aggregate(data=POs,WorkingDays~Qtr,FUN=mean)
Purchasing<-ggplot(Days2PO,aes(x=factor(Qtr),y=WorkingDays))+
    geom_bar(stat="identity",fill="steelblue")+
      ggtitle("Average Business Days to Process Purchase Orders")+
          xlab("Quarters")+ylab("Business Days")+
              geom_text(aes(y=WorkingDays,ymax=WorkingDays+1,label=round(WorkingDays,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
                geom_hline(aes(yintercept=4,colour="#FF0000"),linetype=2,size=1)+
                  theme(plot.title=element_text(size=13,face="bold",vjust=1))   
print(Purchasing)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Procurement/Days to PO.png")

#### Plot the distribution percentages of business days to process by quarter
POdist<-select(POs,Qtr,Under4,Over4)
POdist<-aggregate(cbind(POdist$Under4,POdist$Over4)~Qtr,data=POdist,FUN=sum);colnames(POdist)[grepl("V1", colnames(POdist))] <- "Under4";colnames(POdist)[grepl("V2", colnames(POdist))] <- "Over4"
POdist<-subset(POdist,Qtr>"2012 Q4")
  POdist$Total<-POdist$Under4+POdist$Over4
      POdist$Under_4<-round(POdist$Under4/POdist$Total,3)
          POdist$Over_4<-round(POdist$Over4/POdist$Total,3)
POdist<-select(POdist,Qtr,Under_4,Over_4)
    undermaxPO<-max(POdist$Under_4)
POdist<-melt(POdist,id.vars="Qtr")
POdist$position<-ifelse(POdist$variable=="Under_4",undermaxPO-.10,1-((1-undermaxPO)/2)) # calculate height of data labels
Dist_plot<-ggplot(POdist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
    scale_y_continuous(labels = percent_format())+
       ggtitle("Distribution of Business Days to \nProcess Purchase Orders")+
          xlab("Quarters")+ylab("Percent")+
             geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
                 scale_fill_manual(values=c(lightBlue,red),name=" ",labels=c("<=4 Business Days",">4 Business Days"))
print(Dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Procurement/PO Distribution.png")


#### Plot business days to process by Buyer
r_period<-max(POs$Qtr)
POs$Buyer2<-ifelse(POs$Buyer=="Bernice Ealy","Ealy",
                    ifelse(POs$Buyer=="Kai Wells","Wells",
                           ifelse(POs$Buyer=="Burma Jackson","Jackson",
                                  ifelse(POs$Buyer=="Stephanie Warren","Warren",NA))))
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
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Procurement/Buyer Plot.png")

#### Export the data
write.csv(POs,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Procurement/POs.csv")
write.csv(Buyers,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Procurement/Buyers.csv")
