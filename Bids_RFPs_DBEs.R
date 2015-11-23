# Bids, RFPs, and DBE data

## Read DBE dataset and Bids/RFPs data from share drive
#setInternet2(TRUE)
#download.file("https://data.nola.gov/api/views/8xef-4m72/rows.csv?accessType=DOWNLOAD","O:/Projects/ReqtoCheckStat/Query Files/DBE.csv")
DBE<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/DBE.csv")  
Bids_RFPs<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/Bids-RFPs.csv")

### 

### Create quarter variable
Bids_RFPs$Qtr<-as.yearqtr(Bids_RFPs$EndMonth,"%m/%d/%Y")
Bids_RFPs$First_Qtr<-as.yearqtr(Bids_RFPs$FirstMonth,"%m/%d/%Y")

## Create plot of the bids opened, closed, and open at the end of each quarter
Bid_Flow<-subset(Bids_RFPs,Type=="Bid")
Bid_First_Summary<-data.frame(table(Bid_Flow$First_Qtr))
Bid_End_Summary<-data.frame(table(Bid_Flow$Qtr))
Bid_First_Summary<-rename(Bid_First_Summary,Qtr=Var1,Opened=Freq)
Bid_End_Summary<-rename(Bid_End_Summary,Qtr=Var1,Closed=Freq)
summaryBid_merge<-merge(Bid_First_Summary,Bid_End_Summary,by="Qtr",all=TRUE);summaryBid_merge$Qtr<-as.yearqtr(summaryBid_merge$Qtr)
summaryBid_merge$cumulative_opened<-cumsum(summaryBid_merge$Opened)
summaryBid_merge$cumulative_closed<-cumsum(summaryBid_merge$Closed)
summaryBid_merge$Open_EndofQtr<-summaryBid_merge$cumulative_opened-summaryBid_merge$cumulative_closed
summaryBid_melt<-select(summaryBid_merge,Qtr,Opened,Closed,Open_EndofQtr)
summaryBid_melt<-subset(summaryBid_melt,Qtr>"2013 Q2")
summaryBid_melt<-melt(summaryBid_melt,id.vars="Qtr")
Open_closedBidplot<-ggplot(summaryBid_melt,aes(x=factor(Qtr),y=value,fill=variable))+
  geom_bar(subset=.(variable=="Opened"|variable=="Closed"),width=0.8,aes(fill=variable),position="dodge",stat="identity")+
  ggtitle("Bids Opened,Closed, and in Queue by Quarter")+xlab("Quarters")+ylab("Number of Bids")+
  geom_line(subset=.(variable=="Open_EndofQtr"),aes(fill=variable,group=variable),size=1)+
  geom_text(aes(y=value,ymax=value,label=value),position=position_dodge(width=0.7),size=4)
print(Open_closedBidplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/Opened_Closed_In Queue_Bids.png")

## Create plot of the RFPs opened, closed, and open at the end of each quarter
RFP_Flow<-subset(Bids_RFPs,Type=="RFP/Q")
RFP_First_summary<-data.frame(table(RFP_Flow$First_Qtr))
RFP_End_summary<-data.frame(table(RFP_Flow$Qtr))
RFP_First_summary<-rename(RFP_First_summary,Qtr=Var1,Opened=Freq)
RFP_End_summary<-rename(RFP_End_summary,Qtr=Var1,Closed=Freq)
summaryRFP_merge<-merge(RFP_First_summary,Qtr_End_summary,by="Qtr",all=TRUE);summaryRFP_merge$Qtr<-as.yearqtr(summaryRFP_merge$Qtr)
summaryRFP_merge$cumulative_opened<-cumsum(summaryRFP_merge$Opened)
summaryRFP_merge$cumulative_closed<-cumsum(summaryRFP_merge$Closed)
summaryRFP_merge$Open_EndofQtr<-summaryRFP_merge$cumulative_opened-summaryRFP_merge$cumulative_closed
summaryRFP_melt<-select(summary_merge,Qtr,Opened,Closed,Open_EndofQtr)
summaryRFP_melt<-subset(summary_melt,Qtr>"2012 Q4")
summaryRFP_melt<-melt(summary_melt,id.vars="Qtr")
Open_closedRFPplot<-ggplot(summaryRFP_melt,aes(x=factor(Qtr),y=value,fill=variable))+
  geom_bar(subset=.(variable=="Opened"|variable=="Closed"),width=0.8,aes(fill=variable),position="dodge",stat="identity")+
  ggtitle("RFPs Opened,Closed, and in Queue by Quarter")+xlab("Quarters")+ylab("Number of RFPs")+
  geom_line(subset=.(variable=="Open_EndofQtr"),aes(fill=variable,group=variable),size=1)+
  geom_text(aes(y=value,ymax=value,label=value),position=position_dodge(width=0.7),size=4)
print(Open_closedRFPplot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/Opened_Closed_In Queue_RFPs.png")

### Create distribution bins of bids and proposals
Bids_RFPs$BidUnder3<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses<=2,1,0)
    Bids_RFPs$Bid3_6<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>2 & Bids_RFPs$Responses<=6,1,0)
    Bids_RFPs$Bid7_9<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>6 & Bids_RFPs$Responses<=9,1,0)
    Bids_RFPs$BidOver10<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>=10,1,0)
Bids_RFPs$RFPUnder3<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses<=2,1,0)
    Bids_RFPs$RFP3_6<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>2 & Bids_RFPs$Responses<=6,1,0)
    Bids_RFPs$RFP7_9<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>6 & Bids_RFPs$Responses<=9,1,0)
    Bids_RFPs$RFPOver10<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>=10,1,0)

### Plot the average bid responses per quarter
Bidresponse<-subset(Bids_RFPs,Type=="Bid")
Bidresponse<-aggregate(data=Bidresponse,Responses~Qtr,FUN=mean)
Bidresponse_plot<-ggplot(Bidresponse,aes(x=factor(Qtr),y=Responses))
Bidresponse_plot<-Bidresponse_plot+geom_bar(stat="identity",fill="steelblue")
Bidresponse_plot<-Bidresponse_plot+ggtitle("Average Bid Responses per Bid")
Bidresponse_plot<-Bidresponse_plot+xlab("Quarters")
Bidresponse_plot<-Bidresponse_plot+ylab("Number of Bids")
Bidresponse_plot<-Bidresponse_plot+geom_text(aes(y=Responses,ymax=Responses+1,label=round(Responses,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
Bidresponse_plot<-Bidresponse_plot+geom_hline(aes(yintercept=3,colour="#FF0000"),linetype=2,size=1)
print(Bidresponse_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/Responses per Bid.png")

### Plot the average proposals per quarter
RFPresponse<-subset(Bids_RFPs,Type=="RFP/Q")
RFPresponse<-aggregate(data=RFPresponse,Responses~Qtr,FUN=mean)
RFPresponse_plot<-ggplot(RFPresponse,aes(x=factor(Qtr),y=Responses))
RFPresponse_plot<-RFPresponse_plot+geom_bar(stat="identity",fill="steelblue")
RFPresponse_plot<-RFPresponse_plot+ggtitle("Average Proposals per RFP")
RFPresponse_plot<-RFPresponse_plot+xlab("Quarters")
RFPresponse_plot<-RFPresponse_plot+ylab("Number of Proposals")
RFPresponse_plot<-RFPresponse_plot+geom_text(aes(y=Responses,ymax=Responses+1,label=round(Responses,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)
RFPresponse_plot<-RFPresponse_plot+geom_hline(aes(yintercept=3,colour="#FF0000"),linetype=2,size=1)
print(RFPresponse_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/Proposals per RFP.png")

################
## Subset and aggregate for bids, to prepare to plot distribution
Bid_dist<-subset(Bids_RFPs,Type=="Bid")
Bid_dist<-select(Bid_dist,Qtr,BidUnder3,Bid3_6,Bid7_9,BidOver10)
Bid_dist<-aggregate(cbind(Bid_dist$BidUnder3, Bid_dist$Bid3_6, Bid_dist$Bid7_9, Bid_dist$BidOver10)~Qtr,data=Bid_dist,FUN=sum);colnames(Bid_dist)[grepl("V1", colnames(Bid_dist))] <- "Under3";colnames(Bid_dist)[grepl("V2", colnames(Bid_dist))] <- "Between3_6";colnames(Bid_dist)[grepl("V3", colnames(Bid_dist))] <- "Between7_9";colnames(Bid_dist)[grepl("V4", colnames(Bid_dist))] <- "Over10"

### Calculate percentage of each bin
Bid_dist$Total<-Bid_dist$Under3+Bid_dist$Between3_6+Bid_dist$Between7_9+Bid_dist$Over10 ###Create column of total bids per quarter
    Bid_dist$Under3<-round(Bid_dist$Under3/Bid_dist$Total,3) ### Divide bin by total to get quarterly percentage 
    Bid_dist$Between3_6<-round(Bid_dist$Between3_6/Bid_dist$Total,3) ### Divide bin by total to get quarterly percentage 
    Bid_dist$Between7_9<-round(Bid_dist$Between7_9/Bid_dist$Total,3) ### Divide bin by total to get quarterly percentage 
    Bid_dist$Over10<-round(Bid_dist$Over10/Bid_dist$Total,3) ### Divide bin by total to get quarterly percentage 
###
Bid_dist<-select(Bid_dist,Qtr,Under3,Between3_6,Between7_9,Over10)
##Define data label positions (heights) for distribution
undermaxbid<-max(Bid_dist$Under3); 
min4_6<-min(Bid_dist$Between4_6)+undermaxbid;
min7_9<-min(Bid_dist$Between7_9)+min4_6;
overmin_bid<-min(Bid_dist$Over10)+min7_9
### Melt data frame
Bid_dist<-melt(Bid_dist,id.vars="Qtr")
### Calculate data label position height
Bid_dist$position<-ifelse(Bid_dist$variable=="Under3",Bid_dist$value-.03,
                           ifelse(Bid_dist$variable=="Between4_6",sum(undermaxbid, min4_6)/2,
                                  ifelse(Bid_dist$variable=="Between7_9",sum(min4_6,min7_9)/2,.99))) 
### Plot bids
Bid_dist_plot<-ggplot(Bid_dist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Bid Responses per Bid")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
  scale_fill_manual(values=c(red,darkBlue,lightBlue,"green"),name=" ",labels=c("<=3 Bids","4-6 Bids","7-9 Bids","<=10 Bids"))
print(Bid_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/Bid Distribution.png")

#############
## Subset and aggregate for bids, to prepare to plot distribution
RFP_dist<-subset(Bids_RFPs,Type=="RFP/Q")
RFP_dist<-select(RFP_dist,Qtr,RFPUnder3,RFP3_6,RFP7_9,RFPOver10)
RFP_dist<-aggregate(cbind(RFP_dist$RFPUnder3, RFP_dist$RFP3_6, RFP_dist$RFP7_9, RFP_dist$RFPOver10)~Qtr,data=RFP_dist,FUN=sum);colnames(RFP_dist)[grepl("V1", colnames(RFP_dist))] <- "Under3";colnames(RFP_dist)[grepl("V2", colnames(RFP_dist))] <- "Between3_6";colnames(RFP_dist)[grepl("V3", colnames(RFP_dist))] <- "Between7_9";colnames(RFP_dist)[grepl("V4", colnames(RFP_dist))] <- "Over10"

### Calculate percentage of each bin
RFP_dist$Total<-RFP_dist$Under3+RFP_dist$Between3_6+RFP_dist$Between7_9+RFP_dist$Over10 ###Create column of total bids per quarter
RFP_dist$Under3<-round(RFP_dist$Under3/RFP_dist$Total,3) ### Divide bin by total to get quarterly percentage 
RFP_dist$Between3_6<-round(RFP_dist$Between3_6/RFP_dist$Total,3) ### Divide bin by total to get quarterly percentage 
RFP_dist$Between7_9<-round(RFP_dist$Between7_9/RFP_dist$Total,3) ### Divide bin by total to get quarterly percentage 
RFP_dist$Over10<-round(RFP_dist$Over10/RFP_dist$Total,3) ### Divide bin by total to get quarterly percentage 
###
RFP_dist<-select(RFP_dist,Qtr,Under3,Between3_6,Between7_9,Over10)
##Define data label positions (heights) for distribution
undermaxRFP<-max(RFP_dist$Under3); 
min4_6<-min(RFP_dist$Between4_6)+undermaxRFP;
min7_9<-min(RFP_dist$Between7_9)+min4_6;
overmin_bid<-min(RFP_dist$Over10)+min7_9
### Melt data frame
RFP_dist<-melt(RFP_dist,id.vars="Qtr")
### Calculate data label position height
RFP_dist$position<-ifelse(RFP_dist$variable=="Under3",RFP_dist$value-.03,
                          ifelse(RFP_dist$variable=="Between4_6",sum(undermaxbid, min4_6)/2,
                                 ifelse(RFP_dist$variable=="Between7_9",sum(min4_6,min7_9)/2,.99))) 
### Plot RFPs
RFP_dist_plot<-ggplot(RFP_dist,aes(x = factor(Qtr), y = value,fill = variable)) + 
  geom_bar(position = "stack",stat = "identity") + 
  scale_y_continuous(labels = percent_format())+
  ggtitle("Distribution of Proposals per RFP")+
  xlab("Quarters")+ylab("Percent")+
  geom_text(aes(ymax=value,y=position,label=percent(value)),size=4)+
  scale_fill_manual(values=c(red,darkBlue,lightBlue,"green"),name=" ",labels=c("<=3 Proposals","4-6 Proposals","7-9 Proposals","<=10 Proposals"))
print(RFP_dist_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/RFP Distribution.png")
############################################################################################################################

## Clean data and create linear regression model on the effects of the number of bid/RFP responses on contract value, 
##as compiled from Purchasing and Supplier Diversity 

###
Bids_RFPs<-select(Bids_RFPs,Number,Responses)

###
BidValues<-merge(DBE,Bids_RFPs,by="Number",all=TRUE)

### 
BidValues<-filter(BidValues,!is.na(Responses))

### Coerce contract value variable into numeric class
BidValues$Estimated_Contract_Value<-gsub("\\$","",BidValues$Estimated_Contract_Value)
class(BidValues$Estimated_Contract_Value)<-"numeric"

### Plot bids
BidValues<-select(BidResponse,Number,Value=Estimated_Contract_Value,Responses)
BidValues<-filter(BidValues,!is.na(Value))
BidValues$Val_log<-log(BidValues$Value)

### Create regression plot
Bidvalue_plot<-ggplot(BidValues,aes(x=Val_log,y=Responses))+
  geom_point(shape=1)+
  geom_smooth(method=lm,
              se=TRUE)+
  ggtitle("Regression of the effect of the Number of Responses on Contract Values")+
  facet_grid(facets=.~Stage)+
  geom_text(x=16,y=10,label=lm_eqn(lm(Value~Responses,BidValues)),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
  xlab("Contract Value(log)")+ylab("Responses")
print(Bidvalue_plot)
ggsave("./ReqtoCheckSTAT/Query Files/Slides/Bids-RFPs-DBEs/BidResponse-ContractValue Regression.png")

### Create linear regression model of the effect of the number of responses on the estimated contract value
Bid_Model<-lm(Value~Responses,BidValues)
