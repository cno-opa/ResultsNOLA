# Bids, RFPs, and DBE data

## Read DBE dataset and Bids/RFPs data from share drive
#setInternet2(TRUE)
#download.file("https://data.nola.gov/api/views/8xef-4m72/rows.csv?accessType=DOWNLOAD","O:/Projects/ReqtoCheckStat/Query Files/DBE.csv")
DBE<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/DBE.csv")  
Bids_RFPs<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/Bids_RFPs.csv")

### Data cleaning

#### Create quarter variable
Bids_RFPs$Qtr<-as.yearqtr(Bids_RFPs$EndMonth,"%m/%d/%Y")
Bids_RFPs$First_Qtr<-as.yearqtr(Bids_RFPs$FirstMonth,"%m/%d/%Y")

#### Create distribution bins of bids and proposals
Bids_RFPs$BidUnder3<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses<=2,1,0)
    Bids_RFPs$Bid3_4<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>2 & Bids_RFPs$Responses<5,1,0)
    Bids_RFPs$Bid5_6<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>4 & Bids_RFPs$Responses<7,1,0)
    Bids_RFPs$Bid7_8<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>6 & Bids_RFPs$Responses<9,1,0)
    Bids_RFPs$Bid9More<-ifelse(Bids_RFPs$Type=="Bid" & Bids_RFPs$Responses>=9,1,0)
Bids_RFPs$RFPUnder3<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses<=2,1,0)
    Bids_RFPs$RFP3_4<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>2 & Bids_RFPs$Responses<5,1,0)
    Bids_RFPs$RFP5_6<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>4 & Bids_RFPs$Responses<7,1,0)
    Bids_RFPs$RFP7_8<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>6 & Bids_RFPs$Responses<9,1,0)
    Bids_RFPs$RFP9More<-ifelse(Bids_RFPs$Type=="RFP/Q" & Bids_RFPs$Responses>=9,1,0)


### Plotting - Averages

#### Plot the average bid responses per quarter
Bidresponse<-subset(Bids_RFPs,Type=="Bid")
Bidresponse<-aggregate(data=Bidresponse,Responses~Qtr,FUN=mean)
Bidresponse_plot<-ggplot(Bidresponse,aes(x=factor(Qtr),y=Responses))+
  geom_bar(stat="identity",fill="steelblue")+
    ggtitle("Average Bid Responses per Bid")+
      xlab("Quarters")+ylab("Number of Bids")+
        geom_text(aes(y=Responses,ymax=Responses+1,label=round(Responses,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
           theme(plot.title=element_text(size=14,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(Bidresponse_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Bids-RFPs-DBEs/Responses per Bid.png")

#### Plot the average proposals per quarter
RFPresponse<-subset(Bids_RFPs,Type=="RFP/Q")
RFPresponse<-aggregate(data=RFPresponse,Responses~Qtr,FUN=mean)
RFPresponse_plot<-ggplot(RFPresponse,aes(x=factor(Qtr),y=Responses))+
  geom_bar(stat="identity",fill="steelblue")+
    ggtitle("Average Proposals per RFP")+
      xlab("Quarters")+ylab("Number of Proposals")+
        geom_text(aes(y=Responses,ymax=Responses+1,label=round(Responses,2)),position=position_dodge(width=0.9),vjust=-.5,size=5)+
          theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25,face="bold"))   
print(RFPresponse_plot)
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Bids-RFPs-DBEs/Proposals per RFP.png")


### Plotting - Distributions

#####  Bid response distribution plot
Bid_dist<-subset(Bids_RFPs,Type=="Bid")
Bid_dist<-select(Bid_dist,Qtr,BidUnder3,Bid3_4,Bid5_6,Bid7_8,Bid9More)
Bid_dist<-aggregate(cbind(Bid_dist$BidUnder3, Bid_dist$Bid3_4, Bid_dist$Bid5_6,Bid_dist$Bid7_8, Bid_dist$Bid9More)~Qtr,data=Bid_dist,FUN=sum);colnames(Bid_dist)[grepl("V1", colnames(Bid_dist))] <- "<=3";colnames(Bid_dist)[grepl("V2", colnames(Bid_dist))] <- "3-4";colnames(Bid_dist)[grepl("V3", colnames(Bid_dist))] <- "5-6";colnames(Bid_dist)[grepl("V4", colnames(Bid_dist))] <- "7-8";colnames(Bid_dist)[grepl("V5", colnames(Bid_dist))] <- ">9"
Bid_dist<-melt(Bid_dist,id.vars="Qtr",variable.name="Days")
Bid_dist<-Bid_dist %>% group_by(Qtr, Days) %>% 
  summarise(value = sum(value)) %>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
##### Generate plot
ggplot(Bid_dist, aes(x=factor(Qtr),y=percent, fill=Days)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.01, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="black",size=4) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=c("#FF9999","#FFFFCC","#99FF99","#33CC00","#009900"))+
  labs(y="Distribution", x="Quarter")+
  ggtitle("Responses Received per Bid")+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Bids-RFPs-DBEs/Bid Distribution.png")


#### RFP response distribution plot
RFP_dist<-subset(Bids_RFPs,Type=="RFP/Q")
RFP_dist<-select(RFP_dist,Qtr,RFPUnder3,RFP3_4,RFP5_6,RFP7_8,RFP9More)
RFP_dist<-aggregate(cbind(RFP_dist$RFPUnder3, RFP_dist$RFP3_4, RFP_dist$RFP5_6,RFP_dist$RFP7_8, RFP_dist$RFP9More)~Qtr,data=RFP_dist,FUN=sum);colnames(RFP_dist)[grepl("V1", colnames(RFP_dist))] <- "Under3";colnames(RFP_dist)[grepl("V2", colnames(RFP_dist))] <- "3-4";colnames(RFP_dist)[grepl("V3", colnames(RFP_dist))] <- "5-6";colnames(RFP_dist)[grepl("V3", colnames(RFP_dist))] <- "7-8";colnames(RFP_dist)[grepl("V4", colnames(RFP_dist))] <- ">9"
### Melt data frame
RFP_dist<-melt(RFP_dist,id.vars="Qtr",variable.name="Days")
RFP_dist<-RFP_dist %>% group_by(Qtr, Days) %>% 
  summarise(value = sum(value)) %>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
##### Generate plot
ggplot(RFP_dist, aes(x=factor(Qtr),y=percent, fill=Days)) +
  geom_bar(stat='identity',  width = .7, colour="black", lwd=0.1) +
  geom_text(aes(label=ifelse(percent >= 0.01, paste0(sprintf("%.0f", percent*100),"%"),""),
                y=pos), colour="black",size=4) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values=c("#FF9999","#FFFFCC","#99FF99","#33CC00","#009900"))+
  labs(y="Distribution", x="Quarter")+
  ggtitle("Proposals Received per RFP")+
  theme(plot.title=element_text(size=13,face="bold",vjust=1),panel.background=element_blank(),axis.text.x=element_text(angle=45,hjust=0.25))
ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Bids-RFPs-DBEs/RFP Distribution.png")

############################################################################################################################

## Clean data and create linear regression model on the effects of the number of bid/RFP responses on contract value, 
##as compiled from Purchasing and Supplier Diversity (UNDER CONSTRUCTION)

###
#Bids_RFP_lm<-select(Bids_RFPs,Number,Responses)

###
#BidValues<-merge(DBE,Bids_RFP_lm,by="Number",all=TRUE)

### 
#BidValues<-filter(BidValues,!is.na(Responses))

### Coerce contract value variable into numeric class
#BidValues$Estimated_Contract_Value<-gsub("\\$","",BidValues$Estimated_Contract_Value)
#class(BidValues$Estimated_Contract_Value)<-"numeric"

### Plot bids
#BidValues<-select(BidResponse,Number,Value=Estimated_Contract_Value,Responses)
#BidValues<-filter(BidValues,!is.na(Value))
#BidValues$Val_log<-log(BidValues$Value)

### Create regression plot
#Bidvalue_plot<-ggplot(BidValues,aes(x=Val_log,y=Responses))+
#  geom_point(shape=1)+
 # geom_smooth(method=lm,
#              se=TRUE)+
#  ggtitle("Regression of the effect of the Number of Responses on Contract Values")+
#  facet_grid(facets=.~Stage)+
#  geom_text(x=16,y=10,label=lm_eqn(lm(Value~Responses,BidValues)),parse=TRUE)+ ### lm_eqn is a custom function that adds regression equation and Rsquared to plot
#  xlab("Contract Value(log)")+ylab("Responses")
#print(Bidvalue_plot)
#ggsave("O:/Projects/ReqtoCheckStat/Query Files/Slides/Bids-RFPs-DBEs/BidResponse-ContractValue Regression.png")

### Create linear regression model of the effect of the number of responses on the estimated contract value
#Bid_Model<-lm(Value~Responses,BidValues)
