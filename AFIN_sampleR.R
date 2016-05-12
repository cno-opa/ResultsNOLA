
AFIN_raw<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/AFIN (use for scripting).csv")

AFIN<-
AFIN<-AFIN_raw[!grepl("[A-z]",AFIN$CHECK_NUMBER),]

### Eliminate rows with vendors that should be excluded
AFIN<-AFIN_[!grepl("CITY OF NEW ORLEANS",AFIN$Vendor),]
AFIN<-AFIN[!grepl("LIQUIDATION",AFIN$Vendor),]
AFIN<-AFIN[!grepl("RETIREME",AFIN$Vendor),]
AFIN<-AFIN[!grepl("GOING PLACES TRAVEL",AFIN$Vendor),]

### Eliminate rows with funds that should be excluded
AFIN<-AFIN[AFIN$FUND!="100",]
AFIN<-AFIN[AFIN$FUND!="112",]

### Eliminate rows with transaction types that should be excluded
AFIN<-AFIN[AFIN$TRANSID!="CX",]
AFIN<-AFIN[AFIN$TRANSID!="MW",]

### Eliminate duplicated checks
AFIN<-AFIN[!duplicated(AFIN$Check),]

### Re-code check date column into "Date" class 
AFIN$Check_Date<-AFIN$DATE<-as.Date(as.factor(AFIN$Check_Date),"%Y%m%d")

### Pull out 290 random checks, which should be well within a 5% margin of error
Samp_AFIN <- AFIN[sample(nrow(AFIN), 290), ]

#### Write CSV's
write.csv(Samp_AFIN,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Payments/Samp_AFIN.csv")

names(AFIN_raw)<-c("Org","FUND","TRANSID","x","Check","Vendor","Check_Date","inv"