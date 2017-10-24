Invoice <-  loadDataset("Invoice", refresh_feather = TRUE)
Receipt <- loadDataset("Receipt", refresh_feather = TRUE)

### This is a script to analyze general fund payment processing at Accounts Payable, 
### measuring the time between most recent "receipt" created in Buyspeed by a department,
### and the final approval by an Accounts Payable staffer in Buyspeed

#### Create consolidated Buyspeed payment dataset
Payments<-Invoice%>%
  left_join(Receipt,by="PO")

#### Group payment data by invoice number, and sort so the most recent department receipt date is at the top
Payments<-group_by(Payments,Invoice)%>%
  arrange(desc(ReceiptDate))

### De-duplicate payments to only include the most recent departmental receipt date
Payments<-Payments[!duplicated(Payments$Invoice),]

### Remove payments for which the Accounts Payable approval was after dept receipt date
Payments<-Payments[Payments$ReceiptDate<=Payments$AP_Process_Date,]

#### Subset for payments made after the reporting period and before 2013
Payments<-filter(Payments,AP_Process_Date>"2012-12-31")
Payments<-Payments[as.Date(Payments$AP_Process_Date)<=last,]

### Code quarter variable based on date of Accounts Payable processing
Payments$Qtr<-as.yearqtr(Payments$AP_Process_Date)


### calculate business days from department Buyspeed receipt to final Buyspeed approval by Accounts Payable staff
Payments$APWorkingDays<-bizdays(as.Date(as.character(Payments$ReceiptDate),"%Y-%m-%d %H:%M:%S"),as.Date(as.character(Payments$AP_Process_Date),"%Y-%m-%d %H:%M:%S"),NOLA_calendar)
Payments$APWorkingDays<-Payments$APWorkingDays+1 ##### Adjust calculation up one day, as bizdays function calculates 1 less day than Excel's parallel formula, networkdays 

#### Segment AP processing days into bins
Payments$APUnder7<-ifelse(Payments$APWorkingDays<=7,1,0)
Payments$AP7_14<-ifelse(Payments$APWorkingDays>7 & Payments$APWorkingDays<=14,1,0)
Payments$APOver14<-ifelse(Payments$APWorkingDays>14,1,0)



### Plotting
Payments <- Payments 

#### Plot days to process by Accounts Payable
APWorkingDays <- aggregate(data=Payments,
                           APWorkingDays~Qtr,
                           FUN=mean) 

second_one <- select(aggregate(data=Payments,
                               Invoice~Qtr,
                               FUN=length),
                     -Qtr,
                     Count=Invoice) 

Days2Payment<- APWorkingDays %>%
  cbind(second_one) 


#### Plot distribution of days to process by Accounts Payable
APdist<-select(Payments,Qtr,APUnder7,AP7_14,APOver14)
APdist<-aggregate(cbind(APdist$APUnder7,APdist$AP7_14,APdist$APOver14)~Qtr,data=APdist,FUN=sum);colnames(APdist)[grepl("V1", colnames(APdist))] <- "Under7";colnames(APdist)[grepl("V2", colnames(APdist))] <- "Between7_14";colnames(APdist)[grepl("V3", colnames(APdist))] <- "Over14"
APdist<-melt(APdist,id.vars="Qtr",variable.name="Days")
APdist<-APdist %>% group_by(Qtr, Days) %>% 
  summarise(value = sum(value)) %>%   # Within each quarter, sum all values in each bin of days
  mutate(percent = value/sum(value),
         pos = cumsum(percent) - 0.5*percent)
