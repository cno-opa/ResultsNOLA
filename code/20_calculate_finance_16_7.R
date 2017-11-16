source('code/00_load_dependencies.R')

headings<-c("Dept","Req","FinanceDate","POnumber","POdate","Cost","Vendor","PrintDate","BuyerInitials","Buyer","WorkingDays")

POs1 <- read_csv("data/source/2017_q3/ProcurementReqProcessing.csv",
                 col_names=headings, skip=3)

ReqStatus <- read_csv("data/source/2017_q3/RequsitionStatus.csv",skip=3) %>%
  select(Req=REQ_NBR,Status=STATUS)

### Data cleaning

POs <- POs1 %>%
  left_join(ReqStatus, "Req") %>%
  mutate(Cost = as.numeric(sub("\\$","",Cost)))

Exclude <- POs %>%
  mutate( condition =  (Vendor=="Independent Stationers" & Cost==30.48) | 
            (Vendor=="Independent Stationers" & Cost==0) |
            (Vendor=="FASTENAL COMPANY" & Cost==0 ) |
            (Vendor=="Independent Stationers" & Cost==30.44) |
            (Vendor=="Independent Stationers" & Cost==34.80) |
            (Vendor=="Independent Stationers" & Cost==53.88) |
            (Vendor=="Grainger, Inc." & Cost==99.09) |
            Cost== 0
  ) %>%
  filter(condition | is.na(condition)) %>%
  select(-condition)

POs<-anti_join(POs,Exclude,"Req") %>%
  mutate(FinanceDate = as.Date(FinanceDate,"%m/%d/%Y"),
         POdate = as.Date(POdate,"%m/%d/%Y"),
         Qtr = as.yearqtr(POdate,format="%m/%d/%Y"))

#### Calculate business days (this relies on NOLA_calendar read from github)

POs <- POs %>%
  mutate(WorkingDays = bizdays(FinanceDate,POdate,NOLA_calendar) + 1)

POs <- POs %>%
  mutate(Under4 = ifelse(WorkingDays<=4,1,0),
         Over4 = ifelse(WorkingDays<=4,0,1))

avg_working_days <- aggregate(data=POs,WorkingDays~Qtr,FUN=mean)

number_working_days <- aggregate(data=POs,Req~Qtr,FUN=length) %>%
  select(-Qtr,Count=Req)

Days2PO <- cbind(avg_working_days,number_working_days)   

