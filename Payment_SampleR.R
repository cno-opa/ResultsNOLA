### This script is used to clean and generate the monthly sample of payment data.

#### Read in raw data
Raw_GP<-select(read.csv("O:/Projects/ReqtoCheckStat/Query Files/Raw_GP.csv"),
               Check=Check.Number,PO=Purchase.Order.Number,Account=Account.Number.String,Vendor=Vendor.Name,
               Debit,Inv.Amount:Actual.Apply.To.Amount,Amount=Check.Apply.Amount,Address.1:Zip.Code,
               Desc=Transaction.Description,CheckDate=Check.GL.Posting.Date)
Orgs<-read.csv("O:/Projects/ReqtoCheckStat/Query Files/Org Codes.csv")
Agency<-read.csv("O:/Projects/ReqtoCheckSTAT/Query Files/Agency codes.csv")             
                 
#### Remove rows with invalid check numbers (from dataset
Raw_GP$Check<-as.numeric((gsub("[[:alpha:]]",NA,Raw_GP$Check)))
Raw_GP<-filter(Raw_GP,!is.na(Raw_GP$PO))                      

#### Parse Great Plains Purchase Order numbers to be consistent with BuySpeed, and remove rows without a purchase order number.
Raw_GP$PO<-as.numeric(gsub("\\:.*","",x=Raw_GP$PO))
Raw_GP<-filter(Raw_GP,!is.na(Raw_GP$PO))

#### Remove dollar signs from appropriate columns
Raw_GP[,5:9]<-lapply(Raw_GP[,5:9], function(x) as.numeric(gsub("\\$|\\,", "",x)))

#### 
Raw_GP<-Raw_GP[Raw_GP$Debit>0,]

#### 
Raw_GP<-select(Raw_GP,-Debit,-Inv.Amount,-Actual.Apply.To.Amount,-Credit.Amount)

#### Remove payments that don't lend themselves to measurement
Raw_GP<-Raw_GP[Raw_GP$Vendor!="going places travel, inc",]

#### 
Raw_GP$Account<-as.character(Raw_GP$Account)
Raw_GP$Agency_code<-substr(Raw_GP$Account,6,4)
Raw_GP$Org_code<-substr(Raw_GP$Account,10,4)

#### Filter out rows with duplicate check numbers, and then generate random sample of checks
Raw_GP<-arrange(Raw_GP,desc(Account))
Raw_GP<-Raw_GP[!duplicated(Raw_GP$Check),]
Samp_GP <- Raw_GP[sample(nrow(Raw_GP), 170), ]

#### Write CSV's
write.csv(Samp_GP,"O:/Projects/ReqtoCheckSTAT/Query Files/Output/Payments/Samp_GP.csv")
