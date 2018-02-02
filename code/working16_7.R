source('code/00_load_dependencies.R')

source_dir <- "data/source/2017_q4/"
headings <- c("Dept","Req","FinanceDate","POnumber","POdate","Cost","Vendor",
            "PrintDate","BuyerInitials","Buyer","WorkingDays")

raw_POs <- read_csv(paste0(source_dir, "ProcurementReqProcessing.csv"),
                  skip=3, col_types =  
                   cols(
                    DESC_TEXT = col_character(),
                    REQ_NBR = col_integer(),
                    APPROVAL_DATE = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                    PO_NBR = col_character(),
                    PO_DATE = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                    ACTUAL_COST = col_character(),
                    VENDOR_NAME = col_character(),
                    PRINTED_DATE = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                    INITIALS = col_character(),
                    Textbox25 = col_character(),
                    TimeBetweenReqPO = col_integer()
                  )
                ) %>%
  purrr::set_names(headings)


ReqStatus <- read_csv(paste0(source_dir, "RequsitionStatus.csv"),skip=3,
                      col_types =  
                        cols(
                          REQ_NBR = col_integer(),
                          SHORT_DESC = col_character(),
                          DESCRIPTION = col_character(),
                          ENTERED_DATE = col_character(),
                          DEPARTMENT = col_character(),
                          LOCATION_NBR_REF = col_character(),
                          FIRST_NAME = col_character(),
                          LAST_NAME = col_character(),
                          EST_COST = col_double(),
                          STATUS = col_character()
                        )) %>%
  select(Req=REQ_NBR,Status=STATUS)

raw_POs <- raw_POs %>%
  left_join(ReqStatus, "Req") %>%
  mutate(Cost = as.numeric(sub("\\$","",Cost)))

Exclude <- raw_POs %>%
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

POs<-anti_join(raw_POs,Exclude,"Req") %>%
  mutate(FinanceDate = date(FinanceDate),
         POdate = date(POdate),
         Qtr = as.yearqtr(POdate,format="%m/%d/%Y"),
         WorkingDays = bizdays(FinanceDate,POdate,NOLA_calendar) + 1) %>%
  distinct(Req, WorkingDays, Qtr)

Days2PO <- POs %>%
  group_by(Qtr) %>%
  summarise(WorkingDays = mean(WorkingDays), Req = n())

avg_working_days <- aggregate(data=POs,WorkingDays~Qtr,FUN=mean)

number_working_days <- aggregate(data=POs,Req~Qtr,FUN=length) %>%
  filter(year(Qtr) != 2018) %>%
  select(-Qtr,Count=Req)

Days2PO <- cbind(avg_working_days,number_working_days)   
