configs <- function () {
  c(BS_routing = "select Req=REQ_NBR, Approver=REQ_APPROVER, Request_Date=REQ_REQ_APP_DATE,                 
                              ApprovalDate=APPROVAL_DATE,Ord=ORDER_SEQUENCE, Path_ID=APPROVAL_PATH_ID
    from REQ_ROUTING
    where ORG_ID= '1000'
    and ReQ_REQ_APP_DATE >= Convert(datetime,'2011-1-1')", 
             BS_item =  "select Req=REQ_NBR, Bid=BID_NBR, PO=PO_ID, Class=CLASS_PRIMARY, NIGP=CLASS_ITEM, Vendor_code=RECOMMENDED_VENDOR_NBR
    from REQ_ITEM",
             BS = "select Req=REQ_NBR ,Dept_code=LOC_ID,  Org=DEPT_NBR_SUFFIX , Req_Amount=EST_COST, CreationDate=ENTERED_DATE,	    
    Description=SHORT_DESC, ReqStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,Purchaser=PURCHASER_USER_ID, 
    Requestor=REQUESTOR_ID
    from REQ
    where	ORG_ID='1000'",
             vendor =  "select Vendor_code=VENDOR_NBR,  Vendor=NAME,	Vendor_type=VENDOR_DESCRIPTION
    from VENDOR",
             approval_paths = "select Dept_code=APPROVAL_PATH_ID,  Dept=DESC_TEXT
    from APPROVAL_PATHS",
             Invoice = "select PO=PO_NBR, Invoice=INVOICE_NBR, Vendor_code=VENDOR_NBR, AP_Process_Date=DATE_APPROVED, Approver=USER_APPROVED, 
    Amount=INVOICE_AMT 
    FROM INVOICE_HDR
    where PAYMENT_DATE>= Convert(datetime,'2013-1-1')
    and INVOICE_STATUS = '4IP'",
             Receipt = "select PO=PO_NBR, Receipt=RECEIPT_ID, ORG_CODE=DEPT_NBR_PREFIX,Description=SHORT_DESC, ReceiptDate=DATE_LAST_UPDATED
    from RECEIPT_HEADER
    where CURRENT_HDR_STATUS = '5CA'
    and DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')") %>% 
  data.frame(Query = ., 
             stringsAsFactors = FALSE) %>%
  mutate(Name = row.names(.), 
         FeatherFilePath = paste("feathers/",
                                 Name,
                                 ".feather",
                                 sep = ""))
} 

refeather <- function(query,
                      connection,
                      feather_path)
{
  print(query)
  fresh_data <- sqlQuery(connection, query) 
  
  fresh_data %>%
    write_feather(feather_path)
}



loadDataset <- function(dataset_name,
                        refresh_feather = FALSE)
{
  config <- configs() %>% 
    filter(Name == dataset_name) 
  
  if (refresh_feather == TRUE | file.exists(config$FeatherFilePath) == FALSE)
  {
    
    connector <- odbcDriverConnect(connection="Driver={SQL Server};
                            server=cno-sqlreport01;database=Buyspeed;
                            trusted_connection=yes;")
    
    refeather(config$Query,
              connector,
              config$FeatherFilePath
    )
    
    close(connector)
  } 
  
  read_feather(config$FeatherFilePath) %>%
    return()
}
