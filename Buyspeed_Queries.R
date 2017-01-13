### Create a connection to Buyspeed database  (If running individual queries, always run this snippet first)
Buyspeed<-odbcDriverConnect(connection="Driver={SQL Server};
                            server=cno-sqlreport01;database=Buyspeed;
                            trusted_connection=yes;")

#### Query Buyspeed/ECMS

##### Buyspeed
BS_routing<-sqlQuery(Buyspeed,"select Req=REQ_NBR, Approver=REQ_APPROVER, Request_Date=REQ_REQ_APP_DATE, ApprovalDate=APPROVAL_DATE,Ord=ORDER_SEQUENCE, Path_ID=APPROVAL_PATH_ID 
                        from REQ_ROUTING 
                        where ORG_ID= '1000'
                        and ReQ_REQ_APP_DATE >= Convert(datetime,'2011-1-1')")

BS<-sqlQuery(Buyspeed,"select Req=REQ_NBR ,Dept_code=LOC_ID,  Org=DEPT_NBR_SUFFIX , Req_Amount=EST_COST, CreationDate=ENTERED_DATE,	Description=SHORT_DESC, ReqStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,Purchaser=PURCHASER_USER_ID, Requestor=REQUESTOR_ID
                        from REQ 
                        where	ORG_ID='1000'
                        and ENTERED_DATE >= Convert(datetime,'2011-1-1')")

BS_item<-sqlQuery(Buyspeed, "select Req=REQ_NBR, Bid=BID_NBR, PO=PO_ID, Class=CLASS_PRIMARY, NIGP=CLASS_ITEM, Vendor_code=RECOMMENDED_VENDOR_NBR
                        from REQ_ITEM
                        where DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')")    

BS_po_header<-sqlQuery(Buyspeed,"select PO=PO_NBR, Purchaser=PURCHASER,  AltID=ALTERNATE_ID,  Dept_code=LOC_ID,	PODate=PO_DATE,	Description=SHORT_DESC, Vendor_code=VEND_ID,POStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,	Requestor=REQUESTOR_ID 
                        from PO_HEADER 
                        where	ORG_ID='1000'
                        and PO_DATE >= Convert(datetime,'2011-1-1')")

## Buyspeed payments 
Receipt<-sqlQuery(Buyspeed,"select PO=PO_NBR, Receipt=RECEIPT_ID, ORG_CODE=DEPT_NBR_PREFIX,Description=SHORT_DESC, ReceiptDate=DATE_LAST_UPDATED
                            from RECEIPT_HEADER
                            where CURRENT_HDR_STATUS = '5CA'
                            and DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')")


Invoice<-sqlQuery(Buyspeed, "select PO=PO_NBR, Invoice=INVOICE_NBR, Vendor_code=VENDOR_NBR, AP_Process_Date=DATE_APPROVED, Approver=USER_APPROVED, Amount=INVOICE_AMT 
                            FROM INVOICE_HDR
                            where PAYMENT_DATE>= Convert(datetime,'2013-1-1')
                           and INVOICE_STATUS = '4IP'")

##### ECMS
con_req_routing<-sqlQuery(Buyspeed,"select Req=REQ_NBR, Approver=REQ_APPROVER, Request_Date=REQ_REQ_APP_DATE, ApprovalDate=APPROVAL_DATE,Ord=ORDER_SEQUENCE, Path_ID=APPROVAL_PATH_ID 
                        from REQ_ROUTING 
                        where ORG_ID= 'CONTRACTS'
                        and REQ_REQ_APP_DATE >= Convert(datetime,'2011-1-1')")

po_routing<-sqlQuery(Buyspeed,"select PO=PO_NBR, Approver=PO_APPROVER, LegalReview=PO_REQ_APP_DATE, ApprovalDate=APPROVAL_DATE,Comments=COMMENT_TEXT, Ord=ORDER_SEQUENCE 
                        from PO_ROUTING 
                        where ORG_ID= 'CONTRACTS'")

po_header_con<-sqlQuery(Buyspeed,"select PO=PO_NBR, Purchaser=PURCHASER,  AltID=ALTERNATE_ID,	Dept_code=LOC_ID,	ContractDate=PO_DATE,	Description=SHORT_DESC, Vendor_code=VEND_ID,POStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,	Requestor=REQUESTOR_ID , Fiscal_Year=REQ_NBR_FISCAL_YR
                        from PO_HEADER 
                        where	ORG_ID='CONTRACTS'
                        and PO_DATE >= Convert(datetime,'2011-1-1')") 

po_notes<-sqlQuery(Buyspeed,"select PO=PO_NBR, Note=NOTE 
                        from PO_NOTES
                        where PO_NOTES_DATE >= Convert(datetime,'2011-1-1')")

po_item<-sqlQuery(Buyspeed,"select PO=PO_NBR, Req=REQ_NBR,	NIGP=CLASS_PRIMARY, DATE_LAST_UPDATED, UNIT_COST	
                         from PO_ITEM
                         where DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')") 

###ECMS start and end dates
start_end_dates<-sqlQuery(Buyspeed, "SELECT PO=PO_NBR, Begin_Date=BLANKET_BEG_DATE, End_Date=BLANKET_END_DATE
                              FROM dbo.BLANKET_CONTROL
                              WHERE ORG_ID='CONTRACTS'
                              and BLANKET_BEG_DATE >= Convert(datetime,'2011-1-1')")

write.csv(start_end_dates,"O:/Projects/ReqtoCheckSTAT/Carbons/StartEnd.csv")
                          
##### at-large tables
approval_paths<-sqlQuery(Buyspeed,"select Dept_code=APPROVAL_PATH_ID,  Dept=DESC_TEXT  
                         from APPROVAL_PATHS")

vendor<-sqlQuery(Buyspeed,"select Vendor_code=VENDOR_NBR,  Vendor=NAME,	Vendor_type=VENDOR_DESCRIPTION 
                         from VENDOR")

vendor_category<-sqlQuery(Buyspeed, "select Vendor_code=VENDOR_NBR, Category=CATEGORY_ID_REF, Response=CATEGORY_TYPE_REF, Date=DATE_LAST_UPDATED
                        from VENDOR_CATEGORY
                        where DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')")

req_status_dates<-sqlQuery(Buyspeed,"select Req=REQ_NBR, ReqStatusDate=REQ_STATUS_DATE,MAJOR_STATUS_CODE
                          from REQ_STATUS_DATES
                           where REQ_STATUS_DATE >= Convert(datetime,'2011-1-1')")

po_account<-sqlQuery(Buyspeed,"select PO=PO_NBR, PO_Amt=DOLLAR_AMT
                          from PO_ACCOUNT
                          where DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')")


#### Close database connection
close(Buyspeed)
