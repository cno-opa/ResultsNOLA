### Create a connection to Buyspeed database  
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
                        where	ORG_ID='1000'")

BS_item<-sqlQuery(Buyspeed, "select Req=REQ_NBR, Bid=BID_NBR, PO=PO_ID, Class=CLASS_PRIMARY, NIGP=CLASS_ITEM, Vendor_code=RECOMMENDED_VENDOR_NBR
                        from REQ_ITEM")    

BS_po_header<-sqlQuery(Buyspeed,"select PO=PO_NBR, Purchaser=PURCHASER,  AltID=ALTERNATE_ID,  Dept_code=LOC_ID,	PODate=PO_DATE,	Description=SHORT_DESC, Vendor_code=VEND_ID,POStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,	Requestor=REQUESTOR_ID 
                        from PO_HEADER 
                        where	ORG_ID='1000'")

##### ECMS
con_req_routing<-sqlQuery(Buyspeed,"select Req=REQ_NBR, Approver=REQ_APPROVER, Request_Date=REQ_REQ_APP_DATE, ApprovalDate=APPROVAL_DATE,Ord=ORDER_SEQUENCE, Path_ID=APPROVAL_PATH_ID 
                        from REQ_ROUTING 
                        where ORG_ID= 'CONTRACTS'")

po_routing<-sqlQuery(Buyspeed,"select PO=PO_NBR, Approver=PO_APPROVER, LegalReview=PO_REQ_APP_DATE, ApprovalDate=APPROVAL_DATE,Comments=COMMENT_TEXT, Ord=ORDER_SEQUENCE 
                        from PO_ROUTING 
                        where ORG_ID= 'CONTRACTS'")

po_header_con<-sqlQuery(Buyspeed,"select PO=PO_NBR, Purchaser=PURCHASER,  AltID=ALTERNATE_ID,	Dept_code=LOC_ID,	ContractDate=PO_DATE,	Description=SHORT_DESC, Vendor_code=VEND_ID,POStatus=CURRENT_HDR_STATUS, StatusDate=DATE_LAST_UPDATED,	Requestor=REQUESTOR_ID 
                        from PO_HEADER 
                        where	ORG_ID='CONTRACTS'")

po_notes<-sqlQuery(Buyspeed,"select PO=PO_NBR, Note=NOTE 
                        from PO_NOTES")

po_item<-sqlQuery(Buyspeed,"select PO=PO_NBR, Req=REQ_NBR,	NIGP=CLASS_PRIMARY, DATE_LAST_UPDATED	
                         from PO_ITEM
                         where DATE_LAST_UPDATED >= Convert(datetime,'2011-1-1')")



##### at-large tables
approval_paths<-sqlQuery(Buyspeed,"select Dept_code=APPROVAL_PATH_ID,  Dept=DESC_TEXT  
                         from APPROVAL_PATHS")

vendor<-sqlQuery(Buyspeed,"select Vendor_code=VENDOR_NBR,  Vendor=NAME,	Vendor_type=VENDOR_DESCRIPTION 
                         from VENDOR")

vendor_category<-sqlQuery(Buyspeed, "select Vendor_code=VENDOR_NBR, Category=CATEGORY_ID_REF, Response=CATEGORY_TYPE_REF, Date=DATE_LAST_UPDATED
                       from VENDOR_CATEGORY")

req_status_dates<-sqlQuery(Buyspeed,"select Req=REQ_NBR, ReqStatusDate=REQ_STATUS_DATE,MAJOR_STATUS_CODE
                          from REQ_STATUS_DATES")

po_account<-sqlQuery(Buyspeed,"select PO=PO_NBR, PO_Amt=DOLLAR_AMT
                          from PO_ACCOUNT")

#### Close database connection
close(Buyspeed)
