require(tidyr)
require(dplyr)
require(lubridate)
contracts<-read.csv("Contract Approval Sequence POs.csv",na.strings-c("",NA),skip=3)
contracts<-select(contracts,AltID=ALTERNATE_ID,PO=PO_NBR,POdate=PO_DATE,AttorneyReview=PO_REQ_APP_DATE,ApprovalDate=APPROVAL_DATE,ApproverType=PO_APPROVER_TYPE,Approver=PO_APPROVER)
contracts<-filter(contracts,Approver!="CLCSETTLEMYER",Approver!="CMJAVERILL",Approver!="CRDRIVERS",Approver!="CMESTRICKLAND",Approver!="CJECHRISTOPHER")

contracts$Approver<-paste(contracts$Approver,contracts$ApproverType,sep="_")
contracts<-select(contracts,AltID,PO,POdate,AttorneyReview,ApprovalDate,Approver)

