This repository consists of the scripts to compile the quarterly ReqtoCheckSTAT analyses. ReqtoCheckSTAT reviews performance data related to contracting out goods and services, from the requisition of budgeted funds to the issuance of a check for goods or services rendered. 

###The departments involved in the ReqtoCheck process are:
* Budget Office
* Capital Projects Administration
* Office of Community Development
* Department of Finance
	* Bureau of Accounting
	* Bureau of Purchasing
* Department of Information Technology & Innovation
* Law Department
* Department of Public Works
* Office of Supplier Diversity

### Status of ReqtoCheck scripted sections (as of 9/30).
* ITI - N/A
* Requisitions - In Progress
* Procurement - Complete, with minor chart format tweaks
* Bids/RFPs/DBEs - Bids and RFPs complete, with minor format tweaks.  DBEs in progress.
* Contract reqs - Not started.  Will begin after requisitions are complete.
* Contract purchase orders - Mostly complete, with aging charts needed. Also need to optimize cleaning.
* Payments - Great Plains complete, AFIN in progress
* Invoice Pipelines - Not started


###General to-do's:
* Figure out formulas for automatically determining distribution chart data label heights
* Figure out formula for determining age per quarter

### Instructions for running:
* Set working directory to OPA share drive
* Run Main.R
	* If troubleshooting is needed troubleshooting individual scripts, first run Main.R through to line that sources utility-scripts, and then proceed to script in question.