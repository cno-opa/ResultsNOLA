This repository consists of the scripts to compile the quarterly ReqtoCheckSTAT analyses. ReqtoCheckSTAT reviews City of New Orleans performance data related to contracting out goods and services, from the requisition of budgeted funds to the issuance of a check for goods or services rendered. 


### Status of ReqtoCheck scripted sections
* Requisitions - Nearly complete, with plotting needed.

* Procurement - Complete, with minor chart format tweaks needed.

* Bids/RFPs/DBEs - Bids and RFPs complete, with minor format tweaks needed.  DBEs in progress.

* Contract reqs - Data cleaning complete.

* Contract purchase orders - Data cleaning nearly complete.

* Payments - Mostly complete except for dept coding and department-level tables/charts, 

* Invoice Pipelines - Not started


###General to-do's:
* Figure out formulas for automatically determining distribution chart data label heights
* Figure out function for determining age per quarter

### Instructions for running:
* Set working directory to OPA share drive
* Run Main.R
	* If troubleshooting is needed troubleshooting individual scripts, first run Main.R through to line that sources utility-scripts, and then proceed to script in question.
