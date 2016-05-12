This repository consists of the scripts to compile the quarterly ReqtoCheckSTAT analyses. ReqtoCheckSTAT reviews City of New Orleans performance data related to contracting out goods and services, from the requisition of budgeted funds to the issuance of a check for goods or services rendered. 


### Scripting to-do's
#### Data importing:
	* Procurement
		* Convert csv to SQL script
	* Bids/RFPs/DBEs
		* convert csv to xlsx
	* Payments
		* Convert csv to xlsx

#### Data cleaning:
	* Invoice Pipelines
		* All
	* Payments
		* Tweak script once read from xlsx file
		* Departmental-level coding
	* Reqs
		* Figure out method for determining queue per quarter
	* Bids/RFPs/DBEs
		* Tweak script once read from xlsx file
		* DBE information
		* Figure out method for determining queue per quarter
	* Contract POs
		* Figure out method for determining age and queue per quarter
			* At aggregate, as well as approval levels
	* Procurement
		* Figure out method for determining queue per quarter

#### Plotting:
	* Reqs
		* Workflow chart
	* Purchase Orders
		* Workflow chart
		* Aging chart
	* Bids/RFPs
		* Workflow chart
	* Contract POs
		* Workflow chart
		* Aging charts
		* Approver-level processing
		* Vendor plot
	* contract Reqs
		* Aging charts
		* Approver-level processing
	* Payments
		* Departmental-level 

