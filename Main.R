.libPaths("C:/Rpackages")

## Function for
source_https <- function(u, unlink.tmp.certs = FALSE) {
  require(RCurl)
  
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  eval(parse(text = script), envir= .GlobalEnv)
}
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R") # Load OPA theme
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Requirements.R") # Load required packages
source_https("https://raw.githubusercontent.com/cno-opa/utility-scripts/master/NOLA_calendar.R")# Load calendar for business day calculations

# Load component scripts
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Reqs.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Procurement.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Contract_Approval.R")
source_https("https://raw.githubusercontent.com/cno-opa/ReqtoCheckSTAT-scripts/master/Gen_Fund_Payments.R")