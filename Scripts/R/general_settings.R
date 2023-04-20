# General setting
## Libraries
########################################################################
packageList<-c("rdlocrand", "rddensity", 'rdrobust',  'rdmulti', 'rdpower', 'stargazer', 'bigrquery' )
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}

 

# Authenticate user
project <- "ph-jabri"
bq_auth() #use_oob = TRUE




 