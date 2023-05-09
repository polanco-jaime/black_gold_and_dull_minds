# General setting
## Libraries
########################################################################
packageList<-c("rdlocrand", "rddensity", 'rdrobust',   
               'rdmulti', 'rdpower', 'stargazer', 'bigrquery' , "extrafont" )
# devtools::install_github("apache/arrow/r")

for (i in 1:length(packageList) ) {
  if(packageList[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(packageList[i])
  }
  lapply(packageList[i], library, character.only = TRUE)
}
# # Import all fonts
# font_import()
# loadfonts(device = "win")

# Authenticate user
project <- "ph-jabri"
# bq_auth(path = "eng.pepj@gmail.com" ) #use_oob = TRUE

 