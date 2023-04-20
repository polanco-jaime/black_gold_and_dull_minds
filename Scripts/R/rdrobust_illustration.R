########################################
# Working directory
########################################
if (Sys.info()["nodename"] == "cpossosu") {
  data_dir <- "/Users/cpossosu/Downloads/The Blessing of Oil Fields"
  graphs_dir <- "/Users/cpossosu/Downloads/The Blessing of Oil Fields"
}   else if (Sys.info()["nodename"] == "JAIME") {
  General_path = "C:/Users/USER/Desktop/The Blessing of Oil Fields/Black Gold and Dull Minds/"
  data_dir <- paste0(General_path , "Data/")
  graphs_dir <-  paste0(General_path , "Graph/") 
  tables_dir <- paste0(General_path , "Tables/")  
}
#########################################
## Load required settings
#########################################
source("./Scripts/R/general_settings.R")
source("./Scripts/R/Reading_data.R")
source("./Scripts/R/functions.R")
attach(df)



############################################
## Inference
###########################################
Running_variable <- 'distance'
Outcome <- 'dropout_rate_t2'
 
###########################################
## Single estimation by grade
############################################
data =df
# data <- data[data$Q_artyl > 2 ,]
data <- data[data$distance >= -10000 & data$distance <= 10000,]
# data <- data[data$Q_artyl <= 2,]
grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )
Table_result_ = data.frame()
modelos = list()
q_= 'q_all'
for (grade in grades) {
  # print(grade)
  tempo  =  data[data$GRADO == grade,]
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  modelos[[get_school_stage(grade)]] <- (model)
  est =model
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]], all=TRUE,
             subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
             kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  rd_table_latex = rd_table(model)
  writeLines( text = rd_table_latex, 
              paste0(tables_dir,get_school_stage(grade) ,q_, ".tex") )
  tables_dir
  A =summary(model)
  cat( paste0( 
    sprintf(" \\begin{frame}{General Results by %s} \n %s \n \\end{frame} \n  ", get_school_stage(grade), 
               rd_table_latex ) , "%-----------------------------------------% \n %-----------------------------------------%  \n" )  )
  
  
  png(paste0(graphs_dir,get_school_stage(grade) ,q_, ".png"),  width = 1030, height = 598)
  rdplot(y=tempo[[Outcome]], x=tempo[[Running_variable]],c =0,
         subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
         binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  dev.off() 
  
  # rd_table(model)
  # summary(model)
  # summary(model)
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}
 
 
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Robust") )
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Bias-Corrected") )
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Conventional") )





##
############################################
# Primaria y secundaria
############################################
data =df
data <- data[data$distance >= -10000 & data$distance <= 10000,]
data <- data[data$Q_artyl <= 2 ,]
library(sqldf)
Running_variable <- 'distance'
Outcome <- 'dropout_rate_t1'

data <-  sqldf::sqldf(sprintf("SELECT CASE WHEN GRADO_ BETWEEN 0 AND 5 THEN '1' 
                                   WHEN GRADO_ BETWEEN 6 AND 9 THEN '2'   
                                  ELSE '3' END AS GRADO,GRADO_,
                              distance, %s
             FROM data",  Outcome ) ) 

grades <- c( '1','2', '3' )
Table_result_ = data.frame()
modelos = list()
for (grade in grades) {
  
  tempo  =  data[data$GRADO == grade,]
  print(unique(tempo$GRADO_ ))
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  est =model
  summary(
    rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
             subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
             kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  )
  modelos[[get_school_stage(grade)]] <- (model)
  # summary(model)
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}



#########################################3#
# Cuando la firma se dio en el primer semestre
data =df
data <- data[data$distance >= -10000 & data$distance <= 10000,]
data <- data[data$Q_artyl <= 2 ,]
# data <- data[data$Q_artyl <= 2,]
grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )
Table_result_q2 = data.frame()
modelos_q2 = list()
for (grade in grades) {
  print(grade)
  tempo  =  data[data$GRADO == grade,]
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  
  modelos_q2[[get_school_stage(grade)]] <- (model)
  summary(model)
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_q2 = rbind(Table_result_q2, tempo )
}


summary_plot(subset(Table_result_q2, Table_result_q2$estiamtion == "Robust") )
summary_plot(subset(Table_result_q2, Table_result_q2$estiamtion == "Bias-Corrected") )
summary_plot(subset(Table_result_q2, Table_result_q2$estiamtion == "Conventional") )

##
# Cuando la firma se dio en el segundo semestre
data =df
data <- data[data$distance >= -10000 & data$distance <= 10000,]
data <- data[data$Q_artyl >= 2 ,]
grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )
Table_result_q4 = data.frame()
modelos_q4 = list()
for (grade in grades) {
  print(grade)
  tempo  =  data[data$GRADO == grade,]
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  modelos_q4[[get_school_stage(grade)]] <- (model)
  summary(model)
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_q4 = rbind(Table_result_q4, tempo )
}


summary_plot(subset(Table_result_q4, Table_result_q4$estiamtion == "Robust") )
summary_plot(subset(Table_result_q4, Table_result_q4$estiamtion == "Bias-Corrected") )
summary_plot(subset(Table_result_q4, Table_result_q4$estiamtion == "Conventional") )





#######################################
## Necesito descargar covariables. Altura, precipitaciones, municipio etc.
######################################

str(data[[Outcome]])
str(data[[Running_variable]])
### rdrobust with all estimates
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE))

 
est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] )
summary(
  rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
           subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
           kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
)

 




## rdrobust with covariates within the same window (i.e., using same bandwidths)
est1 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] )
len1 <- est1$ci[3,2] - est1$ci[3,1]
data[[Running_variable]] 
 
est2 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , covs=cbind(sd_geometry,sede_lat), 
                 h = c(est$bws[1,1], est$bws[1,2]), 
                 b = c(est$bws[2,1], est$bws[2,2]))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")


## rdrobust with covariates with data-driven optimal bandwidths
est1 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] )
len1 <- est1$ci[3,2] - est1$ci[3,1]
est2 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , covs=cbind(class,termshouse,termssenate))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")

## rdrobust with useless covariate
est1 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] )
len1 <- est1$ci[3,2] - est1$ci[3,1]
est2 <- rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , covs=cbind(population))
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")

## rdrobust check covariate "balanced"
covs <- cbind(class, termshouse, termssenate, population)
balance <- matrix(NA,4,2)
for (z in 1:ncol(covs)) {
  est <- rdrobust(y=covs[,z], x=data[[Running_variable]] )
  balance[z,1] = est$Estimate[,"tau.us"]
  balance[z,2] = est$pv[3]
}
rownames(balance) = c("class", "termshouse", "termssenate", "population")
colnames(balance) = c("RD Effect", "Robust p-val")
print(balance)

## rdrobust with clustering
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , vce="nn", cluster=state))

## rdrobust with clustering and covariates, and different bandwidth
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , vce="nn", bwselect="msetwo", covs=cbind(class,termshouse,termssenate), cluster=state))

## rdbwselect with all estimates
summary(rdbwselect(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE))

## Other examples
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , kernel="uniform", vce="hc1", cluster=state))
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , bwselect="certwo", vce="hc3"))
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , h=c(12,15), b=c(18,20)))
summary(rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , covs=cbind(class), bwselect="cerrd", scaleregul=0, rho=1))
summary(rdbwselect(y=data[[Outcome]] , x=data[[Running_variable]] , kernel="uniform", vce="hc1", cluster=state, all=TRUE))
summary(rdbwselect(y=data[[Outcome]] , x=data[[Running_variable]] , covs=cbind(class), bwselect="msetwo", vce="hc2", all=TRUE))






Footer
© 2023 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs

 