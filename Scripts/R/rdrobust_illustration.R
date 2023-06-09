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

"
This code creates a set of models using a geographic regression discontinuity design (GRDD)
for different grades. First, it filters the data based on a distance variable,
and then it subsets the data by grade. The RDD models are estimated using the rdrobust package, 
and the resulting tables and graphs are saved. Finally,
a summary table of the results is created by appending the results of each model to a data frame.
"

############################################
## General Inference 
###########################################
"
In a regression discontinuity design (RDD), 
the bandwidth is the range of values around the cutoff point where observations are included in the analysis. 
The choice of bandwidth can have a significant impact on the estimated treatment effect in an RDD. 
Bandwidth sensitivity refers to the degree to which the estimated treatment effect changes as the bandwidth is varied.
A bandwidth is said to be sensitive if the estimated treatment effect changes substantially as the bandwidth is varied.
Therefore, it is important to perform sensitivity analyses with different bandwidths in order to assess the robustness of the estimated treatment effect.
"
data =df_cov
# data <- data[data$distance >= -10000 & data$distance <= 10000,]
Running_variable <- 'distance'
Outcome <- 'dropout_rate_t1'
q_= 'q_all'

est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
summary(est)

model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                 subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                 kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )

summary(model)

png(paste0(graphs_dir,"general_result",  q_ ,Outcome, ".png"),  width = 1030, height = 598)
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
dev.off() 

png(paste0(graphs_dir,"general_result", Outcome ,q_, Outcome, ".png"),  width = 1030, height = 598)
bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable, conf_level = 0.95 , full = 10)[2]
dev.off()   




summary(model)

bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable, conf_level = 0.95 )

rd_table_latex = rd_table(model)
# writeLines( text = rd_table_latex,            paste0(tables_dir,get_school_stage(grade) ,q_, Outcome, ".tex") )
####################################################
"
Control and treated schools differ systematically in this covariate
"

data =df_cov
data <- data[data$distance >= -10000 & data$distance <= 10000,]
Running_variable <- 'distance'
colnames(df_cov)
Outcome_list = c(
  "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
  "FRAC_SUBSIDIADO" , "FRAC_EDAD" ,"FRAC_ESTRATO_1" , "FRAC_ESTRATO_2" , 
  "FRAC_ESTRATO_3","FRAC_ESTRATO_4","FRAC_ESTRATO_5" , "FRAC_ESTRATO_6"     
)
for (Outcome in Outcome_list) {
  q_= 'q_all'
  
  est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
  rd_table_latex = rd_table(est)
  writeLines( text = rd_table_latex,  paste0(tables_dir,"cov_test_", Outcome ,q_, Outcome, ".tex") )
  par(mfrow=c(1,2))
  
  png(paste0(graphs_dir,"cov_test_rdplot",  q_ ,Outcome, ".png"),  width = 1030, height = 598)
  rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0, binselect="esmv", kernel="triangular",  p=1 )
  dev.off() 
  
  png(paste0(graphs_dir,"cov_test_bws", Outcome ,q_, Outcome, ".png"),  width = 1030, height = 598)
    bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable, conf_level = 0.95 , full = 2)[2]
  dev.off()   
}
 





 


 


  

####################################################
"
Sensibility test of bandwidth by all grades 
"


grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )
for (grade in grades) {
  png(paste0(graphs_dir,get_school_stage(grade) ,'_bandwidth_sensibility_test', ".png"),  width = 1030, height = 598)
  bandwidth_sensibility_test(data= subset(data, data$GRADO ==grade),
                             Outcome=Outcome, Running_variable=Running_variable, conf_level = 0.95 )
  dev.off() 
  
}

# cat( paste0( 
#   sprintf(" \\begin{frame}{General Results by %s} \n %s \n \\end{frame} \n  ", get_school_stage(grade), 
#              rd_table_latex ) , "%-----------------------------------------% \n %-----------------------------------------%  \n" )  )
# 

png(paste0(graphs_dir, q_,Outcome, ".png"),  width = 1030, height = 598)
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
dev.off() 
 
###########################################
## Single estimation by grade with all samples 
############################################
data =df


Running_variable <- 'distance'
Outcome <- 'dropout_rate_t1'
# data <- data[data$distance >= -10000 & data$distance <= 10000,]
grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )
Table_result_ = data.frame()
modelos = list()
q_= 'q_all'


"
This begins a for loop that iterates over each element in the 'grades'
vector and assigns each element to the variable 'grades'
"

for (grade in grades) {
  # print(grade)
  tempo  =  data[data$GRADO == grade,]
  # hist(tempo$distance)
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  modelos[[get_school_stage(grade)]] <- (model)
  est =model
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]], all=TRUE,
             subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
             kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  rd_table_latex = rd_table(model)
  writeLines( text = rd_table_latex, 
              paste0(tables_dir,get_school_stage(grade) ,q_, Outcome, ".tex") )
   
  # summary(model)
  pic_in_frame = paste0( "\\begin{center} \n \\includegraphics[scale=0.2]{Graph/", get_school_stage(grade) ,q_,Outcome, ".png }\n \\end{center}  " )
   
  latex_frame = paste0( 
    sprintf(" \\begin{frame}{General Results by %s} \n %s \n %s \n \\end{frame} \n  ", get_school_stage(grade), pic_in_frame,
               rd_table_latex ) , "%-----------------------------------------% \n %-----------------------------------------%  \n" )
  
  cat(latex_frame)

  
  png(paste0(graphs_dir,get_school_stage(grade) ,q_,Outcome, ".png"),  width = 1030, height = 598)
  rdplot(y=tempo[[Outcome]], x=tempo[[Running_variable]],c =0,
         subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
         binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  dev.off() 
 
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}

png(paste0(graphs_dir,"Robust_all_grades_" ,q_, Outcome, ".png"), width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Robust"), conf_level=0.95, TITULO = 'Robust Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Bias-Corrected_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Bias-Corrected"), conf_level=0.95,
             TITULO = 'Bias-Corrected Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Conventional_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Conventional"), conf_level=0.95,
             TITULO = 'Conventional Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 


###########################################
## Single estimation by grade focused the most exposed,
# thus erolled when contract sign was in the first semester.
############################################
data =df
Running_variable <- 'distance'
Outcome <- 'dropout_rate_t2'
data <- data[data$Q_artyl <= 2 ,]

"
This code creates a set of models using a geographic regression discontinuity design (GRDD)
for different grades. First, it filters the data based on a distance variable,
and then it subsets the data by grade. The RDD models are estimated using the rdrobust package, 
and the resulting tables and graphs are saved. Finally,
a summary table of the results is created by appending the results of each model to a data frame.
"

# data <- data[data$distance >= -10000 & data$distance <= 10000,]

grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )

Table_result_ = data.frame()
modelos = list()
q_= 'q_1_2'


for (grade in grades) {
  # print(grade)
  tryCatch( {
  tempo  =  data[data$GRADO == grade,]
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  modelos[[get_school_stage(grade)]] <- (model)
  est =model
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]], all=TRUE,
                   subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
                   kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  summary(model)
  rd_table_latex = rd_table(model)
  writeLines( text = rd_table_latex, 
              paste0(tables_dir,get_school_stage(grade) ,q_, ".tex") )
   
   
  # cat( paste0( 
  #   sprintf(" \\begin{frame}{General Results by %s} \n %s \n \\end{frame} \n  ", get_school_stage(grade), 
  #           rd_table_latex ) , "%-----------------------------------------% \n %-----------------------------------------%  \n" )  )
  # 
  # 

  png(paste0(graphs_dir,get_school_stage(grade) ,q_,Outcome, ".png"),  width = 1030, height = 598)
  rdplot(y=tempo[[Outcome]], x=tempo[[Running_variable]],
         subset =-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
         binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  dev.off() 
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
   
  
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}

png(paste0(graphs_dir,"Robust_all_grades_" ,q_, Outcome, ".png"), width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Robust"), conf_level=0.95, TITULO = 'Robust Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Bias-Corrected_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Bias-Corrected"), conf_level=0.95,
             TITULO = 'Bias-Corrected Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Conventional_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Conventional"), conf_level=0.95,
             TITULO = 'Conventional Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

#######################################################
## Single estimation by grade focused the most exposed,
# thus enrolled when contract sign was in the second semester.
######################################################
data =df
Running_variable <- 'distance'
Outcome <- 'dropout_rate_t2'
data <- data[data$Q_artyl >= 3 ,]


# data <- data[data$distance >= -10000 & data$distance <= 10000,]

grades <- c('1' , '2', '3', '4', '5', '6', '7', '8', '9', '10', '11'  )

Table_result_ = data.frame()
modelos = list()
q_= 'q_3_4'


for (grade in grades) {
  # print(grade)
  tempo  =  data[data$GRADO == grade,]
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]] , all=TRUE)
  modelos[[get_school_stage(grade)]] <- (model)
  est =model
  model = rdrobust(y=tempo[[Outcome]] , x=tempo[[Running_variable]], all=TRUE,
                   subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
                   kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  summary(model)
  rd_table_latex = rd_table(model)
  writeLines( text = rd_table_latex, 
              paste0(tables_dir,get_school_stage(grade) ,q_, ".tex") )
  
  
  # cat( paste0( 
  #   sprintf(" \\begin{frame}{General Results by %s} \n %s \n \\end{frame} \n  ", get_school_stage(grade), 
  #           rd_table_latex ) , "%-----------------------------------------% \n %-----------------------------------------%  \n" )  )
  # 
  # 
  png(paste0(graphs_dir,get_school_stage(grade) ,q_,Outcome, ".png"),  width = 1030, height = 598)
  rdplot(y=tempo[[Outcome]], x=tempo[[Running_variable]],c =0, 
         subset=-est$bws[1,1]<= tempo[[Running_variable]]  & tempo[[Running_variable]]  <= est$bws[1,2],
         binselect="esmv", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  dev.off() 
  
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}

png(paste0(graphs_dir,"Robust_all_grades_" ,q_, Outcome, ".png"), width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Robust"), conf_level=0.95, TITULO = 'Robust Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Bias-Corrected_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Bias-Corrected"), conf_level=0.95,
             TITULO = 'Bias-Corrected Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 

png(paste0(graphs_dir,"Conventional_all_grades_" ,q_,Outcome, ".png"),  width = 1030, height = 598)
summary_plot(subset(Table_result_, Table_result_$estiamtion == "Conventional"), conf_level=0.95,
             TITULO = 'Conventional Estiamtion of exploration announcement effect\n   on dropout rates' )
dev.off() 


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

 