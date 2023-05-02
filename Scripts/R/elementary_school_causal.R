#####################################
"Causal Effect on elementary School"
#####################################
########################################
# Working directory
########################################
if (Sys.info()["nodename"] == "cpossosu") {
  data_dir <- "/Users/cpossosu/Downloads/The Blessing of Oil Fields"
  graphs_dir <- "/Users/cpossosu/Downloads/The Blessing of Oil Fields"
}   else if (Sys.info()["nodename"] == "JAIME") {
  General_path = "C:/Users/USER/Desktop/The Blessing of Oil Fields/Black Gold and Dull Minds/"
  data_dir <- paste0(General_path , "Data/Elementary_school/")
  graphs_dir <-  paste0(General_path , "Graph/Elementary_school/") 
  tables_dir <- paste0(General_path , "Tables/Elementary_school/")  
}
#########################################
## Load required settings
#########################################
source("./Scripts/R/general_settings.R ")
source("./Scripts/R/Reading_data.R")
source("./Scripts/R/functions.R")
attach(df)
#df = df1 
# Data
variation = c("T1", "T2", "TL1")
Columnas =  c("DESERTO_", "STILL_SAME_SCHOOL_", "EMIGRATED_", "IMIGRATED_",
              "EMIGRATED_OUT_AC_AT_", "MALE_DROPOUT_","FEMALE_DROPOUT_" )
Totals = c( "ESTU_TOTALES_")

for (period in variation) {
  if (period != "TL1") {
    for (column_ in Columnas) {
      columna_ = paste0(column_, period)
      Tot_rate = paste0(Totals, period)
      print(columna_ )
      print(Tot_rate)
      df[[ columna_  ]]  =  df[[ columna_  ]] / df[[ Tot_rate  ]]
    }
  } else{
    for (column_ in c("DESERTO_", "STILL_SAME_SCHOOL_", "EMIGRATED_", "IMIGRATED_")) {
      columna_ = paste0(column_, period)
      Tot_rate = paste0(Totals, period)
      print(columna_ )
      print(Tot_rate)
      df[[ columna_  ]]  =  df[[ columna_  ]] / df[[ Tot_rate  ]]
    }
  }
  
}
library(dplyr)
# df <- df %>%
#   filter(distance_to_polygon < -1000 | distance_to_polygon > 1000)

df$distance = df$distance_to_polygon 

df$FRAC_ESTRATO_1_2 = df$FRAC_ESTRATO_1+df$FRAC_ESTRATO_2
df$FRAC_ESTRATO_3_4 = df$FRAC_ESTRATO_3+df$FRAC_ESTRATO_4


############################################
## General Inference 
###########################################


data =df
data = subset(data, data$GRADO_ <= 5)
data = subset(data, data$GRADO_ >= 0)
library(haven)
haven::write_dta(data,paste0(data_dir,"elementary_school_data.dta") )
Running_variable <- 'distance'
Outcome <- 'DESERTO_T1'
q_= 'q_all'


est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
summary(est)
model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                 subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                 kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=2 )

summary(model)
"In this case, a one unit decrease in the ATE is associated with a 32.68% 
  decrease in the mean of the control group."
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular",
       h=c(est$bws[1,1], est$bws[1,2]),  p=1 )

png(paste0(graphs_dir,"general_result",  q_ ,Outcome, ".png"),  width = 1030, height = 598, res = 100)
model_pic  <- model_outputs_plot(est= est, data = data, Running_variable, Outcome)
dev.off()
bw_inference = model_pic[[2]]
png(paste0(graphs_dir,"general_result", Outcome ,q_, Outcome, ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                           bw_mse = bw_inference,
                           conf_level = 0.95 , full = 30)[2]
dev.off()

rd_table_latex = rd_table(model)
cat(rd_table_latex)
writeLines( text = rd_table_latex,         
            paste0(tables_dir,"_general_result_" ,q_, Outcome, ".tex") )
####################################################
"
Control and treated schools differ systematically in this covariate
"
summary(lm(data=subset(data, data$IS_IN_T1 ==T), DESERTO_T1~DESERTO_TL1) )
Running_variable <- 'distance'
colnames(data)
Outcome_list = c(
  "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
  "FRAC_SUBSIDIADO" ,    "elevation"     ,   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD"
)
 
for (Outcome in Outcome_list) {
  q_= 'q_all'
  
  est_model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
  rd_table_latex = rd_table(est)
  
  
  png(paste0(graphs_dir,"cov_test_rdplot",  q_ ,Outcome, ".png"),  width = 1030, height = 598, res = 100)
  model_pic  <- model_outputs_plot(est= est_model, data = data, Running_variable, Outcome)
  dev.off() 
  
  png(paste0(graphs_dir,"cov_test_bws", Outcome ,q_, Outcome, ".png"),  width = 1030, height = 598, res = 100)
  bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                             bw_mse = bw_inference,
                             conf_level = 0.95 , full = 15)[2]
  dev.off()
  
  
  writeLines( text = rd_table_latex,  paste0(tables_dir,"cov_test_", Outcome ,q_, Outcome, ".tex") )
  par(mfrow=c(1,2))

}



############################################
## Results by grade in Elementary School 
###########################################
grades <- c('1' , '2', '3', '4', '5'  )
data =df
data = subset(data, data$GRADO_ <= 5)
data = subset(data, data$GRADO_ >= 0)
table(data$GRADO)

for (grade in grades) {
print(
  summary( lm(data = subset(data, data$distance <= bw_inference & 
                            data$distance >=  -bw_inference & data$GRADO == grade),
   DESERTO_T1 ~ IS_IN_T1 )
      )
)
}
Running_variable <- 'distance'
Outcome <- 'DESERTO_T1'
# data <- data[data$distance >= -10000 & data$distance <= 10000,]

Table_result_ = data.frame()
modelos = list()
q_= 'q_all'


"
This begins a for loop that iterates over each element in the 'grades'
vector and assigns each element to the variable 'grades'
"

Running_variable <- 'distance'
Outcome <- 'DESERTO_T1'
q_= 'q_all'
# window = 800
for (grade in grades) {
  data_ = subset(data, data$GRADO==grade)
  # data_ <- subset(data_, data_[[Running_variable]] < -window | data_[[Running_variable]] > window)
  # data_$distance <- ifelse(data_[[Running_variable]] > 0, data_[[Running_variable]] - window, data_[[Running_variable]] + window)
  
  
  est = rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]] , all=TRUE)
  model = rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]], all=TRUE,
                   # subset=-est$bws[1,1]<= data_[[Running_variable]]  & data_[[Running_variable]]  <= est$bws[1,2],
                   kernel="triangular", 
                   b = c(est$bws[2,1], est$bws[2,2]), 
                   h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  "In this case, a one unit decrease in the ATE is associated with a 32.68% 
  decrease in the mean of the control group."
  
  # png(paste0(graphs_dir,get_school_stage(grade) ,q_,Outcome, ".png"),  width = 1030, height = 598, res=100)
  model_pic  <- model_outputs_plot(est= est, data = data_, Running_variable, Outcome)
  # dev.off()
  tempo = Table_result(model)
  tempo$grade = grade
  tempo$estiamtion = row.names(tempo)
  rownames(tempo) <- NULL
  Table_result_ = rbind(Table_result_, tempo )
}


summary_plot(subset(Table_result_,
                    Table_result_$estiamtion == "Robust"), conf_level=0.95, 
             TITULO = 'Robust Estiamtion of exploration announcement effect\n   on dropout rates' )




################################
"Result of grades with covariates"
################################
data = df 
## rdrobust with covariates within the same window (i.e., using same bandwidths)
grade = "8"
data_ = data #[data$GRADO<=5 & data$GRADO>=0,  ]

# subset(data, data$GRADO==grade)
est1 <- rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]] )
len1 <- est1$ci[3,2] - est1$ci[3,1]
covariates = c(
  "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
  "FRAC_SUBSIDIADO" ,    "elevation"     ,   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD"
)

est2 <- rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]], 
                 covs=data_[covariates], 
                 h = c(est$bws[1,1], est$bws[1,2]), 
                 b = c(est$bws[2,1], est$bws[2,2])
                 )
summary(est2)
summary(model)
len2 <- est2$ci[3,2] - est2$ci[3,1]
paste("CI length change: ", round((len2/len1-1)*100,2), "%")


