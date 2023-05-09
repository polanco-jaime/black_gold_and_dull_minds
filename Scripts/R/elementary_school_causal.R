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
# source("./Scripts/R/Reading_data.R")

source("./Scripts/R/functions.R")
attach(df)
#df = df1 
# Data
level = 'elementary'

############################################
## General Inference 
###########################################


data =df
data = subset(data, data$GRADO_ <= 5)
data = subset(data, data$GRADO_ >= 0)
table(data$GRADO)

library(haven)
# haven::write_dta(data,paste0(data_dir,"elementary_school_data.dta") )
Running_variable <- 'distance'
Outcome <- 'DESERTO_T1'
Outcome_male <- 'MALE_DROPOUT_T1'
Outcome_female <- 'FEMALE_DROPOUT_T1'

q_= 'q_all'


est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
summary(est)
model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                 subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                 kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )

summary(model)
"In this case, a one unit decrease in the ATE is associated with a 32.68% 
  decrease in the mean of the control group."
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular",
       h=c(est$bws[1,1], est$bws[1,2]),  p=1 )
 
png(paste0(graphs_dir,"general_result_",Outcome, "_", q_ , ".png"),  width = 1030, height = 598, res = 105 )
model_pic  <- model_outputs_plot(est= est, data = data, Running_variable, Outcome)
dev.off()

picture_to_latex(paste0("general_result_",Outcome, "_", q_ , ".png" ) ,  
                 name = "General Result in Elementary School", level = level)

bw_inference = model_pic[[2]]

png(paste0(graphs_dir,"bw_sensibility_", Outcome ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                           bw_mse = bw_inference,
                           conf_level = 0.95 , full = 30)[2]
dev.off()


picture_to_latex(paste0("bw_sensibility_", Outcome ,"_",q_,  ".png") ,  
                 name = "General Bandwidth sensibility test in Elementary School", level = level)


rd_table_latex = rd_table(model)
cat(rd_table_latex)
writeLines( text = rd_table_latex, paste0(tables_dir,"general_result_" , Outcome, "_",q_, ".tex") )

##################
"General Effect by gender"
##################

Running_variable <- 'distance'
 



"EFFECT ON BOYS "
est_male = rdrobust(y=data[[Outcome_male]] , x=data[[Running_variable]] , all=TRUE)

png(paste0(graphs_dir,"general_result_",Outcome_male, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_male = model_outputs_plot(est= est_male, data = data, Running_variable, Outcome_male)
dev.off()

picture_to_latex(paste0("general_result_",Outcome_male, "_", q_ , ".png" ) ,  
                 name = "Dropout of males in Elementary School", level = level)

 
png(paste0(graphs_dir,"bw_sensibility_", Outcome_male ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_male, Running_variable=Running_variable,
                           bw_mse = model_pic_male[[2]],
                           conf_level = 0.95 , full = 30)[2]
dev.off() 

picture_to_latex(paste0("bw_sensibility_", Outcome_male ,"_",q_,  ".png" ) ,  
                 name = "Bandwidth sensibility test of males in Elementary School", level = level)


writeLines( text = rd_table(model_pic_male[[3]]), 
            paste0(tables_dir,"general_result_" , Outcome_male, "_",q_, ".tex") )

cat(rd_table(model_pic_male[[3]]))
"EFFECT ON  GIRLS "
est_female = rdrobust(y=data[[Outcome_female]] , x=data[[Running_variable]] , all=TRUE)
# summary(est_female)
png(paste0(graphs_dir,"general_result_",Outcome_female, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_female = model_outputs_plot(est= est_female, data = data, Running_variable, Outcome_female)
dev.off()

picture_to_latex(paste0("general_result_",Outcome_female, "_", q_ , ".png" ) ,  
                 name = "Dropout of females in Elementary School", level = level)

png(paste0(graphs_dir,"bw_sensibility_", Outcome_female ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_female, Running_variable=Running_variable,
                           bw_mse = model_pic_female[[2]],
                           conf_level = 0.95 , full = 30)[2]
dev.off()
picture_to_latex(paste0("bw_sensibility_", Outcome_female ,"_",q_,  ".png" ) ,  
                 name = "Bandwidth sensibility test of females in Elementary School", level = level)
cat(rd_table(model_pic_female[[3]]))
writeLines( text = rd_table(model_pic_female[[3]]),  paste0(tables_dir,"general_result_" , Outcome_female, "_",q_, ".tex") )
#####################################################

"
Control and treated schools differ systematically in this covariate?
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
   
  png(paste0(graphs_dir,"cov_test_rdplot_",  q_ ,Outcome, ".png"),  width = 1030, height = 598, res = 100)
  model_pic  <- model_outputs_plot(est= est_model, data = data, Running_variable, Outcome)
  dev.off() 
  
  png(paste0(graphs_dir,"cov_test_bws_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                             bw_mse = bw_inference,
                             conf_level = 0.95 , full = 15)[2]
  dev.off()
  
  
  writeLines( text = rd_table_latex,  paste0(tables_dir,"cov_test_", Outcome, "_" ,q_,  ".tex") )
  par(mfrow=c(1,2))

}

for (Outcome in Outcome_list) {
  picture_to_latex(paste0("cov_test_bws_", Outcome,"_" ,q_, ".png") ,  
                   name = paste0("Control and treated schools differ by ", Outcome), 
                   level = level)
  
  }
############################################
##  Inference by grade 
###########################################
grades <- c('0','1' , '2', '3', '4', '5'  )

Running_variable <- 'distance'
table(data$GRADO)


if (1==1) {
  q_= 'q_all'
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  dev.off()
  
  
  picture_to_latex(paste0("Inference_by_grade_", Outcome,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade", 
                   level = level)
  ##############################
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome_male , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  
  dev.off()
  picture_to_latex(paste0("Inference_by_grade_", Outcome_male,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade of male students", 
                   level = level)
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome_female , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  dev.off()
  picture_to_latex(paste0("Inference_by_grade_", Outcome_female,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade of female students", 
                   level = level)
}

if (1==1) {
  q_= 'q_12'
  
  
  data =df
  data = subset(data, data$GRADO_ <= 5)
  data = subset(data, data$GRADO_ >= 0)
  data =  subset(data, data$Q_artyl <= 2)
  
  if (1==1) {
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
    
    
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_male , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    
    dev.off()
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_female , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
  }
}

if (1==1) {
  q_= 'q_34'
  
  
  data =df
  data = subset(data, data$GRADO_ <= 5)
  data = subset(data, data$GRADO_ >= 0)
  data =  subset(data, data$Q_artyl > 2)
  
  if (1==1) {
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
    
    
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_male , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    
    dev.off()
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_female , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
  }
}

  # 
# png(paste0(graphs_dir,"general_result_",Outcome_male, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
# model_pic_male = model_outputs_plot(est= est_male, data = data, Running_variable, Outcome_male)
# dev.off()
# 
# 
# png(paste0(graphs_dir,"bw_sensibility_", Outcome_male ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
# bandwidth_sensibility_test(data= data,Outcome=Outcome_male, Running_variable=Running_variable,
#                            bw_mse = model_pic_male[[2]],
#                            conf_level = 0.95 , full = 30)[2]
# dev.off() 
# 




################################
"Result of grades with covariates"
################################
# data = df 
# ## rdrobust with covariates within the same window (i.e., using same bandwidths)
# grade = "8"
# data_ = data #[data$GRADO<=5 & data$GRADO>=0,  ]
# 
# # subset(data, data$GRADO==grade)
# est1 <- rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]] )
# len1 <- est1$ci[3,2] - est1$ci[3,1]
# covariates = c(
#   "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
#   "FRAC_SUBSIDIADO" ,    "elevation"     ,   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD"
# )
# 
# est2 <- rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]], 
#                  covs=data_[covariates], 
#                  h = c(est$bws[1,1], est$bws[1,2]), 
#                  b = c(est$bws[2,1], est$bws[2,2])
#                  )
# summary(est2)
# summary(model)
# len2 <- est2$ci[3,2] - est2$ci[3,1]
# paste("CI length change: ", round((len2/len1-1)*100,2), "%")
####################################################################################################
##################################################
####################################################################################################

#########################################
#####################################
"Causal Effect on elementary School"
#####################################

############################################
## General Inference 
###########################################


data =df
data = subset(data, data$GRADO_ <= 5)
data = subset(data, data$GRADO_ >= 0)
table(data$GRADO)

library(haven)
# haven::write_dta(data,paste0(data_dir,"elementary_school_data.dta") )
Running_variable <- 'distance'
Outcome <- 'DESERTO_T2'
Outcome_male <- 'MALE_DROPOUT_T2'
Outcome_female <- 'FEMALE_DROPOUT_T2'

q_= 'q_all'


est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , all=TRUE)
summary(est)
model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                 subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                 kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )

summary(model)
"In this case, a one unit decrease in the ATE is associated with a 32.68% 
  decrease in the mean of the control group."
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular",
       h=c(est$bws[1,1], est$bws[1,2]),  p=1 )

png(paste0(graphs_dir,"general_result_",Outcome, "_", q_ , ".png"),  width = 1030, height = 598, res = 105 )
model_pic  <- model_outputs_plot(est= est, data = data, Running_variable, Outcome)
dev.off()

picture_to_latex(paste0("general_result_",Outcome, "_", q_ , ".png" ) ,  
                 name = "General Result in Elementary School", level = level)

bw_inference = model_pic[[2]]

png(paste0(graphs_dir,"bw_sensibility_", Outcome ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                           bw_mse = bw_inference,
                           conf_level = 0.95 , full = 10)[2]
dev.off()


picture_to_latex(paste0("bw_sensibility_", Outcome ,"_",q_,  ".png") ,  
                 name = "General Bandwidth sensibility test in Elementary School", level = level)


rd_table_latex = rd_table(model)
cat(rd_table_latex)
writeLines( text = rd_table_latex, paste0(tables_dir,"general_result_" , Outcome, "_",q_, ".tex") )

##################
"General Effect by gender"
##################

Running_variable <- 'distance'




"EFFECT ON BOYS "
est_male = rdrobust(y=data[[Outcome_male]] , x=data[[Running_variable]] , all=TRUE)
summary(est_male)
png(paste0(graphs_dir,"general_result_",Outcome_male, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_male = model_outputs_plot(est= est_male, data = data, Running_variable, Outcome_male)
dev.off()

picture_to_latex(paste0("general_result_",Outcome_male, "_", q_ , ".png" ) ,  
                 name = "Dropout of males in Elementary School", level = level)


png(paste0(graphs_dir,"bw_sensibility_", Outcome_male ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_male, Running_variable=Running_variable,
                           bw_mse = model_pic_male[[2]],
                           conf_level = 0.95 , full = 10)[2]
dev.off() 

picture_to_latex(paste0("bw_sensibility_", Outcome_male ,"_",q_,  ".png" ) ,  
                 name = "Bandwidth sensibility test of males in Elementary School", level = level)


writeLines( text = rd_table(model_pic_male[[3]]), 
            paste0(tables_dir,"general_result_" , Outcome_male, "_",q_, ".tex") )

cat(rd_table(model_pic_male[[3]]))
"EFFECT ON  GIRLS "
est_female = rdrobust(y=data[[Outcome_female]] , x=data[[Running_variable]] , all=TRUE)
# summary(est_female)
png(paste0(graphs_dir,"general_result_",Outcome_female, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_female = model_outputs_plot(est= est_female, data = data, Running_variable, Outcome_female)
dev.off()

picture_to_latex(paste0("general_result_",Outcome_female, "_", q_ , ".png" ) ,  
                 name = "Dropout of females in Elementary School", level = level)

png(paste0(graphs_dir,"bw_sensibility_", Outcome_female ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_female, Running_variable=Running_variable,
                           bw_mse = model_pic_female[[2]],
                           conf_level = 0.95 , full = 10)[2]
dev.off()
picture_to_latex(paste0("bw_sensibility_", Outcome_female ,"_",q_,  ".png" ) ,  
                 name = "Bandwidth sensibility test of females in Elementary School", level = level)
cat(rd_table(model_pic_female[[3]]))
writeLines( text = rd_table(model_pic_female[[3]]),  paste0(tables_dir,"general_result_" , Outcome_female, "_",q_, ".tex") )
#####################################################

grades <- c('0','1' , '2', '3', '4', '5'  )

Running_variable <- 'distance'



if (1==1) {
  data =df
  data = subset(data, data$GRADO_ <= 5)
  data = subset(data, data$GRADO_ >= 0)
  q_= 'q_all'
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  dev.off()
  
  
  picture_to_latex(paste0("Inference_by_grade_", Outcome,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade", 
                   level = level)
  ##############################
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome_male , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  
  dev.off()
  picture_to_latex(paste0("Inference_by_grade_", Outcome_male,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade of male students", 
                   level = level)
  png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                             Outcome = Outcome_female , conf_level = 0.9,
                             TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  dev.off()
  picture_to_latex(paste0("Inference_by_grade_", Outcome_female,"_" ,q_, ".png") ,  
                   name =  "General Inference by grade of female students", 
                   level = level)
}

if (1==1) {
  q_= 'q_12'
  
  
  data =df
  data = subset(data, data$GRADO_ <= 5)
  data = subset(data, data$GRADO_ >= 0)
  data =  subset(data, data$Q_artyl <= 2)
  
  if (1==1) {
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
    
    
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_male , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    
    dev.off()
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_female , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
  }
}

if (1==1) {
  q_= 'q_34'
  
  
  data =df
  data = subset(data, data$GRADO_ <= 5)
  data = subset(data, data$GRADO_ >= 0)
  data =  subset(data, data$Q_artyl > 2)
  
  if (1==1) {
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
    
    
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_male , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    
    dev.off()
    
    png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
    summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
                               Outcome = Outcome_female , conf_level = 0.9,
                               TITULO = '', heterogenity =grades,estiamtion =  "Robust")
    dev.off()
    
  }
}

# 
# png(paste0(graphs_dir,"general_result_",Outcome_male, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
# model_pic_male = model_outputs_plot(est= est_male, data = data, Running_variable, Outcome_male)
# dev.off()
# 
# 
# png(paste0(graphs_dir,"bw_sensibility_", Outcome_male ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
# bandwidth_sensibility_test(data= data,Outcome=Outcome_male, Running_variable=Running_variable,
#                            bw_mse = model_pic_male[[2]],
#                            conf_level = 0.95 , full = 30)[2]
# dev.off() 
# 




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




