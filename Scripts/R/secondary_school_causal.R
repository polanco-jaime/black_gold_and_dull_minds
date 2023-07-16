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
  data_dir <- paste0(General_path , "Data/Secondary_school/")
  graphs_dir <-  paste0(General_path , "Graph/Secondary_school/") 
  tables_dir <- paste0(General_path , "Tables/Secondary_school/")  
}

############################################
## General Inference 
###########################################



level = 'Secondary_school'
data =df
# data = subset(data, data$TOTAL_STUDENTS >= 1 & data$TOTAL_STUDENTS <= 10)
# data <- data[data$Q_artyl<=3, ]
Running_variable <- 'distance'
Outcome_male <- 'MALE_DROPOUT_T1'
Outcome_female <- 'FEMALE_DROPOUT_T1'
Outcome <- 'DESERTO_T1'

grades = grades_effect_extention(Outcome= Outcome, level = 'secondary')

# data <- data[data$GRADO %in% c(grades, "11"), ]

data <- data[data$GRADO %in% grades , ]

# library(haven)
# haven::write_dta(data,paste0(data_dir,"T1_","secondary_school_data.dta") )

q_= 'q_all'
unique(data$GRADO)
est = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] ,all=TRUE)
summary(est)
model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                 subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                 kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )

summary(model)
# "In this case, a one unit decrease in the ATE is associated with a 32.68% 
#   decrease in the mean of the control group."
rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
       binselect="es", kernel="triangular",
       h=c(est$bws[1,1], est$bws[1,2]),  p=1 )
# # ?rdplot

png(paste0(graphs_dir,"general_result_",Outcome, "_", q_ , ".png"),  width = 1030, height = 598, res = 100 )
model_pic  <- model_outputs_plot(est= est, data = data, Running_variable, Outcome)
dev.off()

picture_to_latex(paste0("general_result_",Outcome, "_", q_ , ".png" ) ,  
                 name = "General Result in Secondary School", level = level)


bw_inference = model_pic[[2]]

png(paste0(graphs_dir,"bw_sensibility_", Outcome ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome, Running_variable=Running_variable,
                           bw_mse = bw_inference,
                           conf_level = 0.90 , full = 30)[2]
dev.off()


picture_to_latex(paste0("bw_sensibility_", Outcome ,"_",q_,  ".png") ,  
                 name = "General Bandwidth sensibility test in Secondary School", level = level)

rd_table_latex = rd_table(model)
cat(rd_table_latex)
writeLines( text = rd_table_latex, paste0(tables_dir,"general_result_" , Outcome, "_",q_, ".tex") )

##################
"General Effect by gender"
##################
 
Running_variable <- 'distance'
 

data <- data[data$GRADO %in% grades, ]
# data <- data[data$GRADO %in% c(grades, '11'), ]


table(data$GRADO)
 


"EFFECT ON BOYS "
est_male = rdrobust(y=data[[Outcome_male]] , x=data[[Running_variable]] , all=TRUE)

png(paste0(graphs_dir,"general_result_",Outcome_male, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_male = model_outputs_plot(est= est_male, data = data, Running_variable, Outcome_male)
dev.off()


png(paste0(graphs_dir,"bw_sensibility_", Outcome_male ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_male, Running_variable=Running_variable,
                           bw_mse = model_pic_male[[2]],
                           conf_level = 0.9 , full = 30)[2]
dev.off() 

writeLines( text = rd_table(model_pic_male[[3]]), 
            paste0(tables_dir,"general_result_" , Outcome_male, "_",q_, ".tex") )


"EFFECT ON  GIRLS "
est_female = rdrobust(y=data[[Outcome_female]] , x=data[[Running_variable]] , all=TRUE)
# summary(est_female)
png(paste0(graphs_dir,"general_result_",Outcome_female, "_", q_ , ".png"),  width = 1030, height = 598, res = 100)
model_pic_female = model_outputs_plot(est= est_female, data = data, Running_variable, Outcome_female)
dev.off()

png(paste0(graphs_dir,"bw_sensibility_", Outcome_female ,"_",q_,  ".png"),  width = 1030, height = 598, res = 100)
bandwidth_sensibility_test(data= data,Outcome=Outcome_female, Running_variable=Running_variable,
                           bw_mse = model_pic_female[[2]],
                           conf_level = 0.95 , full = 30)[2]
dev.off()
writeLines( text = rd_table(model_pic_female[[3]]),  paste0(tables_dir,"general_result_" , Outcome_female, "_",q_, ".tex") )
#####################################################



############################################
##  Inference by grade 
###########################################




if (1==1) {
  # data =df
  # grades = grades_effect_extention(Outcome= Outcome, level = 'secondary')
  # data <- data[data$GRADO %in% grades, ]
  q_= 'q_all'
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

if (1==1) {
  # q_= 'q_12'
  # 
  # 
  # data =df
  # data = subset(data, data$GRADO_ <= 11)
  # data = subset(data, data$GRADO_ >= 6)
  # data =  subset(data, data$Q_artyl <= 2)
  # 
  # if (1==1) {
  #   
  #   png(paste0(graphs_dir,"Inference_by_grade_", Outcome,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  #   summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
  #                              Outcome = Outcome , conf_level = 0.9,
  #                              TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  #   dev.off()
  #   
  #   
  #   
  #   
  #   png(paste0(graphs_dir,"Inference_by_grade_", Outcome_male,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  #   summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
  #                              Outcome = Outcome_male , conf_level = 0.9,
  #                              TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  #   
  #   dev.off()
  #   
  #   png(paste0(graphs_dir,"Inference_by_grade_", Outcome_female,"_" ,q_, ".png"),  width = 1030, height = 598, res = 100)
  #   summary_plot_heterogenity( data = data, Running_variable=Running_variable, 
  #                              Outcome = Outcome_female , conf_level = 0.9,
  #                              TITULO = '', heterogenity =grades,estiamtion =  "Robust")
  #   dev.off()
  #   
  # }
}
sum(subset(df, df$IS_IN_T1 == T   )$ESTU_TOTALES_T1)

sum(subset(df, df$IS_IN_T1 == T & df$GRADO== "1"   )$ESTU_TOTALES_T1)

sum(subset(df, df$IS_IN_T1 == T   )$ESTU_TOTALES_T1)

sum(subset(df, df$IS_IN_T1 == T   )$ESTU_TOTALES_T1)

if (1==1) {
  q_= 'q_34'
  
  
  data =df
  grades_effect_extention(Outcome= Outcome, level = 'secondary')
  data <- data[data$GRADO %in% grades, ]
  
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
