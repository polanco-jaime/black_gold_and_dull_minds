library(dplyr)
data = df
data = subset(data, data$distance <= 5000 & data$distance  >= -5000)
data = subset(data, as.numeric(data$GRADO)>=0 & as.numeric(data$GRADO)<=11 )
variable_list = c( 
  "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
  "FRAC_SUBSIDIADO"    ,   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD",
  "DESERTO_T1",'MALE_DROPOUT_T1' , 'FEMALE_DROPOUT_T1' ,
  "EMIGRATED_T1", "IMIGRATED_T1", "EMIGRATED_OUT_AC_AT_T1",
  "DESERTO_T2", 'MALE_DROPOUT_T2' , 'FEMALE_DROPOUT_T2' ,
  "EMIGRATED_T2", "IMIGRATED_T2", "EMIGRATED_OUT_AC_AT_T2"
)


descriptive_table  = data.frame()
for (i in variable_list) {
  data1 <- data %>% filter(IS_IN_T1 == T)
  data0 <- data %>% filter(IS_IN_T1 == F)
  
  temp = data.frame("Variable" = i ,
                    "N of treated" = nrow(data1),
                    "mean of treated" = paste0(round(mean(data1[[i]]), 2) ,"  (", round(sd(data1[[i]]), 2) , ")" ),
                    "N of control" = nrow(data0),
                    "mean of control"  = paste0(round(mean(data0[[i]]), 2) ,"  (", round(sd(data0[[i]]), 2) , ")" ),
                    "Difference" = paste0(round(mean(data1[[i]]) -mean(data0[[i]])  , 3) ,
                                          "  (", round(sd(data1[[i]])- sd(data0[[i]]), 3) , ")" ),
                    "Pr(>|t|)" =  round( summary(lm(data=data, as.formula( paste0(i, ' ~ ',   "IS_IN_T1" ) )  ) )[["coefficients"]][2,4] 
                                         , 3)
  )
  
  descriptive_table = rbind(temp, descriptive_table)
}
cat(df_to_latex(descriptive_table))

A = gsub(df_to_latex(descriptive_table) , pattern = "" , replacement = "")

A = gsub(A , pattern = "EMIGRATED_OUT_AC_AT_" , replacement = "EMIGRATED OUT $A^C$ $A^T$ ")
A = gsub(A , pattern = "FEMALE_DROPOUT_" , replacement = "FEMALE DROPOUT RATE ")
A = gsub(A , pattern = "DESERTO_" , replacement = "DROPOUT RATE ")
A = gsub(A , pattern = "MALE_DROPOUT_" , replacement = "MALE DROPOUT RATE ")
A = gsub(A , pattern = "FRAC_ESTRATO_1_2" , replacement = "LOW ECONOMIC LEVEL")
A = gsub(A , pattern = "REPITENTE" , replacement = "REPEATERS ")
A = gsub(A , pattern = "NUEVO" , replacement = "NEW STUDENTS")
A = gsub(A , pattern = "SUBSIDIADO" , replacement = "SUBSIDIZED")
A = gsub(A , pattern = "FRAC_ESTRATO_3_4" , replacement = "MEDIUM ECONOMIC LEVEL")
A = gsub(A , pattern = "EDAD" , replacement = "AGE")
A = gsub(A , pattern = "_" , replacement = " ")
# A = gsub(A , pattern = "" , replacement = "")

cat(A  )

library(stringr)

#################
"Estadisticas descriptivas por grado"
##############

variable_list = c( 
  "FRAC_FEMALE"   ,      "FRAC_MALE", "FRAC_NUEVO", "FRAC_REPITENTE" ,
  "FRAC_SUBSIDIADO"    ,   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD",
  "DESERTO_T1",'MALE_DROPOUT_T1' , 'FEMALE_DROPOUT_T1' ,  "DESERTO_T2", 'MALE_DROPOUT_T2' , 'FEMALE_DROPOUT_T2' 
)
descriptive_table  = data.frame()
grades <- c('1' , '2', '3', '4', '5' , '6','7', '8' ,'9', '10', '11'  )
data$level = ifelse(as.numeric(data$GRADO) <=5, 'Elementary', 'Secondary' )
for (grade in unique(data$level)) {
  data1 <- data %>% filter(IS_IN_T1 == T)
  data0 <- data %>% filter(IS_IN_T1 == F)
  data1 <- data1 %>% subset(data1$level== grade  )
  data0 <- data0 %>% subset(data0$level== grade  )
  for (i in variable_list) {

    
    temp = data.frame("Variable" = i ,
                      'level' = grade,
                      # "N of treated" = nrow(data1),
                      "mean of treated" = paste0(round(mean(data1[[i]]), 2) ,"  (", round(sd(data1[[i]]), 2) , ")" ),
                      # "N of control" = nrow(data0),
                      "mean of control"  = paste0(round(mean(data0[[i]]), 2) ,"  (", round(sd(data0[[i]]), 2) , ")" ),
                      "Difference" = paste0(round(mean(data1[[i]]) -mean(data0[[i]])  , 3) ,
                                            "  (", round(sd(data1[[i]])- sd(data0[[i]]), 3) , ")" ) )
    
    descriptive_table = rbind(temp, descriptive_table)
  }
}


cat(df_to_latex(descriptive_table) )



