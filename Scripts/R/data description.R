#########################################
## Load required settings
#########################################
source("./Scripts/R/general_settings.R ")
# source("./Scripts/R/Reading_data.R")

source("./Scripts/R/functions.R")
attach(df)

df= subset(df, df$distance< 8500  & df$distance >-8500 )

length(unique(df$CODIGO_DANE_SEDE)) # 1625

# escuelas a menos de 8.5 km de un borde
length(unique(subset(df, df$distance< 0  )$CODIGO_DANE_SEDE) ) #control 762
length(unique(subset(df, df$distance>= 0   )$CODIGO_DANE_SEDE) ) # treat 863

# Escuelas a menos de 8.5 de escuela elemental
 
length(unique(subset(df, df$distance< 0   & as.numeric(df$GRADO)<=5 )$CODIGO_DANE_SEDE) ) #control 728
length(unique(subset(df, df$distance>= 0     & as.numeric(df$GRADO)<=5 )$CODIGO_DANE_SEDE) ) # treat 817

# Escuelas a menos de 8.5 de escuela secondaria
length(unique(subset(df, df$distance< 0      & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 )$CODIGO_DANE_SEDE) ) #control 236
length(unique(subset(df, df$distance>= 0   & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 )$CODIGO_DANE_SEDE) ) # treat 254

# Escuelas a menos de 8.5 de escuela elemental con mas de 10 estudiantes
summary(df$TOTAL_STUDENTS)
length(unique(subset(df, df$distance< 0   & as.numeric(df$GRADO)<=5 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) #control 424
length(unique(subset(df, df$distance>= 0     & as.numeric(df$GRADO)<=5 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) # treat 528

# Escuelas a menos de 8.5 de escuela secondaria
length(unique(subset(df, df$distance< 0      & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) #control 220
length(unique(subset(df, df$distance>= 0   & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) # treat 246


# Escuelas a menos de 8.5 de escuela elemental con mas de 10 estudiantes y segun naturaleza
df$SECTOR
(table(subset(df, df$distance< 0   & as.numeric(df$GRADO)<=5 & df$TOTAL_STUDENTS >=10  )$SECTOR) ) #control 424
(table(subset(df, df$distance>= 0     & as.numeric(df$GRADO)<=5 & df$TOTAL_STUDENTS >=10  )$SECTOR) ) # treat 528

# Escuelas a menos de 8.5 de escuela secondaria
length(unique(subset(df, df$distance< 0      & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) #control 220
length(unique(subset(df, df$distance>= 0   & as.numeric(df$GRADO)>5 & as.numeric(df$GRADO)<11 & df$TOTAL_STUDENTS >=10  )$CODIGO_DANE_SEDE) ) # treat 246


############################################## Student Characteristics #################################################
 
(summary(subset(df, df$distance< 0   & as.numeric(df$GRADO)<=5  )$FRAC_ESTRATO_1_2) ) #control 0.9097
(summary(subset(df, df$distance>= 0  & as.numeric(df$GRADO)> 5 & as.numeric(df$GRADO)<=11 )$FRAC_ESTRATO_3_4) ) # treat 0.8336
##


summary(subset(df, df$distance< 0  & df$distance >-8500 )$distance)
hist(subset(df, df$distance< 0  & df$distance >-8500 )$distance)
# /Treat
summary(subset(df, df$distance>= 0   & df$distance <= 8500)$distance)
hist(subset(df, df$distance>= 0   & df$distance <= 8500)$distance)
