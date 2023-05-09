# Store the project ID
projectid = "ph-jabri"

# Set your query
 
sql <- ' SELECT cast(CODIGO_DANE_SEDE as string) as CODIGO_DANE_SEDE
                , * except( CODIGO_DANE_SEDE ), 
                CASE 
                    WHEN EXTRACT(month FROM SAFE_CAST( FECHA_FIRM AS DATE)) BETWEEN 0 AND 3 THEN 1
                    WHEN EXTRACT(month FROM SAFE_CAST( FECHA_FIRM AS DATE)) BETWEEN 3 AND 6 THEN 2
                    WHEN EXTRACT(month FROM SAFE_CAST( FECHA_FIRM AS DATE)) BETWEEN 6 AND 9 THEN 3
                    WHEN EXTRACT(month FROM SAFE_CAST( FECHA_FIRM AS DATE)) BETWEEN 9 AND 12 THEN 4
                    ELSE NULL END  Q_artyl , 
                    EXTRACT(year FROM SAFE_CAST( FECHA_FIRM AS DATE)) YEAR_FRIM, 
                safe_cast(GRADO as float64) as GRADO_ , 
                FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.cod_dane_year_dropout`  ' 

# Run the query; this returns a bq_table object that you can query further
df <- bq_project_query(projectid, sql)

# Store the first 10 rows of the data in a tibble
df <-bq_table_download(df, n_max = Inf)

columnas = c("MALE_DROPOUT_T2" , "FEMALE_DROPOUT_T2" , "FEMALE_T2" ,  "MALE_T2" , "MALE_DROPOUT_T1" , "FEMALE_DROPOUT_T1" , "FEMALE_T1" ,  "MALE_T1")          
for (col in columnas ) {
  df[[col]] = ifelse( is.na(df[[col]]  ) , 0, df[[col]]   ) 
  print(summary(df[[col]]))
}
options(scipen=999)
df[["MALE_DROPOUT_T2" ]] = (df$FRAC_MALE*df[["MALE_DROPOUT_T2" ]] / df[["ESTU_TOTALES_T2" ]] )
df[["FEMALE_DROPOUT_T2" ]] = (df$FRAC_FEMALE*df[["FEMALE_DROPOUT_T2" ]] / df[["ESTU_TOTALES_T2" ]] )

df[["MALE_DROPOUT_T1" ]] = (df$FRAC_MALE*df[["MALE_DROPOUT_T1" ]] / df[["ESTU_TOTALES_T1" ]] )
df[["FEMALE_DROPOUT_T1" ]] = (df$FRAC_FEMALE*df[["FEMALE_DROPOUT_T1" ]] / df[["ESTU_TOTALES_T1" ]] )


variation = c("T1", "T2", "TL1")
Columnas =  c("DESERTO_", "STILL_SAME_SCHOOL_", "EMIGRATED_", "IMIGRATED_",
              "EMIGRATED_OUT_AC_AT_"  )
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

