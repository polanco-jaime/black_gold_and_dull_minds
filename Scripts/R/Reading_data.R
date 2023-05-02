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
                safe_cast(GRADO as float64) as GRADO_ 
                FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.cod_dane_year_dropout`  ' 

# Run the query; this returns a bq_table object that you can query further
df <- bq_project_query(projectid, sql)

# Store the first 10 rows of the data in a tibble
df <-bq_table_download(df, n_max = Inf)


 

 