# Store the project ID
projectid = "ph-jabri"

# Set your query
 
sql <- ' SELECT cast(CODIGO_DANE_SEDE as string) as CODIGO_DANE_SEDE
                , * except( CODIGO_DANE_SEDE ),  cast( QUARTYL_FIRM as float64) Q_artyl , 
                safe_cast(GRADO as float64) as GRADO_ 
                FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.cod_dane_year_dropout`  ' 

# Run the query; this returns a bq_table object that you can query further
df <- bq_project_query(projectid, sql)

# Store the first 10 rows of the data in a tibble
df <-bq_table_download(df, n_max = Inf)

# Print the 10 rows of data
 str(df)

 