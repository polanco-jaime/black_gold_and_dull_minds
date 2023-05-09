CREATE OR REPLACE TABLE `ph-jabri.01_Black_Gold_and_Dull_Minds.cod_dane_year_dropout` 
OPTIONS () AS
WITH
/*--------------------*/
/* CON ESTA BASE INTENTO ENCONTRAR LOS ESTUDIANTES MATRICULADOS Y EL GRADO QUE CURSABAN EN ESCUELAS DE AREAS DE INFLUENCIA EN EL AÑO DE FIRMA */
/*--------------------*/
COLEGIOS_TRATADO_TIEMPO_  AS 
        (
          SELECT DISTINCT COD_COL,   
                      SPLIT(FECHA_FIRM, '-')[OFFSET(0)] YEAR_FIRM, IS_IN
                FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.E_a_P_distancias_sedes` 
        ),
/*--------------------*/
/*  729346 REGISTROS */
/*--------------------*/
ESTUDIANTES_YEAR_TREATED AS
        (
          SELECT * FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.cod_dane_year_treated` 
        ),
/*--------------------*/
/*    729346 REGISTROS */
/*--------------------*/
BASE AS 
        (
          SELECT *  FROM ESTUDIANTES_YEAR_TREATED
        ), 
/*--------------------*/
/*  */
/*--------------------*/


cod_dane_year_treated AS 
        (
          SELECT 
            DISTINCT NRO_DOCUMENTO, GRADO, SIMAT_YEAR, YEAR_FIRM_TL1,
                            CODIGO_DANE_SEDE, FECHA_NACIMIENTO, 
                            extract(year from FECHA_NACIMIENTO) BIRTH_YEAR , IS_IN_tl1   
          FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.simat`
          INNER JOIN  
              (SELECT  COD_COL , (CAST(YEAR_FIRM as int64)-1)   YEAR_FIRM_TL1   , IS_IN as IS_IN_tl1 FROM  COLEGIOS_TRATADO_TIEMPO_)
          ON COD_COL = CODIGO_DANE_SEDE AND SIMAT_YEAR = YEAR_FIRM_TL1
        ) ,
/*--------------------*/
/* ACA SELECCIONO TODOS LOS ESTUDIANTES CON RELACION ALGUNA UN AÑO DESPUES DEL TRATAMIENTO EN CUALQUIER GRADO */
/*--------------------*/ 


SIMAT_ESTUDIANTES_T_PLUS_1 AS 
    (
         SELECT
                CODIGO_DANE_SEDE ,  CAST( GRADO AS STRING ) GRADO,
                IS_IN IS_IN_T1, 
                SUM(CASE WHEN DESERTO IS TRUE THEN 1 ELSE 0 END) DESERTO_T1,
                COUNT(DISTINCT NRO_DOCUMENTO ) ESTU_TOTALES_T1, 
                SUM(   CASE WHEN STILL_SAME_SCHOOL IS FALSE THEN 0 ELSE 1 END ) STILL_SAME_SCHOOL_T1,
                SUM(   CASE WHEN EMIGRATED IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_T1,
                SUM(   CASE WHEN IMIGRATED IS FALSE THEN 0 ELSE 1 END ) IMIGRATED_T1,
                SUM(   CASE WHEN EMIGRATED_OUT_AC_AT IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_OUT_AC_AT_T1,
                SUM(MALE_DROPOUT) AS MALE_DROPOUT_T1, 
                SUM(FEMALE_DROPOUT) AS FEMALE_DROPOUT_T1, 
          FROM (
                SELECT DISTINCT
                IFNULL(DESERTO, TRUE) AS DESERTO,
                BASE.* , GRADO AS GRADO_T_1 ,
                IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) STILL_SAME_SCHOOL,
                IS_IN_SIMAT  , 
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AC Y LUEGO FUE A AT ENTONCES IMIGRO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN IS FALSE AND IS_IN_SIMAT IS TRUE THEN 
                TRUE ELSE FALSE END IMIGRATED,
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AT Y LUEGO FUE A AREA CONTROL ENTONCES EMIGRO*/
                  CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN IS TRUE AND IS_IN_SIMAT IS FALSE THEN 
                TRUE ELSE FALSE END EMIGRATED,
                /* NO DESERTO, NO SIGUE EN EL MISMO COLEGIO Y NO ESTA NI EN AT NI EN AC ENTONCES EMIGRO FUERA DEL ESTUDIO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN_SIMAT IS NULL THEN 
                TRUE ELSE FALSE END EMIGRATED_OUT_AC_AT,
                /* SI DESERTO  Y ES HOMBRE O ES MUJER ENTONCES 1 */
                CASE WHEN DESERTO IS NULL AND MALE = 1 THEN 1 ELSE 0 END MALE_DROPOUT,
                CASE WHEN DESERTO IS NULL AND FEMALE = 1 THEN 1 ELSE 0 END FEMALE_DROPOUT,
                
                FROM  BASE  -- /*  */
                /*  En esta etapa la base apodada B toma todos los estudiantes desde grado 1 hasta 11*/
                LEFT JOIN (   SELECT NRO_DOCUMENTO ,  SIMAT_YEAR , FALSE AS DESERTO, CODIGO_DANE_SEDE, IS_IN AS IS_IN_SIMAT , MALE, FEMALE FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.simat`  ) B
                ON  BASE.NRO_DOCUMENTO = B.NRO_DOCUMENTO AND (BASE.SIMAT_YEAR +1) = B.SIMAT_YEAR   
                /*  el left join pretende encontrar estudiantes un año despues del tratamiento, A +1  cruza con B */
                          )
          GROUP BY 1,2,3
        ),
SIMAT_ESTUDIANTES_T_PLUS_2 AS 
          (
         SELECT
                CODIGO_DANE_SEDE ,  CAST( GRADO AS STRING ) GRADO,
                IS_IN IS_IN_T2, 
                SUM(CASE WHEN DESERTO IS TRUE THEN 1 ELSE 0 END) DESERTO_T2,
                COUNT(DISTINCT NRO_DOCUMENTO ) ESTU_TOTALES_T2, 
                SUM(   CASE WHEN STILL_SAME_SCHOOL IS FALSE THEN 0 ELSE 1 END ) STILL_SAME_SCHOOL_T2,
                SUM(   CASE WHEN EMIGRATED IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_T2,
                SUM(   CASE WHEN IMIGRATED IS FALSE THEN 0 ELSE 1 END ) IMIGRATED_T2,
                SUM(   CASE WHEN EMIGRATED_OUT_AC_AT IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_OUT_AC_AT_T2,
                SUM(MALE_DROPOUT) AS MALE_DROPOUT_T2, 
                SUM(FEMALE_DROPOUT) AS FEMALE_DROPOUT_T2, 
                
          FROM (
                SELECT DISTINCT
                IFNULL(DESERTO, TRUE) AS DESERTO,
                BASE.* , GRADO AS GRADO_T_1 ,
                IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) STILL_SAME_SCHOOL,
                IS_IN_SIMA IS_IN_SIMAT  , 
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AC Y LUEGO FUE A AT ENTONCES IMIGRO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN IS FALSE AND IS_IN_SIMA IS TRUE THEN 
                TRUE ELSE FALSE END IMIGRATED,
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AT Y LUEGO FUE A AREA CONTROL ENTONCES EMIGRO*/
                  CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN IS TRUE AND IS_IN_SIMA IS FALSE THEN 
                TRUE ELSE FALSE END EMIGRATED,
                /* NO DESERTO, NO SIGUE EN EL MISMO COLEGIO Y NO ESTA NI EN AT NI EN AC ENTONCES EMIGRO FUERA DEL ESTUDIO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( BASE.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND IS_IN_SIMA IS NULL THEN 
                TRUE ELSE FALSE END EMIGRATED_OUT_AC_AT,
                /* SI DESERTO  Y ES HOMBRE O ES MUJER ENTONCES 1 */
                CASE WHEN DESERTO IS NULL AND MALE = 1 THEN 1 ELSE 0 END MALE_DROPOUT,
                CASE WHEN DESERTO IS NULL AND FEMALE = 1 THEN 1 ELSE 0 END FEMALE_DROPOUT,
                
                FROM  BASE  -- /*  */
                /*  En esta etapa la base apodada B toma todos los estudiantes desde grado 1 hasta 11*/
                LEFT JOIN (   SELECT NRO_DOCUMENTO ,  SIMAT_YEAR , FALSE AS DESERTO, CODIGO_DANE_SEDE, IS_IN AS IS_IN_SIMA , MALE, FEMALE    FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.simat`  ) B
                ON  BASE.NRO_DOCUMENTO = B.NRO_DOCUMENTO AND (BASE.SIMAT_YEAR +2) = B.SIMAT_YEAR   
                /*  el left join pretende encontrar estudiantes un año despues del tratamiento, A +2  cruza con B */
                          )
          GROUP BY 1,2,3
        ),



SIMAT_ESTUDIANTES_T_LESS_1 AS 
        (
          SELECT
                CODIGO_DANE_SEDE ,  CAST( GRADO AS STRING ) GRADO,
                IS_IN_tl1, 
                SUM(CASE WHEN DESERTO IS TRUE THEN 1 ELSE 0 END) DESERTO_TL1,
                COUNT(DISTINCT NRO_DOCUMENTO ) ESTU_TOTALES_TL1, 
                SUM(   CASE WHEN STILL_SAME_SCHOOL IS FALSE THEN 0 ELSE 1 END ) STILL_SAME_SCHOOL_TL1,
                SUM(   CASE WHEN EMIGRATED IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_TL1,
                SUM(   CASE WHEN IMIGRATED IS FALSE THEN 0 ELSE 1 END ) IMIGRATED_TL1,
                SUM(   CASE WHEN EMIGRATED_OUT_AC_AT IS FALSE THEN 0 ELSE 1 END ) EMIGRATED_OUT_AC_AT_TL1,
          FROM (
                SELECT DISTINCT
                IFNULL(DESERTO, TRUE) AS DESERTO,A.* , GRADO_T_1 ,
                  -- A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE STILL_SAME_SCHOOL
                IFNULL( A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) STILL_SAME_SCHOOL,
                B.IS_IN as IS_IN_SIMAT  , 
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AC Y LUEGO FUE A AT ENTONCES IMIGRO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND A.IS_IN_tl1 IS FALSE AND B.IS_IN  IS TRUE THEN 
                TRUE ELSE FALSE END IMIGRATED,
                /* SI NO DESERTO Y CAMBIO DE COELGIO E INICIALMENTE SE ENCONTRABA EN AT Y LUEGO FUE A AREA CONTROL ENTONCES EMIGRO*/
                  CASE WHEN DESERTO IS FALSE AND IFNULL( A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND A.IS_IN_tl1 IS TRUE AND B.IS_IN  IS FALSE THEN 
                TRUE ELSE FALSE END EMIGRATED,
                /* NO DESERTO, NO SIGUE EN EL MISMO COLEGIO Y NO ESTA NI EN AT NI EN AC ENTONCES EMIGRO FUERA DEL ESTUDIO*/
                CASE WHEN DESERTO IS FALSE AND IFNULL( A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE, FALSE) IS FALSE AND B.IS_IN  IS NULL THEN 
                TRUE ELSE FALSE END EMIGRATED_OUT_AC_AT,

                  FROM  cod_dane_year_treated A -- /*  */
                  LEFT JOIN (
                            SELECT * , FALSE AS DESERTO, GRADO AS GRADO_T_1 
                              FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.simat` 
                             -- WHERE  cast( grado as numeric) BETWEEN 0 AND 11
                            ) B  
                  /*  En esta etapa la base apodada B toma todos los estudiantes desde grado 1 hasta 11*/
                  ON  A.NRO_DOCUMENTO = B.NRO_DOCUMENTO AND (A.SIMAT_YEAR +1) = B.SIMAT_YEAR  
                  /*  el left join pretende encontrar estudiantes un año despues del tratamiento, A +1  cruza con B */
                )
          GROUP BY 1,2,3
        ),
/*--------------------*/
/*  18518 registros */ 
/*--------------------*/ 
T1_T2 AS(
        SELECT A.*,
                B.DESERTO_T2,
                B.ESTU_TOTALES_T2,
                STILL_SAME_SCHOOL_T2,
                EMIGRATED_T2,
                IMIGRATED_T2,
                EMIGRATED_OUT_AC_AT_T2,
                MALE_DROPOUT_T2 , 
                FEMALE_DROPOUT_T2
        FROM SIMAT_ESTUDIANTES_T_PLUS_1 A
        LEFT JOIN SIMAT_ESTUDIANTES_T_PLUS_2 B
        ON A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE AND A.GRADO=B.GRADO AND A.IS_IN_T1 =B.IS_IN_T2
), 
T12_L1 AS (
            select A.*,
                  DESERTO_TL1,
                  ESTU_TOTALES_TL1,
                  STILL_SAME_SCHOOL_TL1,
                  EMIGRATED_TL1,
                  IMIGRATED_TL1,
                  EMIGRATED_OUT_AC_AT_TL1,
            from T1_T2 A
            LEFT JOIN SIMAT_ESTUDIANTES_T_LESS_1 B
            ON A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE AND A.GRADO=B.GRADO AND A.IS_IN_T1 =B.IS_IN_TL1
),
DISTANCIAS AS (

              SELECT * FROM T12_L1
              INNER JOIN (SELECT * EXCEPT(IS_IN ), 
                                  CASE WHEN IS_IN  IS TRUE THEN 1 ELSE 0 END AS treatment
                                  FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.E_a_P_distancias_sedes` )
              ON CODIGO_DANE_SEDE =COD_COL
)
SELECT * FROM DISTANCIAS A
INNER JOIN (SELECT * EXCEPT(CODIGO_DANE_SEDE, GRADO), CODIGO_DANE_SEDE AS CODIGO_DANE_SEDE_COV, GRADO AS GRADO_COV FROM `ph-jabri.01_Black_Gold_and_Dull_Minds.simat_covariates`) B
        ON A.CODIGO_DANE_SEDE = B.CODIGO_DANE_SEDE_COV
        AND EXTRACT(YEAR FROM SAFE_CAST( FECHA_FIRM AS DATE)) = SIMAT_YEAR
        AND SAFE_CAST(A.GRADO  AS INT64)= B.GRADO_COV
-- where CODIGO_DANE_SEDE = 215790000354