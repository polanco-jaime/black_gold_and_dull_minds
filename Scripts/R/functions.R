###### Fimction of homogenization the grade ######
get_school_stage <- function(grade){
  grade <- as.integer(grade)
  if (grade <= 6){
    return(paste0(grade, "th grade elementary school"))
  } else if (grade <= 9){
    return(paste0(grade, "th grade junior high school"))
  } else {
    return(paste0(grade, "th grade high school"))
  }
}



Table_result = function(A){
  Table_result =   cbind(as.data.frame(A[3]), 
                         as.data.frame(A[4]) , 
                         as.data.frame(A[5]) , 
                         as.data.frame(A[6]) , 
                         as.data.frame(A[7])  )
  return(Table_result)
}

###### Fimction of homogenization the grade ######
library(ggplot2)

plot_coefficients_by_grade <- function(data, coeff_col, se_col, grade_col, title) {
  
  # Aggregate the data by grade, calculating means and standard errors of the coefficients
  a 
  # Create the plot
  plot <- ggplot(data, aes_string(x = grade_col, y = coeff_col, fill = grade_col)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes_string(ymin = paste0(coeff_col, "-", se_col, "_mean", "-", se_col, "_sd"),
                             ymax = paste0(coeff_col, "-", se_col, "_mean", "+", se_col, "_sd")),
                  width = 0.2, position = position_dodge(0.9)) +
    labs(title = title, y = coeff_col, x = grade_col)+
    xlab("Point Estimate and 95% Confidence Interval")
  
  return(plot)
}






summary_plot = function (out,  TITULO= '') {
 
  library(ggplot2)
  library("scales")
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    legend.position = "bottom"  )
  estimators = unique(out$grade)
   y_lims = c(min(out$ci.CI.Lower), max(out$ci.CI.Upper)) * 1.05

  out$grade = as.numeric(out$grade)
  library(sqldf)
  out = sqldf::sqldf("SELECT * FROM out ORDER BY CAST(GRADE AS NUMERIC) ")  
  Plot = ggplot2::ggplot(data = out, ggplot2::aes(x = .data$grade, 
                                                  y = .data$Coeff, 
                                                  ymin = .data$ci.CI.Lower , 
                                                  ymax = .data$ci.CI.Upper)) +
    theme_light()  + ggplot2::geom_point( size = 1.8) + ggplot2::geom_errorbar() +
      ggplot2::theme(  plot.title = element_text(hjust = 0.5, vjust = 0.5),
                       axis.line.x = element_line(color="steelblue4", size = 0.5),
                       axis.line.y = element_line(color="steelblue4", size = 0.5)) +
      ggplot2::ggtitle(TITULO) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = "Point Estimate and 95% Confidence Interval", 
                  x = "Event Time", color = "Estimator") 
  P =  Plot +      mynamestheme  
  print(P  )
}

rd_table <- function(rd_model) {
  rd_model = model
  x  = capture.output(summary(rd_model) ) 
  
  # split the input string into lines
  x = gsub(pattern = "Sharp RD estimates using local polynomial regression." ,replacement ="" , x)
   x = gsub(pattern = "=",replacement ="" , x)
   x = gsub(pattern = "        Method",replacement ="Method" , x)
   x = gsub(pattern = "        Robust",replacement ="Robust" , x)
   x = gsub(pattern = "  Conventional",replacement ="Conventional" , x)
   x = gsub(pattern = " , ",replacement ="," , x)
   x = gsub(pattern = " 95% C.I. ",replacement ="95%C.I." , x)
   x = gsub(pattern = "Std. Err.",replacement ="Std.Err." , x)
   #
   input_lines <- x[nzchar(x)]
   #
   obs <- as.numeric(gsub("[^0-9]", "", input_lines[1])) 
   kernel = gsub( pattern =  " ", replacement = "", gsub("Kernel", "", input_lines[3]) )
   VCE = gsub( pattern =  " ", replacement = "", gsub("VCE method", "", input_lines[4]) )
   
   control_treat = strsplit( trimws(gsub("[^0-9]+", " ", input_lines[5]) ) , " ")[[1]]
   order_poly = strsplit( trimws(gsub("[^0-9]+", " ", input_lines[7]) ) , " ")[[1]]
   Bt_w  =  trimws(  gsub( "h" , "",  gsub( "[(()]" , "", gsub("BW est. ", "", input_lines[9] ) ) ) )  
   
   bw = strsplit(  Bt_w  , "\\s+")[[1]]
   # extract the relevant lines
   if (VCE=="NN") {
     body_vce= "the variance-covariance matrix estimator is computed with nearest neighbor variance estimator for heteroskedasticity-robust" 
   } else if (VCE=="HC0"){
     body_vce= "the variance-covariance matrix for heteroskedasticity-robust plug-in residuals variance estimator  is computed without weights" 
   }else if (VCE=="HC1"){
     body_vce= "the variance-covariance matrix for heteroskedasticity-robust plug-in residuals variance estimator  is computed with HC1 weights2" 
   }else if (VCE=="HC2"){
     body_vce= "the variance-covariance matrix for heteroskedasticity-robust plug-in residuals variance estimator  is computed with HC2 weights" 
   }
   #### message por order of poly
   if (order_poly[1] == order_poly[2]) {
     order_poly_ = sprintf("The global polynomial fit in  $A^{T}$ and $A^{c}$ is of order %s",order_poly[1] )
   } else{
    order_poly_ = sprintf("The global polynomial fit in $A^{T}$ is %s and in $A^{c}$ is %s",order_poly[1], order_poly[2]  )
  }
   #### message fro bandwith
   
   if (bw[1] == bw[2]) {
     bw = sprintf("the bandwith where the global polynomial fit is of %s meters.",bw[1] )
   } else{
     bw = sprintf("the bandwith where the global polynomial fit  in $A^{T}$ is %s and for $A^{c}$is %s",bw[1], bw[2]  )
   }
   
  # extract the relevant lines
  table_lines <- input_lines[c(13:16)]
  
  # split the table lines by whitespace
  # table_lines <- strsplit(table_lines, "\\s+")[[1]]
  header <- strsplit(table_lines, "\\s+")[[1]]
  conventional <- strsplit(table_lines, "\\s+")[[2]]
  bias  <- strsplit(table_lines, "\\s+")[[3]]
  Robust <- strsplit(table_lines, "\\s+")[[4]]
 
  
    # create the LaTeX table

  table_str <- "\\begin{table}[htbp]\\centering\n \\footnotesize \n"
  table_str <- paste0(table_str, "% \\caption{Regression Discontinuity Estimates}\n")
  table_str <- paste0(table_str, "\\begin{tabular}{lccccc}\n\\hline\\hline\n")
  table_str <- paste0(table_str, "Method & Coef. & Std. Err. & z & P$>|$z$|$ & [95\\% C.I.] \\\\ \n")
  table_str <- paste0(table_str, "\\hline \\hline  \n")
  table_str <- paste0(table_str, paste(conventional, collapse = " & ") ,  " \\\\ \n ")
  table_str <- paste0(table_str, paste(bias, collapse = " & ") ,  " \\\\ \n")
  table_str <- paste0(table_str, paste(Robust, collapse = " & ") ,  " \\\\ \n  ")
  table_str <- paste0(table_str, "\\hline\\hline\n\\end{tabular}\n\\label{table:rd}\n")
  table_str <- paste0(table_str, "\\begin{tablenotes} \n  \\justifying \\tiny \\textbf{Note: }    \n   ")
  
  # create the note
  note_str <-  sprintf( "The analysis is based on a sample of %s observations, with %s located within the area of exploration announcement ($A^{T}$) and %s located near the border but outside the area of exploration  ($A^{c}$). 
           We employ a %s kernel and %s. %s, %s. We estimate all coefficients using conventional, bias-corrected, and robust estimators, and we cluster standard errors at the school level. " ,obs,   control_treat[2], control_treat[1] , kernel, body_vce, order_poly_, bw ) 
  
  table_str  <- paste0(table_str, note_str )
  table_str  <- paste0(table_str, "\\end{tablenotes} \n \\end{table} " )
  # print the note and table
 
  # cat(table_str)
  return(table_str)
}
 