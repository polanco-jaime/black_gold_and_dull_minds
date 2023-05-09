###### Fimction of homogenization the grade ######
source("C:/Users/USER/Desktop/The Blessing of Oil Fields/Black Gold and Dull Minds/Scripts/R/rdplot_function_modifies.R")

grades_effect_extention <- function(Outcome, level = 'secondary'){
  if (grepl("T2", Outcome)) {
    if (level == 'secondary') {
      grades <- c('6' , '7', '8', '9'  )  
    } else{
      grades <- c('0','1' , '2', '3', '4', '5'  )
    } 
  } 
  else if (grepl("T1", Outcome)) {
    if (level == 'secondary') {
      grades <- c('6' , '7', '8', '9', '10'  ) 
    } else{
      grades <- c('0','1' , '2', '3', '4', '5'  )
    }
    
  } else {
    grades <- c('0','1' , '2', '3', '4', '5','6' , '7', '8', '9', '10' , '11'  )
  }
  return(grades)
}
get_school_stage <- function(grade){
  grade <- as.integer(grade)
  if (grade <= 6){
    return(paste0(grade, "th grade elementary school"))
  } else if (grade <= 9){
    return(paste0(grade, "th grade elementary school"))
  } else {
    return(paste0(grade, "th grade elementary school"))
  }
  
  # if (grade <= 6){
  #   return(paste0(grade, "th grade elementary school"))
  # } else if (grade <= 9){
  #   return(paste0(grade, "th grade junior high school"))
  # } else {
  #   return(paste0(grade, "th grade high school"))
  # }
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


library(ggplot2)
library("scales")


summary_plot = function (out, conf_level = 0.95, TITULO = '') {
  

  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "cornflowerblue", size = (12)),
    axis.text.x = element_text(angle = 90, vjust = 0.5 ),
    legend.position = "bottom"
  )
  
  out$grade <- as.integer(out$grade)
  out$grade_name <- sapply(out$grade, get_school_stage) #get_school_stage(out$grade)
  out$grade_name <- factor(out$grade_name, levels = unique(out$grade_name)[order(unique(out$grade))])
  print_conf_level = paste0(conf_level*100, '%')
  # Define confidence interval multiplier based on confidence level
  if (conf_level == 0.95) {
    conf_mult <- 1.96
  } else if (conf_level == 0.90) {
    conf_mult <- 1.645
  } else if (conf_level == 0.85) {
    conf_mult <- 1.44
  } else {
    stop("Invalid confidence level. Please choose 0.95, 0.90, or 0.85.")
  }
  # Compute y-axis limits based on standard errors and confidence interval multiplier
  y_lims <- c(min(out$Coeff - conf_mult * out$Std..Err.), 
              max(out$Coeff + conf_mult * out$Std..Err.)) * 1.05
  
  out$ci.CI.Lower = (out$Coeff - conf_mult * out$Std..Err.)
  out$ci.CI.Upper = (out$Coeff + conf_mult * out$Std..Err.)
  
  Plot <- ggplot2::ggplot(data = out, ggplot2::aes(x = .data$grade_name,
                                                   y = .data$Coeff, 
                                                   ymin = .data$ci.CI.Lower , 
                                                   ymax = .data$ci.CI.Upper)) +
    theme_light()  + ggplot2::geom_point( size = 1.8) + ggplot2::geom_errorbar(width  = 0.1) +
    scale_x_discrete() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5, vjust = 0.5),
      axis.line.x = element_line(color="steelblue4", size = 0.5),
      axis.line.y = element_line(color="steelblue4", size = 0.5)
    ) +
    ggplot2::ggtitle(TITULO) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = paste0("Point Estimate and ",print_conf_level," Confidence Interval\n Dropout rate"), 
                   color = "Estimator") +xlab("Scholar Grade")
  
  P <- Plot + mynamestheme  
  print(P)
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
 


 
bandwidth_sensibility_test = function (data, Outcome, Running_variable, bw_mse=NULL, conf_level = 0.95 , full = NULL) {
  maximo =  round( as.numeric(quantile(  subset(data, data$distance>=0)$distance  , probs = c(0.9))) /100) * 100
  minimo = 800 #round( as.numeric(quantile(  subset(data, data$distance>=0)$distance  , probs = c(0.05))) /100) * 100
  # Estimate RDD model for a range of bandwidths
  if( is.numeric(full) ==T){
    iteraciones =  ifelse(full>=30,30, full)
  }else{iteraciones =  30}
  bw_range <- seq(minimo, maximo, by = minimo/iteraciones ) #/iteraciones) # define range of bandwidths
  te_estimates <- vector() # initialize vector to store treatment effect estimates
  se_estimates <- vector()
  
  if (conf_level == 0.95) {
    conf_mult <- 1.96
  } else if (conf_level == 0.90) {
    conf_mult <- 1.645
  } else if (conf_level == 0.85) {
    conf_mult <- 1.44
  } else {
    stop("Invalid confidence level. Please choose 0.95, 0.90, or 0.85.")
  }
  pb <- txtProgressBar(min = 0, max = length(bw_range), style = 3)
  for (bw in bw_range) {
    
    model <-  rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , h = c(bw, bw) )
    # summary(model)
    #rdrobust(data$dropout_rate_t1 ~ rdropt(data$distance, cutpoint = 0, bw = bw), data = data, covs = NULL)
    te_estimates <- c(te_estimates, model$Estimate[1])
    se_estimates <- c(se_estimates, model$Estimate[3])
    setTxtProgressBar(pb, which(bw_range == bw) )
  }
  Tempo <- data.frame(bw_range, te_estimates, se_estimates)
  
  y_lims <- c(min(Tempo$te_estimates  - conf_mult * Tempo$se_estimates), 
              max(Tempo$te_estimates  + conf_mult * Tempo$se_estimates)) * 1.05
  
  Tempo$ci_low = (Tempo$te_estimates  - conf_mult * Tempo$se_estimates)
  Tempo$ci_high = (Tempo$te_estimates  + conf_mult * Tempo$se_estimates)
  
  center_bw_effect =  mean(Tempo$ci_high + Tempo$ci_low )
  
  row_index <- which(sign(Tempo$ci_low) == sign(Tempo$ci_high))[1]
  
  # Combine the data into a data frame
  # Define color scale for the line
  colors <- viridisLite::inferno(length(Tempo$te_estimates))
  
  # Define the plot
  P =  ggplot(Tempo, aes(x = bw_range, y = te_estimates, color = bw_range)) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.5, size = 0.5) +
    geom_line(size = 1.0) +
    scale_color_gradientn(colors = colors, limits = c(min(Tempo$bw_range), Tempo[row_index, 1] ),
                          name = "Bandwidth Sensitivity Estimates") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
    ggplot2::geom_vline(xintercept = Tempo[row_index, 1], linetype = "dashed", size = 0.5) +
    ggplot2::geom_vline(xintercept = bw_mse , linetype = "dashed", size = 0.5) +
    annotate("text", x = bw_mse, y = center_bw_effect,  label = "MSE optimal bandwidth" , angle = 0 ,
             color = "gray20", box.color = "gray20", box.fill = "white" )+ 
    scale_x_continuous("Bandwidth Range", expand = c(0.02, 0)) +
    scale_y_continuous("TE Estimates", expand = c(0.02, 0)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text( vjust = 0.2, face="bold", family = "Tahoma",angle = 270, size = 10)) +
    labs(title = "Regression Discontinuity Design",
         subtitle = paste0("TE Estimates with", conf_level*100 ,"% Confidence Intervals") )
  print(P)
  return( list(Tempo[row_index-1, 1] , P) )
}



# library(parallel)
# 
# bandwidth_sensibility_test = function (data, Outcome, Running_variable, conf_level = 0.95 ) {
#   maximo =  round( as.numeric(quantile(  subset(data, data$distance>=0)$distance  , probs = c(0.9))) /100) * 100
#   minimo = round( as.numeric(quantile(  subset(data, data$distance>=0)$distance  , probs = c(0.05))) /100) * 100
#   # Estimate RDD model for a range of bandwidths
#   
#   bw_range <- seq(minimo, maximo, by = minimo/10) # define range of bandwidths
#   te_estimates <- numeric(length(bw_range)) # initialize vector to store treatment effect estimates
#   se_estimates <- numeric(length(bw_range))
#   
#   if (conf_level == 0.95) {
#     conf_mult <- 1.96
#   } else if (conf_level == 0.90) {
#     conf_mult <- 1.645
#   } else if (conf_level == 0.85) {
#     conf_mult <- 1.44
#   } else {
#     stop("Invalid confidence level. Please choose 0.95, 0.90, or 0.85.")
#   }
#   
#   cl <- makeCluster(detectCores()) # create a cluster using all available cores
#   # pb <- txtProgressBar(min = 0, max = length(bw_range), style = 3)
#   clusterExport(cl, c("data", "Outcome", "Running_variable", "bw_range", "conf_mult")) # export variables to cluster
#   
#   te_se_estimates <- clusterApply(cl, bw_range, function(bw) {
#     library(rdrobust)
#     model <-  rdrobust(y=data[[Outcome]] , x=data[[Running_variable]] , h = c(bw, bw) )
#     te_estimate <- model$Estimate[1]
#     se_estimate <- model$Estimate[3]
#     # setTxtProgressBar(pb, which(bw_range == bw) )
#     return(c(te_estimate, se_estimate))
#   })
#   
#   te_estimates <- unlist(lapply(te_se_estimates, `[`, 1)) # extract treatment effect estimates from list
#   se_estimates <- unlist(lapply(te_se_estimates, `[`, 2)) # extract standard error estimates from list
#   
#   stopCluster(cl) # stop the cluster to free up resources
#   
#   Tempo <- data.frame(bw_range, te_estimates, se_estimates)
#   
#   y_lims <- c(min(Tempo$te_estimates  - conf_mult * Tempo$se_estimates), 
#               max(Tempo$te_estimates  + conf_mult * Tempo$se_estimates)) * 1.05
#   
#   Tempo$ci_low = (Tempo$te_estimates  - conf_mult * Tempo$se_estimates)
#   Tempo$ci_high = (Tempo$te_estimates  + conf_mult * Tempo$se_estimates)
#   
#   
#   row_index <- which(sign(Tempo$ci_low) == sign(Tempo$ci_high))[1]
#   
#   # Combine the data into a data frame
#   # Define color scale for the line
#   colors <- viridisLite::inferno(length(Tempo$te_estimates))
#   
#   # Define the plot
#   P =  ggplot(Tempo, aes(x = bw_range, y = te_estimates, color = bw_range)) +
#     geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.5, size = 0.5) +
#     geom_line(size = 1.0) +
#     scale_color_gradientn(colors = colors, limits = c(min(Tempo$bw_range), Tempo[row_index, 1] ),
#                           name = "Bandwidth Sensitivity Estimates") +
#     ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.5) +
#     ggplot2::geom_vline(xintercept = Tempo[row_index, 1], linetype = "dashed", size = 0.5) +
#     scale_x_continuous("Bandwidth Range", expand = c(0.02, 0)) +
#     scale_y_continuous("TE Estimates", expand = c(0.02, 0)) +
#     theme(panel.background = element_rect(fill = "white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.text = element_text(size = 12),
#           axis.title = element_text(size = 14, face = "bold"),
#           legend.key.size = unit(1, 'cm'), #change legend key size
#           legend.key.height = unit(1, 'cm'), #change legend key height
#           legend.key.width = unit(1, 'cm'), #change legend key width
#           legend.position = "bottom",
#           legend.title = element_text(size = 12),
#           legend.text = element_text( vjust = 0.2, face="bold", family = "Tahoma",angle = 270, size = 10)) +
#     labs(title = "Regression Discontinuity Design",
#          subtitle = paste0("TE Estimates with", conf_level*100 ,"% Confidence Intervals") )
#   print(P)
#   return( list(Tempo[row_index-1, 1] , P) )
# }


model_outputs_plot <- function(est, data, Running_variable, Outcome ){
  data$area <- ifelse(data[[Running_variable]] < 0, "Control area", "Treatment area")
  bw_ = data[data[[Running_variable]] >= -est$bws[1,2] , ]
  bw_ = bw_[bw_[[Running_variable]] <= est$bws[1,1] , ]
  
  model = rdrobust(y=data[[Outcome]] , x=data[[Running_variable]], all=TRUE,
                   subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                   kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  
  control_mean = mean( bw_[bw_[[Running_variable]] <= 0 , ][[Outcome]] , na.rm = T)
  treatment_mean = mean( bw_[bw_[[Running_variable]] > 0 , ][[Outcome]] , na.rm = T)
  ate = round(model$coef[3], 4)
  se_ate = round( model$se [3] , 3)
  
  h = est$bws[1,1]
  round(-h+ (h/5)/100) * 100
  breaks_ =  c(round((-h )/100) * 100,
               round((-h+ (h/5))/100) * 100 ,
               round((-h+ (h/3))/100) * 100 ,
               round((-h+ (h/2))/100) * 100,  
               round((-h+ (h/1.5))/100) * 100,
               round((-h+ (h/1.2))/100) * 100,  
               0,
               round((h- (h/1.2))/100) * 100 , 
               round((h- (h/1.5))/100) * 100 , 
               round((h- (h/2))/100) * 100 , 
               round((h- (h/3))/100)* 100 ,
               round((h- (h/5))/100)* 100 ,
               round((h )/100) * 100)
  
  labels_ = c(round((-h )/100) * 100,
              round((-h+ (h/5))/100) * 100 ,
              "\n \n Control area",
              round((-h+ (h/2))/100) * 100,  
              round((-h+ (h/1.5))/100) * 100,
              round((-h+ (h/1.2))/100) * 100,  
              0,
              round((h- (h/1.2))/100) * 100 , 
              round((h- (h/1.5))/100) * 100 , 
              round((h- (h/2))/100) * 100 , 
              "\n \n Treatment area" ,
              round((h- (h/5))/100)* 100 ,
              round((h )/100) * 100)
  
  Graph_plot  = rdplot(y=data[[Outcome]], x=data[[Running_variable]],c =0,
                       subset=-est$bws[1,1]<= data[[Running_variable]]  & data[[Running_variable]]  <= est$bws[1,2],
                       binselect="es", kernel="triangular", h=c(est$bws[1,1], est$bws[1,2]), p=1 )
  box_text = paste0("Relative effect = ", round(ate / control_mean,3),
                    "\n", "ATE (se) = ", ate," (", se_ate, ")\n ",
                    "Mean of controls = ", round( control_mean,3) )
  
  plot_graph = Graph_plot[[2]]  +
    xlab("Distance from Exploration Border under an E&P contract (m) ") + 
    ylab("Effect on Dropout Rate (percentage points)") + 
    scale_x_continuous(breaks = breaks_, labels =  labels_) 

    # scale_x_continuous(labels = function(x) ifelse(x < 100, paste0(abs(x), "\nControl area"), paste0(x, "\nTreatment area"))) 

  bloxplot_ = geom_text(aes(x =  - h + h /2 , y = treatment_mean -  treatment_mean/5 , 
                            label = box_text), 
                        size = 4, color = "black", fill = "white", 
                        hjust = 0.5, vjust = -1.5)
  #     
  
  print(plot_graph +  bloxplot_)
  
  return( list(plot_graph+  bloxplot_ , h, model))
}

##############################################
# pre_treatment_difference(data = data, treatment_variable = "IS_IN_T1", outcome_variable = "DESERTO_T1" )

run_rd_tests <- function(data, distance_var, outcome_var, covariates = NULL) {
  
  # Subset data into control and treatment groups
  # distance_var = "distance"
  # outcome_var = "DESERTO_T1"
  data$distance_var = data[[distance_var]]
  data$outcome_var = data[[outcome_var]]
  # covariates = c( "FRAC_FEMALE"   ,      "FRAC_MALE",   "FRAC_ESTRATO_1_2" , "FRAC_ESTRATO_3_4" , "EDAD")

  control_data <- subset(data, distance_var < 0)
  treatment_data <- subset(data, distance_var >= 0)
  
  # Falsification test 1: Testing for manipulation of the running variable
  # rd_plot <- ggplot(data, aes(x = distance_var, y = outcome_var)) +
  #   geom_point() +
  #   geom_vline(xintercept = 0, linetype = "dashed") +
  #   xlab("Running variable") +
  #   ylab("Outcome") +
  #   ggtitle("RD Plot")
  # print(rd_plot)
  
  # Falsification test 2: Testing for pre-treatment differences in covariates
  if (!is.null(covariates)) {
    covariate_diffs <- lapply(covariates, function(covariate) {
      mean(treatment_data[[covariate]], ) - mean(control_data[[covariate]])
    })
    
    
    cat("Pre-treatment differences in covariates:\n")
    print(covariate_diffs)
  }
  
  # Falsification test 3: Testing for manipulation of treatment assignment
  rd_est_1 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300), data = data)
  rd_est_2 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300) + 
                   factor(distance_var >= 1000), data = data)
  rd_est_3 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300) + 
                   factor(distance_var >= 1000) + factor(distance_var >= 2000), data = data)
  rd_est_4 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300) + 
                   factor(distance_var >= 1000) + factor(distance_var >= 2000) + 
                   factor(distance_var >= 3000), data = data)
  rd_est_5 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300) + 
                   factor(distance_var >= 1000) + factor(distance_var >= 2000) + 
                   factor(distance_var >= 3000) + factor(distance_var >= 4000), data = data)
  rd_est_6 <- lm(outcome_var ~ distance_var + factor(distance_var >= 300) + 
                   factor(distance_var >= 1000) + factor(distance_var >= 2000) + 
                   factor(distance_var >= 3000) + factor(distance_var >= 4000) + 
                   factor(distance_var >= 5000), data = data)
  
  rd_tests <- list()
  
  # Test 1: Coefficient on running variable should be significant
  test_1 <- summary(rd_est_1)$coef[2, 4] < 0.05
  rd_tests[["Test 1"]] <- test_1
  
  # Test 2: Coefficients on running variable and treatment indicator should be significant
  test_2 <- all(summary(rd_est_2)$coef[c(2, 3), 4] < 0.05)
  rd_tests[["Test 2"]] <- test_2
  
  # Test 3: Coefficients on running variable, treatment indicator, and cutoff indicators should be significant
  test_3 <- all(summary(rd_est_3)$coef[c(2, 3, 4), 4] < 0.05)
  rd_tests[["Test 3"]] <- test_3
  
  # Test 4: Coefficients on running variable and high-order polynomial terms should not be significant
  rd_est_4 <- lm(outcome_var ~ distance_var + I(distance_var^2) + I(distance_var^3) + I(distance_var^4), data = data)
  test_4 <- all(summary(rd_est_4)$coef[c(2:5), 4] > 0.05)
  rd_tests[["Test 4"]] <- test_4
  
  # Test 5: Coefficients on running variable and covariates should be significant
  if (!is.null(covariates)) {
    rd_est_5 <- lm(outcome_var ~ distance_var + covariates + factor(distance_var >= 0), data = data)
    test_5 <- all(summary(rd_est_5)$coef[2:(length(covariates)+2), 4] < 0.05)
    rd_tests[["Test 5"]] <- test_5
  }
  
  # Test 6: Coefficients on running variable and interaction terms between running variable and covariates should not be significant
  if (!is.null(covariates)) {
    interaction_terms <- lapply(covariates, function(covariate) {
      paste0("distance_var:", covariate)
    })
    interaction_terms <- unlist(interaction_terms)
    rd_est_6 <- lm(outcome_var ~ distance_var + covariates + interaction_terms + factor(distance_var >= 0), data = data)
    test_6 <- all(summary(rd_est_6)$coef[-1, 4] > 0.05)
    rd_tests[["Test 6"]] <- test_6
  }
  
  # Falsification test 4: Testing for sensitivity to bandwidth choice
  rd_est_bw1 <- lm(outcome_var ~ distance_var + factor(distance_var >= 0), data = data)
  rd_est_bw2 <- lm(outcome_var ~ distance_var + factor(distance_var >= 0), data = data,
                   subset = abs(distance_var) < 200)
  rd_est_bw3 <- lm(outcome_var ~ distance_var + factor(distance_var >= 0), data = data,
                   subset = abs(distance_var) < 100)
  test_7 <- all(c(summary(rd_est_bw1)$coef[2, 4], summary(rd_est_bw2)$coef[2, 4], summary(rd_est_bw3)$coef[2, 4]) < 0.05)
  rd_tests[["Test 7"]] <- test_7
  
  # Falsification test 5: Testing for density discontinuities
  rd_density_plot <- ggplot(data, aes(x = distance_var)) +
    geom_density(aes(fill = factor(distance_var >= 0)), alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    xlab("Running variable") +
    ylab("Density") +
    ggtitle("RD Density Plot")
  print(rd_density_plot)
  
  test_8 <- t.test(data$outcome_var[data$distance_var < 0], data$outcome_var[data$distance_var >= 0], var.equal = FALSE)$p.value > 0.05
  rd_tests[["Test 8"]] <- test_8
  
  return(rd_tests)
}


summary_plot_heterogenity = function ( data, Running_variable, Outcome , conf_level = 0.9, TITULO = '', 
                                       heterogenity ="",estiamtion =  "Robust") {
  
  Table_result_ = data.frame()
  "EFFECT BY GRADES "
  data$area <- ifelse(data[[Running_variable]] < 0, "Control area", "Treatment area")
  for (grade in heterogenity ) {
    data_ = subset(data, data$GRADO==grade)
    est_hete = rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]] , all=TRUE)
    
    bw_h = data[data[[Running_variable]] >= -est_hete$bws[1,2] , ]
    bw_h = bw_h[bw_h[[Running_variable]] <= est_hete$bws[1,1] , ]
    
    
    model_hete = rdrobust(y=data_[[Outcome]] , x=data_[[Running_variable]], all=TRUE, 
                          kernel="triangular",
                          b = c(est_hete$bws[2,1], est_hete$bws[2,2] ), 
                          h=c(est_hete$bws[1,1] , est_hete$bws[1,2] ) ,
                          p=1 )
    control_mean = mean( bw_h[bw_h[[Running_variable]] <= 0 , ][[Outcome]] , na.rm = T)
    treatment_mean = mean( bw_h[bw_h[[Running_variable]] > 0 , ][[Outcome]] , na.rm = T)
    ate = round(model_hete$coef[3], 4)
    se_ate = round( model_hete$se [3] , 3)
    
    h = est_hete$bws[1,1]
    "In this case, a one unit decrease in the ATE is associated with a 32.68% 
  decrease in the mean of the control group."
    
    # png(paste0(graphs_dir,get_school_stage(grade) ,q_,Outcome, ".png"),  width = 1030, height = 598, res=100)
    # model_pic  <- model_outputs_plot(est= est, data = data_, Running_variable, Outcome)
    # dev.off()
    tempo = Table_result(model_hete)
    tempo$grade = grade
    tempo$estiamtion = row.names(tempo)
    tempo$control_mean = control_mean
    
    
    rownames(tempo) <- NULL
    Table_result_ = rbind(Table_result_, tempo )
  }
  estiamtion_ =  Table_result_$estiamtion ==estiamtion
  out = subset(Table_result_, estiamtion_ )
  rownames(out) <- NULL
  mynamestheme <- ggplot2::theme(
    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5, vjust = 0.5),
    legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
    legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
    axis.title = element_text(family = "Helvetica", face = "bold", size = (12), colour = "steelblue4"),
    axis.text = element_text(family = "Courier", face = "bold", colour = "steelblue", size = (11)  ), #colour = "cornflowerblue"
    axis.text.x = element_text(angle = 90, vjust = 0.5 ),
    legend.position = "bottom"
  )
  out$grade <- as.integer(out$grade)
  
  out$grade_name <- sapply(out$grade, get_school_stage) #get_school_stage(out$grade)
  
  for (i in 1:nrow(out)) {
    out[i,10] = gsub(pattern = " elementary school",
                     replacement =  paste0("\n", 
                                           "ATE (se) = ", round( out[i,1] , 4 ) , " (", round(out[i,2], 3) , ")\n " ,
                                           "Mean of controls = ", round( out[i,9],3) , "\n "   ,
                                           "Relative effect = " , round(out[i,1] / out[i,9] ,3) ) ,
                     out[i,10]
    )
  }
  
  out$grade_name <- factor(out$grade_name, levels = unique(out$grade_name)[order(unique(out$grade))])
  print_conf_level = paste0(conf_level*100, '%')
  # Define confidence interval multiplier based on confidence level
  if (conf_level == 0.95) {
    conf_mult <- 1.96
  } else if (conf_level == 0.90) {
    conf_mult <- 1.645
  } else if (conf_level == 0.85) {
    conf_mult <- 1.44
  } else {
    stop("Invalid confidence level. Please choose 0.95, 0.90, or 0.85.")
  }
  # Compute y-axis limits based on standard errors and confidence interval multiplier
  y_lims <- c(min(out$Coeff - conf_mult * out$Std..Err.), 
              max(out$Coeff + conf_mult * out$Std..Err.)) * 1.05
  
  out$ci.CI.Lower = (out$Coeff - conf_mult * out$Std..Err.)
  out$ci.CI.Upper = (out$Coeff + conf_mult * out$Std..Err.)
  
  Plot <- ggplot2::ggplot(data = out, ggplot2::aes(x = .data$grade_name,
                                                   y = .data$Coeff, 
                                                   ymin = .data$ci.CI.Lower , 
                                                   ymax = .data$ci.CI.Upper)) +
    theme_light()  + ggplot2::geom_point( size = 1.8) + ggplot2::geom_errorbar(width  = 0.1) +
    scale_x_discrete() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5, vjust = 0.5),
      axis.line.x = element_line(color="steelblue4", size = 0.5),
      axis.line.y = element_line(color="steelblue4", size = 0.5)
    ) +
    ggplot2::ggtitle(TITULO) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(y = paste0("Point Estimate and ",print_conf_level," Confidence Interval\n Dropout rate"), 
                  color = "Estimator") +xlab("Scholar Grade")
  
  P <- Plot + mynamestheme  
  print(P)
}



df_to_latex <- function(df) {
  # create xtable object with dataframe
  tbl <- xtable(df)
  
  # add column names as first row
  addtorow <- list(names(df))
  tbl <- addtolength(tbl, width=dim(df)[2])
  tbl <- addtorow(tbl, addtorow, position=1)
  
  # print LaTeX table
  print(tbl, include.rownames=FALSE, sanitize.text.function = function(x){x})
}


# Function to convert a data frame to a LaTeX table
df_to_latex <- function(df, caption = NULL, NOTES =NULL) {
  # Convert column names to LaTeX format
  colnames_ <- colnames(df)
  header_ = ""
  for (variable in 1:length(colnames_) ) {
    if (variable==length(colnames_) ) {
      header_ =  paste0(header_, colnames_[variable] )
    }else{
      header_ =  paste0(header_, colnames_[variable], " & ")
    } 
  }
  
  table_str <- "\\begin{table}[htbp]\\centering\n \\footnotesize \n"
  table_str <- paste0(table_str, " \\caption{", caption ,"}\n")
  table_str <- paste0(table_str, "\\begin{tabular}{l",paste(rep('c', length(colnames_)-1), collapse = ''),"}\n\\hline\\hline\n")
  table_str <- paste0(table_str, header_, "  \\\\ \n")
  table_str <- paste0(table_str, "\\hline \\hline  \n")
  
  for (i  in 1:nrow(df)) {
    table_str <- paste0(table_str, paste( df[i,] , collapse = " & ") ,  " \\\\ \n ")
  }
  
  table_str <- paste0(table_str, "\\hline\\hline\n\\end{tabular}\n\\label{table:rd}\n")
  table_str <- paste0(table_str, "\\begin{tablenotes} \n  \\justifying \\tiny \\textbf{Note: }    \n   ")
  table_str  <- paste0(table_str, NOTES )
  table_str  <- paste0(table_str, "\\end{tablenotes} \n \\end{table} " )
  # cat(table_str)
  return(table_str)
}
homogenization_values_control <- function(string){
  
  A = gsub(string , pattern = "" , replacement = "")
  
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
  return(A)
}

picture_to_latex <- function(saved_in, name = '', level = 'elementary' ) {
  name = homogenization_values_control(name)
  if (level == 'elementary') {
    Graph_ = " \\includegraphics[scale=0.46]{Graph/Elementary_school/"
  }else{
    Graph_ = " \\includegraphics[scale=0.46]{Graph/Secondary_school/"
  }
  
  table_str <- paste0("\\begin{frame}{" ,name , "}\n \\centering\n \\footnotesize \n")
  table_str <- paste0( table_str, " \\begin{center} \n "  )
  table_str <-   paste0(table_str,  Graph_ , saved_in, "}\n \\end{center} ")
  table_str <-   paste0(table_str,  "\n \\end{frame} ")
 
  cat(table_str)
}

