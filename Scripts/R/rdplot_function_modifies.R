rdplot = function(y, x, c=0, p=4, nbins = NULL, binselect = "esmv", scale = NULL, 
                  kernel = "uni", weights = NULL, h = NULL, 
                  covs = NULL,  covs_eval = "mean", covs_drop = TRUE, ginv.tol = 1e-20,
                  support = NULL, subset = NULL, masspoints = "adjust",
                  hide = FALSE, ci = NULL, shade = FALSE, 
                  title = NULL, x.label = NULL, y.label = NULL, x.lim = NULL, y.lim = NULL, 
                  col.dots = NULL, col.lines = NULL) {
  
  ############################################################################################
  #start_time <- Sys.time()
  ############################################################################################
  if (!is.null(subset)) {
    x <- x[subset]
    y <- y[subset]
  }
  na.ok <- complete.cases(x) & complete.cases(y)
  
  if (!is.null(covs)){
    if (!is.null(subset))  covs <- subset(covs,subset)
    na.ok <- na.ok & complete.cases(covs)
  } 
  
  if (!is.null(weights)){
    if (!is.null(subset)) weights <- weights[subset]
    na.ok <- na.ok & complete.cases(weights) & weights>=0
  } 
  
  x <- x[na.ok]
  y <- y[na.ok]
  
  if (!is.null(covs))    covs    = as.matrix(covs)[na.ok, , drop = FALSE]
  if (!is.null(weights)) weights = as.matrix(weights[na.ok])  
  
  x_min = min(x);	x_max = max(x)
  ind_l = x<c;  ind_r = x>=c
  x_l = x[ind_l]; x_r = x[ind_r]	
  y_l = y[ind_l];	y_r = y[ind_r]
  
  if (!is.null(support)) {
    support_l = support[1]
    support_r = support[2]
    if (support_l<x_min) x_min = support_l
    if (support_r>x_max) x_max = support_r
  }
  
  range_l = c - x_min
  range_r = x_max - c
  
  n_l = length(x_l)
  n_r = length(x_r)
  n = n_l + n_r
  meth="es"
  
  if (is.null(scale)) {
    scale = scale_l = scale_r = 1  
  } else{
    if (length(scale)==1) scale_l = scale_r = scale
    if (length(scale)==2) {
      scale_l = scale[1]
      scale_r = scale[2]
    }
  }
  
  if (!is.null(nbins)) {
    if (length(nbins)==1) nbins_l = nbins_r = nbins
    if (length(nbins)==2) {
      nbins_l = nbins[1]
      nbins_r = nbins[2]
    }
  }
  
  if (is.null(h)) {
    h_l = range_l
    h_r = range_r
  } else{
    if (length(h)==1) h_l = h_r = h
    if (length(h)==2) {
      h_l = h[1]
      h_r = h[2]
    }
  }
  
  flag_no_ci <- FALSE
  if (is.null(ci)) {
    ci<- 95
    flag_no_ci <- TRUE
  }
  
  kernel_type = "Uniform"
  if (kernel=="epanechnikov" | kernel=="epa") kernel_type = "Epanechnikov"
  if (kernel=="triangular"   | kernel=="tri") kernel_type = "Triangular"
  
  ### Mass Points
  if (is.null(masspoints)) masspoints=FALSE
  mN = n;  M_l = n_l;  M_r = n_r
  if (masspoints=="check" | masspoints=="adjust") {
    X_uniq_l = sort(unique(x_l), decreasing=TRUE)
    X_uniq_r = unique(x_r)
    M_l = length(X_uniq_l)
    M_r = length(X_uniq_r)
    M = M_l + M_r
    mass_l = 1-M_l/n_l
    mass_r = 1-M_r/n_r				
    if (mass_l>=0.2 | mass_r>=0.2){
      print("Mass points detected in the running variable.")
      if (masspoints=="check") print("Try using option masspoints=adjust.")
      if (masspoints=="adjust") {
        if (binselect=="es")    binselect="espr"
        if (binselect=="esmv")  binselect="esmvpr"
        if (binselect=="qs")    binselect="qspr"
        if (binselect=="qsmv")  binselect="qsmvpr"
      }
    }
  }				
  
  ############## COLLINEARITY
  covs_drop_coll=dZ=0
  if (!is.null(covs)) dZ = ncol(covs)
  if (covs_drop == TRUE) covs_drop_coll = 1 
  
  if (!is.null(covs) & isTRUE(covs_drop)) {
    covs.names = colnames(covs)
    if (is.null(covs.names)) {
      covs.names = paste("z",1:ncol(covs),sep="")
      colnames(covs) = covs.names
    }
    covs = covs[,order(nchar(covs.names))]
    covs = as.matrix(covs)
    dZ = length(covs.names)
    covs.check = covs_drop_fun(covs)
    if (covs.check$ncovs < dZ) {
      covs  <- as.matrix(covs.check$covs)
      dZ    <- covs.check$ncovs
      warning("Multicollinearity issue detected in covs. Redundant covariates dropped.")  
    }
  }
  
  #####  ERRORS
  exit=0
  if (c<=x_min | c>=x_max){
    print("c should be set within the range of x")
    exit = 1
  }
  
  if (kernel!="uni" & kernel!="uniform" & kernel!="tri" & kernel!="triangular" & kernel!="epa" & kernel!="epanechnikov" & kernel!="" ){
    print("kernel incorrectly specified")
    exit = 1
  }
  
  if (p<0 ){
    print("p should be a positive number")
    exit = 1
  }
  
  if (scale<=0 |scale_l<=0 |scale_r<=0){
    print("scale should be a positive number")
    exit = 1
  }
  
  p_ceiling = ceiling(p)/p
  
  if (p_ceiling!=1 & p>0) {
    print("p should be an integer number")
    exit = 1
  }
  
  if (n<20){
    print("Not enough observations to perform bin calculations")
    exit = 1
  }
  
  if (exit>0) stop()
  
  ############################################################################################
  #cat(paste("Stop 1: Preps     -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  
  
  ###################################################################
  ### Polynomial curve (order = p) ##################################
  ###################################################################
  W_h_l = rdrobust_kweight(x_l, c, h_l, kernel)
  W_h_r = rdrobust_kweight(x_r, c, h_r, kernel)
  
  n_h_l = sum(W_h_l>0)
  n_h_r = sum(W_h_r>0)
  
  if (!is.null(weights)) {
    fw_l = weights[ind_l];  W_h_l = fw_l*W_h_l
    fw_r = weights[ind_r];	W_h_r = fw_r*W_h_r
  }
  
  R_p_l = outer(x_l-c, Y=0:p, FUN = "^")
  R_p_r = outer(x_r-c, Y=0:p, FUN = "^")	
  invG_p_l  = qrXXinv((sqrt(W_h_l)*R_p_l))	
  invG_p_r  = qrXXinv((sqrt(W_h_r)*R_p_r))
  
  gamma_p = NULL
  if (is.null(covs)) {
    gamma_p1_l = invG_p_l%*%crossprod(R_p_l*W_h_l, y_l)	
    gamma_p1_r = invG_p_r%*%crossprod(R_p_r*W_h_r, y_r)
  } else {
    z_l  = covs[ind_l,]; z_r  = covs[ind_r,]	
    D_l  = cbind(y_l,z_l); D_r = cbind(y_r,z_r)
    U_p_l = crossprod(R_p_l*W_h_l,D_l); U_p_r = crossprod(R_p_r*W_h_r,D_r)
    beta_p_l = invG_p_l%*%crossprod(R_p_l*W_h_l,D_l); beta_p_r = invG_p_r%*%crossprod(R_p_r*W_h_r,D_r); 
    
    ZWD_p_l  = crossprod(z_l*W_h_l,D_l)
    ZWD_p_r  = crossprod(z_r*W_h_r,D_r)
    colsZ = 2:max(c(2+dZ-1,2))
    UiGU_p_l =  crossprod(U_p_l[,colsZ],invG_p_l%*%U_p_l) 
    UiGU_p_r =  crossprod(U_p_r[,colsZ],invG_p_r%*%U_p_r) 
    ZWZ_p_l = ZWD_p_l[,colsZ] - UiGU_p_l[,colsZ] 
    ZWZ_p_r = ZWD_p_r[,colsZ] - UiGU_p_r[,colsZ]     
    ZWY_p_l = ZWD_p_l[,1] - UiGU_p_l[,1] 
    ZWY_p_r = ZWD_p_r[,1] - UiGU_p_r[,1]     
    ZWZ_p = ZWZ_p_r + ZWZ_p_l
    ZWY_p = ZWY_p_r + ZWY_p_l
    if (covs_drop_coll == 0) gamma_p = chol2inv(chol(ZWZ_p))%*%ZWY_p
    if (covs_drop_coll == 1) gamma_p = ginv(ZWZ_p, tol = ginv.tol)%*%ZWY_p
    s_Y = c(1 ,  -gamma_p[,1])
    
    gamma_p1_l = t(s_Y%*%t(beta_p_l))
    gamma_p1_r = t(s_Y%*%t(beta_p_r))
  }
  
  
  ###############################################
  ### Preparte data for polynomial curve plot ###
  ###############################################
  
  nplot = 500
  x_plot_l = seq(c-h_l, c, length.out = nplot)
  x_plot_r = seq(c, c+h_r, length.out = nplot)
  
  rplot_l = outer(x_plot_l-c, Y = 0:p, FUN = "^")
  rplot_r = outer(x_plot_r-c, Y = 0:p, FUN = "^")
  
  y_hat_l = rplot_l%*%gamma_p1_l
  y_hat_r = rplot_r%*%gamma_p1_r
  
  if (!is.null(covs) & covs_eval=="mean" ) {
    gammaZ = colMeans(covs)%*%gamma_p
    y_hat_l = rplot_l%*%gamma_p1_l + c(gammaZ)
    y_hat_r = rplot_r%*%gamma_p1_r + c(gammaZ)
  }
  
  
  ############################################################################################
  #cat(paste("Stop 2: Polynomial fit -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  ###############################################
  ### Optimal Bins (using polynomial order k) ###
  ###############################################
  k=4
  
  rk_l = outer(x_l, Y = 0:k, FUN = "^")
  rk_r = outer(x_r, Y = 0:k, FUN = "^")
  
  invG_k_l = try(qrXXinv(rk_l),silent=TRUE)
  invG_k_r = try(qrXXinv(rk_r),silent=TRUE)
  
  if (class(invG_k_l)[1] == "try-error" | class(invG_k_r)[1] == "try-error") {
    k = 3
    rk_l = outer(x_l, Y = 0:k, FUN = "^")
    rk_r = outer(x_r, Y = 0:k, FUN = "^")
    invG_k_l = try(qrXXinv(rk_l),silent=TRUE)
    invG_k_r = try(qrXXinv(rk_r),silent=TRUE)		
  }
  
  if (class(invG_k_l)[1] == "try-error" | class(invG_k_r)[1] == "try-error") {
    k = 2
    rk_l = outer(x_l, Y = 0:k, FUN = "^")
    rk_r = outer(x_r, Y = 0:k, FUN = "^")
    invG_k_l = qrXXinv(rk_l)
    invG_k_r = qrXXinv(rk_r)		
  }
  
  gamma_k1_l = invG_k_l%*%crossprod(rk_l, y_l)  
  gamma_k2_l = invG_k_l%*%crossprod(rk_l, y_l^2)
  gamma_k1_r = invG_k_r%*%crossprod(rk_r, y_r)  
  gamma_k2_r = invG_k_r%*%crossprod(rk_r, y_r^2)
  
  ### Bias w/sample
  mu0_k1_l = rk_l%*%gamma_k1_l
  mu0_k1_r = rk_r%*%gamma_k1_r
  mu0_k2_l = rk_l%*%gamma_k2_l
  mu0_k2_r = rk_r%*%gamma_k2_r
  drk_l = matrix(NA,n_l,k)
  drk_r = matrix(NA,n_r,k)
  for (j in 1:k) {
    drk_l[,j] = j*x_l^(j-1)
    drk_r[,j] = j*x_r^(j-1)
  }
  
  ind_l = order(x_l); ind_r = order(x_r)
  x_i_l = x_l[ind_l]; y_i_l = y_l[ind_l]
  x_i_r = x_r[ind_r]; y_i_r = y_r[ind_r]
  
  dxi_l=(x_i_l[2:n_l]-x_i_l[1:(n_l-1)]); dyi_l=(y_i_l[2:n_l]-y_i_l[1:(n_l-1)])
  dxi_r=(x_i_r[2:n_r]-x_i_r[1:(n_r-1)]); dyi_r=(y_i_r[2:n_r]-y_i_r[1:(n_r-1)])
  
  x_bar_i_l = (x_i_l[2:n_l] + x_i_l[1:(n_l-1)]) /2
  x_bar_i_r = (x_i_r[2:n_r] + x_i_r[1:(n_r-1)]) /2
  
  drk_i_l = matrix(NA,n_l-1,k);	rk_i_l  = matrix(NA,n_l-1,(k+1))
  drk_i_r = matrix(NA,n_r-1,k);	rk_i_r  = matrix(NA,n_r-1,(k+1))
  
  for (j in 1:(k+1)) {
    rk_i_l[,j] = x_bar_i_l^(j-1)    
    rk_i_r[,j] = x_bar_i_r^(j-1)
  }
  
  for (j in 1:k) {
    drk_i_l[,j] = j*x_bar_i_l^(j-1)
    drk_i_r[,j] = j*x_bar_i_r^(j-1)
  }
  
  mu1_i_hat_l = drk_i_l%*%(gamma_k1_l[2:(k+1)])
  mu1_i_hat_r = drk_i_r%*%(gamma_k1_r[2:(k+1)])
  
  mu0_i_hat_l = rk_i_l%*%gamma_k1_l
  mu0_i_hat_r = rk_i_r%*%gamma_k1_r
  mu2_i_hat_l = rk_i_l%*%gamma_k2_l
  mu2_i_hat_r = rk_i_r%*%gamma_k2_r
  
  mu0_hat_l = rk_l%*%gamma_k1_l
  mu0_hat_r = rk_r%*%gamma_k1_r
  mu2_hat_l = rk_l%*%gamma_k2_l
  mu2_hat_r = rk_r%*%gamma_k2_r
  
  mu1_hat_l = drk_l%*%(gamma_k1_l[2:(k+1)])
  mu1_hat_r = drk_r%*%(gamma_k1_r[2:(k+1)])
  
  mu1_i_hat_l = drk_i_l%*%(gamma_k1_l[2:(k+1)])
  mu1_i_hat_r = drk_i_r%*%(gamma_k1_r[2:(k+1)])
  
  var_y_l = var(y_l)
  var_y_r = var(y_r)
  
  sigma2_hat_l_bar = mu2_i_hat_l - mu0_i_hat_l^2
  sigma2_hat_r_bar = mu2_i_hat_r - mu0_i_hat_r^2
  ind_s2_l = sigma2_hat_l_bar<0
  ind_s2_r = sigma2_hat_r_bar<0
  sigma2_hat_l_bar[ind_s2_l] = var_y_l 
  sigma2_hat_r_bar[ind_s2_r] = var_y_r  
  
  sigma2_hat_l = mu2_hat_l - mu0_hat_l^2
  sigma2_hat_r = mu2_hat_r - mu0_hat_r^2
  ind_s2_l = sigma2_hat_l<0
  ind_s2_r = sigma2_hat_r<0
  sigma2_hat_l[ind_s2_l] = var_y_l 
  sigma2_hat_r[ind_s2_r] = var_y_r  
  
  B_es_hat_dw = c( ((c-x_min)^2/(12*n))*sum(mu1_hat_l^2),((x_max-c)^2/(12*n))*sum(mu1_hat_r^2))
  V_es_hat_dw = c((0.5/(c-x_min))*sum(dxi_l*dyi_l^2),(0.5/(x_max-c))*sum(dxi_r*dyi_r^2))
  V_es_chk_dw = c((1/(c-x_min))*sum(dxi_l*sigma2_hat_l_bar),(1/(x_max-c))*sum(dxi_r*sigma2_hat_r_bar))
  J_es_hat_dw = J.fun(B_es_hat_dw, V_es_hat_dw, n)
  J_es_chk_dw = J.fun(B_es_hat_dw, V_es_chk_dw, n)
  
  B_qs_hat_dw = c((n_l^2/(24*n))*sum(dxi_l^2*mu1_i_hat_l^2), (n_r^2/(24*n))*sum(dxi_r^2*mu1_i_hat_r^2))
  V_qs_hat_dw = c((1/(2*n_l))*sum(dyi_l^2),(1/(2*n_r))*sum(dyi_r^2))
  V_qs_chk_dw = c((1/n_l)*sum(sigma2_hat_l), (1/n_r)*sum(sigma2_hat_r))
  J_qs_hat_dw = J.fun(B_qs_hat_dw, V_qs_hat_dw, n)
  J_qs_chk_dw = J.fun(B_qs_hat_dw, V_qs_chk_dw, n)
  
  J_es_hat_mv  = c(ceiling((var_y_l/V_es_hat_dw[1])*(n/log(n)^2)), ceiling((var_y_r/V_es_hat_dw[2])*(n/log(n)^2)))
  J_es_chk_mv  = c(ceiling((var_y_l/V_es_chk_dw[1])*(n/log(n)^2)), ceiling((var_y_r/V_es_chk_dw[2])*(n/log(n)^2)))
  J_qs_hat_mv  = c(ceiling((var_y_l/V_qs_hat_dw[1])*(n/log(n)^2)), ceiling((var_y_r/V_qs_hat_dw[2])*(n/log(n)^2)))
  J_qs_chk_mv  = c(ceiling((var_y_l/V_qs_chk_dw[1])*(n/log(n)^2)), ceiling((var_y_r/V_qs_chk_dw[2])*(n/log(n)^2)))
  
  
  #########################################################
  if (binselect=="es") {
    J_star_orig = J_es_hat_dw
    meth="es"
    binselect_type="IMSE-optimal evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV   = J_es_hat_mv
  }
  if (binselect=="espr") {
    J_star_orig = J_es_chk_dw
    meth="es"
    binselect_type="IMSE-optimal evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV   = J_es_chk_mv
  }
  if (binselect=="esmv" ) {
    J_star_orig = J_es_hat_mv
    meth="es"
    binselect_type="mimicking variance evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV   = J_es_hat_mv
  }
  if (binselect=="esmvpr" ) {
    J_star_orig = J_es_chk_mv
    meth="es"
    binselect_type="mimicking variance evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV   = J_es_chk_mv
  }
  if (binselect=="qs" ) {
    J_star_orig = J_qs_hat_dw
    meth="qs"
    binselect_type="IMSE-optimal quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV   = J_qs_hat_mv
  }
  if (binselect=="qspr" ) {
    J_star_orig = J_qs_chk_dw
    meth="qs"
    binselect_type="IMSE-optimal quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV   = J_qs_chk_mv
  }
  if (binselect=="qsmv" ) {
    J_star_orig = J_qs_hat_mv
    meth="qs"
    binselect_type="mimicking variance quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV   = J_qs_hat_mv
  }
  if (binselect=="qsmvpr" ) {
    J_star_orig = J_qs_chk_mv
    meth="qs"
    binselect_type="mimicking variance quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV   = J_qs_chk_mv
  }
  
  J_star_l = scale_l*J_star_orig[1]
  J_star_r = scale_r*J_star_orig[2]
  
  if (!is.null(nbins)) {
    J_star_l = nbins_l
    J_star_r = nbins_r
    binselect_type="manually evenly spaced"
  }
  
  if (var_y_l==0) {
    J_star_l = J_star_l_orig = 1
    print("Warning: not enough variability in the outcome variable below the threshold")
  }
  
  if (var_y_r==0) {
    J_star_r = J_star_r_orig = 1
    print("Warning: not enough variability in the outcome variable above the threshold")
  }
  
  rscale_l = J_star_l / J_IMSE[1]
  rscale_r = J_star_r / J_IMSE[2]
  
  ############################################################################################
  #cat(paste("Stop 3: Optimal Bins   -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  
  jump_l  = range_l/J_star_l;jump_r = range_r/J_star_r;
  
  if (meth=="es") {
    jumps_l=seq(x_min,c,jump_l)
    jumps_r=seq(c,x_max,jump_r)
  }   else if (meth=="qs") {
    jumps_l=quantile(x_l,probs=seq(0,1,1/J_star_l))
    jumps_r=quantile(x_r,probs=seq(0,1,1/J_star_r))
  }
  
  bin_x_l = findInterval(x_l, jumps_l,  rightmost.closed=TRUE) - J_star_l-1
  bin_x_r = findInterval(x_r, jumps_r,  rightmost.closed=TRUE) 
  
  ############################################################################################
  #cat(paste("Stop 4a: Loop bins     -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  
  rdplot_l = aggregate(cbind(y_l,x_l), list(bin_x_l), FUN=mean)
  rdplot_r = aggregate(cbind(y_r,x_r), list(bin_x_r), FUN=mean)
  
  rdplot_bin_l       =  rdplot_l[,1]
  rdplot_mean_y_l    =  rdplot_l[,2]
  rdplot_mean_x_l    =  rdplot_l[,3]
  
  rdplot_bin_r       =  rdplot_r[,1]
  rdplot_mean_y_r    =  rdplot_r[,2]
  rdplot_mean_x_r    =  rdplot_r[,3]
  
  if (!is.null(covs) & covs_eval=="mean") {
    covs_model_l = lm(y_l~ z_l + factor(bin_x_l)) 
    covs_model_r = lm(y_r~ z_r + factor(bin_x_r)) 
    yhatZ_l = predict(covs_model_l)
    yhatZ_r = predict(covs_model_r)
    rdplot_mean_y_l = aggregate(yhatZ_l, list(bin_x_l), FUN=mean)[,2]
    rdplot_mean_y_r = aggregate(yhatZ_r, list(bin_x_r), FUN=mean)[,2]
  }
  
  t_ind_l = 1:J_star_l
  t_ind_r = 1:J_star_r
  rdplot_mean_bin_l = rowMeans(cbind(matrix(jumps_l[t_ind_l]),matrix(jumps_l[(t_ind_l+1)])))
  rdplot_mean_bin_r = rowMeans(cbind(matrix(jumps_r[t_ind_r]),matrix(jumps_r[(t_ind_r+1)])))
  
  #rdplot_mean_bin_l = rdplot_mean_bin_l[rev(-rdplot_bin_l)]
  rdplot_mean_bin_l = rdplot_mean_bin_l[(rdplot_bin_l+J_star_l+1)]
  rdplot_mean_bin_r = rdplot_mean_bin_r[rdplot_bin_r]
  
  bin_x = c(bin_x_l,bin_x_r)
  rdplot_mean_bin = c(rdplot_mean_bin_l, rdplot_mean_bin_r)
  rdplot_mean_x   = c(rdplot_mean_x_l,   rdplot_mean_x_r)
  rdplot_mean_y   = c(rdplot_mean_y_l,   rdplot_mean_y_r)
  
  
  ############################################################################################
  #cat(paste("Stop 4b: Loop bins     -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  rdplot_N_l    = aggregate(y_l, list(-bin_x_l), FUN=length)[,2]
  rdplot_N_r    = aggregate(y_r, list(bin_x_r),  FUN=length)[,2]
  rdplot_sd_y_l = aggregate(y_l, list(-bin_x_l), FUN=sd)[,2]
  rdplot_sd_y_r = aggregate(y_r, list(bin_x_r),  FUN=sd)[,2]
  
  rdplot_N = c(rev(rdplot_N_l),rdplot_N_r)
  
  quant = -qt((1-(ci/100))/2,pmax(rdplot_N-1,1))
  
  rdplot_sd_y_l[is.na(rdplot_sd_y_l)] =0
  rdplot_sd_y_r[is.na(rdplot_sd_y_r)] =0
  
  rdplot_sd_y = c(rev(rdplot_sd_y_l),rdplot_sd_y_r)
  rdplot_se_y <- rdplot_sd_y/sqrt(rdplot_N)
  
  rdplot_cil_bin = rdplot_mean_y - quant*rdplot_se_y
  rdplot_cir_bin = rdplot_mean_y + quant*rdplot_se_y
  
  temp_plot =NULL
  
  rdplot_min_bin_l = jumps_l[1:J_star_l]
  rdplot_max_bin_l = jumps_l[2:(J_star_l+1)]
  
  rdplot_min_bin_r = jumps_r[1:J_star_r]
  rdplot_max_bin_r = jumps_r[2:(J_star_r+1)]
  
  bin_length_l = rdplot_max_bin_l - rdplot_min_bin_l
  bin_length_r = rdplot_max_bin_r - rdplot_min_bin_r
  
  bin_avg_l = mean(bin_length_l)
  bin_avg_r = mean(bin_length_r)
  
  bin_med_l = median(bin_length_l)
  bin_med_r = median(bin_length_r)
  
  rdplot_min_bin = c(rdplot_min_bin_l[rev(-rdplot_bin_l)], rdplot_min_bin_r[rdplot_bin_r])
  rdplot_max_bin = c(rdplot_max_bin_l[rev(-rdplot_bin_l)], rdplot_max_bin_r[rdplot_bin_r])
  bin_length     = c(bin_length_l, bin_length_r)
  bin_avg        = c(bin_avg_l, bin_avg_r)
  bin_med        = c(bin_med_l, bin_avg_r)
  
  vars_bins = data.frame("rdplot_mean_bin"=rdplot_mean_bin,"rdplot_mean_x"=rdplot_mean_x, "rdplot_mean_y"=rdplot_mean_y, "rdplot_min_bin"=rdplot_min_bin, "rdplot_max_bin"=rdplot_max_bin, "rdplot_se_y"=rdplot_se_y, "rdplot_N"=rdplot_N, "rdplot_ci_l"=rdplot_cil_bin, "rdplot_ci_r"=rdplot_cir_bin)
  vars_poly = data.frame("rdplot_x"= c(x_plot_l, x_plot_r), "rdplot_y"= c(y_hat_l, y_hat_r))
  
  ############################################################################################
  #cat(paste("Stop 4d: Loop bins     -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  
  if (is.null(col.lines)) col.lines = "red"
  if (is.null(col.dots))  col.dots  = "darkblue"
  #if (is.null(type.dots)) type.dots = 20
  if (is.null(title)) title="RD Plot"
  if (is.null(x.label)) x.label="X axis"
  if (is.null(y.label)) y.label="Y axis"
  #if (is.null(x.lim)) x.lim=c(min(x_l),max(x_r))
  #if (is.null(y.lim)) y.lim=c(min(c(y_l,y_r)),max(c(y_l,y_r)))
  #if (is.null(y.lim)) y.lim=c(min(rdplot_mean_y),max(rdplot_mean_y))
  
  data_bins <- data.frame(rdplot_mean_bin, rdplot_mean_y, rdplot_cil_bin, rdplot_cir_bin)
  data_poly <- data.frame(x_plot_l, y_hat_l, x_plot_r, y_hat_r)
  
  temp_plot <- ggplot() + theme_bw() +
    geom_point(data=data_bins, aes(x=rdplot_mean_bin, y=rdplot_mean_y), col=col.dots, na.rm=TRUE) +
    geom_line( data=data_poly, aes(x=x_plot_l, y=y_hat_l), col=col.lines, na.rm=TRUE) +
    geom_line( data=data_poly, aes(x=x_plot_r, y=y_hat_r), col=col.lines, na.rm=TRUE) 
  
  if (flag_no_ci==FALSE)
    temp_plot <- temp_plot +
    geom_errorbar(data=data_bins, aes(x=rdplot_mean_bin, ymin=rdplot_cil_bin, ymax=rdplot_cir_bin), linetype = 1) 
  if (shade==TRUE){
    temp_plot <- temp_plot +
      geom_ribbon(data=data_bins, aes(x=rdplot_mean_bin, ymin=rdplot_cil_bin, ymax=rdplot_cir_bin))
  }
  
  temp_plot <- temp_plot + labs(x = x.label, y = y.label) + ggtitle(title)+
    coord_cartesian(xlim = x.lim, ylim = y.lim) +
    theme(legend.position = "None") +
    geom_vline(xintercept = c, size = 0.5) 
  
  if (hide == FALSE) print(temp_plot)
  
  
  
  ############################################################################################
  #cat(paste("Stop 5: Plot      -> ",  Sys.time()-start_time,"\n", sep=""))
  #start_time <- Sys.time()
  ############################################################################################
  coef = cbind(gamma_p1_l,gamma_p1_r)
  colnames(coef)=c("Left","Right")
  
  out=list(coef=coef, rdplot=temp_plot, vars_bins=vars_bins, vars_poly=vars_poly,
           J=c(J_star_l,J_star_r), J_IMSE=J_IMSE, J_MV=J_MV, 
           scale=c(scale_l,scale_r), rscale=c(rscale_l,rscale_r),
           bin_avg=c(bin_avg_l,bin_avg_r), bin_med=c(bin_med_l,bin_med_r),
           p=p, c=c, h=c(h_l,h_r), N=c(n_l,n_r), N_h=c(n_h_l,n_h_r), 
           binselect=binselect_type, kernel=kernel_type, coef_covs=gamma_p)
  
  out$call <- match.call()
  class(out) <- "rdplot"
  return(list(invisible(out) , temp_plot  ))
}

print.rdplot <- function(x,...){
  cat("Call: rdplot\n\n")
  
  cat(paste("Number of Obs.           ",  format(x$N[1]+x$N[2], width=10, justify="right"),"\n", sep=""))
  cat(paste("Kernel                   ",  format(x$kernel,      width=10, justify="right"),"\n", sep=""))
  cat("\n")
  cat(paste("Number of Obs.           ",  format(x$N[1],    width=10, justify="right"),  "      ", format(x$N[2],     width=10, justify="right"), "\n", sep=""))
  cat(paste("Eff. Number of Obs.      ",  format(x$N_h[1],  width=10, justify="right"),  "      ", format(x$N_h[2],   width=10, justify="right"), "\n", sep=""))
  cat(paste("Order poly. fit (p)      ",  format(x$p,       width=10, justify="right"),  "      ", format(x$p,        width=10, justify="right"), "\n", sep=""))
  cat(paste("BW poly. fit (h)         ",  format(sprintf("%10.3f",x$h[1])),      "      ", format(sprintf("%10.3f",x$h[2])),     "\n", sep=""))
  cat(paste("Number of bins scale     ",  format(sprintf("%10.3f",x$scale[1])),  "      ", format(sprintf("%10.3f",x$scale[2])), "\n", sep=""))
  cat("\n")
}

summary.rdplot <- function(object,...) {
  x    <- object
  args <- list(...)
  
  cat("Call: rdplot\n\n")
  
  cat(paste("Number of Obs.           ",  format(x$N[1]+x$N[2], width=10, justify="right"),"\n", sep=""))
  cat(paste("Kernel                   ",  format(x$kernel,      width=10, justify="right"),"\n", sep=""))
  cat("\n")
  cat(paste("Number of Obs.           ",  format(x$N[1],     width=10, justify="right"),  "      ", format(x$N[2],     width=10, justify="right"),        "\n", sep=""))
  cat(paste("Eff. Number of Obs.      ",  format(x$N_h[1],   width=10, justify="right"),  "      ", format(x$N_h[2],   width=10, justify="right"),        "\n", sep=""))
  cat(paste("Order poly. fit (p)      ",  format(x$p,        width=10, justify="right"),  "      ", format(x$p,        width=10, justify="right"),       "\n", sep=""))
  cat(paste("BW poly. fit (h)         ",  format(sprintf("%10.3f",x$h[1])),      "      ", format(sprintf("%10.3f",x$h[2])),        "\n", sep=""))
  cat(paste("Number of bins scale     ",  format(sprintf("%10.0f",x$scale[1])),  "      ", format(sprintf("%10.0f",x$scale[2])),    "\n", sep=""))
  cat("\n")
  cat(paste("Bins Selected            ",  format(x$J[1],       width=10, justify="right"),  "      ", format(x$J[2], width=10, justify="right"),        "\n", sep=""))
  cat(paste("Average Bin Length       ",  format(sprintf("%10.3f",x$bin_avg[1])),  "      ", format(sprintf("%10.3f",x$bin_avg[2])),  "\n", sep=""))
  cat(paste("Median Bin Length        ",  format(sprintf("%10.3f",x$bin_med[1])),  "      ", format(sprintf("%10.3f",x$bin_med[2])),  "\n", sep=""))
  cat("\n")
  cat(paste("IMSE-optimal bins        ",  format(x$J_IMSE[1], width=10, justify="right"),  "      ", format(x$J_IMSE[2], width=10, justify="right"),  "\n", sep=""))
  cat(paste("Mimicking Variance bins  ",  format(x$J_MV[1],   width=10, justify="right"),  "      ", format(x$J_MV[2],   width=10, justify="right"),  "\n", sep=""))
  cat("\n")
  cat(paste("Relative to IMSE-optimal:",   "\n", sep=""))
  cat(paste("Implied scale            ",  format(sprintf("%10.3f",x$rscale[1])),                      "      ", format(sprintf("%10.3f",x$rscale[2])),                      "\n", sep=""))
  cat(paste("WIMSE variance weight    ",  format(sprintf("%10.3f",1/(1+x$rscale[1]^3))),              "      ", format(sprintf("%10.3f",1/(1+x$rscale[2]^3))),              "\n", sep=""))
  cat(paste("WIMSE bias weight        ",  format(sprintf("%10.3f",x$rscale[1]^3/(1+x$rscale[1]^3))),  "      ", format(sprintf("%10.3f",x$rscale[2]^3/(1+x$rscale[2]^3))),  "\n", sep=""))
  cat("\n")
}


qrXXinv = function(x, ...) {
  #tcrossprod(solve(qr.R(qr(x, tol = 1e-10)), tol = 1e-10))
  #tcrossprod(solve(qr.R(qr(x))))
  chol2inv(chol(crossprod(x))) 
}

qrreg = function(x,y,w,s2=0,var.comp=TRUE, ...) {
  M.X = sqrt(w)*x
  X.M.X_inv = qrXXinv(M.X) 
  X.M.Y = crossprod(M.X,sqrt(w)*y)
  beta.hat = X.M.X_inv%*%X.M.Y
  Psi.hat=Sigma.hat=0
  if (var.comp==TRUE) {
    Psi.hat = crossprod((w*s2*w)*x,x)
    Sigma.hat = crossprod(Psi.hat%*%X.M.X_inv,X.M.X_inv)
  }
  output = list(X.M.X_inv=X.M.X_inv, X.M.Y=X.M.Y, beta.hat=beta.hat, Psi.hat=Psi.hat, Sigma.hat=Sigma.hat)
  return(output)
}

rdrobust_kweight = function(X, c,  h,  kernel){
  u = (X-c)/h
  if (kernel=="epanechnikov" | kernel=="epa") {
    w = (0.75*(1-u^2)*(abs(u)<=1))/h
  }
  else if (kernel=="uniform" | kernel=="uni") {
    w = (0.5*(abs(u)<=1))/h
  }
  else {
    w = ((1-abs(u))*(abs(u)<=1))/h
  }
  return(w)	
}

rdrobust_res = function(X, y, T, Z, m, hii, vce, matches, dups, dupsid, d) {
  n = length(y)
  dT=dZ=0
  if (!is.null(T)) dT = 1
  if (!is.null(Z)) dZ = ncol(Z)
  res = matrix(NA,n,1+dT+dZ)  	
  
  if (vce=="nn") {
    for (pos in 1:n) {
      rpos = dups[pos] - dupsid[pos]
      lpos = dupsid[pos] - 1
      while (lpos+rpos < min(c(matches,n-1))) {
        if (pos-lpos-1 <= 0) rpos = rpos + dups[pos+rpos+1]
        else if (pos+rpos+1>n) lpos = lpos + dups[pos-lpos-1]
        else if ((X[pos]-X[pos-lpos-1]) > (X[pos+rpos+1]-X[pos])) rpos = rpos + dups[pos+rpos+1]
        else if ((X[pos]-X[pos-lpos-1]) < (X[pos+rpos+1]-X[pos])) lpos = lpos + dups[pos-lpos-1]
        else {
          rpos = rpos + dups[pos+rpos+1]
          lpos = lpos + dups[pos-lpos-1]
        }
      }
      ind_J = max(c(0,(pos-lpos))):min(c(n,(pos+rpos)))
      y_J   = sum(y[ind_J])-y[pos]
      Ji = length(ind_J)-1
      res[pos,1] = sqrt(Ji/(Ji+1))*(y[pos] - y_J/Ji)
      if (!is.null(T)) {
        T_J = sum(T[ind_J])-T[pos]
        res[pos,2] = sqrt(Ji/(Ji+1))*(T[pos] - T_J/Ji)
      }
      if (!is.null(Z)) {
        for (i in 1:dZ) {
          Z_J = sum(Z[ind_J,i])-Z[pos,i]
          res[pos,1+dT+i] = sqrt(Ji/(Ji+1))*(Z[pos,i] - Z_J/Ji)
        }
      }
    }		
  }
  else {
    if (vce=="hc0") w = 1
    else if (vce=="hc1") w = sqrt(n/(n-d))
    else if (vce=="hc2") w = sqrt(1/(1-hii))
    else                 w =      1/(1-hii)
    res[,1] = w*(y-m[,1])
    if (dT==1) res[,2] = w*(T-m[,2])
    if (dZ>0) {
      for (i in 1:dZ) {
        res[,1+dT+i] = w*(Z[,i]-m[,1+dT+i])
      }
    }
  }
  return(res)
}


rdrobust_bw = function(Y, X, T, Z, C, W, c, o, nu, o_B, h_V, h_B, scale, vce, nnmatch, kernel, dups, dupsid, covs_drop_coll, ginv.tol){
  dT = dZ = dC = 0
  w = rdrobust_kweight(X, c, h_V, kernel)
  dW = length(W)
  if (dW>1) {
    w = W*w
  }
  
  ind_V = w> 0; eY = Y[ind_V];eX = X[ind_V];eW = w[ind_V]
  n_V = sum(ind_V)
  D_V = eY
  R_V = matrix(NA,n_V,o+1)
  for (j in 1:(o+1)) R_V[,j] = (eX-c)^(j-1)
  invG_V = qrXXinv(R_V*sqrt(eW))
  e_v = matrix(0,(o+1),1); e_v[nu+1]=1
  s = 1
  eT=eC=eZ=NULL
  if (!is.null(T)) {
    dT = 1
    eT = T[ind_V]
    D_V = cbind(D_V,eT)
  }
  if (!is.null(Z)) {
    dZ = ncol(Z)
    eZ = Z[ind_V,,drop=FALSE]
    D_V = cbind(D_V,eZ)
    U = crossprod(R_V*eW,D_V)
    ZWD  = crossprod(eZ*eW,D_V)
    colsZ = (2+dT):max(c(2+dT+dZ-1,(2+dT)))
    UiGU =  crossprod(matrix(U[,colsZ],nrow=o+1),invG_V%*%U) 
    ZWZ = ZWD[,colsZ] - UiGU[,colsZ] 
    ZWY = ZWD[,1:(1+dT)] - UiGU[,1:(1+dT)] 
    #if (covs_drop_coll==0) gamma = chol2inv(chol(ZWZ))%*%ZWY
    if (covs_drop_coll==1) {
      gamma = ginv(ZWZ, tol=ginv.tol)%*%ZWY
    }
    else {
      gamma = chol2inv(chol(ZWZ))%*%ZWY
    }
    s = c(1 , -gamma[,1])
  }
  if (!is.null(C)) {
    dC = 1
    eC =  C[ind_V] 
  }
  beta_V = invG_V%*%crossprod(R_V*eW,D_V)	
  if (is.null(Z) & !is.null(T)) {	
    tau_Y = c(factorial(nu)*beta_V[nu+1,1])
    tau_T = c(factorial(nu)*beta_V[nu+1,2])
    s = c(1/tau_T , -(tau_Y/tau_T^2))
  }
  if (!is.null(Z) & !is.null(T)) {	
    s_T = c(1 , -gamma[,2])
    tau_Y = c(factorial(nu)*t(s)%*%  c(beta_V[nu+1,1],beta_V[nu+1,colsZ]))
    tau_T = c(factorial(nu)*t(s_T)%*%c(beta_V[nu+1,2],beta_V[nu+1,colsZ]))
    s = c(1/tau_T , -(tau_Y/tau_T^2) , -(1/tau_T)*gamma[,1] + (tau_Y/tau_T^2)*gamma[,2])
  }	
  dups_V=dupsid_V=predicts_V=0
  
  if (vce=="nn") {
    dups_V   = dups[ind_V]
    dupsid_V = dupsid[ind_V]
  }
  
  if (vce=="hc0" | vce=="hc1" | vce=="hc2" | vce=="hc3") {
    predicts_V=R_V%*%beta_V
    if (vce=="hc2" | vce=="hc3") {
      hii = rowSums((R_V%*%invG_V)*(R_V*eW))
    }
  }	
  res_V = rdrobust_res(eX, eY, eT, eZ, predicts_V, hii, vce, nnmatch, dups_V, dupsid_V, o+1)
  V_V = (invG_V%*%rdrobust_vce(dT+dZ, s, R_V*eW, res_V, eC)%*%invG_V)[nu+1,nu+1]
  v = crossprod(R_V*eW,((eX-c)/h_V)^(o+1))
  Hp = 0
  for (j in 1:(o+1)) Hp[j] = h_V^((j-1))
  BConst = (Hp*(invG_V%*%v))[nu+1]
  
  w = rdrobust_kweight(X, c, h_B, kernel)
  if (dW>1) {
    w = W*w
  }
  ind = w> 0 
  n_B = sum(ind)
  eY = Y[ind];eX = X[ind];eW = w[ind]
  D_B = eY
  R_B = matrix(NA,n_B,o_B+1)
  for (j in 1:(o_B+1)) R_B[,j] = (eX-c)^(j-1)
  invG_B = qrXXinv(R_B*sqrt(eW))
  eT=eC=eZ=NULL
  if (!is.null(T)) {
    eT = T[ind]
    D_B = cbind(D_B,eT)
  }
  if (!is.null(Z)) {
    eZ = Z[ind,,drop=FALSE]
    D_B = cbind(D_B,eZ)
  }
  if (!is.null(C)) {
    eC=C[ind]
  }	
  beta_B = invG_B%*%crossprod(R_B*eW,D_B)	
  BWreg=0
  if (scale>0) {
    e_B = matrix(0,(o_B+1),1); e_B[o+2]=1
    dups_B=dupsid_B=hii=predicts_B=0
    if (vce=="nn") {
      dups_B   = dups[ind]
      dupsid_B = dupsid[ind]
    }
    if (vce=="hc0" | vce=="hc1" | vce=="hc2" | vce=="hc3") {
      predicts_B=R_B%*%beta_B
      if (vce=="hc2" | vce=="hc3") {
        hii = rowSums((R_B%*%invG_B)*(R_B*eW))
      }
    }	
    res_B = rdrobust_res(eX, eY, eT, eZ, predicts_B, hii, vce, nnmatch, dups_B, dupsid_B,o_B+1)
    V_B = (invG_B%*%rdrobust_vce(dT+dZ, s, R_B*eW, res_B, eC)%*%invG_B)[o+2,o+2]
    BWreg = 3*BConst^2*V_B
  }
  B =  sqrt(2*(o+1-nu))*BConst%*%(t(s)%*%(beta_B[o+2,]))
  V = (2*nu+1)*h_V^(2*nu+1)*V_V
  R = scale*(2*(o+1-nu))*BWreg
  rate = 1/(2*o+3)
  output = list(V=V,B=B,R=R,rate=rate)
  return(output)
}

rdrobust_vce = function(d, s, RX, res, C) {	
  k = ncol(as.matrix(RX))
  M = matrix(0,k,k)
  n  = length(C)
  if (is.null(C)) {
    w = 1
    if (d==0){
      M  = crossprod(c(res)*RX)
    }
    else {
      for (i in 1:(1+d)) {
        SS = res[,i]*res
        for (j in 1:(1+d)) {
          M = M + crossprod(RX*(s[i]*s[j])*SS[,j],RX)
        }
      }
    }
  }
  else {	
    clusters = unique(C)
    g     = length(clusters)
    w=((n-1)/(n-k))*(g/(g-1))
    if (d==0){
      for (i in 1:g) {
        ind=C==clusters[i]
        Xi = RX[ind,,drop=FALSE]
        ri = res[ind,,drop=FALSE]
        Xr = t(crossprod(Xi,ri))
        M = M + crossprod(Xr,Xr)
      }
    }
    else {
      for (i in 1:g) {
        ind=C==clusters[i]
        Xi = RX[ind,,drop=FALSE]
        ri = res[ind,,drop=FALSE]
        MHolder = matrix(0,1+d,k)
        for (l in 1:(1+d)) {	
          MHolder[l,] = t(crossprod(Xi,s[l]*ri[,l]))
        }	
        summedvalues = t(colSums(MHolder))
        M = M + crossprod(summedvalues,summedvalues)
      }
    }
  }
  return(w*M)		
}

J.fun = function(B,V,n) {ceiling((((2*B)/V)*n)^(1/3))}








bwconst = function(p,v,kernel){
  if (kernel=="epanechnikov" | kernel=="epa" | kernel==3) {
    K.fun = function(u) {(0.75*(1-u^2)*(abs(u)<=1))}
  }
  else if (kernel=="uniform" | kernel=="uni" | kernel==2) {
    K.fun = function(u) {(0.5*(abs(u)<=1))}
  }
  else  {
    K.fun = function(u) {((1-abs(u))*(abs(u)<=1))}
  }
  p1 = p+1  
  Gamma_p = Phi_p = matrix(NA,p1,p1)
  Omega_pq = matrix(NA,p1,1)
  for (i in 1:p1) {
    Omega.fun = function(u) {K.fun(u)*(u^(p1))*(u^(i-1))}
    Omega_pq[i] = integrate(Omega.fun,lower=0,upper=1)$value
    for (j in 1:p1) {
      Gamma.fun = function(u) {K.fun(u)*(u^(i-1))*(u^(j-1))}
      Phi.fun   = function(u) {(K.fun(u)^2)*(u^(i-1))*(u^(j-1))}
      Gamma_p[i,j] = integrate(Gamma.fun,lower=0,upper=1)$value
      Phi_p[i,j] = integrate(Phi.fun,lower=0,upper=1)$value
    }
  }
  B_const = solve(Gamma_p)%*%Omega_pq
  V_const = solve(Gamma_p)%*%Phi_p%*%solve(Gamma_p)
  C1 = B_const[v+1,1]
  C2 = V_const[v+1,v+1]
  return(c(C1,C2))
}

rdvce= function(X,y,frd=NULL,p,h,matches,vce,kernel){
  m = matches+1
  n = length(X)
  p1 = p+1
  sigma = matrix(0,n,1)
  if (vce=="resid") {
    for (k in 1:n) {
      cutoff = matrix(X[k],n,1)
      cutoff1 = X[k]
      W = rdrobust_kweight(X,cutoff1,h,"kernel")
      ind=W>0
      if (sum(ind)>5) {
        w.p=W[ind]; X.p=X[ind]; y.p=y[ind]
        XX.p = matrix(c((X.p-cutoff1)^0, poly(X.p-cutoff1,degree=p,raw=T)),length(X.p),p+1)
        mu0_phat_y = qr.coef(qr(XX.p*sqrt(w.p), tol = 1e-10), sqrt(w.p)*y.p)[1]
        if (is.null(frd)) {
          sigma[k] = (y[k] - mu0_phat_y)^2
        }
        else if (!is.null(frd)) {
          z.p=frd[ind]
          out=qrreg(XX.p, z.p, w.p, var.comp=FALSE) 
          mu0_phat_z = out$beta.hat[1]
          sigma[k] = (y[k] - mu0_phat_y)*(frd[k] - mu0_phat_z)
        }
      }
    }
  }
  else  {
    #y_match_avg = z_match_avg = matrix(NA,n,1)
    for (k in 1:n) {
      diffx = abs(X - X[k])
      m.group = sort(unique(diffx))[2:m]
      ind = which(diffx %in% m.group)
      y_match_avg = z_match_avg = mean(y[ind])
      Ji = length(ind)
      if (is.null(frd)) {
        sigma[k] = (Ji/(Ji+1))*(y[k] - y_match_avg)^2
      } 
      else if (!is.null(frd)) {
        z_match_avg = mean(frd[ind])
        sigma[k] = (Ji/(Ji+1))*(y[k] - y_match_avg)*(frd[k] - z_match_avg)
      }
    }
  }
  return(sigma)
}

regconst = function(d,h){
  d2 = 2*d+1
  d1 = d+1
  mu = matrix(0,d2, 1)
  mu[1] = 1
  XX = matrix(0,d1,d1)
  for (j in 2:d2) {
    i = j-1
    if (j%%2==1) {
      mu[j] = (1/(i+1))*(h/2)^i
    }
  }
  for (j in 1:d1) {
    XX[j,] = t(mu[j:(j+d)])
  }
  invXX =solve(XX)
  return(invXX)
}

covs_drop_fun <- function(z) {
  z          <- as.matrix(z)
  ncovs      <- ncol(z)
  df         <- data.frame(z = z)
  constant   <- rep(1,nrow(df))
  tmp        <- lm(constant ~ ., data=df)
  to_keep    <- tmp$coefficients[!is.na(tmp$coefficients)]
  ncovs_keep <- length(to_keep)
  to_keep    <- names(to_keep[2:ncovs_keep])
  ncovs_keep <- ncovs_keep-1
  covs <- as.matrix(df[to_keep])
  #qr.X <- qr(x,  LAPACK = FALSE, tol = 1e-2) 
  #(rnkX <- qr.X$rank)
  #(keep <- qr.X$pivot[seq_len(rnkX)])
  #xx <- as.matrix(x[,keep])
  #output = list(xx=xx,rank_covs=rnkX)
  output = list(covs=covs, ncovs=ncovs_keep)
  return(output)
}