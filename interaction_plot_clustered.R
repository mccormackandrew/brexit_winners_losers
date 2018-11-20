interaction_plot_binary <- function(model, effect, moderator, interaction, varcov="default", conf=.999, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", factor_labels=c(0,1)){
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- c(0,1)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Initialize plotting window
  print(data.frame(upper_bound = upper_bound,
                   lower_bound = lower_bound,
                   x = x_2,
                   y = delta_1))
  
}



predict.lm <- function(mod, predict.df, rob, cluster = NULL){
  
  ##Written by Joshua Gubler ~  http://scholar.byu.edu/jgubler
  ##Last updated on 26 June 2014
  ##Updated by Baobao Zhang
  ##This provides an option for robust (including cluster robust) or non-robust standard errors
  ##Note: when estimating a polynomial, you must create the quadratic/cubic as a separate variable first!!  This is also the best procedure when estimating logged effects.  However, when estimating interaction effects, there is no need to create a separate interaction term.
  ##Also note, that for this function to work well, you must input factor variables with more than two levels individually (as indiviual dummies).
  
  if(missing(predict.df)){ predict1.df <- mod$model }
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  tt <- terms(mod)
  Terms <- delete.response(tt)
  m.mat <- model.matrix(Terms,data=(if(missing(predict.df)){predict1.df}else{predict.df}))
  t1 <- mod$model
  fit <- as.vector(m.mat %*% mod$coef)
  
  if(missing(rob)){
    varcov <- vcov(mod)
    se.fit <- sqrt(diag(m.mat%*%varcov%*%t(m.mat)))
    ci.lower95 <- fit - 1.96*se.fit
    ci.upper95 <- fit + 1.96*se.fit
  }
  else{
    ##To generate the robust covariance matrix
    X <- model.matrix(mod)
    u2 <- residuals(mod)^2
    if (is.null(cluster)) {
      XDX <- 0 
      for(i in 1:nrow(X)) {
        XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
      }
      # degrees of freedom adjustment
      dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
      # inverse(X'X)
      XX1 <- solve(t(X)%*%X)  
      # Variance calculation (Bread x meat x Bread)
      robcov <- XX1 %*% XDX %*% XX1
      se.fit <- dfc*sqrt(diag(m.mat%*%robcov%*%t(m.mat)))
    } else { # cluster robust standard errors
      M <- length(unique(cluster))
      N <- length(cluster)
      K <- mod$rank
      # degrees of freedom adjustment
      dfc <- (M/(M-1))*((N-1)/(N-K))
      # meat
      XDX <- crossprod(apply(estfun(mod), 2, function(x) tapply(x, cluster, sum)))/N
      # variance calculation 
      vcovCL <- dfc*sandwich(mod, meat=XDX)
      se.fit <- sqrt(diag(m.mat%*%vcovCL%*%t(m.mat)))
    }
    ci.lower95 <- fit - 1.96*se.fit
    ci.upper95 <- fit + 1.96*se.fit
  }
  
  pred.df <- data.frame(predicted.value=fit,se=se.fit,ci.lower95=ci.lower95,ci.upper95=ci.upper95)
  nm <-deparse(substitute(mod))
  mdlname1 <- paste(nm,"allpred.df",sep=".")
  mdlname2 <- paste(nm,"pred.df",sep=".")
  if(missing(predict.df)){
    allpred.df <- cbind(m.mat,t1[1],pred.df)
    assign(mdlname1,allpred.df,envir = .GlobalEnv)}
  else{assign(mdlname2,pred.df,envir = .GlobalEnv)}
  
  #Example prediction plots (this one with avginc as a polynomial)
  
  #require(ggplot2)
  #predplot <- ggplot(pred.df,aes(avginc,testscr)) + geom_point() + theme_bw() 
  #predplot + geom_line(aes(pred.df$avginc,pred.df$predicted.value)) + geom_errorbar(aes(ymin=pred.df$ci.lower95,ymax=pred.df$ci.upper95))
}
