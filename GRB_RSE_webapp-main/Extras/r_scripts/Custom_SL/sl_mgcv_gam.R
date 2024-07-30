## generalized additive models (degree = 2)
# functions considers any variable with more than 4 (change with cts.num) unique values to be continuous and able to be in smoothing splines. 
# easy to add additional algorithms with different degrees
# SL.gam.3 <- function(...,deg.gam = 3) SL.gam(..., deg.gam = deg.gam)

SL.mgcv_gam <- function(Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, gam.model = NA, verbose = F, ...) {
  # using require instead of requireNamespace() to allow the formula to parse correctly with s(), gam::s() doesn't work, is not recognized as a special function
  #if(!require('gam')) {stop("SL.gam requires the gam package, but it isn't available")} 
  #if("mgcv" %in% loadedNamespaces()) warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  # create the formula for gam with a spline for each continuous variable
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if(is.atomic(gam.model)){
    if (sum(!cts.x) > 0) { 
      gam.model <- as.formula(paste("Y~", paste(paste("s(", colnames(X[, cts.x, drop = FALSE]), ",", deg.gam,")", sep=""), collapse = "+"), "+", paste(colnames(X[, !cts.x, drop=FALSE]), collapse = "+")))
    } else {
      gam.model <- as.formula(paste("Y~", paste(paste("s(", colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, ")", sep=""), collapse = "+")))
    }
    # fix for when all variables are binomial
    if (sum(!cts.x) == length(cts.x)) {
      gam.model <- as.formula(paste("Y~", paste(colnames(X), collapse = "+"), sep = ""))
    }
  } else {
    
    if(verbose){print(gam.model)}
    fn <- strsplit(as.character(gam.model),split = '~')
    gam.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET AS Y
  }
  
  if(verbose){print(gam.model)}
  
  
  fit.gam <- mgcv::gam(gam.model, data = X, family = family, control = mgcv::gam.control(maxit = 50), weights = obsWeights)
  
  #fit.gam <- mgcv::gam(test_formula, data = TrainingData[,-c(1,3)], family = gaussian(), control = mgcv::gam.control(maxit = 50))#, weights = obsWeights)
  
  if(verbose){print('fitted')}
  
  if(packageVersion('gam') >= 1.15) {
    pred <- mgcv::predict.gam(fit.gam, newdata = newX, type = "response") # updated gam class in version 1.15
  } else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.mgcv_gam")
  return(out)
}

predict.SL.mgcv_gam <- function(object, newdata, ...){
  #.SL.require('mgcv')
  if(packageVersion('gam') >= 1.15) {
    pred <- mgcv::predict.gam(object = object$object, newdata = newdata, type = "response") # updated gam class in version 1.15
  } else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  
  return(pred)
}
