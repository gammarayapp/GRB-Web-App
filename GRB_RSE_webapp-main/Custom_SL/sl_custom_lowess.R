# lowess {stats}
# l.family can be either 'gaussian' (least squares) or 'symmetric' (M-estimator with Tukey's biweight)

SL.custom_lowess <- function(Y, X, newX, family,lowess.model = NA, obsWeights, span = 0.75, l.family = "gaussian", ...) {
  if(family$family == "gaussian") {
    
    if(is.atomic(lowess.model)){
            lowess.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
    } else {
          print(lowess.model)
          fn <- strsplit(as.character(lowess.model),split = '~')
          lowess.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET 
          print(lowess.model)
    }
    
    fit.lowess <- lowess(lowess.model, data = X, family = l.family, span = span, control = lowess.control(surface = "direct"), weights = obsWeights)
  }
    
   # fit.lowess <- lowess(as.formula(paste("Y~", names(X))), data = X, family = l.family, span = span, control = lowess.control(surface = "direct"), weights = obsWeights)
  
  if(family$family == "binomial") {
    stop('family = binomial() not currently implemented for SL.lowess')
  }
  pred <- predict(fit.lowess, newdata = newX)
  fit <- list(object = fit.lowess)
  out <- list(pred = pred,fit = fit)
  class(out$fit) <- c("SL.custom_lowess")
  return(out)
}

# 
predict.SL.custom_lowess <- function(object, newdata, ...) {
  pred <- predict(object = object$object, newdata = newdata)
  return(pred)
}