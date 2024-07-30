# loess {stats}
# l.family can be either 'gaussian' (least squares) or 'symmetric' (M-estimator with Tukey's biweight)

SL.custom_loess <- function(Y, X, newX, family,loess.model = NA, obsWeights, span = 0.75, l.family = "gaussian", ...) {
  if(family$family == "gaussian") {
    
    if(is.atomic(loess.model)){
            loess.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
    } else {
          print(loess.model)
          fn <- strsplit(as.character(loess.model),split = '~')
          loess.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET 
          print(loess.model)
    }
    
    fit.loess <- loess(loess.model, data = X, family = l.family, span = span, control = loess.control(surface = "direct"), weights = obsWeights)
  }
    
   # fit.loess <- loess(as.formula(paste("Y~", names(X))), data = X, family = l.family, span = span, control = loess.control(surface = "direct"), weights = obsWeights)
  
  if(family$family == "binomial") {
    stop('family = binomial() not currently implemented for SL.loess')
  }
  pred <- predict(fit.loess, newdata = newX)
  fit <- list(object = fit.loess)
  out <- list(pred = pred,fit = fit)
  class(out$fit) <- c("SL.custom_loess")
  return(out)
}

# 
predict.SL.custom_loess <- function(object, newdata, ...) {
  pred <- predict(object = object$object, newdata = newdata)
  return(pred)
}