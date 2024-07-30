# bayesglm{arm}
# Bayesian generalized linear regression
require('arm')

SL.custom_bayesglm <- function(Y, X, newX, family, obsWeights, bglm.model = NA, ...){
  
  
  if(is.atomic(bglm.model)){
    bglm.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
  } else {
    print(bglm.model)
    fn <- strsplit(as.character(bglm.model),split = '~')
    bglm.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET 
  }
  
  fit.glm <- arm::bayesglm(bglm.model, data = X, family = family, weights = obsWeights)
  
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.custom_bayesglm")
  return(out)
}

predict.SL.custom_bayesglm <- function(object, newdata, ...){
  #.SL.require('arm')
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  return(pred)
}