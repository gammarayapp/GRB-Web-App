# bayesglm{arm}
# Bayesian generalized linear regression
require('arm')

SL.custom_glm <- function(Y, X, newX, family, obsWeights, glm.model = NA, ...){
  
  
  if(is.atomic(glm.model)){
    glm.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
  } else {
    print(glm.model)
    fn <- strsplit(as.character(glm.model),split = '~')
    glm.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET 
    print(glm.model)
  }
  
  fit.glm <- glm(glm.model, data = X, family = family, weights = obsWeights)
  
  pred <- predict(fit.glm, newdata = newX, type = "response")
  fit <- list(object = fit.glm)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.custom_glm")
  return(out)
}

predict.SL.custom_glm <- function(object, newdata, ...){
  #.SL.require('arm')
  pred <- predict(object = object$object, newdata = newdata, type = "response")
  return(pred)
}