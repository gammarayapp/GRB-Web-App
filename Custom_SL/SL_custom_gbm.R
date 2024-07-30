# gbm{gbm}
# generalized boosting algorithm
# can alter number ot trees in intial fit
# also alter interaction depth (SL.gbm.1 and SL.gbm.2)
SL.custom_gbm <- function(Y, X, newX, family, obsWeights, gbm.model=NA, gbm.trees = 10000, interaction.depth = 2, shrinkage = 0.001, bag.fraction = 0.5, cv.folds = 5, n.minobsinnode = 10, n.cores = NULL, ...) {
  #.SL.require('gbm')
  
  if(is.na(gbm.model)){
    gbm.model <- as.formula(paste("Y~", paste(colnames(X), collapse="+")))
  } else {
    print(gbm.model)
    fn <- strsplit(as.character(gbm.model),split = '~')
    gbm.model <- as.formula(paste('Y ~',fn[[3]][1])) # CREATING THE FORMULA WITH THE RESPONSE VARIABLE SET 
  }
  
  
  if(family$family == "gaussian") {  
    fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "gaussian", n.trees = gbm.trees, interaction.depth = interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction, n.minobsinnode = n.minobsinnode, cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights, verbose = FALSE, n.cores = n.cores)
  }
  if(family$family == "binomial") {
    fit.gbm <- gbm::gbm(formula = gbm.model, data = X, distribution = "bernoulli", n.trees = gbm.trees, interaction.depth = interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction, n.minobsinnode = n.minobsinnode, cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights, verbose = FALSE, n.cores = n.cores)
  }
  
  best.iter <- gbm::gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
  pred <- predict(fit.gbm, newdata = newX, n.trees = best.iter, type = "response")
  fit <- list(object = fit.gbm, n.trees = best.iter)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.custom_gbm")
  return(out)
}

predict.SL.custom_gbm <- function(object, newdata,...){
  #.SL.require('gbm')
  pred <- predict(object$object, newdata = newdata, n.trees = object$n.trees, type = "response")
  return(pred)
}
