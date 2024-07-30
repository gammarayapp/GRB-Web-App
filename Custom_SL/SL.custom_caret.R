SL.caret <- function(Y, X, newX, family, obsWeights, method = "rf", tuneLength = 3, trControl = caret::trainControl(method = "cv", number = 10, verboseIter = TRUE), metric = ifelse(family$family == 'gaussian', 'RMSE', 'Accuracy'), ...) {
  .SL.require('caret')
  if (family$family == "gaussian") {
    fit.train <- caret::train(x = X, y = Y, weights = obsWeights, metric = metric, method = method, tuneLength = tuneLength, trControl = trControl)
    pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (family$family == "binomial") {
    # outcome must be factor, and have real labels
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, metric = metric, method = method, tuneLength = tuneLength, trControl = trControl)
    pred <- predict(fit.train, newdata = newX, type = "prob")[, 2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}

predict.SL.caret <- function(object, newdata, ...) {
  .SL.require('caret')
  if (object$object$modelType == "Regression") {
    pred <- predict(object$object, newdata = newdata, type = "raw")
  } else if (object$object$modelType == "Classification") {
    pred <- predict(object$object, newdata = newdata, type = "prob")[, 2]
  }
  return(pred)
}

# how to change to a different method:
SL.caret.rpart <- function(..., method = "rpart") {
  SL.caret(..., method = method)
}


SL.caret.ANFIS <- function(..., method = "ANFIS") {
  SL.caret(..., method = method)
}


SL.caret.brnn <- function(..., method = "brnn") {
  SL.caret(..., method = method)
}

SL.caret.bridge <- function(..., method = "bridge") {
  SL.caret(..., method = method)
}

SL.caret.blassoAveraged <- function(..., method = "blassoAveraged") {
  SL.caret(..., method = method)
}

SL.caret.cubist <- function(..., method = "cubist") {
  SL.caret(..., method = method)
}

SL.caret.DENFIS <- function(..., method = "DENFIS") {
  SL.caret(..., method = method)
}

SL.caret.enet <- function(..., method = "enet") {
  SL.caret(..., method = method)
}

SL.caret.FIR.DM <- function(..., method = "FIR.DM") {
  SL.caret(..., method = method)
}

SL.caret.GFS.FR.MOGUL <- function(..., method = "GFS.FR.MOGUL") {
  SL.caret(..., method = method)
}

SL.caret.GFS.THRIFT <- function(..., method = "GFS.THRIFT") {
  SL.caret(..., method = method)
}

SL.caret.GFS.LT.RS <- function(..., method = "GFS.LT.RS") {
  SL.caret(..., method = method)
}

SL.caret.HYFIS <- function(..., method = "HYFIS") {
  SL.caret(..., method = method)
}

SL.caret.icr <- function(..., method = "icr") {
  SL.caret(..., method = method)
}

SL.caret.lars <- function(..., method = "lars") {
  SL.caret(..., method = method)
}

SL.caret.lars2 <- function(..., method = "lars2") {
  SL.caret(..., method = method)
}

SL.caret.lm <- function(..., method = "lm") {
  SL.caret(..., method = method)
}

SL.caret.leapBackward <- function(..., method = "leapBackward") {
  SL.caret(..., method = method)
}

SL.caret.leapFoward <- function(..., method = "leapFoward") {
  SL.caret(..., method = method)
}

SL.caret.lmStepAIC <- function(..., method = "lmStepAIC") {
  SL.caret(..., method = method)
}

SL.caret.M5Rules <- function(..., method = "M5Rules") {
  SL.caret(..., method = method)
}

SL.caret.M5 <- function(..., method = "M5") {
  SL.caret(..., method = method)
}

SL.caret.glm.nb <- function(..., method = "glm.nb") {
  SL.caret(..., method = method)
}

SL.caret.rqnc <- function(..., method = "rqnc") {
  SL.caret(..., method = method)
}

SL.caret.null <- function(..., method = "null") {
  SL.caret(..., method = method)
}

SL.caret.nnls <- function(..., method = "nnls") {
  SL.caret(..., method = method)
}

SL.caret.penalized <- function(..., method = "penalized") {
  SL.caret(..., method = method)
}

SL.caret.treebag <- function(..., method = "treebag") {
  SL.caret(..., method = method)
}

SL.caret.logicBag <- function(..., method = "logicBag") {
  SL.caret(..., method = method)
}

SL.caret.bagEarth <- function(..., method = "bagEarth") {
  SL.caret(..., method = method)
}

SL.caret.bagEarthGCV <- function(..., method = "bagEarthGCV") {
  SL.caret(..., method = method)
}

SL.caret.bag <- function(..., method = "bag") {
  SL.caret(..., method = method)
}

SL.caret.bartMachine <- function(..., method = "bartMachine") {
  SL.caret(..., method = method)
}

SL.caret.bayesglm <- function(..., method = "bayesglm") {
  SL.caret(..., method = method)
}

SL.caret.gamboost <- function(..., method = "gamboost") {
  SL.caret(..., method = method)
}

SL.caret.glmboost <- function(..., method = "glmboost") {
  SL.caret(..., method = method)
}

SL.caret.BstSm <- function(..., method = "BstSm") {
  SL.caret(..., method = method)
}

SL.caret.blackboost <- function(..., method = "blackboost") {
  SL.caret(..., method = method)
}

SL.caret.bstTree <- function(..., method = "bstTree") {
  SL.caret(..., method = method)
}

SL.caret.rpart1SE <- function(..., method = "rpart1SE") {
  SL.caret(..., method = method)
}

SL.caret.rpart2 <- function(..., method = "rpart2") {
  SL.caret(..., method = method)
}

SL.caret.cforest <- function(..., method = "cforest") {
  SL.caret(..., method = method)
}

SL.caret.ctree <- function(..., method = "ctree") {
  SL.caret(..., method = method)
}

SL.caret.ctree2 <- function(..., method = "ctree2") {
  SL.caret(..., method = method)
}

SL.caret.randomGLM <- function(..., method = "randomGLM") {
  SL.caret(..., method = method)
}

SL.caret.xgbDART <- function(..., method = "xgbDART") {
  SL.caret(..., method = method)
}

SL.caret.xgbLinear <- function(..., method = "xgbLinear") {
  SL.caret(..., method = method)
}

SL.caret.xgbTree <- function(..., method = "xgbTree") {
  SL.caret(..., method = method)
}

SL.caret.elm <- function(..., method = "elm") {
  SL.caret(..., method = method)
}

SL.caret.gaussprLinear <- function(..., method = "gaussprLinear") {
  SL.caret(..., method = method)
}

SL.caret.gaussprPoly <- function(..., method = "gaussprRadial") {
  SL.caret(..., method = method)
}

SL.caret.gamLoess <- function(..., method = "gamLoess") {
  SL.caret(..., method = method)
}

SL.caret.bam <- function(..., method = "bam") {
  SL.caret(..., method = method)
}

SL.caret.gam <- function(..., method = "gam") {
  SL.caret(..., method = method)
}

SL.caret.krlsPoly <- function(..., method = "krlsPoly") {
  SL.caret(..., method = method)
}

SL.caret.pcr <- function(..., method = "pcr") {
  SL.caret(..., method = method)
}

SL.caret.ppr <- function(..., method = "ppr") {
  SL.caret(..., method = method)
}

SL.caret.qrf <- function(..., method = "qrf") {
  SL.caret(..., method = method)
}

SL.caret.qrnn <- function(..., method = "qrnn") {
  SL.caret(..., method = method)
}

SL.caret.rqlasso <- function(..., method = "rqlasso") {
  SL.caret(..., method = method)
}

SL.caret.krlsRadial <- function(..., method = "krlsRadial") {
  SL.caret(..., method = method)
}

SL.caret.rbf <- function(..., method = "rbf") {
  SL.caret(..., method = method)
}

SL.caret.rbfDDA <- function(..., method = "rbfDDA") {
  SL.caret(..., method = method)
}

SL.caret.ranger <- function(..., method = "ranger") {
  SL.caret(..., method = method)
}

SL.caret.Rborist <- function(..., method = "Rborist") {
  SL.caret(..., method = method)
}

SL.caret.rf <- function(..., method = "rf") {
  SL.caret(..., method = method)
}

SL.caret.extraTrees <- function(..., method = "extraTrees") {
  SL.caret(..., method = method)
}

SL.caret.rfRules <- function(..., method = "rfRules") {
  SL.caret(..., method = method)
}

SL.caret.RRF <- function(..., method = "RRF") {
  SL.caret(..., method = method)
}

SL.caret.relaxo <- function(..., method = "relaxo") {
  SL.caret(..., method = method)
}

SL.caret.rvmLinear <- function(..., method = "rvmLinear") {
  SL.caret(..., method = method)
}

SL.caret.rvmPoly <- function(..., method = "rvmPoly") {
  SL.caret(..., method = method)
}

SL.caret.rvmRadial <- function(..., method = "rvmRadial") {
  SL.caret(..., method = method)
}

SL.caret.ridge <- function(..., method = "ridge") {
  SL.caret(..., method = method)
}

SL.caret.foba <- function(..., method = "foba") {
  SL.caret(..., method = method)
}

SL.caret.rlm <- function(..., method = "rlm") {
  SL.caret(..., method = method)
}

SL.caret.xyf <- function(..., method = "xyf") {
  SL.caret(..., method = method)
}

SL.caret.slps <- function(..., method = "slps") {
  SL.caret(..., method = method)
}

SL.caret.spikeslab <- function(..., method = "spikeslab") {
  SL.caret(..., method = method)
}

SL.caret.dnn <- function(..., method = "dnn") {
  SL.caret(..., method = method)
}

SL.caret.gbm <- function(..., method = "gbm") {
  SL.caret(..., method = method)
}

SL.caret.SBC <- function(..., method = "SBC") {
  SL.caret(..., method = method)
}

SL.caret.gamSpline <- function(..., method = "gamSpline") {
  SL.caret(..., method = method)
}

SL.caret.glm <- function(..., method = "glm") {
  SL.caret(..., method = method)
}

SL.caret.glmStepAIC <- function(..., method = "glmStepAIC") {
  SL.caret(..., method = method)
}

SL.caret.glmnet_h2o <- function(..., method = "glmnet_h2o") {
  SL.caret(..., method = method)
}

SL.caret.glmnet <- function(..., method = "glmnet") {
  SL.caret(..., method = method)
}

SL.caret.gbm_h2o <- function(..., method = "gbm_h2o") {
  SL.caret(..., method = method)
}

SL.caret.kknn <- function(..., method = "kknn") {
  SL.caret(..., method = method)
}

SL.caret.knn <- function(..., method = "knn") {
  SL.caret(..., method = method)
}

SL.caret.svmLinear3 <- function(..., method = "svmLinear3") {
  SL.caret(..., method = method)
}

SL.caret.logreg <- function(..., method = "logreg") {
  SL.caret(..., method = method)
}

SL.caret.avNNet <- function(..., method = "avNNet") {
  SL.caret(..., method = method)
}

SL.caret.monmlp <- function(..., method = "monmlp") {
  SL.caret(..., method = method)
}

SL.caret.mlp <- function(..., method = "mlp") {
  SL.caret(..., method = method)
}

SL.caret.mlpWeightDecay <- function(..., method = "mlpWeightDecay") {
  SL.caret(..., method = method)
}

SL.caret.mlpWeightDecayML <- function(..., method = "mlpWeightDecayML") {
  SL.caret(..., method = method)
}

SL.caret.mlpML <- function(..., method = "mlpML") {
  SL.caret(..., method = method)
}

SL.caret.msaenet <- function(..., method = "msaenet") {
  SL.caret(..., method = method)
}

SL.caret.mlpSGD <- function(..., method = "mlpSGD") {
  SL.caret(..., method = method)
}

SL.caret.mlpKerasDropout <- function(..., method = "mlpKerasDropout") {
  SL.caret(..., method = method)
}

SL.caret.mlpKerasDecay <- function(..., method = "mlpKerasDecay") {
  SL.caret(..., method = method)
}

SL.caret.earth <- function(..., method = "earth") {
  SL.caret(..., method = method)
}

SL.caret.gcvEarth <- function(..., method = "gcvEarth") {
  SL.caret(..., method = method)
}

SL.caret.mxnet <- function(..., method = "mxnet") {
  SL.caret(..., method = method)
}

SL.caret.mxnetAdam <- function(..., method = "mxnetAdam") {
  SL.caret(..., method = method)
}

SL.caret.neuralnet <- function(..., method = "neuralnet") {
  SL.caret(..., method = method)
}

SL.caret.nnet <- function(..., method = "nnet") {
  SL.caret(..., method = method)
}

SL.caret.superpc <- function(..., method = "superpc") {
  SL.caret(..., method = method)
}

SL.caret.svmBoundrangeString <- function(..., method = "svmBoundrangeString") {
  SL.caret(..., method = method)
}

SL.caret.svmExpoString <- function(..., method = "svmExpoString") {
  SL.caret(..., method = method)
}

SL.caret.svmLinear <- function(..., method = "svmLinear") {
  SL.caret(..., method = method)
}

SL.caret.svmLinear2 <- function(..., method = "svmLinear2") {
  SL.caret(..., method = method)
}

SL.caret.svmPoly <- function(..., method = "svmPoly") {
  SL.caret(..., method = method)
}

SL.caret.svmRadial <- function(..., method = "svmRadial") {
  SL.caret(..., method = method)
}

SL.caret.svmRadialCost <- function(..., method = "svmRadialCost") {
  SL.caret(..., method = method)
}

SL.caret.svmRadialSigma <- function(..., method = "svmRadialSigma") {
  SL.caret(..., method = method)
}

SL.caret.svmSpectrumString <- function(..., method = "svmSpectrumString") {
  SL.caret(..., method = method)
}

SL.caret.blasso <- function(..., method = "blasso") {
  SL.caret(..., method = method)
}

SL.caret.lasso <- function(..., method = "lasso") {
  SL.caret(..., method = method)
}

SL.caret.evtree <- function(..., method = "evtree") {
  SL.caret(..., method = method)
}

SL.caret.nodeHarvest <- function(..., method = "nodeHarvest") {
  SL.caret(..., method = method)
}

SL.caret.WM <- function(..., method = "WM") {
  SL.caret(..., method = method)
}

SL.caret.pcaNNet <- function(..., method = "pcaNNet") {
  SL.caret(..., method = method)
}

SL.caret.parRF <- function(..., method = "parRF") {
  SL.caret(..., method = method)
}

SL.caret.partDSA <- function(..., method = "partDSA") {
  SL.caret(..., method = method)
}

SL.caret.kernelpls <- function(..., method = "kernelpls") {
  SL.caret(..., method = method)
}

SL.caret.pls <- function(..., method = "pls") {
  SL.caret(..., method = method)
}

SL.caret.simpls <- function(..., method = "simpls") {
  SL.caret(..., method = method)
}

SL.caret.widekernelpls <- function(..., method = "widekernelpls") {
  SL.caret(..., method = method)
}

SL.caret.plsRglm <- function(..., method = "plsRglm") {
  SL.caret(..., method = method)
}
