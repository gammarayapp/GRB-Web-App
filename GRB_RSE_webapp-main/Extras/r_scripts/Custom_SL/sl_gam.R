SL.gam<-function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4, formula, gam.model = NULL, 
          ...) 
{
  SuperLearner::: .SL.require("gam")
  if ("mgcv" %in% loadedNamespaces()) 
    warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if(is.null(gam.model)){  
    if (sum(!cts.x) > 0) {
      gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+"), "+", paste(colnames(X[, 
                                                                                                           !cts.x, drop = FALSE]), collapse = "+")))
    }
    else {
      gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+")))
    }
    if (sum(!cts.x) == length(cts.x)) {
      gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                              collapse = "+"), sep = ""))
    }
  }
    fit.gam <- mgcv::gam(gam.model, data = X, family = family, 
                      control = gam::gam.control(maxit = 50, bf.maxit = 50), 
  
                                          weights = obsWeights)
  if (packageVersion("gam") >= 1.15) {
    pred <- mgcv::predict.Gam(fit.gam, newdata = newX, type = "response")
  }
  else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}
