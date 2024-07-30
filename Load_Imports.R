# require(doParallel)
# require(mice)
# require(VIM) # mice graph 1
# require(ggplot2) # mice graph 2
# require(lattice) # GAM predicted vs observed plot
# require(stringr) # string remove
# require(dplyr) # arrange (sorting for GAM)
# require(MASS) # fit normal dist in GAM (super balanced sampling)
# require(randomForest)
# require(earth)
# library(glmnet) # LASSO, ElasticNet
# require(SuperLearner)
# require(mgcv)
# require(xgboost)
# require(gbm)
# require(caret)
# require(party)
# library(arm)
# library(dgof)
# library(kSamples)
#library(biglasso)
#require(cforest)


# Package names
packages <- c("doParallel", "mice", "VIM", "ggplot2", "lattice", "stringr", "dplyr", "MASS", "randomForest", "earth", "glmnet", "SuperLearner", "mgcv", "xgboost", "gbm", "caret", "party", "arm", "dgof", "kSamples", "latticeExtra", "biglasso", "Matrix", "e1071")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))