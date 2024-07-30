# 10FCV for testing GAM
############10fCV manually##########
source('Custom_SL/sl_mgcv_gam.R')

numCores = detectCores()
clust <- makeCluster(numCores)
registerDoParallel(clust)

PredictionData$Response <- PredictionData$log10z
PredData <- subset(PredictionData, select = -c(log10z, Redshift_crosscheck))

linResponse <- TrainingData$Redshift_crosscheck
logResponse <- TrainingData$log10z

nwo = nrow(TrainingData)

CVpred <- matrix(0,nrow = dim(PredData)[1],ncol = dim(PredData)[2])
# Valpred <- matrix(0,nrow = dim(SqrValidationSet)[1],ncol = dim(SqrValidationSet)[2])
CVmodel <- foreach(j = 1:loop
                     ,.packages=c("mgcv", "caret")
)%do%{
  
  results_cv<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))
  folds <- createFolds(linResponse, k = 10) # use caret to make k folds (this return the indices of the fold)
  
  for (i in 1:length(folds)) {
    
    train_set<-TrainingData[-folds[[i]],]
    test_set<-TrainingData[folds[[i]],]
    
    Ztrain<-linResponse[-folds[[i]]]
    Ztest<-linResponse[folds[[i]]]
    
    train_set$Response<-logResponse[-folds[[i]]]
    test_set$Response<-logResponse[folds[[i]]]

    gam_model <- gam(formula(gam_formula),
                           data=train_set,
                           family=gaussian())

    results_cv$Predicted[folds[[i]]] <- predict(gam_model,test_set)
    results_cv$Observed[folds[[i]]] <- test_set$Response
    
    CVpred <- cbind(predict(gam_model,PredData),CVpred)
    
  }

  return(list(rowMeans(CVpred),
              results_cv$Predicted))
                       
}

stopCluster(clust)
doParallel::stopImplicitCluster()
closeAllConnections()
