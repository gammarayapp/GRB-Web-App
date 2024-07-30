############10fCV manually##########
source('Balanced_Sampling.R')
numCores = detectCores()
clust <- makeCluster(numCores) #This line will take time
registerDoParallel(clust)

TrainData <- TrainingData

# library(class)
#
# res <- kmeans(all_data_scale_wo, centers = 2)
# all_data_scale_wo$High <- res$cluster
######################################Running 10fCV 100 times
#CVparallel <- function()

Algo_coeff <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_coeff) <- libs

Algo_risk <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_risk) <- libs

CVpred <- matrix(0,nrow = dim(PredictionData)[1],ncol = dim(PredictionData)[2])
Valpred <- matrix(0,nrow = dim(ValidationSet)[1],ncol = dim(ValidationSet)[2])

print(
  system.time({
    CVmodel<-foreach(i = 1:loop, .packages=c("SuperLearner", "caret" ,"xgboost", "randomForest", "gbm", "lattice", "latticeExtra", "Matrix", "glmnet", "biglasso","e1071",'earth','party'), 
                     # .export = c("xgb_400_4_0.015","SL.glmnet.interactions","SL.gam") 
                     .export = c(libs,'PLOTaddr') # c('libs','PredictorMatrix','Response','TrainData') #c('xgb_400_4_1','xgb_400_4_0.1','xgb_400_4_0.015','xgb_400_4_0.01','xgb_400_4_0.001')
    )%dopar%{  
      
      source('Custom_SL/sl_mgcv_gam.R')

      #Coefficients <- data.frame()
      namecols <- c('z', 'invz', 'log10z','Fit')
      
      all_data_scale_wo <- TrainData
      #all_redshift_data <- scndOrderVarsTrain
      
      all_data_scale_wo <- subset(all_data_scale_wo, 
                                  select = !(colnames(TrainData) %in% namecols)) # THIS PICKS UP THOSE COLUMNS THAT ARE NOT IN namecols
      PredictionData <- PredictionData[,colnames(all_data_scale_wo)]
      
      allZ_wo <- TrainData$z
      invallZ_wo <- TrainData$log10z
      
      nwo = nrow(all_data_scale_wo)
      
      
      results_cv<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))#,AGN=TrainData$Label)
      results_cv_log<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))#,AGN=TrainData$Label)
      ind<-sample(nwo,nwo)
      folds<-vector('list',10)
      foldlen <- floor(nwo/10)
      
      #for(i in 1:10){
      #  if(i<10){
      #    folds[[i]]=ind[((i-1)*foldlen+1):(i*foldlen)]
      #  }
      #  else{
      #    folds[[i]]=ind[((i-1)*foldlen+1):nwo]
      #  }
      #}
      
      #folds <- createFolds(allZ_wo, k = (nrow(TrainData)-1)) # use caret to make k folds (this return the indices of the fold)
      
      folds <- createFolds(allZ_wo, k = 10) # use caret to make k folds (this return the indices of the fold)
      
      
      for(i in 1:length(folds)){
        
        
        
        train_set<-all_data_scale_wo[-folds[[i]],]
        test_set<-all_data_scale_wo[folds[[i]],]
        
        
        
        # {
        #   scndOrderVarsVal <- train_set
        #   scndOrderVarsVal <- scale(scndOrderVarsVal)
        #   lasmod<-LASSO(scndOrderVarsVal,AGNDat2[-folds[[i]],]$InvRedshift)
        #   lasso_coef<-as.data.frame(lasmod$glmnet.fit$beta[,lasmod$glmnet.fit$lambda==lasmod$lambda.1se])
        #   for (a in 1:100) {
        #     lasmod<-LASSO(scndOrderVarsVal,AGNDat2[-folds[[i]],]$InvRedshift) # LASSO IS PERFORMED
        #     lasso_coef<-cbind(lasso_coef,lasmod$glmnet.fit$beta[,lasmod$glmnet.fit$lambda==lasmod$lambda.1se])
        #   }
        #   lc<-data.frame(co=rowMeans(lasso_coef))
        #   NumOvar<-row.names(lc)[10*abs(lc$co)>0.05] # CHOOSE THOSE PARAMETERS WHICH HAVE MORE THAN 5% WEIGHT
        # }
        #
        Ztrain<-allZ_wo[-folds[[i]]]
        Ztest<-allZ_wo[folds[[i]]]
        
        
        invZtrain<-invallZ_wo[-folds[[i]]]
        invZtest<-invallZ_wo[folds[[i]]]
        
        # TO DO LASSO FEATURE SELECTION FOR EACH FOLD
        # WE SET UP THE PredictorMatrix AND 
        # Response VARIABLE
        #PredictorMatrix <- train_set # -1 BECAUSE WE WANT TO SKIP THE CATEGORICAL VARIABLE
        #Response <- invZtrain
        
        #source('LASSO_feature_selection.R',local = TRUE)
        #print(LassoVars)
        
        #NumOvar <- c(LassoVars)
        
        #train_set <- train_set[,NumOvar]
        #test_set <- test_set[,NumOvar]
        
        #train_set[,c(NumOvar)] <- scale(train_set[,c(NumOvar)])
        #test_set[,c(NumOvar)] <- scale(test_set[,c(NumOvar)])
        
        train_set<-as.data.frame(train_set)
        test_set<-as.data.frame(test_set)
        
        #print(dim(train_set))
        source('Balanced_Sampling.R')
          temp <- balancing(train_set,invZtrain,10)
          train_set<-temp[,-1]
          invZtrain<-temp[,1]
          remove(temp)
        #print(dim(train_set))
        
        s9<-SuperLearner(Y = invZtrain, X = train_set, family = gaussian(), newX=test_set, SL.library = libs)
        
        pr<- s9$SL.predict # PREDICTIONS FOR 1/10
        
        Zpred <- 10^(pr[,1]) - 1
        
        results_cv$Predicted[folds[[i]]]<-pr[,1]
        results_cv$Observed[folds[[i]]]=invZtest
        
        Algo_coeff <- rbind(Algo_coeff,coef(s9))
        
        Algo_risk <- rbind(Algo_risk,s9$cvRisk)
        
        CVpred <- cbind(predict(s9,PredictionData)$pred,CVpred) # POTENTIAL ERROR POINT
        Valpred <- cbind(predict(s9,ValidationSet)$pred,Valpred) # POTENTIAL ERROR POINT
        
        gc()
      }
      
      Algo_coeff <- na.omit(Algo_coeff)
      Algo_risk <- na.omit(Algo_risk)
      
      #return(list(rowMeans(CVpred),colMeans(coef(CV)),CV$SL.predict,rowMeans(ValPred),colMeans(r))) 
      return(list(rowMeans(CVpred),colMeans(Algo_coeff),results_cv$Predicted,colMeans(Algo_risk),rowMeans(Valpred)))
    }
    
  }))
###Performing 10f-CV 100 times
#CVmodel <- foreach(i = 1:2, .combine = cbind, .packages = c('mice', 'SuperLearner', 'glmnet', 'class')) %dopar% {
#  temp_result = CVparallel()
#  temp_result
#}

stopCluster(clust)