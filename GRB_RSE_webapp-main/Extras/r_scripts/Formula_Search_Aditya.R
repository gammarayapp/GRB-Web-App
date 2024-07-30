source('Load_Imports.R')

run_locally = F

if(run_locally){
  raw_xray_data <- read.csv("combined_data_with_redshift.csv", header = TRUE, row.names = 1)
  correlation_cutoff = 0.99
  RMSE_cutoff = 0.03
} else {
  #ADD getting both cutoffs from user
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  output_file <- args[2]
  correlation_cutoff = as.numeric(args[3])
  RMSE_cutoff = as.numeric(args[4])
  raw_xray_data <- read.csv(input_file, header = TRUE, row.names = 1)
}

do_mice = T

###USERS INPUTS THEIR OWN TRAINING SET INSTEAD OF THIS ONE
GRBPred <- read.csv(file = "OutputFiles/MEstimator/grb_xray_m_est.csv", header = TRUE, row.names = 1) 

addr <-paste("Results/") #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
PLOTaddr <-paste("Plot_Output/") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

## CREATE DIRECTORIES IF THEY DONT EXIST
if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
if(!dir.exists(addr)){dir.create(addr)}

# colnames(GRBPred)

##### NEEDS TO BE EDITED WHEN WE WORK ON MICE
# checking for NA values, if using MICE, should have none
#md.pattern(GRBPred, rotate.names = T)


SqrTermGen <- function(inputData) {
  indVar <- colnames(inputData)
  for (i in 1:length(indVar)) { # Loop over all variables in indVar
    for (j in i:(length(indVar))) { # Loop over all variables at index i and greater than index i
      # If i and j correspond to the same varaible call the variable varSqr
      if (indVar[i] == indVar[j]) {
        inputData[[paste(indVar[i], "Sqr", sep = "")]] <- inputData[, indVar[i]] * inputData[, indVar[j]]
      } # else{
      # inputData[[paste(indVar[i],indVar[j],sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      # }
    }
  }
  return(inputData)
}

#addr <-paste("Results_",Sys.Date(),"/Files/",sep='') #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
#PLOTaddr <-paste("Plot_Output") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

sz<-0.8
rez=120


## CREATE DIRECTORIES IF THEY DONT EXIST
# if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
# if(!dir.exists(addr)){dir.create(addr)}


# Pick out Long GRBs
raw_xray_data = raw_xray_data[raw_xray_data$T90 > 2,]

raw_xray_data$log10T90 = log10(raw_xray_data$T90)


# creating new subset for only numeric features that are not tied to the response (Redshift_crosscheck)
features_for_mice_preds = subset(raw_xray_data,select = c(log10T90,
                                                          log10Fa,
                                                          log10Ta,
                                                          Alpha,
                                                          Beta,
                                                          Gamma,
                                                          log10Fluence,
                                                          PhotonIndex,
                                                          log10NH,
                                                          log10PeakFlux))

features_for_mice_errs = subset(raw_xray_data,select = c(T90Err,
                                                         log10FaErr,
                                                         log10TaErr,
                                                         AlphaErr,
                                                         BetaErr,
                                                         FluenceErr,
                                                         PhotonIndexErr,
                                                         PeakFluxErr))

# replacing inf in log10PeakFlux feature with NAs
features_for_mice_preds$log10PeakFlux[is.infinite(features_for_mice_preds$log10PeakFlux)] <- NA

# removing all log10NH values lower than 20
features_for_mice_preds$log10NH[features_for_mice_preds$log10NH < 20] <- NA

features_for_mice_preds$Beta[features_for_mice_preds$Beta > 3] <- NA

features_for_mice_preds$Gamma[features_for_mice_preds$Gamma > 3] <- NA

features_for_mice_preds$Alpha[features_for_mice_preds$Alpha > 3] <- NA

features_for_mice_preds$PhotonIndex[features_for_mice_preds$PhotonIndex < 0] <- NA


if(do_mice){
  set.seed(1)
  
  # features_for_mice_all <- cbind(features_for_mice_preds, features_for_mice_errs)
  mice_model_preds <- mice(data = features_for_mice_preds,
                           m = 20,
                           method = 'midastouch',
                           printFlag = F)
  features_for_mice_preds <- complete(mice_model_preds,20)
  
  mice_model_errs <- mice(data = features_for_mice_errs,
                          m = 20,
                          method = 'midastouch',
                          printFlag = F)
  features_for_mice_errs <- complete(mice_model_errs,20)
  
  GRBPred <- cbind(features_for_mice_preds, features_for_mice_errs)
  # GRBPred<-complete(mice_model,20)
  
} else {
  #NEEDS TO BE EXPANDED IF NO MICE
  GRBPred <- rs_data_preds
}

########### Adding log error columns ###########
# log10T90
T90 <- 10^GRBPred$log10T90
Fluence <- 10^GRBPred$log10Fluence
PeakFlux <- 10^GRBPred$log10PeakFlux

GRBPred$log10T90Err <- GRBPred$T90Err/(T90 * log(10))
GRBPred$log10FluenceErr <- GRBPred$FluenceErr/(Fluence * log(10))
GRBPred$log10PeakFluxErr <- GRBPred$PeakFluxErr/(PeakFlux * log(10))

source('lasso.R')

# generating squared terms for future ML methods
Variables <- subset(GRBPred, select = c(lassovar))
Err <- subset(GRBPred, select = colnames(GRBPred) %in% paste(lassovar,"Err",sep=""))

#GRBPred <- SqrTermGen(Variables)
#GRBPred <- cbind(GRBPred, Err)

# adding z back into original dset
GRBPred$Redshift_crosscheck <- raw_xray_data$Redshift_crosscheck
# unscaled_GRBPred$Redshift_crosscheck <- GRBPred$Redshift_crosscheck

# adding log10z back into original dataset
GRBPred$log10z <- log10(raw_xray_data$Redshift_crosscheck + 1)
# unscaled_GRBPred$log10z <- GRBPred$log10z 

# GRBPred$invz <- 1/(raw_xray_data$Redshift_crosscheck + z_e)
# unscaled_GRBPred$invz <- GRBPred$invz


# writing file to output directory
if (do_mice){
  write.csv(GRBPred, "OutputFiles/DataHandle/grb_xray_imputed.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
} else {
  write.csv(GRBPred, "OutputFiles/DataHandle/grb_xray.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
}


#WE'LL DECIDE IF WE NEED IT
##source("m_estimator.R")

#PredictionData <- tail(GRBPred, n = 0.20 * nrow(GRBPred))

#TrainData <- GRBPred[!(rownames(GRBPred) %in% rownames(PredictionData)),]

#Response <- TrainData$log10z
#Predictors <- subset(TrainData
#                     ,select = -c(log10z, Redshift_crosscheck)) # EXCLUDING LOG10Z, INVZ AND Z

TrainData=GRBPred
#TrainData=read.csv("Total_useable_data_w_MICE_NH&Peak_imputed.csv",header = T,row.names = 1)
PredictionData <- tail(TrainData,n = nrow(TrainData)*(0.1))

source('Formula_Generation/GAM_formula_generator.R')

Response = TrainData$log10z
Predictor = subset(TrainData,select = lassovar)

O2Predictor = quadTermGen(Predictor)
SQRPredictor = SqrTermGen(Predictor)


SqrTrainData = SqrTermGen(Predictor)

SqrTrainData$Response <- TrainData$log10z

GamTrainData <- subset(SqrTrainData, select = !colnames(SqrTrainData) %in% c("log10z"))

GamTestData <- head(GamTrainData,n = (0.05*nrow(GamTrainData)))
dim(GamTestData)

GamValidationData <- tail(GamTrainData,n = 0.05*nrow(GamTrainData) )
dim(GamValidationData)

GamTrainData <- GamTrainData[!(rownames(GamTrainData)%in%c(rownames(GamTestData),rownames(GamValidationData))),]

intersect(rownames(GamTestData),rownames(GamTrainData))
intersect(rownames(GamTestData),rownames(GamValidationData))

######### SETTING UP GAM FUNCTION ######

#source('Custom_SL/sl_mgcv_gam.R')

# bestGAM <- Response ~ (logPeak + Beta)^2 + (PhotonIndex + logNH)^2 + (PhotonIndex + Beta)^2 + (logNH + logFa)^2 + (logT_a + Alpha)^2
# AftrGlw1 <- as.formula("Response ~ (logPeak + Alpha)^2 + logFa + logFluence + logNH")
# AftrGlw2 <- as.formula("Response ~ logPeak + s(logNH) + logFluence + logFa")
# AftrGlw3 <- as.formula("Response ~ logPeak + s(logNH) + logFluence + logFa")
# AftrGlw4 <- as.formula("Response ~ (logPeak + logNH)^2")
# BasicForm <- as.formula("Response ~ logFa + logNH + logT_a + logPeak + logT90 + logFluence + Alpha + Beta + PhotonIndex")


#mgcv_gam <- mgcv::gam(formula(bestGAM), data=TrainingData, select=TRUE, drop.intercept = TRUE)


#test_formula <- as.formula('Response ~ (logPeak + Beta) + (PhotonIndex + logNH) + (PhotonIndex + Beta) + (logNH + logFa) + (logT_a + Alpha)')

bestGAM <- as.formula("Response ~ (logPeak + Beta)^2 + (PhotonIndex + logNH)^2 + (PhotonIndex + Beta)^2 + (logNH + logFa)^2 + (logT_a + Alpha)^2")

# formula_list = GAM_formula(O2Predictor)
# write.table(formula_list,file = "Gam_formula_list.txt")


###### SNIPPET TO READ IN THE FORMULA FILE #####
O1_formula_list = read.csv("Formula_Generation/O1_formula_list",header = T,row.names = 1)
#O1_formula_list = read.csv(paste0("O1_formula_list_",ncol(Predictor),"features.csv"),row.names = 1)

#O1_gam_formulas = apply(as.matrix(O1_formula_list), 1, as.formula)

O2_formula_list = read.csv("Formula_Generation/O2_formula_list",header = T,row.names = 1)
#O2_gam_formulas = apply(as.matrix(O2_formula_list), 1, as.formula)

SO1_formula_list = read.csv("Formula_Generation/SmoothedO1_formula_list",header = T,row.names = 1)
#SO1_gam_formulas = apply(as.matrix(SO1_formula_list), 1, as.formula)
###############################################

# ########### TESTING SUPERLEARNER #############
# # system.time({gam_sl = SuperLearner(Y = Response,
# #                        X = SQRPredictor,
# #                        family = gaussian(),
# #                        SL.library = c(learners2$names),
# #                        control = list(saveFitLibrary=T),
# #                        verbose = F
# #                        #,obsWeights = c(1:nrow(Predictors))
# # )
# # })

################ TESTING MGCV::GAM ###########

all_formula_list <- rbind(O1_formula_list
                          #SO1_formula_list
#                          ,O2_formula_list
)

#O1andSO1=dim(O1_formula_list)[1] + dim(SO1_formula_list)[1]

#best_O1_SO1 = as.formula("Response ~ (logPeak + logFluence + PhotonIndex + Alpha)^2 + logFa + logT_a + Beta + logT90 + logNH")

#best_O2_run1 = as.formula("Response ~ (logNHSqr + BetaSqr + logPeak + logNH + logT90 + Beta)^2 + logFa + logT_a + Alpha + PhotonIndex + logFluence + logFaSqr + logT_aSqr + AlphaSqr + PhotonIndexSqr + logT90Sqr + logFluenceSqr + logPeakSqr")

#best_O2_run2 = as.formula("Response ~ (logPeakSqr + logFluenceSqr + logT90Sqr + BetaSqr + logFaSqr + logPeak + logT90)^2 + logFa + logT_a + Alpha + Beta + PhotonIndex + logNH + logFluence + logT_aSqr + AlphaSqr + PhotonIndexSqr + logNHSqr")

# WE ARE KEEPING FORMULAS TILL 262450 BECAUSE AFTER THIS ERRORS OCCUR DUE TO TOO MANY VARIABLES
#all_formula_list <- all_formula_list[1:max_formula,]
all_formula_list = na.omit(all_formula_list)
#all_formula <- c(bestGAM,O1_gam_formulas,SO1_gam_formulas,O2_gam_formulas[18:10000])

all_formula <- apply(as.matrix(all_formula_list), 1, as.formula)



  formula_analysis_addr = paste0("Formula_Generation/GAM_",Sys.Date(),"/")
  if(!dir.exists(formula_analysis_addr)){dir.create(formula_analysis_addr)}


GamValidationData = rbind(GamValidationData,GamTestData)
#write.csv(GamTestData,file = "MGCV/Gam_test_set.csv",row.names = rownames(GamTestData))
write.csv(GamValidationData,file = paste0(formula_analysis_addr,"Gam_validation_set.csv"),row.names = rownames(GamValidationData))
write.csv(GamTrainData,file = paste0(formula_analysis_addr,"Gam_train_set.csv"),row.names = rownames(GamTrainData))

GamTrainData_unscaled = GamTrainData
GamValidationData_unscaled = GamValidationData

#### STORE THE SD AND MEAN OF THE GAM TRAIN DATA
# THEN SCALE THE VALIDATION SET WITH THAT SD AND MEAN
GamTrainData_sd = apply(GamTrainData,2,sd)
GamTrainData_mean = apply(GamTrainData,2,mean)

# if(Scaling){
#   GamTrainData[,1:12] = scale(GamTrainData[,1:12])
#   GamValidationData[,1:12] = scale(GamValidationData[,1:12]
#                                    ,center = GamTrainData_mean[1:12] # the validation set is scaled by the 
#                                    ,scale = GamTrainData_sd[1:12]) # mean and sd of the training set
#   
#   write.csv(GamValidationData,file = paste0(formula_analysis_addr,"Scaled_Gam_validation_set.csv"),row.names = rownames(GamValidationData))
#   write.csv(GamTrainData,file = paste0(formula_analysis_addr,"Scaled_Gam_train_set.csv"),row.names = rownames(GamTrainData))
#   write.csv(rbind(GamTrainData_mean,GamTrainData_sd),file = paste0(formula_analysis_addr,"Gam_train_set_Scale&Mean.csv"))
# }


#print(max_formula)
#print(max_parallel)
#print(num_cores)

print(paste(
  "Number of formulas=",length(all_formula)
  ,"| Dimension of training set=",dim(GamTrainData)
)
  #,"Scaling the data = ",Scaling)
)


#sink("MGCV/progress.txt",append=T)
tick <- proc.time()
manual_cv <- foreach(j = 1:length(all_formula)
                     #,.export = c(all_formula)
                     ,.packages=c("SuperLearner","mgcv", "caret" ,"xgboost", "randomForest", "gbm", "lattice", "Matrix", "glmnet", "biglasso","e1071",'earth','party')
) %dopar% {
  
  InnerLoop = 50
  
  test_preds <- data.frame(Predicted= numeric(nrow(GamTrainData)),Observed= numeric(nrow(GamTrainData)))
  test_preds_loop= matrix(nrow = nrow(GamTrainData),ncol = InnerLoop)
  
  set.seed(j)
  
  gam_formula = all_formula[[j]]
  
  for (k in 1:InnerLoop) {
    
    folds <- createFolds(y = GamTrainData$Response, k = 10)
    
    ###### THE 10FCV SECTION ##########
    for (i in 1:length(folds)) {
      train_set <- GamTrainData[-c(folds[[i]]),]
      test_set <- GamTrainData[c(folds[[i]]),]
      
      #print(rownames(head(train_set)))
      #print(rownames(test_set))
      
      #gam_model <- MASS::rlm(gam_formula,train_set,method = 'M')
      gam_model <- mgcv::gam(formula = gam_formula
                             ,data = train_set
                             ,family = gaussian())
      
      
      ###### TEST SET PREDICTION ###########
      #predict(gam_model,test_set)
      test_preds$Predicted[c(folds[[i]])] <- predict(gam_model,test_set)
      test_preds$Observed[c(folds[[i]])] <- test_set$Response
    }
    
    test_preds_loop[,k] = test_preds$Predicted
    
  }
  
  test_preds$Predicted = rowMeans(test_preds_loop)
  
  # THE ACTUAL RETURN STATEMENT
  gam_model <- mgcv::gam(formula = gam_formula
                         ,data = GamTrainData
                         ,family = gaussian())
  #gam_model <- MASS::rlm(gam_formula,train_set,method = 'M')
  return(list(test_preds$Predicted,predict(gam_model,GamValidationData)))
}

tock <- proc.time() - tick

# NEED TO TEST THIS TIMING
cat(tock)

#sink();
# CREATE DIRECTORIES IF THEY DONT EXIST
saveRDS(manual_cv,file = paste0(formula_analysis_addr,"SuperLearner_complete_data.rds"))
saveRDS(all_formula,file=paste0(formula_analysis_addr,"Formulas_used.rds"))

print("Running reading results code")




############# READING THE RESULTS #############
print("Reading results for formula")
# READING IN THE SERVER RESULTS
results = readRDS(paste0(formula_analysis_addr,"SuperLearner_complete_data.rds"))


# READ IN THE FORMULAS USED
libs=readRDS(paste0(formula_analysis_addr,"Formulas_used.rds"))

#GamTestData <- read.csv("MGCV/Gam_test_set.csv",header = T,row.names = 1)


GamTrainData <- read.csv(paste0(formula_analysis_addr,"Gam_train_set.csv"),header = T,row.names = 1)
GamValidationData <- read.csv(paste0(formula_analysis_addr,"Gam_validation_set.csv"),header = T,row.names = 1)


Response <- GamTrainData$Response

Validation_response <- GamValidationData$Response

libs = libs[1:length(results)]
# READ IN THE FULL TRAINING SET RESPONSE AND PREDICTORS
#Response = read.csv("Response.csv",row.names = 1,header = T)
#colnames(Response) = "Response"
#O2Predictor=read.csv("O2Predictor.csv",row.names = 1)

# READ IN THE TRAINING SET RESPONSE AND PREDICTORS
#Validation_response=read.csv("Validation_response.csv",row.names = 1)
#Validation_predictors=read.csv("Validation_predictors.csv",row.names = 1)


#algo_coef = matrix(nrow = length(results[[2,1]]), ncol = max_loop)
#row.names(algo_coef) = names(results[[2,1]]) # SETTING THE ROW NAMES = ALGO NAME
#preds = matrix(nrow = length(results[[1,1]]), ncol = max_loop)

CV_Prediction_matrix = matrix(nrow = length(results[[1]][[1]]), ncol = length(libs))
colnames(CV_Prediction_matrix)=names(libs)

Validation_Prediction_matrix = matrix(nrow = length(results[[1]][[2]]), ncol = length(libs))
colnames(Validation_Prediction_matrix)=names(libs)

Validation_correlations = vector(length = length(libs))
Validation_RMSE = vector(length = length(libs))
Validation_MAD = vector(length = length(libs))

CV_correlation = vector(length = length(libs))
CV_RMSE = vector(length = length(libs))
CV_MAD = vector(length = length(libs))

for(j in 1:length(libs)){
  #algo_coef[,j] = results[[j]][[2]]
  #preds[,j] = results[[j]][[1]]
  #algo_coef[,j] = results[[2,j]]
  #preds[,j] = results[[1,j]]
  
  CV_Prediction_matrix[,j] = results[[j]][[1]]
  
  CV_correlation[j] = cor(CV_Prediction_matrix[,j],Response)
  CV_RMSE[j]=sqrt(mean((CV_Prediction_matrix[,j] - Response)^2))
  CV_MAD[j]=mad((CV_Prediction_matrix[,j] - Response))
  
  
  Validation_Prediction_matrix[,j] = results[[j]][[2]]
  Validation_correlations[j] = cor(Validation_Prediction_matrix[,j],Validation_response)
  Validation_RMSE[j]=sqrt(mean((Validation_Prediction_matrix[,j] - Validation_response)^2))
  Validation_MAD[j]=mad((Validation_Prediction_matrix[,j] - Validation_response))
  
}


#Testset_RMSE = sqrt(mean((Test_Prediction_matrix - Test_response$x)^2))



# mean_algo_coef = apply(algo_coef, 1, mean)
# sorted_algo_coef = sort(mean_algo_coef)
# 
# par(mar=c(5,8,5,5))
# barplot(sorted_algo_coef[sorted_algo_coef>0.02],horiz = T,las=2)
# 
# cor(preds,Response)

plot(CV_RMSE)
plot(Validation_RMSE)

plot(CV_MAD)
plot(Validation_MAD)

plot(CV_correlation,Validation_correlations)
plot(CV_RMSE,Validation_RMSE)

library(ggplot2)

CV_results = cbind(CV_correlation,CV_RMSE,CV_MAD)
rownames(CV_results) = libs

Validation_results = cbind(Validation_correlations,Validation_RMSE,Validation_MAD)
rownames(Validation_results) = libs

CV_results = as.data.frame(CV_results)
Validation_results = as.data.frame(Validation_results)

##### FORMULA BASED ON CORRELATION #######
{
  CV_correlation_cutoff = quantile(CV_results$CV_correlation, correlation_cutoff)#0.55
  Validation_correlation_cutoff = 0.6
  
  
  CV_RMSE_cutoff      = quantile(CV_results$CV_RMSE, RMSE_cutoff)#0.22
  Validation_RMSE_cutoff = 0.15
  
  plot(CV_correlation,CV_RMSE,lwd=1)
  abline(v= CV_correlation_cutoff)#mean(CV_results$CV_correlation)*CV_correlation_cutoff)
  abline(h= CV_RMSE_cutoff)#mean(Validation_correlations)*Validation_correlation_cutoff)
  
  Correlation_formula = intersect(
    rownames(CV_results)[CV_results$CV_RMSE < CV_RMSE_cutoff]#mean(Validation_results$Validation_correlations)*Validation_correlation_cutoff]
    ,rownames(CV_results)[CV_results$CV_correlation > CV_correlation_cutoff]#mean(CV_results$CV_correlation)*CV_correlation_cutoff]
  )
  
  #print(Correlation_formula)
  
  # THIS HIGHLIGHTS THE POINTS BELOW THE CHOSE CUTOFFS
  points( x = CV_results$CV_correlation[rownames(CV_results) %in% Correlation_formula]
          ,y = CV_results$CV_RMSE[rownames(CV_results) %in% Correlation_formula]
          ,col = 'red',lwd=5)
}



Best_formula = na.omit(Correlation_formula)#intersect(Correlation_formula,RMSE_formula)
Best_formula
### NOW THAT WE HAVE THE BEST FORMULAS BASED ON
### CORRELATION AND RMSE, WE NEED TO TEST THEM
### ON THE TEST SET AND SEE WHICH PERFORMS BEST
### THIS WILL ELIMINATE DATA BLEEDING AND OVERFITTING

library(SuperLearner)
library(mgcv)

# O1_formula = read.csv(file = "Formula_Generation/O1_formula_list",row.names = 1)
# O1_formula = apply(as.matrix(O1_formula), 1, as.formula)


Test_response = GamValidationData$Response#read.csv(file = "Test_response.csv",row.names = 1,header = T)
Test_predictors = GamValidationData[,c(1:ncol(GamValidationData)-1)]   #read.csv(file = "Test_predictors.csv",row.names = 1,header = T)



#colnames(Response) = "Response"

test_preds = matrix(nrow = nrow(Test_predictors),ncol = length(Best_formula))
cc=1

# LOOP OVER ALL THE FORMULAS IN Best_formula AND GET THE PREDICTION  RESULTS ON THE TEST SET
for (i in 1:length(Best_formula)) {
  test_gam = mgcv::gam(formula = as.formula(Best_formula[[i]]), data = GamTrainData, family = gaussian())
  #test_gam = MASS::rlm(as.formula(Best_formula[[i]]),GamTrainData,method = 'M')
  test_preds[,cc] = predict(test_gam,Test_predictors)
  cc=cc+1
}


test_preds = as.data.frame(test_preds)
rownames(test_preds) = rownames(Test_predictors)
colnames(test_preds) = Best_formula
head(test_preds)


Test_Prediction_matrix = matrix(nrow = ncol(test_preds),ncol = 3)
Test_Prediction_matrix[,1] = cor(test_preds,Test_response)

# LOOP OVER THE COLUMNS (FORMULAS) AND GENERATE THE RMSE AND MAD METRICS
for (j in 1:ncol(test_preds)) {
  Test_Prediction_matrix[j,2] = sqrt(mean((test_preds[[j]] - Test_response)^2))
  Test_Prediction_matrix[j,3] = mad(test_preds[[j]] - Test_response)
}


Test_Prediction_matrix = as.data.frame(Test_Prediction_matrix)
rownames(Test_Prediction_matrix) = colnames(test_preds)
colnames(Test_Prediction_matrix) = c("Correlation","RMSE","MAD")

plot(Test_Prediction_matrix[,c(1,2)]
     ,col= Test_Prediction_matrix$MAD*100
     ,pch = 3, lwd = 4)

abline(h = min(Test_Prediction_matrix$RMSE)
       ,v = max(Test_Prediction_matrix$Correlation) 
       ,col='red')

plot(Test_Prediction_matrix[,c(1,3)]
     ,lwd= Test_Prediction_matrix$RMSE*20
     ,pch = 3)

abline(h = min(Test_Prediction_matrix$MAD)
       ,v = max(Test_Prediction_matrix$Correlation) 
       ,col='red')


Formula_for_outlier = rownames(Test_Prediction_matrix)[Test_Prediction_matrix$Correlation == max(Test_Prediction_matrix$Correlation)]

print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$Correlation == max(Test_Prediction_matrix$Correlation)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$RMSE == min(Test_Prediction_matrix$RMSE)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$MAD == min(Test_Prediction_matrix$MAD)])

sink(file = paste0(formula_analysis_addr,"Best_formula",Sys.Date(),
                   ".txt"))
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$Correlation == max(Test_Prediction_matrix$Correlation)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$RMSE == min(Test_Prediction_matrix$RMSE)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$MAD == min(Test_Prediction_matrix$MAD)])
sink()

sink(file = paste0("Best_formula.txt"))
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$Correlation == max(Test_Prediction_matrix$Correlation)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$RMSE == min(Test_Prediction_matrix$RMSE)])
print(rownames(Test_Prediction_matrix)[Test_Prediction_matrix$MAD == min(Test_Prediction_matrix$MAD)])
sink()
flush.console()

save.image(paste0(formula_analysis_addr,"Formula_generated.Rdata"))

{
  png(paste(PLOTaddr,"Corr_RMSE_compare.png",sep=''),height=800,width=800,res=160)
  par(mar=c(4,5,1,1)+.1)
  plot(CV_correlation,CV_RMSE,lwd=1,cex.axis=2,cex.lab=2,font.axis=1,font.lab=2)
  abline(v= CV_correlation_cutoff)#mean(CV_results$CV_correlation)*CV_correlation_cutoff)
  abline(h= CV_RMSE_cutoff)#mean(Validation_correlations)*Validation_correlation_cutoff)
  
  Correlation_formula = intersect(
    rownames(CV_results)[CV_results$CV_RMSE < CV_RMSE_cutoff]#mean(Validation_results$Validation_correlations)*Validation_correlation_cutoff]
    ,rownames(CV_results)[CV_results$CV_correlation > CV_correlation_cutoff]#mean(CV_results$CV_correlation)*CV_correlation_cutoff]
  )
  box('plot',lwd=5)
  points( x = CV_results$CV_correlation[rownames(CV_results) %in% Correlation_formula]
          ,y = CV_results$CV_RMSE[rownames(CV_results) %in% Correlation_formula]
          ,col = 'red',lwd=5)
  dev.off()
  
  
  
  png(paste(PLOTaddr,'Testset_compare.png',sep = ''),height=800,width=800,res=160)
  par(mar=c(4,5,1,1)+.1)
  plot(Test_Prediction_matrix[,c(1,2)]
       ,col= Test_Prediction_matrix$MAD*100
       ,pch = 3, lwd = 4,xlab="Test set correlations",ylab="Test set RMSE"
       ,cex.axis=2,cex.lab=2,font.axis=1,font.lab=2
  )
  box(which = "plot",lwd=5)
  abline(h = min(Test_Prediction_matrix$RMSE)
         ,v = max(Test_Prediction_matrix$Correlation) 
         ,col='red')
  dev.off()
  
}



#source('Formula_Generation/Find_Best_GAM.R')

#write.csv(top_3, file = output_file, row.names = FALSE)




