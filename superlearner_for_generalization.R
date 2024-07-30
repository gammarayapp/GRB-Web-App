#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source('Load_Imports.R')

run_locally = T  #Set to true for debugging.

if(run_locally){
  #raw_xray_data <- read.csv("combined_data_with_redshift_V3.csv", header = T, row.names = 1)
  raw_xray_data <- read.csv("combined_data_with_redshift_V8.csv", header = T, row.names = 1)
  do_mice = T 
  do_m_estimator = T
  custom_models = F
  weight_threshold = 0.65
  loop = 5
} else {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  do_mice <- as.logical(tolower(args[2]) == "true")
  do_m_estimator <- as.logical(tolower(args[3]) == "true")
  custom_models <- as.logical(tolower(args[4]) == "true")
  weight_threshold <- as.numeric(args[5])
  loop <- as.numeric(args[6])
  raw_xray_data <- read.csv(input_file, header = TRUE, row.names = 1)
}


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

addr <-paste("Results") #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
PLOTaddr <-paste("Plot_Output_New_6gam+4glm+rf_V8.1_7V/") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

if (!dir.exists(PLOTaddr)) {
  dir.create(PLOTaddr)
}

sz<-0.8
rez=120


## CREATE DIRECTORIES IF THEY DONT EXIST
#if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
#if(!dir.exists(addr)){dir.create(addr)}


# Pick out Long GRBs
dim(raw_xray_data)
raw_xray_data = raw_xray_data[raw_xray_data$T90 > 2,]
dim(raw_xray_data)
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

colnames(raw_xray_data)

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
  
  png(filename = paste0(PLOTaddr,"MICE_missing_features.png"),width = 1000, height = 1000, res = 200)
  md.pattern(features_for_mice_preds,rotate.names = T)
  dev.off()
  
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
  #na.omit(features_for_mice_preds)
  #na.omit(features_for_mice_errs)
  #GRBPred <- rs_data_preds
  GRBPred <- na.omit(cbind(features_for_mice_preds, features_for_mice_errs))
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
lassovar=head(lassovar,7)

# generating squared terms for future ML methods
Variables <- subset(GRBPred, select = lassovar)#colnames(features_for_mice_preds))
Responses_and_Err <- subset(GRBPred, select = !colnames(GRBPred) %in% colnames(Variables))

GRBPred <- SqrTermGen(Variables)
GRBPred <- cbind(GRBPred, Responses_and_Err)
dim(GRBPred)
# adding z back into original dset
#GRBPred$Redshift_crosscheck <- raw_xray_data$Redshift_crosscheck
#GRBPred$Redshift_crosscheck <- raw_xray_data$Redshift_crosscheck[rownames(GRBPred)]
GRBPred$Redshift_crosscheck <- raw_xray_data[rownames(GRBPred),1]
# unscaled_GRBPred$Redshift_crosscheck <- GRBPred$Redshift_crosscheck

# adding log10z back into original dataset
#GRBPred$log10z <- log10(raw_xray_data$Redshift_crosscheck + 1)
GRBPred$log10z <- log10(GRBPred$Redshift_crosscheck + 1)
# unscaled_GRBPred$log10z <- GRBPred$log10z 

# GRBPred$invz <- 1/(raw_xray_data$Redshift_crosscheck + z_e)
# unscaled_GRBPred$invz <- GRBPred$invz

if (!dir.exists("OutputFiles")) {
  dir.create("OutputFiles")
}

# writing file to output directory
if (do_mice){
  write.csv(GRBPred, "OutputFiles/grb_xray_imputed.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
} else {
  write.csv(GRBPred, "OutputFiles/grb_xray.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
}

#source("Omar_upsampling.R")

if (do_m_estimator){
  source("m_estimator.R")
}
dim(GRBPred)
#WE'LL DECIDE IF WE NEED IT
##source("m_estimator.R")

Responses <- subset(GRBPred,select = c("Redshift_crosscheck", "log10z"))

# cutting all but the 6 best predictors
# Predictors <- subset(GRBPred, select = c(log10Fa,
#                                          log10Ta,
#                                          log10NH,
#                                          log10PeakFlux,
#                                          log10T90,
#                                          PhotonIndex,
#                                          log10FaSqr,
#                                          log10TaSqr,
#                                          log10NHSqr,
#                                          log10PeakFluxSqr,
#                                          log10T90Sqr,
#                                          PhotonIndexSqr
# ,
# log10FaErr,
# log10TaErr,
# log10PeakFluxErr,
# log10T90Err,
# PhotonIndexErr
##))
# EXCLUDING LOG10Z, INVZ, Z,
# Alpha, Beta, Gamma, and Fluence
O1Predictors = subset(GRBPred,select=lassovar)
O2Predictors = SqrTermGen(O1Predictors)

GRBPred <- cbind(O2Predictors, Responses)

PredictionData <- tail(GRBPred, n = 0.20 * nrow(GRBPred))
dim(PredictionData)

TrainingData <- GRBPred[!(rownames(GRBPred) %in% rownames(PredictionData)),]
intersect(rownames(PredictionData),rownames(TrainingData))
dim(PredictionData)
dim(TrainingData)
#write.csv(TrainingData,paste(addr,'TrainingData_for_the_run.csv',sep=''))

Response <- TrainingData$log10z
Predictors <- subset(TrainingData
                     ,select = -c(log10z, Redshift_crosscheck)) # EXCLUDING LOG10Z, INVZ AND Z

source('Custom_SL/sl_mgcv_gam.R')
# 
# bestGAM1 <- Response ~ s(log10NH) + s(log10T90) + s(log10Ta) + log10Fa + PhotonIndex + log10PeakFlux
# 
# tuner = list(gam.model = c(bestGAM1),
#              select = TRUE,
#              drop.intercept = TRUE
# )

#formula_table_GAM = read.table("Omar_best_formulas/Best_formula_GAM.txt")
formula_table_GAM = read.table("Best_formula_GAM.txt")
bestGAM1 = apply(as.matrix(formula_table_GAM[,2]),1,as.formula)
tuner = list(gam.model = c(bestGAM1),
             select = TRUE,
             drop.intercept = TRUE
)

learner1 = create.Learner("SL.mgcv_gam", tune = tuner, detailed_names = F, name_prefix = "gam",verbose = T)

#best_lm3 <- log10z ~ (log10NHSqr + log10T90Sqr + log10TaSqr + log10FaSqr + log10NH + PhotonIndex + log10T90 + log10Ta)^2 + log10Fa + log10PeakFlux + PhotonIndexSqr + log10PeakFluxSqr

source('Custom_SL/sl_custom_glm.R')

#formula_table_GLM = read.table("Omar_best_formulas/Best_formula_GLM.txt")
formula_table_GLM = read.table("Best_formula_GLM.txt")
best_lm3 = apply(as.matrix(formula_table_GLM[,2]),1,as.formula)

sl_glm1 <- create.Learner('SL.custom_glm',
                          tune = list(glm.model=c(best_lm3)))


# source('Custom_SL/sl_custom_bayesglm.R')
# formula_table_BGLM = read.table("7Variables/GLM_2024-01-10/Best_GLM_formula2024-01-10.txt")
# best_bglm3 = apply(as.matrix(formula_table_BGLM[,2]),1,as.formula)
# 
# sl_bglm1 <- create.Learner('SL.custom_bayesglm',
#                           tune = list(bglm.model=c(best_bglm3)))


if (custom_models){
  ###EDIT TO READ MODEL NAMES BASED ON USER SELECTION  
  libs_line <- readLines("selected_models.txt")
  eval(parse(text = libs_line))
  print(libs)
} else {
  libs <- c(learner1$names, sl_glm1$names,'SL.stepAIC')
  #libs <- c(sl_bglm1$names)
  libnames <- "_OG_3BGLM_"
}
#libnames <- "_OG_1GAM_1GLM_"
plotnames<- "_OG_3GAM_3GLM_100times"

###### CARET #####

tune_caret = list(
  #method = c("glmStepAIC",'xgbDART','gcvEarth','rf','ranger')
  method = c('rf')
  #method = c('gbm')
  ,tuneLength=1,verboseIter=F
)

caret_learner <- create.Learner('SL.caret',
                                tune = tune_caret,detailed_names = T)


libs=c(learner1$names, sl_glm1$names,caret_learner$names)
#libs=c(learner1$names,learner1$names,caret_learner$names)
#libs=c(learner1$names, sl_glm1$names)
#libs=c(sl_glm1$names)
print(libs)
#test_caret_SL = SuperLearner(Y = Response, X = Predictors,SL.library = libs,family = "gaussian",verbose = F)
#test_caret_SL = CV.SuperLearner(Y = Response, X = Predictors,SL.library = libs,family = "gaussian",verbose = F)


#### Super Learner Cross-validation and Plotting Results ####
test = F
balancing = F
analyze_all = F

####START OF RUN SUPERLEARNER
### Setup ###

numCores = detectCores()
clust <- makeCluster(numCores)
registerDoParallel(clust)
#source("Custom_SL/SL.custom_caret.R")
if(analyze_all){
  libs = c(#'SL.rpartPrune', 'SL.ridge', 'SL.lm','SL.glmnet', 'SL.glm.interaction','SL.glm',
    #'SL.cforest', 'SL.bayesglm', 'SL.biglasso', 
    #'SL.ksvm', #probably the line that errors out
    #'SL.caret', #takes too long
    'SL.caret.rpart', 'SL.earth', 'SL.ipredbagg',
    'SL.loess', 'SL.mean', 'SL.nnet',  'SL.randomForest', 'SL.ranger',
    'SL.rpart',  'SL.step', 'SL.step.forward',
    'SL.step.interaction', 'SL.stepAIC', 'SL.xgboost', 
    learner1$names, sl_glm1$names) # the 29 that work + GAM1
  libnames<- '_ALL_'
}



#plotnames<-paste(libnames,length(libs),"algo_",ncol(Predictors),"vrb_",loop,"times",sep = "")


print(libnames)
print(plotnames)
print(loop)
print("Starting 10fCV")

TrainData <- TrainingData

Algo_coeff <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_coeff) <- libs

print(Algo_coeff)

Algo_risk <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_risk) <- libs

all_lasso_vars <- character()

# print(
# system.time({
CVmodel<-foreach(j = 1:loop, .packages=c("SuperLearner", "caret" ,"xgboost", "randomForest", "gbm", "lattice", "latticeExtra", "Matrix", "glmnet", "biglasso","e1071",'earth','party'), 
                 .export = c(libs,'PLOTaddr')
)%dopar%{
  #print("Loop: ",i)
  source('Custom_SL/sl_mgcv_gam.R')
  source('Custom_SL/sl_custom_glm.R')
  source('Custom_SL/sl_custom_bayesglm.R')
  
  responses <- c('Redshift_crosscheck', 'invz', 'log10z')
  all_data_scale_wo <- TrainData
  
  all_data_scale_wo <- subset(all_data_scale_wo, 
                              select = !(colnames(TrainData) %in% responses)) # eliminates Z dependent cols
  PredictionData <- PredictionData[,colnames(all_data_scale_wo)]
  allZ_wo <- TrainData$Redshift_crosscheck
  invallZ_wo <- TrainData$log10z
  
  nwo = nrow(all_data_scale_wo)
  
  results_cv<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))
  results_cv_log<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))
  
  results_cv_mgcv<-data.frame(Predicted=numeric(nwo),Observed=numeric(nwo))
  
  ind<-sample(nwo,nwo)
  folds<-vector('list',10)
  foldlen <- floor(nwo/10)
  
  folds <- createFolds(allZ_wo, k = 10) # use caret to make k folds (this return the indices of the fold)
  
  CVpred <- matrix(0,nrow = dim(PredictionData)[1],ncol=0)
  #Valpred <- matrix(0,nrow = dim(ValidationSet)[1],ncol=0)
  # UKpred <- matrix(0,nrow = dim(UnknownSet)[1],ncol=0)
  
  correlation_log_test <- numeric()
  rmse_log_test <- numeric()
  sus_GRBs <- character()
  
  for(i in 1:length(folds)){
    
    train_set<-all_data_scale_wo[-folds[[i]],]
    test_set<-all_data_scale_wo[folds[[i]],]
    
    #
    Ztrain<-allZ_wo[-folds[[i]]]
    Ztest<-allZ_wo[folds[[i]]]
    
    
    invZtrain<-invallZ_wo[-folds[[i]]]
    invZtest<-invallZ_wo[folds[[i]]]
    
    train_set<-as.data.frame(train_set)
    test_set<-as.data.frame(test_set)
    
    capture.output(
      s9<-SuperLearner(Y = invZtrain, X = train_set, family = gaussian(), newX=test_set, SL.library = libs,verbose = F)
      ,file=nullfile())
    
    pr<- s9$SL.predict 
    
    
    
    
    Zpred <- 10^(pr[,1]) - 1
    
    results_cv$Predicted[folds[[i]]]<-pr[,1]
    results_cv$Observed[folds[[i]]]=invZtest
    
    logZpred<-pr[,1]
    logZtest<-invZtest
    test
    
    correlation_log_test = c(correlation_log_test, c(cor(logZpred,logZtest)))
    rmse_log_test = c(rmse_log_test, sqrt(mean((logZpred - logZtest)^2)))
    
    testsetGRB <- rownames(test_set)
    
    Algo_coeff <- rbind(Algo_coeff,coef(s9))
    
    Algo_risk <- rbind(Algo_risk,s9$cvRisk)
    
    CVpred <- cbind(predict(s9,PredictionData)$pred,CVpred) # POTENTIAL ERROR POINT
    #Valpred <- cbind(predict(s9,ValidationSet)$pred,Valpred) # POTENTIAL ERROR POINT
    gc()
  }
  
  Algo_coeff <- na.omit(Algo_coeff)
  Algo_risk <- na.omit(Algo_risk)
  return(list(rowMeans(CVpred),colMeans(Algo_coeff),results_cv$Predicted,colMeans(Algo_risk), correlation_log_test, sus_GRBs, results_cv_mgcv$Predicted, rmse_log_test))
}

# }))

stopCluster(clust)

doParallel::stopImplicitCluster()
closeAllConnections()

#save.image(file = paste("Workspace_10fCV",plotnames,".Rdata",sep = ""))

z_e = 1

#source('Plot_Results.R')
correl<-as.vector(0)
LinearCorrel<-as.vector(0)

# Initialize vectors
preds<-matrix(nrow = nrow(Predictors),ncol = loop) #JUST TO HOLD THE PREDICTION VALUES. LATER ITS AVERAGED OVER TO GET THE CV PLOT
# ukpreds<-matrix(nrow = nrow(UnknownSet),ncol = loop)

co<-matrix(nrow = loop,ncol = length(libs))
AlgoRisks<-matrix(nrow = loop,ncol = length(libs))

logBias<- as.vector(0)
logMAD<- as.vector(0)
logNMAD<- as.vector(0)
logrms<- as.vector(0)
linearBias<- as.vector(0)
linearMAD<- as.vector(0)
linearNMAD<- as.vector(0)
linearrms<- as.vector(0)

# Can change CVmodel[[j]][[3]] to CVmodel[[j]][[4]] (#*) to calculate metrics for ValidationSet
for (j in 1:loop) { # Iterate through the number of times SuperLearner was run to add each metric into a vector.
  
  preds[,j]<-CVmodel[[j]][[3]]
  
  # predicted values for unknown rs dataset
  # ukpreds[,j] <- CVmodel[[j]][[9]]
  
  # Validationpreds[,j]<-CVmodel[[j]][[4]] # THIS LINE COMMENTED OUT BECAUSE VALIDATION SET RESULTS ARE GENERATED IN DIFFERENT CODE
  co[j,]<-CVmodel[[j]][[2]]
  AlgoRisks[j,]<-CVmodel[[j]][[4]]
  
  correl[j]<-cor(preds[,j],Response) #*
  
  LinearCorrel[j]<-cor( 10^preds[,j] - z_e, TrainingData$Redshift_crosscheck  ) #*
  
  linearrms[j] = sqrt(mean((TrainingData$Redshift_crosscheck - (10^preds[,j]-1))^2))
  
  # CVmetrics <- metrics(Response, preds[,j], linear = FALSE, print = FALSE)
  # 
  # logBias[j]<-CVmetrics[1]
  # logMAD[j]<-CVmetrics[2]
  # logNMAD[j]<-CVmetrics[3]
  # logrms[j]<-CVmetrics[4]
  # CVLinearmetrics <- metrics(TrainingData$Redshift_crosscheck, 10^CVmodel[[j]][[3]] - z_e, linear = TRUE, print = FALSE) #*
  # 
  # linearBias[j]<- CVLinearmetrics[1]
  # linearMAD[j]<- CVLinearmetrics[2]
  # linearNMAD[j]<- CVLinearmetrics[3]
  # linearrms[j]<- CVLinearmetrics[4]
}

#barplot(colMeans(co), names.arg = libs, horiz = T, las=1,)
par(mar=c(5, 4, 4, 2))

### Plotting observed vs predicted

###### STATISTICAL PARAMETERS ####
result_plotter <- function(names,p, o,pred.max=0,pred.min=0,pred.sd = 0, linpred.sd = 0){
  
  results<-data.frame(InvZphot=round(p,4)
                      ,InvZspec=round(o,4)
                      ) # THIS IS NEW CODE. NEED TO BE IMPROVED
  
  rownames(results) <- names
  
  results$Dlogz <- (results$InvZspec - results$InvZphot)
  results$Zphot  <- 10^results$InvZphot - 1 #    (1/results$InvZphot)-1
  results$Zspec  <- 10^results$InvZspec - 1#  (1/results$InvZspec)-1
  results$Dz     <- (results$Zspec - results$Zphot) # DIFFERENCE BETWEEN OBS AND PRED Z
  results$normDz <- results$Dz/(1+results$Zspec) # NORMALIZED RESIDUALS
  results$SD <- pred.sd #apply(preds) # CALCULATE THE STANDARD DEVIATIONS
  results$z_SD <- linpred.sd #rowSds(z_preds) # CALCULATE THE STANDARD DEVIATION IN Z 
  results$pred_max <- pred.max
  results$pred_min <- pred.min
  
  results$linpred_max <- 10^(pred.max) - 1
  results$linpred_min <- 10^(pred.min) - 1
  
  write.csv(results,file = paste(addr,'10fCVResults',plotnames,'.csv',sep = ''))
  
  #### CALCULATING SIGMA IN LOG SCALE ###
  
  L_Sigma <- sd(results$Dlogz) # STANDARD DEVIATION IN LOGSCALE
  L_2Sigma <-2*L_Sigma
  
  normRMS<-sqrt(mean((results$normDz)^2))
  InvRMS<-sqrt(mean((results$Dlogz)^2))
  
  InvBias<-mean(results$Dlogz)
  Bias<-mean(results$Dz) #BIAS CALCULATED FROM DELTA Z 
  normBias<-mean(results$normDz) # NORMALIZED BIAS CALCULATED FROM NORMALIZED DELTA Z
  
  Sigma <- sqrt(sum((results$Dz-Bias)^2)/(nrow(results))) # SIGMA CALCULATED AS THE ROOT MEAN SQUARE OF DELTA Z - BIAS
  
  normSigma<-sqrt(mean((results$normDz-normBias)^2 )) # NORM SIGMA CALCULATED AS THE ROOT MEAN SQUARE OF NORM DELTA Z - NORM BIAS
  
  MAD<-median(abs(results$Dz)) 
  NMAD<-median(abs(results$normDz))
  
  CVboxplot<-boxplot(results$InvZphot-results$InvZspec) # BOXPLOT IN Log(z+1)
  ##### SIGMA CONE CALCULATIONS 
  belowUpperLine<-results[results$InvZphot-(1)*results$InvZspec < 2*L_Sigma,]
  aboveLowerLine<-results[results$InvZphot-(1)*results$InvZspec > -2*L_Sigma,]
  
  InsideTheCone<-results[intersect(rownames(belowUpperLine),rownames(aboveLowerLine)),]
  
  print(rownames(InsideTheCone))
  write.csv(InsideTheCone,file = paste(addr,'Results_wo_catout',plotnames,'.csv',sep = ''))
  
  sigma1_belowUpperLine<-results[results$InvZphot-(1)*results$InvZspec < L_Sigma,]
  sigma1_aboveLowerLine<-results[results$InvZphot-(1)*results$InvZspec > -L_Sigma,]
  
  sigma_1InsideTheCone<-results[intersect(rownames(sigma1_belowUpperLine),rownames(sigma1_aboveLowerLine)),]
  
  return(results)
}



{ # THIS PLOTS ALL THE GRBS CORRELATION PLOT
  #plotnames<-paste(libnames,length(libs),"algo_",ncol(Predictors),"vrb_",loop,"times",sep = "")
  plotnames<- "_OG_1GAM_1GLM_2algo_17vrb_20times"
  plotnames<-paste('_OG_',plotnames,sep='')
  results<-result_plotter(rownames(TrainingData),rowMeans(preds),Response
                          ,apply(preds,1,max),apply(preds,1,min)) # HERE THE MAX MIN PREDICTIONS ARE DETERMINED
}

InsideCone <- read.csv(paste(addr,'Results_wo_catout',plotnames,'.csv',sep = ''),row.names = 1)
rownames(InsideCone)


{ # THIS PRINTS THE CORRELATION PLOT FOR DATA INSIDE 2SIGMA
  #plotnames<-paste(libnames,length(libs),"algo_",ncol(Predictors),"vrb_",loop,"times",sep = "")
  plotnames<- "_OG_1GAM_1GLM_2algo_17vrb_20times"
  plotnames<-paste('_removing_CatOutl_',plotnames)
  Good_results <-  result_plotter(rownames(InsideCone),InsideCone$InvZphot,InsideCone$InvZspec
                                  ,InsideCone$pred_max,InsideCone$pred_min)
}

# addr=PLOTaddr
# source('GRB_3way_Split_Bias_correction_zspec.R')
# 
# #### FOR PLOTTING RMSE VS REDSHIFT ####
# 
# for (j in 1:loop) {
#   linearrms[j] = sqrt(mean((TrainingData$Redshift_crosscheck - (10^preds[,j]-1))^2))
# }
# 
# RMSE_vector=apply(10^preds-1, 1, function(x){sqrt(mean(x^2))}   )
#RMSE_vector=apply(10^preds-1, 1, sd  )

#png(filename = "RMSE_vs_z.png",width = 800,height = 800,res = 150)
#par(mar=c(4,5,1,1))
#plot(TrainingData$Redshift_crosscheck,RMSE_vector
 #    ,xlab='Redshift',ylab="RMSE"
#     ,cex.axis=2,cex.lab=1.75,font.axis=1,font.lab=2,pch=4
#)
#abline(h = mean(RMSE_vector)
#       ,col='red')
#dev.off()

save.image(file = paste0(PLOTaddr,"Workspace.Rdata"))

