source('Load_Imports.R')

run_locally = T  #Set to true for debugging.

if(run_locally){
  raw_xray_data <- read.csv("combined_data_with_redshift.csv", header = T, row.names = 1)
} else {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  raw_xray_data <- read.csv(input_file, header = TRUE, row.names = 1)
}
do_mice = T
do_m_estimator = T


#### function definition ####
XGBHyperParameter <- function(model,riks=T,tune,cv=T){
  #t=T
  #riks=T
  #print(slmodel)
  
    n_o_m <- length(tune$mtry) * length(tune$ntree) * length(tune$maxnodes)
  
  
  if (cv) {
    #cvrisk <- matrix(data = model,nrow = length(tune$ntrees),ncol = length(tune$max_depth),byrow = F)
    cvrisk <- matrix(data = model,nrow = length(tune$ntrees),ncol = length(tune$shrinkage),byrow = F)
    #cvrisk <- matrix(data = model,nrow = length(tune$max_depth),ncol = length(tune$shrinkage),byrow = F)
    mm<-'10fCV'
  }else{
    #cvrisk <- matrix(data = model$cvRisk,nrow = length(tune$ntrees),ncol = length(tune$max_depth),byrow = F)
    cvrisk <- matrix(data = model$cvRisk,nrow = length(tune$ntrees),ncol = length(tune$shrinkage),byrow = F)
    #cvrisk <- matrix(data = model$cvRisk,nrow = length(tune$max_depth),ncol = length(tune$shrinkage),byrow = F)
    mm<-'1SL'
  }
  
  rownames(cvrisk)<-tune$ntrees
  #rownames(cvrisk)<-tune$max_depth
  colnames(cvrisk)<-tune$shrinkage
  
  print(cvrisk)
  
  if(riks){
    #nm<-'XGB hyperparameters VS RMSE\n Shrinkage='
    nm<-paste0(model_name,' hyperparameters VS RMSE\n Maxnodes=')
    mm<-paste(mm,'_risk',sep = '')
    yl<-'RMSE'
    pos<-'topright'
  }
  else{
    #nm<-'XGB hyperparameters VS Correlation\n Shrinkage='
    nm<-paste0(model_name,' hyperparameters VS Correlation\n Maxnodes =')
    mm<-paste(mm,'_corr',sep = '')
    yl<-'Correlation'
    pos<-'bottomright'
  }
  
  png(filename = paste(model_name,'_HP_plot',mm,'_.png',sep = ''),width=3*200,height=2*200)
  #plot(rownames(cvrisk),cvrisk[,1],type = 'l',col='red',xlab = 'Max Depth',ylab = yl,
  plot(rownames(cvrisk),cvrisk[,1],type = 'l',col='red',xlab = 'Number of trees',ylab = yl,
       #main = paste(nm,tune$ntrees),
       main = paste(nm,tune$max_depth),
       ylim = c(min(cvrisk),max(cvrisk)))
  
  for (i in 1:ncol(cvrisk)) {
    lines(rownames(cvrisk),cvrisk[,i],col=i)
    points(rownames(cvrisk),cvrisk[,i],col=i,pch=16)
  }
  legend( #title = 'XGB depth',
    title = paste0(model_name,'shrinkage'),
    pos,
    inset = 0.1,
    legend = colnames(cvrisk),
    fill = c(1:ncol(cvrisk)),
    col = c(1:ncol(cvrisk)),
    # pch = 16
  )
  dev.off()
}

##### function definition for RF #####
RFHyperParameter <- function(model,riks=T,tune,cv=T){
  #t=T
  #riks=T
  #print(slmodel)
  
    n_o_m <- length(tune$mtry) * length(tune$ntree) * length(tune$maxnodes)
  
  
  if (cv) {
    cvrisk <- matrix(data = model,nrow = length(tune$ntree),ncol = length(tune$maxnodes),byrow = F)
    #cvrisk <- matrix(data = model,nrow = length(tune$mtry),ncol = length(tune$maxnodes),byrow = F)
    mm<-'10fCV'
  }else{
    cvrisk <- matrix(data = model$cvRisk,nrow = length(tune$ntree),ncol = length(tune$maxnodes),byrow = F)
    #cvrisk <- matrix(data = model$cvRisk,nrow = length(tune$mtry),ncol = length(tune$maxnodes),byrow = F)
    mm<-'1SL'
  }
  
  #rownames(cvrisk)<-tune$mtry
  rownames(cvrisk)<-tune$ntree
  colnames(cvrisk)<-tune$maxnodes
  
  print(cvrisk)
  
  if(riks){
    #nm<-'XGB hyperparameters VS RMSE\n Shrinkage='
    nm<-paste0(model_name,' hyperparameters VS RMSE\n mtry =')
    mm<-paste(mm,'_risk',sep = '')
    yl<-'RMSE'
    pos<-'topright'
  }
  else{
    #nm<-'XGB hyperparameters VS Correlation\n Shrinkage='
    nm<-paste0(model_name,' hyperparameters VS Correlation\n mtry =')
    mm<-paste(mm,'_corr',sep = '')
    yl<-'Correlation'
    pos<-'bottomright'
  }
  
  png(filename = paste(model_name,'_HP_plot',mm,'_.png',sep = ''),width=3*200,height=2*200)
  plot(rownames(cvrisk),cvrisk[,1],type = 'l',col='red',xlab = 'ntree value',ylab = yl,
       main = paste(nm,tune$mtry),
       #main = paste(nm,tune$ntree),
       ylim = c(min(cvrisk),max(cvrisk)))
  
  for (i in 1:ncol(cvrisk)) {
    lines(rownames(cvrisk),cvrisk[,i],col=i)
    points(rownames(cvrisk),cvrisk[,i],col=i,pch=16)
  }
  legend( 
    #title = 'XGB depth',
    title = paste0(model_name,' maxnodes'),
    pos,
    inset = 0.1,
    legend = colnames(cvrisk),
    fill = c(1:ncol(cvrisk)),
    col = c(1:ncol(cvrisk)),
    # pch = 16
  )
  dev.off()
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
PLOTaddr <-paste("Plot_Output") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

sz<-0.8
rez=120


## CREATE DIRECTORIES IF THEY DONT EXIST
#if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
#if(!dir.exists(addr)){dir.create(addr)}


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


# generating squared terms for future ML methods
Variables <- subset(GRBPred, select = colnames(features_for_mice_preds))
Responses_and_Err <- subset(GRBPred, select = !colnames(GRBPred) %in% colnames(Variables))

GRBPred <- SqrTermGen(Variables)
GRBPred <- cbind(GRBPred, Responses_and_Err)

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


# writing file to output directory
if (do_mice){
  write.csv(GRBPred, "OutputFiles/DataHandle/grb_xray_imputed.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
} else {
  write.csv(GRBPred, "OutputFiles/DataHandle/grb_xray.csv")
  # write.csv(GRB_Err, "OutputFiles/DataHandle/grb_xray_errors.csv")
}

if (do_m_estimator){
  source("m_estimator.R")
}
#WE'LL DECIDE IF WE NEED IT
##source("m_estimator.R")

Responses <- subset(GRBPred,select = c("Redshift_crosscheck", "log10z"))

# cutting all but the 6 best predictors
Predictors <- subset(GRBPred, select = c(log10Fa,
                                         log10Ta,
                                         log10NH,
                                         log10PeakFlux,
                                         log10T90,
                                         PhotonIndex,
                                         log10FaSqr,
                                         log10TaSqr,
                                         log10NHSqr,
                                         log10PeakFluxSqr,
                                         log10T90Sqr,
                                         PhotonIndexSqr
                                         # log10FaErr,
                                         # log10TaErr,
                                         # log10PeakFluxErr,
                                         # log10T90Err,
                                         # PhotonIndexErr
                                         ))
# EXCLUDING LOG10Z, INVZ, Z,
# Alpha, Beta, Gamma, and Fluence

GRBPred <- cbind(Predictors, Responses)

PredictionData <- tail(GRBPred, n = 0.20 * nrow(GRBPred))
dim(PredictionData)

TrainingData <- GRBPred[!(rownames(GRBPred) %in% rownames(PredictionData)),]
intersect(rownames(PredictionData),rownames(TrainingData))

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

# formula_table_GAM = read.table("Best_formula_GAM.txt")
# bestGAM1 = apply(as.matrix(formula_table_GAM[,2]),1,as.formula)
# tuner = list(gam.model = c(bestGAM1),
#              select = TRUE,
#              drop.intercept = TRUE
# )

#learner1 = create.Learner("SL.mgcv_gam", tune = tuner, detailed_names = F, name_prefix = "gam",verbose = T)

#best_lm3 <- log10z ~ (log10NHSqr + log10T90Sqr + log10TaSqr + log10FaSqr + log10NH + PhotonIndex + log10T90 + log10Ta)^2 + log10Fa + log10PeakFlux + PhotonIndexSqr + log10PeakFluxSqr

source('Custom_SL/sl_custom_glm.R')

# formula_table_GLM = read.table("Best_formula_GLM.txt")
# best_lm3 = apply(as.matrix(formula_table_GLM[,2]),1,as.formula)
# 
# sl_glm1 <- create.Learner('SL.custom_glm',
#                           tune = list(glm.model=c(best_lm3)))

source('Custom_SL/sl_custom_bayesglm.R')



# libs <- c(learner1$names, sl_glm1$names)
# libnames <- "_OG_1GAM_1GLM_"



############ LIBRARY SET UP ################

XGB=F
if(XGB){
  
tune = list(ntrees = c(2:8)*50,#800,900),#,500,600,700,800),
            max_depth = c(4),
            shrinkage = c((16:22)/1000),
            booster = "gbtree" #remove for xgb
            )


learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "gb")
model_name="GB"
#learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")
#model_name="XGB"
}else{
tune = list(mtry = c(2)
            , ntree = c(5:13)*50
            , maxnodes = c((1:5)*100)
)

learners = create.Learner('SL.randomForest', tune = tune, detailed_names = T,name_prefix = 'RF')
model_name="RF"
}

libs = SL.library = c(learners$names)

libnames<-libs

################# SINGLE SL RUN ##############

system.time({
    model <- SuperLearner(Y = Response, X = Predictors, # USING LABELS AS FACTORS FOR PREDICTION
                        family = gaussian(), 
                        SL.library = libs,verbose = F)
})

#####WORKING UNTIL HERE
if(XGB){
  XGBHyperParameter(model,T,tune,F)
}else{
  RFHyperParameter(model,T,tune,F)
}


coef(model)

print(model$cvRisk)

#model$cvRisk
barplot((model$cvRisk)-min(model$cvRisk),horiz = T,names.arg = libnames,xpd = F, las =2,
        main = paste('Minimum risk mode=',model$libraryNames[model$cvRisk==min(model$cvRisk)])
)
#abline(v=min((model$cvRisk)))

Preds<-model$SL.predict


#model <- CV.SuperLearner(Y = Response, X = Predictors,family = gaussian(),SL.library = libs,verbose = F)
#XGBHyperParameter(model,T,tune,T)

cor(model$library.predict,Response)
   
########### 10fCV SECTION ############

loop<-4
#plotnames<-paste("_",length(libs),"algo_",ncol(TrainData),"vrb_",loop,"times",sep = "")
plotnames<-paste0(model_name,"_optimization")

system.time({
  numCores = detectCores()
  registerDoParallel(numCores)
  CVmodel<-foreach(i = 1:loop, .packages=c("SuperLearner", "caret" ,"xgboost", "randomForest", "gbm", "lattice", "latticeExtra", "Matrix", "glmnet", "biglasso","e1071"), 
                   
                   .export = libs 
  )%dopar%{
    gc()
    CV<-CV.SuperLearner(Y = Response,X =Predictors,V = 10,family = gaussian(),SL.library = libs,control = list(saveFitLibrary=T)) 
    
     
    # THIS EXTRACTS THE CROSS VALIDATIONED RISK VALUE FOR THE ALGORITHMs
    
    # r<-CV$AllSL$`1`$cvRisk
    # r<-rbind(r,CV$AllSL$`2`$cvRisk)
    # r<-rbind(r,CV$AllSL$`3`$cvRisk)
    # r<-rbind(r,CV$AllSL$`4`$cvRisk)
    # r<-rbind(r,CV$AllSL$`5`$cvRisk)
    # r<-rbind(r,CV$AllSL$`6`$cvRisk)
    # r<-rbind(r,CV$AllSL$`7`$cvRisk)
    # r<-rbind(r,CV$AllSL$`8`$cvRisk)
    # r<-rbind(r,CV$AllSL$`9`$cvRisk)
    # r<-rbind(r,CV$AllSL$`10`$cvRisk)
    
    s<-summary(CV)$Table$Ave[c(-1,-2)]
    #cor(rowMeans(ValPred),ValidationSet_z[,2])
    
    xgbCorr<-cor(
      (10^(CV$library.predict)-1),((10^Response)-1)
    )
    
    #return(list(rowMeans(CVpred),colMeans(coef(CV)),CV$SL.predict,rowMeans(ValPred),s,xgbCorr)) 
    return(list(
      colMeans(coef(CV))
      ,CV$SL.predict
      ,s
      ,xgbCorr
      ))
    # 1st column = Mean prediction on the prediction data set
    # 2nd column = Coefficients of algorithms, averaged over each fold of the 10fCV
    # 3rd column = The 10fCV predictions
    # 4th column = Validation set prediction
    # 5th column = average risk estimate for the V folds
    
    #return(list(colMeans(coef(CV)),CV$SL.predict,rowMeans(ValPred)))
    
    #gc()
    
    
  }
  save.image(file = paste(addr,"Workspace_10fCV",plotnames,".Rdata",sep = ""))
  
})

##### DATA EXTRACTION ####

correl<-as.vector(0)
LinearCorrel<-as.vector(0)

#ValidationCorr<-as.vector(0) # HOLD THE CORRELATION VALUES OF THE VALIDATION SET

preds<-matrix(0,nrow = length(Response),ncol = loop) #JUST TO HOLD THE PREDICTION VALUES. LATER ITS AVERAGED OVER TO GET THE CV PLOT
#Validationpreds<-matrix(nrow = nrow(ValidationSet_z),ncol = loop)

co<-matrix(nrow = loop,ncol = length(libs))
AlgoRisks<-matrix(nrow = loop,ncol = length(libs))

xgbrisk<-matrix(nrow = length(libs),ncol = loop)
xgbcorr<-matrix(nrow = length(libs),ncol = loop)

for (j in 1:loop) {
  #CVmodel[[j]]$SL.predict
  preds[,j]<-CVmodel[[j]][[2]]
  #Validationpreds[,j]<-CVmodel[[j]][[4]]
  co[j,]<-CVmodel[[j]][[1]]
  
  #AlgoRisks[j,]<-CVmodel[[j]][[5]]
  
  xgbrisk[,j]<-CVmodel[[j]][[3]]
  xgbcorr[,j]<-CVmodel[[j]][[4]]
  
  #preds[,j]<-CVmodel[[j]][[2]]
  #Validationpreds[,j]<-CVmodel[[j]][[3]]
  #co[j,]<-CVmodel[[j]][[1]]#colMeans(coef(CVmodel[[j]]))
  print(co[j,])
  #invRedshiftVec_WO
  #cors<-c(cors,cor(CVmodel[[j]]$SL.predict,invRedshiftVec_WO))
  #preds
  #correl[j]<-cor(CVmodel[[j]][[3]],invRedshiftVec_WO)
  
  #LinearCorrel[j]<-cor((1/CVmodel[[j]][[3]])-1,RedshiftVec_WO)
  
  #ValidationCorr[j]<-cor(CVmodel[[j]][[4]],ValidationSet_z[,2]) # ASSIGNING THE CORRELATION VALUES OF VALIDATION SET
  
  #print(mean(correl))
  #print(mean(ValidationCorr))
  # barplot(as.numeric(colMeans(coef(CVmodel[[j]]))), horiz = T, las=2, names.arg=c("StepAIC","RF","BigL","SVM",'GBM','XGB','GAM','GLM'), main = paste("10 fold CV correlation=",cor(pr,invRedshiftVec_WO)), xpd = F)
  barplot(as.numeric(co[j,]), horiz = T, las=2, 
          #names.arg=c("SVM",'XGB','BigL','RF','GAM1'),#,'GAM2','GLM','StepAIC'),#
          names.arg=libs, 
          main = paste("Coefficients | 10 fold CV correlation=",signif(cor(preds[,j],Response),4)), 
          xpd = F)
  
}




# ASSIGN THE LIBRARY NAMES 
rownames(xgbrisk)=libnames
rownames(xgbcorr)=libnames


# PLOT RMSE VS MODEL

#plot(x = rownames(xgbrisk),y = rowMeans(xgbrisk),type='l')

if(XGB){
  XGBHyperParameter(rowMeans(xgbrisk),T,tune)
  XGBHyperParameter(rowMeans(xgbcorr),F,tune)
}else{
  RFHyperParameter(rowMeans(xgbrisk),T,tune)
  RFHyperParameter(rowMeans(xgbcorr),F,tune)
}



cor(rowMeans(preds),Response)

#print(mean(correl))
#print(mean(ValidationCorr))

AlgoCoef<-data.frame(Algo=libs,Coeffs=colMeans(co))
AlgoCoef<-AlgoCoef[AlgoCoef$Coeffs>0,]

#AlgoRisk<-data.frame(Algo=libs,Coeffs=colMeans(AlgoRisks))
#AlgoCoef<-AlgoCoef[AlgoCoef$Coeffs>0,]
#AlgoCoef<-AlgoCoef[AlgoCoef$Coeffs>mean(AlgoCoef$Coeffs),]

png(filename = paste(model_name,"AlgoWeight",plotnames,'.png',sep=''),width = 500,height = 900)
par(mar=c(5,12,5,5))
barplot(as.numeric(AlgoCoef$Coeffs), horiz = T, las=2, 
        names.arg=AlgoCoef$Algo,#libnames,#,'GAM2','GLM','StepAIC'), 
        main = paste("Coefficients plot | 10 fold CV correlation=", signif(cor(rowMeans(preds),Response),3)), 
        xpd = F)

dev.off()




