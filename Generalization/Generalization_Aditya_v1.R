source('../Load_Imports.R')
setwd("D:/Work/grb-webapp-main/GRB_RSE_webapp/Generalization")
run_locally = T  #Set to true for debugging.

if(run_locally){
  raw_xray_data <- read.csv("../combined_data_with_redshift.csv", header = T, row.names = 1)
  do_mice = T 
  do_m_estimator = T
  custom_models = F
  weight_threshold = 0.5
  loop = 100
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


cor_pval <- function(data, mapping, method = "pearson") {
  corr <- cor.test(data[, mapping$x], data[, mapping$y], method = method)
  return(corr$p.value)
}
printVar = function(x,y){
  # vals = cor.test(x,y,
  #                 method="pearson")[c("estimate","p.value")]

  vals = c(cor.test(x,y,
                  method="pearson")[c("estimate")]
           ,ks.test(x,y)[c('p.value')]
  )
  
  
  names(vals) = c("r=","p=")
  vals$`p=`=formatC(vals$`p=`,format = 'e',digits = 2)
  vals$`r=`=formatC(vals$`r=`,format = 'e',digits = 2)
  print(vals)
  paste(names(vals),unlist(vals),collapse="\n")
}
my_fn <- function(data, mapping, ...){
  # takes in x and y for each panel
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  mainCor = printVar(xData,yData)
  p <- ggplot(data = data, mapping = mapping) +
    annotate(x=0.5,y=0.5,label=mainCor,geom="text",size=4) +
    theme_void() + ylim(c(0,1))
  p
}


add_error <- function(measurement, error) {
  # GENERATE 100 SAMPLES FROM A NORMAL DISTRIBUTION
  # THIS NORMAL DISTRIBUTION IS CENTERED ON THE measurement
  # AND HAS A SD OF error/2
  # ONE VALUE IS PICKED FROM THE RANDOM SAMPLE GENERATED
  
  new_measurement <- rep(0, length(measurement))
  print(error[1])
  for (i in 1:length(error)){
    new_measurement[i] <- sample(rnorm(100, measurement[i], abs(error[i] / 2)), 1)
  }
  
  return(round(new_measurement, 4))
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
PLOTaddr <-paste("Plot_Output_3gam3glmEarth/") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

if (!dir.exists(PLOTaddr)) {
  dir.create(PLOTaddr)
}

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
  
  png(filename = "MICE_missing_features.png",width = 1000, height = 1000, res = 200)
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

source('../lasso.R')
lassovar=head(lassovar,7)

# generating squared terms for future ML methods
Variables <- subset(GRBPred, select = lassovar)#colnames(features_for_mice_preds))
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
  source("../m_estimator.R")
}
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

#write.csv(TrainingData,paste(addr,'TrainingData_for_the_run.csv',sep=''))

Response <- TrainingData$log10z
Predictors <- subset(TrainingData
                     ,select = -c(log10z, Redshift_crosscheck)) # EXCLUDING LOG10Z, INVZ AND Z

source('../Custom_SL/sl_mgcv_gam.R')
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

source('../Custom_SL/sl_custom_glm.R')

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
plotnames<- "_3GAM&GLM,earth_100times"

###### CARET #####

tune_caret = list(
  method = c("gcvEarth")
  ,tuneLength=1,verboseIter=F
)

caret_learner <- create.Learner('SL.caret',
                                tune = tune_caret,detailed_names = T)

# ##### SLOPE ######
# 
# source("Custom_SL/sl_SLOPE.R")
# 
# SLOPE_learner = create.Learner('SL.SLOPE',detailed_names = T)
# 
# SLOPE_Superlearner = SuperLearner(Y = Response
#                                   ,X = Predictors
#                                   ,newX = PredictionData[,c(1:14)]
#                                   ,SL.library = SLOPE_learner$names
#                                   ,family = gaussian()
#                                   ,verbose = T)
# 
# Slope_model=SLOPE(x = Predictors,y = Response,family = 'gaussian')
# predict(Slope_model,PredictionData[,c(1:14)])
# 
# plot(Slope_model)



libs=c(learner1$names, sl_glm1$names,caret_learner$names)

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
#print(loop)
#print("Starting 10fCV")

TrainData <- TrainingData

Algo_coeff <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_coeff) <- libs

print(Algo_coeff)

Algo_risk <- as.data.frame(matrix(nrow = 1,ncol = length(libs)))
colnames(Algo_risk) <- libs

all_lasso_vars <- character()


sl_model=SuperLearner(Y = Response, X = Predictors,family = gaussian(), SL.library = libs,verbose = F)
saveRDS(sl_model, file = "superlearner_model")

######## READING IN THE GENERALIZATION DATA ###########

if(run_locally){
  gen_dat <- read.csv("TOTAL_GENERALIZATION_DATA_v3.csv", header = TRUE, row.names = 1)
  do_mice = T 
  gen_dat$photon_index <- as.numeric(gsub("[^0-9.]", "", gen_dat$photon_index))
  gen_dat$logPeakFlux = as.numeric(gen_dat$logPeakFlux)
  gen_dat$errorphotonindex = as.numeric(gsub("[^0-9.]", "", gen_dat$errorphotonindex))
  
} else {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  output_file <- args[2]
  do_mice <- as.logical(tolower(args[3]) == "true")
  gen_dat <- read.table(input_file, header = TRUE, row.names = 1)
}
summary(gen_dat)

gen_dat$log10FaErr <- (gen_dat$F_max - gen_dat$F_min)/2
gen_dat$log10TaErr <- (gen_dat$T_amax - gen_dat$T_amin)/2
gen_dat$log10T90Err <- gen_dat$T90_err/(10^(gen_dat$logT90)*log(10))

gen_dat <- subset(gen_dat,
                  select = !colnames(gen_dat) %in% c("F_max",
                                                     "F_min",
                                                     "T_amin",
                                                     "T_amax",
                                                     "T90_err"))

gen_dat_preds <- subset(gen_dat,
                        select = !colnames(gen_dat) %in% c("log10TaErr",
                                                           "log10FaErr",
                                                           "errorlogPeakFlux",
                                                           "errorphotonindex",
                                                           "log10T90Err"))

gen_dat_errs <- subset(gen_dat, select = !colnames(gen_dat) %in% c(colnames(gen_dat_preds), "GRBid"))

colnames(gen_dat_preds) = c('log10Fa','log10Ta','log10T90','log10PeakFlux','PhotonIndex','log10NH','Gamma')

gen_dat_preds$log10NH[gen_dat_preds$log10NH < 20]=NA
gen_dat_preds$PhotonIndex[gen_dat_preds$PhotonIndex < 0]=NA
gen_dat_preds$Gamma[gen_dat_preds$Gamma>3]=NA

png(filename = "GeneralizationSet_MICE_missing_features.png",width = 1000, height = 1000, res = 200)
md.pattern(gen_dat_preds,rotate.names = T)
dev.off()

mice_model_genset <- mice(data = gen_dat_preds,
                         m = 20,
                         method = 'midastouch',
                         printFlag = F)
gen_dat_preds <- complete(mice_model_genset,20)

mice_model_genset <- mice(data = gen_dat_errs,
                          m = 20,
                          method = 'midastouch',
                          printFlag = F)
gen_dat_errs <- complete(mice_model_genset,20)


#gen_dat_preds <- gen_dat_preds[gen_dat_preds$logNH > 20,]

#gen_dat_preds <- gen_dat_preds[gen_dat_preds$photon_index > 0,]


{
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10NH > min(Predictors$log10NH),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10NH < max(Predictors$log10NH),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$PhotonIndex > min(Predictors$PhotonIndex),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$PhotonIndex < max(Predictors$PhotonIndex),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10Fa > min(Predictors$log10Fa),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10Fa < max(Predictors$log10Fa),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10Ta > min(Predictors$log10Ta),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10Ta < max(Predictors$log10Ta),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10PeakFlux > min(Predictors$log10PeakFlux),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10PeakFlux < max(Predictors$log10PeakFlux),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10T90 > min(Predictors$log10T90),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$log10T90 < max(Predictors$log10T90),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$Gamma > min(Predictors$Gamma),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$Gamma < max(Predictors$Gamma),]
  
}

nrow(gen_dat_preds)
colnames(gen_dat_preds)

#gen_dat_preds$logPeakFlux <- as.numeric(gen_dat_preds$logPeakFlux)

gen_dat_preds <- na.omit(gen_dat_preds)

dim(gen_dat_preds)

#gen_dat_preds$photon_index <- as.numeric(gsub("[^0-9.]", "", gen_dat_preds$photon_index))

# very specific weird data
#gen_dat_errs$errorphotonindex[c(20,30)] <- "0.2"
#gen_dat_errs$log10T90Err[108] <- 0

#gen_dat_errs$errorphotonindex <- as.numeric(gsub("[^0-9.]", "", gen_dat_errs$errorphotonindex))

gen_dat_preds <- SqrTermGen(gen_dat_preds)

gen_dat_errs <- gen_dat_errs[rownames(gen_dat_errs) %in% rownames(gen_dat_preds),]

gen_dat <- cbind(gen_dat_preds, gen_dat_errs)

# gen_dat <- gen_dat[-c(which(rownames(gen_dat) == "GRB150817A")),]

# gen_dat


gen_err <- subset(gen_dat,
                  select = c(
                    "log10TaErr",
                    "log10FaErr",
                    "errorphotonindex",
                    "errorlogPeakFlux",
                    "log10T90Err"
                  )
)
#gen_err$errorphotonindex
#colnames(gen_err)
#dim(gen_dat_preds)
#colnames(gen_dat_preds)

gen_predictors <- subset(gen_dat, select = !colnames(gen_dat) %in% colnames(gen_err))

error_matrix <- gen_err[rownames(gen_err) %in% rownames(gen_predictors), ]

dim(error_matrix)

# Predictors_w_errors <- gen_err

loop = 10


group<-NA
group[row.names(gen_predictors)]<-1#'Generalization set GRBs'
group[row.names(Predictors)]<-2#'Training set GRBs'
group=na.omit(group)
Totdata = rbind(gen_predictors,Predictors)

#Totdata[row.names(Totdata)=="091127A",]
#Totdata_2=Totdata[row.names(Totdata)!="091127A",]

#group = group[rownames(Totdata)]
{
  png(file = paste("Genset+Trainingset",".png",sep = ""),width = 1500,height = 1500,res=120)
  pairs(Totdata[,c(1:7)],
        #pairs(as.matrix(Totdata[,c(4,6:16)]),
        
        #pairs(as.matrix(fullDatMat_WO[,c('InvRedshift','Frac_Variability','Gaia_G_Magnitude')]),
        horOdd = T ,
        #pch=3,
        col=c('red','black')[group],
        cex=0.5,
        cex.labels=1.4,
        #cex.angle=45,
        main=paste('Scatter Plot of',dim(Totdata)[1],' samples')
        #lower.panel = as.matrix(lowredshift[,3:6])
  )
  dev.off()
  
  
  library(ggplot2)
  library(GGally)
  library(ggpubr)
  
  png(file = paste("Genset+Trainingset_good.png",sep = ""),width = 2000,height = 2000,res=125)
  ggpairs(Totdata[,c(1:7)]
          ,aes(color=factor(group))
          ,axisLabels = c("show")
          ,columnLabels = c('log(Fa)','log(Ta)','log(T90)','log(Peak Flux)','Photon index','log(NH)','γ')
          ,upper=list(continuous=my_fn)# GGally::wrap("cor", method="pearson", stars=FALSE, size=4,col='blue'))
          ,diag = list(continuous = wrap("barDiag",bins=10,fill='red',col='black')),
          
  )+theme_bw()+theme(panel.background = element_rect(colour = 'white')
                     ,panel.grid = element_blank()
                     ,axis.text = element_text(colour = 'black')
                     ,strip.text=ggplot2::element_text(size=11,face="bold")
  )
  dev.off()
  
 namelist =   c("log10Fa"='log(Fa)'
                ,"log10Ta"='log(Ta)'
                ,"log10T90"='log(T90)'
                ,"log10PeakFlux"='log(Peak Flux)'
                ,"PhotonIndex"='Photon index'
                ,"log10NH"='log(NH)'
                ,"Gamma"='γ')
  
  {png(paste0("Distributions.png"),width = 3000,height = 3500,res = 350)
    par(mfrow=c(3,3),lwd=2)
    for(col in colnames(gen_predictors)[1:7]){
      #print(col)
      brk=hist(TrainingData[,col],plot = F)
      brk2=hist(gen_predictors[,col]
                ,breaks = brk$breaks,plot = F)
      
      hist(TrainingData[,col],ylim=c(0,max(brk2$counts))
           ,main=paste0(namelist[col],' (KS p-value=',round(ks.test(TrainingData[,col],gen_predictors[,col])$p.value,2),')')
           ,xlab = colnames(TrainingData)[col]
           ,freq = T
           ,breaks = brk$breaks
           ,col = rgb(1,0,0,0))
      hist(gen_predictors[,col],add=T
           ,freq = T
           ,breaks = brk$breaks
           ,col = rgb(1,1,1,0.5),lty=3)
    }
    dev.off()}
  
}


##### START OF PREDICTION #####

gen_pred_list <- foreach(
  j = 1:loop, .packages = c("SuperLearner", "caret"),
  .export = c(libs),
  .combine = cbind
) %dopar% {
  
  source("../Custom_SL/sl_mgcv_gam.R")
  source("../Custom_SL/sl_custom_glm.R")
  
  n = 100
  prediction_vector <- matrix(ncol = n, nrow = nrow(gen_predictors))
  
  # train_data <- subset(Predictors, select = !colnames(Predictors) %in% c("log10FaErr", 
  #                                                                        "log10TaErr",
  #                                                                        "log10PeakFluxErr",
  #                                                                        "log10T90Err",
  #                                                                        "PhotonIndexErr"))
  # 
  # train_data_errors <- Predictors_w_errors
  # 
  # train_response <- Response
  # 
  # SL <- model <- SuperLearner(
  #   Y = train_response, X = train_data, # USING LABELS AS FACTORS FOR PREDICTION
  #   family = gaussian(),
  #   SL.library = libs, verbose = F
  # )
  # 
  base_Fa <- gen_predictors$log10Fa
  base_Ta <- gen_predictors$log10Ta
  base_Pi <- gen_predictors$PhotonIndex
  base_Pf <- gen_predictors$log10PeakFlux
  base_T90 <- gen_predictors$log10T90
  
  for (k in 1:n) {
    # HERE THE VALUES OF 4 PREDICTORS ARE CHANGED BASED ON THEIR
    # MEASUREMENT ERROR FROM SWIFT
    print(base_Fa)
    gen_predictors$log10Fa <- add_error(base_Fa, gen_err$log10FaErr)
    gen_predictors$log10FaSqr <- gen_predictors$log10Fa^2
    
    gen_predictors$log10Ta <- add_error(base_Ta, gen_err$log10TaErr)
    gen_predictors$log10TaSqr <- gen_predictors$log10Ta^2
    
    gen_predictors$PhotonIndex <- add_error(base_Pi, gen_err$errorphotonindex)
    gen_predictors$PhotonIndexSqr <- gen_predictors$PhotonIndex^2
    
    gen_predictors$log10PeakFlux <- add_error(base_Pf, gen_err$errorlogPeakFlux)
    gen_predictors$log10PeakFluxSqr <- gen_predictors$log10PeakFlux^2
    
    gen_predictors$log10T90 <- add_error(base_T90, gen_err$log10T90Err)
    gen_predictors$log10T90Sqr <- gen_predictors$log10T90^2
    
    prediction_vector[, k] <- predict(sl_model, gen_predictors)$pred
  }
  
  return(prediction_vector)
}


gen_pred_dataframe = as.data.frame(gen_pred_list)

{
  prediction_sd <- apply(gen_pred_list, 1, sd)
  prediction_mean <- rowMeans(gen_pred_list)
  
  prediction_max <- apply(gen_pred_list, 1, max)
  prediction_min <- apply(gen_pred_list, 1, min)
  
  linear_prediction_list <- 10^(gen_pred_list) - 1
  linear_prediction_mean <- rowMeans(linear_prediction_list)
  linear_prediction_sd <- apply(linear_prediction_list, 1, sd)
  
  linear_prediction_max <- apply(linear_prediction_list, 1, max)
  linear_prediction_min <- apply(linear_prediction_list, 1, min)
}

which.min(prediction_min)



hist(10^Response-1,breaks = 30,ylim=c(0,45),col=rgb(0,0,0,0.5))
hist(linear_prediction_mean,add=T,breaks = 10,col=rgb(0,0,1,0.5))

ks.test(Responses$Redshift_crosscheck,linear_prediction_mean)


rownames(gen_pred_dataframe) = rownames(gen_dat)

grb_list = data.frame(Names=rownames(gen_dat),prediction = rowMeans(gen_pred_list))

#ordered_GRB_list <- gen_pred_list[order(rowMeans(gen_pred_list)), ]
ordered_GRB_list <- grb_list[order(grb_list$prediction), ]


{
  #png(filename = paste0(PLOTaddr,"boxplot.png"), width = 1600, height = 1000)
  png(filename = "boxplot.png", width = 1600, height = 1000)
  par(mar=c(10,6,2,2))
  boxplot(t(gen_pred_dataframe[order(rowMeans(gen_pred_dataframe)),]), names = rownames(gen_pred_dataframe)
          ,ylab = "",xlab = ""
          ,las = 2,pch = '.', axes = TRUE, frame = TRUE)
  mtext("GRB names", side=1, line=8, cex = 3)
  mtext("log(1+z)", side=2, line = 3, cex = 3)
  dev.off()
}


gen_new <- cbind(gen_dat,
                 linear_prediction_mean,
                 linear_prediction_mean - linear_prediction_sd,
                 linear_prediction_mean + linear_prediction_sd)

colnames(gen_new) <- c(colnames(gen_dat), "Zphot", "Zphot_min", "Zphot_max")

head(gen_new)

gen_new <- gen_new[order(gen_new$Zphot), ]

#write.csv(gen_new, "gen_dat_w_err_preds.csv")

# gen_results <- data.frame(GRB = NULL, Zphot = NULL)


gen_results <- data.frame(GRBID = grb_list$Names,
                          InvZphot = prediction_mean,
                          pred_max = prediction_max, 
                          pred_min = prediction_min)

gen_results$Zphot <- 10^(gen_results$InvZphot) - 1

gen_results$InvZphot

# Histograms
#png(filename = paste(PLOTaddr,'z_histogram.png',sep = ''),res=700,width=3000,height=3000)
png(filename = 'z_histogram.png',res=700,width=3000,height=3000)
hist(gen_results$Zphot, 
     main = paste("Distribution of Redshift (Linear)",sep = ''), 
     xlab = "z",
     ylab = "Frequency"
)
dev.off()


#png(filename = paste(PLOTaddr,'log10z_histogram.png',sep = ''),res=700,width=3000,height=3000)
png(filename = 'log10z_histogram.png',res=700,width=3000,height=3000)
hist(gen_results$InvZphot, 
     main = paste("Distribution of Redshift (Log)",sep = ''), 
     xlab = "log(z+1)", 
     ylab = "Frequency",
)
dev.off()



temp_dset = data.frame(gen_results$InvZphot)
temp_dset <- c(gen_results$InvZphot, gen_results$pred_max, gen_results$pred_min)

#OUTPUT_FILE
write.csv(gen_results, file = 'Generalization_results.csv', row.names = FALSE)

##### BIAS CORRECTION #####

load(file = "../Plot_Output_3gam3glmEarth_v2/Workspace.Rdata")

s1<-sort(results$InvZphot)
w2<-sort(results$InvZspec)

plot(w2~s1,xlab='Sorted observed log(z+1)',ylab='Sorted predicted log(z+1)')
op_lin_1 <- lm(w2 ~ s1)
abline(op_lin_1, col = 'red' , lw = 2)
legend('topleft',legend = 'Linear fit',lty=1,col='red',lwd=2)

gen_results$corrected_prediction = op_lin_1$coefficients[1] + gen_results$InvZphot * op_lin_1$coefficients[2]
gen_results$corrected_pred_max   = op_lin_1$coefficients[1] + gen_results$pred_max * op_lin_1$coefficients[2]
gen_results$corrected_pred_min   = op_lin_1$coefficients[1] + gen_results$pred_min * op_lin_1$coefficients[2]

source('Bias_Correction_function.R')
gen_results$corrected_prediction = BC_1way(crossvalidated_results = results,gen_results)
gen_results$corrected_prediction = BC_2way(crossvalidated_results = results,gen_results)
gen_results$corrected_prediction = BC_3way(crossvalidated_results = results,gen_results)

gen_results$Linear_corrected_prediction = 10^gen_results$corrected_prediction - 1
gen_results$Linear_corrected_pred_max   = 10^gen_results$corrected_pred_max - 1
gen_results$Linear_corrected_pred_min   = 10^gen_results$corrected_pred_min - 1





{png(filename = 'Generalization_set_log(redshift)_distributions.png',res=475,width=3500,height=3000)
par(lwd=3,mar=c(5,5,4,2))
hist(results$InvZspec,breaks = 15,ylim=c(0,100),col=rgb(1,1,1,0.5)
     ,main='Generalization set predictions vs observed log(z+1)'
     ,xlab='log(z+1)',ylab='Number of GRBs'
     ,cex.axis = 1.5,cex.lab=1.75
     ,font.axis=2,font.lab=2
     )
hist(gen_results$InvZphot,add=T,col=rgb(1,1,1,0.5),lty=3)
hist(gen_results$corrected_prediction,add=T,col=rgb(1,1,1,0.5),lty=1,border = 'red')
legend('topright'
       ,col = c('black','black','red')
       ,legend = c('Observed log(z+1)','Generalization set ','Bias corrected generalization set')
       ,lty=c(1,3,2),lwd=3,text.font = 2
       )
dev.off()}

{png(filename = 'Generalization_set_redshift_distributions.png',res=475,width=3500,height=3000)
  par(lwd=2,mar=c(5,5,4,2))
  
  hist(results$Zspec,breaks = 30,ylim=c(0,2),col=rgb(1,0,0,0.5)
       ,freq = F
       ,main='Generalization set predictions vs observed redshift'
       ,xlab='Redshift',ylab='Number of GRBs'
       ,cex.axis = 1.5,cex.lab=1.75
       ,font.axis=2,font.lab=2
  )
  hist(gen_results$Zphot,add=T,col=rgb(0,1,0,0.5),lty=1,breaks = 10
       ,freq = F)
  
  hist(gen_results$Linear_corrected_prediction,add=T,col=rgb(1,1,1,0.5),lty=3
       ,breaks = 20
       ,freq = F)
  
  legend('topright'
         ,col = c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,0,1))
         ,legend = c('Observed redshift','Generalization set ','Bias corrected generalization set')
         ,lty=c(1,1,3),lwd=3,text.font = 2
  )
  dev.off()}






#### BOXPLOT FOR GEN SET #####

gen_pred_dataframe_BC = apply(gen_pred_dataframe,2, function(x){op_lin_1$coefficients[1] + x * op_lin_1$coefficients[2]})

{
  #png(filename = paste0(PLOTaddr,"boxplot.png"), width = 1600, height = 1000)
  png(filename = "BC_boxplot.png", width = 1600, height = 1000)
  par(mar=c(10,6,2,2))
  boxplot(t(gen_pred_dataframe_BC[order(rowMeans(gen_pred_dataframe_BC)),]), names = rownames(gen_pred_dataframe_BC)
          ,ylab = "",xlab = ""
          ,las = 2,pch = '.', axes = TRUE, frame = TRUE)
  mtext("GRB names", side=1, line=8, cex = 3)
  mtext("log(1+z)", side=2, line = 3, cex = 3)
  dev.off()
}


gen_linpred_dataframe_BC = 10^gen_pred_dataframe_BC - 1
gen_linpred_dataframe = 10^gen_pred_dataframe - 1

{
  #png(filename = paste0(PLOTaddr,"boxplot.png"), width = 1600, height = 1000)
  png(filename = "Redshift_boxplot.png", width = 1600, height = 1000)
  par(mar=c(10,6,2,2))
  boxplot(t(gen_linpred_dataframe_BC[order(rowMeans(gen_linpred_dataframe_BC)),]), names = rownames(gen_linpred_dataframe_BC)
          ,ylab = "",xlab = "",ylim=c(-0.5,10)
          ,las = 2,pch = '.', axes = TRUE, frame = TRUE)
  
  boxplot(t(gen_linpred_dataframe[order(rowMeans(gen_linpred_dataframe)),]), names = rownames(gen_linpred_dataframe)
          ,ylab = "",xlab = "",ylim=c(-0.5,10),add=T,border='red'
          ,las = 2,pch = '.', axes = TRUE, frame = TRUE)
  
  mtext("GRB names", side=1, line=8, cex = 3)
  dev.off()
}




