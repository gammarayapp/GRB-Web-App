source('Load_Imports.R')

run_locally = F

if(run_locally){
  raw_xray_data <- read.csv("SORTED_FINAL_X-Ray_DATA.csv", header = TRUE, row.names = 1)
} else {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  output_file <- args[2]
  raw_xray_data <- read.csv(input_file, header = TRUE, row.names = 1)
}

do_mice = T

###USERS INPUTS THEIR OWN TRAINING SET INSTEAD OF THIS ONE
GRBPred <- read.csv(file = "OutputFiles/MEstimator/grb_xray_m_est.csv", header = TRUE, row.names = 1) 

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
PLOTaddr <-paste("Plot_Output") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

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

GRBPred <- SqrTermGen(Variables)
GRBPred <- cbind(GRBPred, Err)

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

PredictionData <- tail(GRBPred, n = 0.20 * nrow(GRBPred))

TrainingData <- GRBPred[!(rownames(GRBPred) %in% rownames(PredictionData)),]

Response <- TrainingData$log10z
Predictors <- subset(TrainingData
                     ,select = -c(log10z, Redshift_crosscheck)) # EXCLUDING LOG10Z, INVZ AND Z

source('Formula_Generation/GAM_formula_generator.R')
source('Formula_Generation/Find_Best_GAM.R')

write.csv(top_3, file = output_file, row.names = FALSE)