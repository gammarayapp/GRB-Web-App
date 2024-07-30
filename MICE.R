source('Load_Imports.R')

run_locally = F  #Set to true for debugging.

if(run_locally){
  raw_xray_data <- read.csv("combined_data_with_redshift.csv", header = T, row.names = 1)
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

sz<-0.8
rez=120

addr <-paste("Results/") #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
PLOTaddr <-paste("Plot_Output/") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT

## CREATE DIRECTORIES IF THEY DONT EXIST
if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
if(!dir.exists(addr)){dir.create(addr)}

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

do_mice = T
# inf_names <- c(
#   "log10Fa" = "log(Fa)",
#   "log10Ta" = "log(Ta)",
#   "Alpha" = "α",         
#   "Beta"  = "β",         
#   "PhotonIndex" = "Photon Index",   
#   "log10NH" = "log(NH)",
#   "log10PeakFlux" = "log(Peak flux)",
#   "log10T90" = expression("log(T90)"),
#   "log10Fluence" = expression("log( Fluence )"),
#   "Gamma" = "γ",
#   "log10FaSqr"  = expression("log(Fa)"^2),     
#   "log10TaSqr" = expression("log(Ta)"^2),     
#   "AlphaSqr"  = expression("α"^2),     
#   "BetaSqr"   = expression("β"^2),    
#   "PhotonIndexSqr"=expression("Photon Index"^2),
#   "log10NHSqr"  = expression("log(NH)"^2),     
#   "log10PeakFluxSqr" = expression("log(Peak flux)"^2),
#   "log10T90Sqr" = expression("log(T90)"^2),
#   "GammaSqr" = expression("γ"^2) 
# )



mice_names=features_for_mice_preds
# colnames(mice_names)=inf_names[1:10]

mice_names <- mice_names %>%
  rename(
    `log(Fa)` = log10Fa,
    `log(Ta)` = log10Ta,
    `α` = Alpha,
    `β` = Beta,
    `Photon Index` = PhotonIndex,
    `log(NH)` = log10NH,
    `log(Peak flux)` = log10PeakFlux,
    `log(T90)` = log10T90,
    `log( Fluence )` = log10Fluence,
    `γ` = Gamma,
  )
if(do_mice){
  set.seed(1)
  png(filename = paste(PLOTaddr,'MICE_missing_features.png',sep = ''),width = 1000, height = 1000, res = 200)
  md.pattern(mice_names,rotate.names = T)
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
