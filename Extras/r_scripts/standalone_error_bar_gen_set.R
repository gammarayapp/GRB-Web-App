### Starts with processing Generalization data, then Error Bar Code

library(mice)
library(dplyr)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

SqrTermGen <- function(inputData) {
  indVar <- colnames(inputData)
  for (i in 1:length(indVar)) { # Loop over all variables in indVar
    for (j in i:(length(indVar))) { # Loop over all variables at index i and greater than index i
      # If i and j correspond to the same varaible call the variable varSqr
      print(indVar[i])
      print(indVar[j])
      if (indVar[i] == indVar[j]) {
        inputData[[paste(indVar[i], "Sqr", sep = "")]] <- inputData[, indVar[i]] * inputData[, indVar[j]]
      } # else{
      # inputData[[paste(indVar[i],indVar[j],sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      # }
    }
  }
  return(inputData)
}


raw_xray_data <- read.csv("SORTED_FINAL_X-Ray_DATA.csv", header = T, row.names = 1)

##Should be replaced and read by the user
gen_dat <- read.table("TOTAL_GENERALIZATION_DATA.txt", header = TRUE, row.names = 1)
## DO USER INPUT HERE ##
#gen_dat <- read.table(input_file, header = TRUE, row.names = 1)

######



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

colnames(gen_dat_preds)

# making some cuts of possible outliers
gen_dat_preds <- gen_dat_preds[gen_dat_preds$logNH > 20,]

gen_dat_preds <- gen_dat_preds[gen_dat_preds$photon_index > 0,]

# subsetting redshift error
rs_data_preds <- subset(raw_xray_data, select = c("log10Ta",
                                                  "log10Fa",
                                                  "log10PeakFlux",
                                                  "PhotonIndex",
                                                  "log10NH",
                                                  "log10Fluence",
                                                  "T90"))

rs_data_preds <- na.omit(rs_data_preds)

rs_data_preds <- rs_data_preds[rs_data_preds$T90 > 2.0, ]

rs_data_preds$log10T90 <- log10(rs_data_preds$T90)

rs_data_preds <- rs_data_preds[rs_data_preds$log10NH > 20, ]

rs_data_preds <- rs_data_preds[rs_data_preds$PhotonIndex > 0, ]

rs_data_preds <- subset(rs_data_preds, select = !colnames(rs_data_preds) %in% c("T90"))

# creating group for plotting
rs_data_preds$group <- rep(2, nrow(rs_data_preds))

#This cuts 222 GRBs to 155
{
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logNH > min(rs_data_preds$log10NH),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logNH < max(rs_data_preds$log10NH),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$photon_index > min(rs_data_preds$PhotonIndex),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$photon_index < max(rs_data_preds$PhotonIndex),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$Fbest > min(rs_data_preds$log10Fa),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$Fbest < max(rs_data_preds$log10Fa),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$T_abest > min(rs_data_preds$log10Ta),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$T_abest < max(rs_data_preds$log10Ta),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logPeakFlux > min(rs_data_preds$log10PeakFlux),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logPeakFlux < max(rs_data_preds$log10PeakFlux),]
  
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logT90 > min(rs_data_preds$log10T90),]
  gen_dat_preds <- gen_dat_preds[gen_dat_preds$logT90 < max(rs_data_preds$log10T90),]
}

nrow(gen_dat_preds)
colnames(gen_dat_preds)

gen_dat_preds$logPeakFlux <- as.numeric(gen_dat_preds$logPeakFlux)

gen_dat_preds <- na.omit(gen_dat_preds)

dim(gen_dat_preds)

gen_dat_preds$photon_index <- as.numeric(gsub("[^0-9.]", "", gen_dat_preds$photon_index))

# very specific weird data
gen_dat_errs$errorphotonindex[c(20,30)] <- "0.2"
gen_dat_errs$log10T90Err[108] <- 0

gen_dat_errs$errorphotonindex <- as.numeric(gsub("[^0-9.]", "", gen_dat_errs$errorphotonindex))

gen_dat_preds <- SqrTermGen(gen_dat_preds)

gen_dat_errs <- gen_dat_errs[rownames(gen_dat_errs) %in% rownames(gen_dat_preds),]

gen_dat_preds$log10T90 > 0.3

#write.csv(cbind(gen_dat_preds,gen_dat_errs), paste0("x_ray_unknown_set.csv"))



### ERROR BAR CODE

library(ggplot2)

Predictors_w_errors = read.csv("predictors_w_errors.csv")

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

source('Load_Imports.R')

getwd()
#setwd("D:/Desktop/Folder/....)

do_scale <- F
do_mice <- T
z_e <- 1

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

metrics <- function(observed, predicted, linear, print = TRUE) {
  if (linear) {
    dev = (observed-predicted)/(1+observed)
    
  } else {
    dev = observed-predicted
  }
  bias = sum(dev)/length(observed)
  MAD = median(abs(dev))
  NMAD = 1.48*MAD
  rms = sqrt(sum((dev)^2)/length(observed))
  if (print == TRUE){
    print(noquote(paste("Bias:", signif(bias,3))))
    print(noquote(paste("MAD:", signif(MAD,3))))
    print(noquote(paste("NMAD:", signif(NMAD,3))))
    print(noquote(paste("RMSE:", signif(rms,3))))
  }
  return(c(bias, MAD, NMAD, rms))
}

# if(do_mice){
#   addr <-paste("Results_w_MICE_",Sys.Date(),"/Files/",sep='') #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
#   PLOTaddr <-paste("Results_w_MICE_",Sys.Date(),'/',sep='') #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT
# }else{
#   addr <-paste("Results_",Sys.Date(),"/Files/",sep='') #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
#   PLOTaddr <-paste("Results_",Sys.Daten (),'/',sep='') #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT
# }

sz<-0.8
rez=120

# # CREATE DIRECTORIES IF THEY DONT EXIST
# if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
# if(!dir.exists(addr)){dir.create(addr)}

if(do_mice){
  GRBPred <- read.csv(file = "OutputFiles/MEstimator/grb_xray_m_est.csv", header = TRUE, row.names = 1) 
}else{
  GRBPred <- read.csv(file = "OutputFiles/DataHandle/grb_xray.csv", header = TRUE,row.names = 1) 
  #GRBPred <- read.csv(file = "OutputFiles/DataHandle/grb_xray_imputed.csv", header = TRUE,row.names = 1) 
}

colnames(GRBPred)

# checking for NA values, if using MICE, should have none
md.pattern(GRBPred, rotate.names = T)

# if there are any NA values, remove them
GRBPred <- na.omit(GRBPred)

####   Units   ####

#GRBRaw$Redshift_crosscheck  = linear redshift
#GRBRaw$T90                  = seconds
#GRBRaw$Type                 = type of GRB
#GRBRaw$log10Fa              = log flux: ergs/cm^2/seconds
#GRBRaw$log10Ta              = seconds
#GRBRaw$Alpha                = no units (power law coefficient)
#GRBRaw$Beta.                = no units (power law coefficient)
#GRBRaw$Fluence              = BAT Fluence (15-150 keV) [10^-7 erg/cm^2]
#GRBRaw$PhotonIndex          = BAT Photon Index (15-150 keV) (PL = simple power-law, CPL = cutoff power-law)
#GRBRaw$log10NH              = log XRT Column Density (NH) [10^21 cm^-2]

#   End of Units   #

# Histograms
#png(filename = paste(PLOTaddr,'z_histogram.png',sep = ''),res=700,width=3000,height=3000)
png(filename = 'z_histogram.png',res=700,width=3000,height=3000)
hist(GRBPred$Redshift_crosscheck, 
     main = paste("Distribution of Redshift (Linear)",sep = ''), 
     xlab = "z",
     ylab = "Frequency"
)
dev.off()


#png(filename = paste(PLOTaddr,'log10z_histogram.png',sep = ''),res=700,width=3000,height=3000)
png(filename = 'log10z_histogram.png',res=700,width=3000,height=3000)
hist(GRBPred$log10z, 
     main = paste("Distribution of Redshift (Log)",sep = ''), 
     xlab = "z", 
     ylab = "Frequency",
)
dev.off()

Totdata <- subset(GRBPred, 
                  select = c(log10T90,
                             log10Fa,
                             log10Ta,
                             Alpha,
                             Beta,
                             Gamma,
                             log10Fluence,
                             PhotonIndex,
                             log10NH,
                             log10PeakFlux,
                             Redshift_crosscheck,
                             log10z))


#### LASSO ####
LASSO <- function(X,Y)
{
  X<-as.matrix(X) # THE TRAINING DATA
  Y<-as.vector(Y) # THE RESPONSE VECTOR
  lasso_model<-cv.glmnet(X,Y,alpha=1) # LASSO REGRESSION
  return(lasso_model)
}

### Generating Squared Parameters ###
SqrTermGen <- function(inputData){
  indVar = colnames(inputData)
  for (i in 1:length(indVar)){ #Loop over all variables in indVar
    for (j in i:(length(indVar))){ #Loop over all variables at index i and greater than index i
      #If i and j correspond to the same varaible call the variable varSqr
      if (indVar[i] == indVar[j]){
        inputData[[paste(indVar[i],"Sqr",sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      }#else{
      # inputData[[paste(indVar[i],indVar[j],sep="")]] <- inputData[,indVar[i]]*inputData[,indVar[j]]
      #}
    }
  }
  return(inputData)
}

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
                                         PhotonIndexSqr,
                                         log10FaErr,
                                         log10TaErr,
                                         log10PeakFluxErr,
                                         log10T90Err,
                                         PhotonIndexErr))
# EXCLUDING LOG10Z, INVZ, Z,
# Alpha, Beta, Gamma, and Fluence

if (do_scale) {
  Predictors <- scale(Predictors)
  Predictors <- data.frame(Predictors)
}

GRBPred <- cbind(Predictors, Responses)

PredictionData <- tail(GRBPred, n = 0.20 * nrow(GRBPred))
dim(PredictionData)

TrainingData <- GRBPred[!(rownames(GRBPred) %in% rownames(PredictionData)),]
intersect(rownames(PredictionData),rownames(TrainingData))

#write.csv(TrainingData,paste(addr,'TrainingData_for_the_run.csv',sep=''))

Response <- TrainingData$log10z
Predictors <- subset(TrainingData
                     ,select = -c(log10z, Redshift_crosscheck)) # EXCLUDING LOG10Z, INVZ AND Z

# source("LASSO_feature_selection.R")

# lasso <- LASSO(Predictors, Response)
# coefficients <- coef(lasso)

ValidationSet_Response <- PredictionData$log10z
ValidationSet <- subset(PredictionData
                        ,select = -c(Redshift_crosscheck,log10z)) # EXCLUDING LOG10Z, INVZ AND Z


source('Custom_SL/sl_mgcv_gam.R')


bestGAM1 <- Response ~ s(log10NH) + s(log10T90) + s(log10Ta) + log10Fa + PhotonIndex + log10PeakFlux
tuner = list(gam.model = c(bestGAM1),
             select = TRUE,
             drop.intercept = TRUE
)

learner1 = create.Learner("SL.mgcv_gam", tune = tuner, detailed_names = F, name_prefix = "gam",verbose = T)




######## CUSTOM GLM CODE #####
best_lm1 <- log10z ~ (log10NHSqr + log10T90Sqr + log10TaSqr + log10NH + PhotonIndex + log10T90 + log10Ta + log10Fa)^2 + log10PeakFlux + log10FaSqr + PhotonIndexSqr + log10PeakFluxSqr
best_lm2 <- log10z ~ (log10NHSqr + PhotonIndexSqr + log10T90Sqr + log10FaSqr + log10NH + log10T90 + log10Ta)^2 + log10Fa + PhotonIndex + log10PeakFlux + log10TaSqr + log10PeakFluxSqr
best_lm3 <- log10z ~ (log10NHSqr + log10T90Sqr + log10TaSqr + log10FaSqr + log10NH + PhotonIndex + log10T90 + log10Ta)^2 + log10Fa + log10PeakFlux + PhotonIndexSqr + log10PeakFluxSqr


source('Custom_SL/sl_custom_glm.R')
# source('Custom_SL/sl_custom_lm.R')

sl_glm1 <- create.Learner('SL.custom_glm',
                          tune = list(glm.model=c(best_lm3)))

###### CUSTOM BAYES GLM CODE ########
source('Custom_SL/sl_custom_bayesglm.R')



libs <- c(learner1$names, sl_glm1$names)
# libs <- c(learner2$names)
libnames <- "_OG_1GAM_1GLM_"

# FIRST BRING IN THE ERROR DATA
numCores <- detectCores()
clust <- makeCluster(numCores) # This line will take time
registerDoParallel(clust)


# head(Predictors)
# head(GRBRaw)
# 
# row.names(Predictors) %in% GRBRaw$Name



# Adding generalization set
## skipped the next two lines and replaced by reading the data fram directly as we've combined the files
# WE STILL USE THE NAME gen_dat, BUT THAT'S AFTER GENERALIZATION DATA HANDLE
#gen_dir <- paste0("x_ray_unknown_set.csv")
#gen_dat <- read.csv(gen_dir, header = T, row.names = 1)
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
gen_err$errorphotonindex

colnames(gen_err)
#dim(gen_dat_preds)
#colnames(gen_dat_preds)

gen_predictors <- subset(gen_dat, select = !colnames(gen_dat) %in% colnames(gen_err))

colnames(gen_predictors) <- c("log10Fa", "log10Ta", "log10T90", "log10PeakFlux", 
                              "PhotonIndex","log10NH", "log10FaSqr","log10TaSqr",
                              "log10T90Sqr", "log10PeakFluxSqr" , "PhotonIndexSqr", "log10NHSqr")


error_matrix <- gen_err[rownames(gen_err) %in% rownames(gen_predictors), ]

dim(error_matrix)

# Predictors_w_errors <- gen_err

loop = 10

gen_pred_list <- foreach(
  j = 1:loop, .packages = c("SuperLearner", "caret"),
  .export = c(libs),
  .combine = cbind
) %dopar% {

  source("Custom_SL/sl_mgcv_gam.R")
  source("Custom_SL/sl_custom_glm.R")

  n = 100
  prediction_vector <- matrix(ncol = n, nrow = nrow(gen_predictors))

  train_data <- subset(Predictors, select = !colnames(Predictors) %in% c("log10FaErr", 
                                                                        "log10TaErr",
                                                                        "log10PeakFluxErr",
                                                                        "log10T90Err",
                                                                        "PhotonIndexErr"))

  train_data_errors <- Predictors_w_errors

  train_response <- Response

  SL <- model <- SuperLearner(
        Y = train_response, X = train_data, # USING LABELS AS FACTORS FOR PREDICTION
        family = gaussian(),
        SL.library = libs, verbose = F
      )

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

    prediction_vector[, k] <- predict(SL, gen_predictors)$pred
  }

  return(prediction_vector)
}

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

rownames(gen_pred_list) <- rownames(gen_dat)

#write.csv(gen_pred_list, paste0(addr,"gen_err_preds_raw.csv"))

ordered_GRB_list <- gen_pred_list[order(rowMeans(gen_pred_list)), ]

{
  #png(filename = paste0(PLOTaddr,"boxplot.png"), width = 1600, height = 1000)
  png(filename = "boxplot.png", width = 1600, height = 1000)
  par(mar=c(10,6,2,2))
  boxplot(t(ordered_GRB_list)
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


gen_results <- data.frame(InvZphot = prediction_mean,
                          pred_max = prediction_max, 
                          pred_min = prediction_min)

gen_results$Zphot <- 10^(gen_results$InvZphot) - 1

gen_results$InvZphot

temp_dset = data.frame(gen_results$InvZphot)
temp_dset <- c(gen_results$InvZphot, gen_results$pred_max, gen_results$pred_min)

#OUTPUT_FILE
write.csv(gen_results, file = output_file, row.names = FALSE)

# boxplot(gen_results$InvZphot)
# dev.off()

# png("gen_set_preds_w_error.png")
# boxplot(temp_dset)
# dev.off()

