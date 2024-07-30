correl<-as.vector(0)
LinearCorrel<-as.vector(0)

# Initialize vectors
preds<-matrix(nrow = nrow(Predictors),ncol = loop) #JUST TO HOLD THE PREDICTION VALUES. LATER ITS AVERAGED OVER TO GET THE CV PLOT

# logBias<- as.vector(0)
# logMAD<- as.vector(0)
# logNMAD<- as.vector(0)
# logrms<- as.vector(0)
# linearBias<- as.vector(0)
# linearMAD<- as.vector(0)
# linearNMAD<- as.vector(0)
# linearrms<- as.vector(0)

# Can change CVmodel[[j]][[3]] to CVmodel[[j]][[4]] (#*) to calculate metrics for ValidationSet
for (j in 1:loop) { # Iterate through the number of times SuperLearner was run to add each metric into a vector.
  preds[,j]<-CVmodel[[j]][[2]]
  correl[j]<-cor(preds[,j], logResponse)

  LinearCorrel[j]<-cor( 10^preds[,j] - 1, TrainingData$Redshift_crosscheck) #*
  # CVmetrics <- metrics(Response, preds[,j], linear = FALSE, print = FALSE)
  
  # logBias[j]<-CVmetrics[1]
  # logMAD[j]<-CVmetrics[2]
  # logNMAD[j]<-CVmetrics[3]
  # logrms[j]<-CVmetrics[4]
  # CVLinearmetrics <- metrics(TrainingData$Redshift_crosscheck, 10^CVmodel[[j]][[2]] - 1, linear = TRUE, print = FALSE) #*
  
  # linearBias[j]<- CVLinearmetrics[1]
  # linearMAD[j]<- CVLinearmetrics[2]
  # linearNMAD[j]<- CVLinearmetrics[3]
  # linearrms[j]<- CVLinearmetrics[4]
}

# # Print average metrics across all SuperLearner Iterations
# sink(paste(addr,'GAM_CV_results.csv',sep=''))
# print(paste("Log Correlation:", mean(correl)))
# # print(paste("Log Bias:", mean(logBias)))
# # print(paste("Log MAD:", mean(logMAD)))
# # print(paste("Log NMAD:", mean(logNMAD)))
# # print(paste("Log rms:", mean(logrms)))
# print(paste("Linear Correlation:", mean(LinearCorrel)))
# # print(paste("Linear Bias:", mean(linearBias)))
# # print(paste("Linear MAD:", mean(linearMAD)))
# # print(paste("Linear NMAD:", mean(linearNMAD)))
# # print(paste("Linear rms:", mean(linearrms)))
# sink()

# Create histograms
# png(filename = paste(PLOTaddr,plotnames,'GAM-trainCV-logCor-hist.png',sep = ''),res=400,width=3000,height=2000)
# hist(correl, main=mean(correl))
# dev.off()

# png(filename = paste(PLOTaddr,'GAM-trainCV-linearCor-hist.png',sep = ''),res=400,width=3000,height=2000)
# hist(LinearCorrel, main=mean(LinearCorrel))
# dev.off()

# png(filename = paste(PLOTaddr,'GAM-trainCV-logRms-hist.png',sep = ''),res=400,width=3000,height=2000)
# hist(logrms, main=mean(logrms))
# dev.off()

linear_preds <- 10^(preds) - 1
linear_Response <- 10^(Response) - 1
linear_rmse_distribution <- vector()

for (j in 1:loop) {
  linear_rmse_distribution[j] <- sqrt(mean((linear_preds[,j] - linear_Response)^2))
}
hist(linear_rmse_distribution)
# png(filename = paste(PLOTaddr,'GAM-trainCV-linearRMSE.png',sep = ''),res=400,width=3000,height=2000)
# hist(linear_rmse_distribution
#      ,main=paste('Mean=',round(mean(linear_rmse_distribution),3)
#                  ,'\nSD=',round(sd(linear_rmse_distribution),3))
# )
# dev.off()