library(caret)
#GRBPred <- read.csv("grb_xray_imputed.csv", header = TRUE, row.names = 1)

#hist(GRBPred$Redshift_crosscheck)
# Class1 = sum(GRBPred$Redshift_crosscheck <= 2)  #Check number of GRBs
# Class2 = sum(GRBPred$Redshift_crosscheck > 2)
# 
set.seed(9560)
GRBPred$Class <- factor(ifelse(GRBPred$Redshift_crosscheck > 2.5, "Class2", "Class1"))

up_train <- upSample(x = GRBPred[, -ncol(GRBPred)],
                     y = GRBPred$Class)
up_train <- up_train[, -ncol(up_train)]
#write.csv(up_train, "up_sampled.csv")
#GRBPred = read.csv("up_sampled.csv", header = TRUE, row.names = 1)
GRBPred = up_train
#hist(GRBPred$Redshift_crosscheck)