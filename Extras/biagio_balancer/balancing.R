# Importing libraries and files (manually)

library(caret)
#library(unbalanced) # this is commented since the library is no longer available in CRAN, download files manually
# Download files manually from https://github.com/dalpozz/unbalanced/tree/master/R
# The needed files are ubBalance.R, ubSMOTE.R, ubSmoteExs.R
source("ubBalance.R")
source("ubSMOTE.R")
source("ubSmoteExs.R")

# Importing data to balance

GRBPred <- read.csv("grb_xray_imputed.csv", header = TRUE, row.names = 1)

# PART 1: using the upSample function

hist(GRBPred$Redshift_crosscheck)

GRBPred$Class <- factor(ifelse(GRBPred$Redshift_crosscheck > 2, "Class2", "Class1"))

up_train <- upSample(x = GRBPred[, -ncol(GRBPred)],
                     y = GRBPred$Class)

hist(up_train$Redshift_crosscheck)

# PART 2: using the ubBalance functions
# Remember to download the needed function files from the repository
# https://github.com/dalpozz/unbalanced/tree/master/R
# and then import them manually with the command source()
# The guide for this function is on: http://cran.nexr.com/web/packages/unbalanced/unbalanced.pdf

hist(GRBPred$Redshift_crosscheck)

GRBPred$Class <- factor(ifelse(GRBPred$Redshift_crosscheck > 4, "1", "0"))

# write.csv(GRBPred, "before_ubsampled.csv") # not needed, to export again the same imported file

input <- GRBPred # uncomment this if you want to compare the redshift distributions, because here we keep the redshift
 
# input <- GRBPred[, -((ncol(up_train)-2):ncol(up_train))] # this is without redshift

output <- GRBPred$Class

data <- ubBalance(X=input, Y=output, positive=1, type="ubSMOTE", percOver=200, percUnder=0, verbose=TRUE)

# The ubBalance assigns a different notation to the columns name in the file
# We export the balanced dataframe in order to reimport it after and correct
# the header labels, otherwise the SuperLearner will stuck

write.csv(data, "ubsampled_draft.csv") 

reimported <- read.csv("ubsampled_draft.csv", header = TRUE, row.names = 1)

# Loop for correcting header in the reimported dataframe

for (i in 1:ncol(GRBPred)) {
colnames(reimported)[i] <- gsub('X.','',colnames(reimported)[i])
}

# Histogram after the ubBalance compared with the initial one

# Histogram before the ubBalance

hist(GRBPred$Redshift_crosscheck)

# Histogram after the ubBalance
# To plot this is important to keep the redshift in the dataframe before ubBalance

hist(reimported$Redshift_crosscheck)

# Histogram after the ubBalance compared with the initial one
# To plot this is important to keep the redshift in the dataframe before ubBalance

hist(GRBPred$Redshift_crosscheck)
hist(reimported$Redshift_crosscheck,col=rgb(0,0,1,0.5),add=TRUE)

# Export the dataframe with corrected header (or alternatively use the reimported as variable)

write.csv(reimported, "ubsampled_correctheader.csv")