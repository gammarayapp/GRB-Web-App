######### LASSO FEATURE SELECTION ######

library(glmnet)

LASSO <- function(X,Y)
{
  X<-as.matrix(X) # THE TRAINING DATA
  Y<-as.vector(Y) # THE RESPONSE VECTOR
  lasso_model<-cv.glmnet(X,Y,alpha=1) # LASSO REGRESSION
  return(lasso_model)
}

lasso_coef=vector()

for (a in 1:100) {
  #lasmod=LASSO(Y = log10(raw_xray_data$Redshift_crosscheck + 1), X = features_for_mice_preds)
  lasmod=LASSO(Y = log10(raw_xray_data[rownames(GRBPred),1] + 1), X = GRBPred[,colnames(features_for_mice_preds)])
  lasso_coef<-cbind(lasso_coef,lasmod$glmnet.fit$beta[,lasmod$glmnet.fit$lambda==lasmod$lambda.1se])
}
lasso_coef_avg = rowMeans(lasso_coef)

png(filename = paste(PLOTaddr,'LassoFeatures.png',sep = ''))#,width = 600,height = 1000)

par(mar=c(5,9,1,1))
barplot(sort(abs(lasso_coef_avg))
        ,horiz = T,las=1,xlab="Coefficient"
        ,cex.names = 1.5
        ,cex.axis = 1.5,cex.lab=1.75
        ,font.axis=2,font.lab=2
        )
axis(1,lwd=3,cex.axis=1.5)
dev.off()
print(lasso_coef_avg)
#lassovar = head(names(lasso_coef_avg[order(abs(lasso_coef_avg), decreasing=TRUE)]), 6)
lassovar = names(lasso_coef_avg[order(abs(lasso_coef_avg), decreasing=TRUE)])