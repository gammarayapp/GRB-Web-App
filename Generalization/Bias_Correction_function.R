BC_1way <- function(crossvalidated_results
                    ,generalization_set=NA
                    ){
  
  results=crossvalidated_results
  gen_preds=generalization_set
  
  gen_preds$Zphot = 10^gen_preds$InvZphot-1
  
  s1<-sort(results$InvZphot)
  w2<-sort(results$InvZspec)
  
  plot(w2~s1)
  
  # THIS LINE SHOWS HOW THE MAX VALUE WE PREDICT COMPARES TO THE MAX VALUE
  # OF THE OBS VALUE.
  # WE TRANSFORM SO THAT THE DISTRIBUTION REMAINS THE SAME.
  op_lin_1 <- lm(w2 ~ s1)
  abline(op_lin_1, col = 'red' , lw = 2)
  
  hist(results$InvZphot,ylim = c(0,100),breaks = 20,col=rgb(1,0,0,1))
  hist(results$InvZspec,ylim = c(0,60),breaks = 20,col=rgb(0,1,0,1),add=T)
  
  if(!any(is.na(gen_preds))){
    gen_preds$corrected_InvZphot = op_lin_1$coefficients[1] + gen_preds$InvZphot*op_lin_1$coefficients[2]
    #gen_preds$corrected_InvZphot[gen_preds$Zphot>cut2] <- op_lin_2$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut2] * op_lin_2$coefficients[2]
    #gen_preds$corrected_InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] <- op_lin_3$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] * op_lin_3$coefficients[2]
    hist(gen_preds$InvZphot,add=T,col=rgb(0,0,0,0.5))
    hist(gen_preds$corrected_InvZphot,add=T,col=rgb(1,1,1,0.75),breaks = 10)
    return(gen_preds$corrected_InvZphot)
  }
  
}


##### 3WAY BIAS CORRECTION ####
BC_2way <- function(crossvalidated_results
                    ,generalization_set=NA
                    ,cut1=2.5
                    ){
  
  results=crossvalidated_results
  gen_preds=generalization_set
  
  gen_preds$Zphot = 10^gen_preds$InvZphot-1
  
  s1<-sort(results$InvZphot[results$Zspec<cut1])
  w2<-sort(results$InvZspec[results$Zspec<cut1])
  
  plot(w2~s1)
  
  # THIS LINE SHOWS HOW THE MAX VALUE WE PREDICT COMPARES TO THE MAX VALUE
  # OF THE OBS VALUE.
  # WE TRANSFORM SO THAT THE DISTRIBUTION REMAINS THE SAME.
  op_lin_1 <- lm(w2 ~ s1)
  abline(op_lin_1, col = 'red' , lw = 2)
  
  
  s1<-sort(results$InvZphot[results$Zspec>cut1])
  w2<-sort(results$InvZspec[results$Zspec>cut1])
  
  plot(w2~s1)
  op_lin_2 <- lm(w2 ~ s1)
  abline(op_lin_2, col = 'red' , lw = 2)
  
  hist(results$InvZphot,ylim = c(0,100),breaks = 20,col=rgb(1,0,0,1))
  hist(results$InvZspec,ylim = c(0,60),breaks = 20,col=rgb(0,1,0,1),add=T)
  
  if(!any(is.na(gen_preds))){
    gen_preds$corrected_InvZphot[gen_preds$Zphot<cut1] <- op_lin_1$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot<cut1] * op_lin_1$coefficients[2]
    gen_preds$corrected_InvZphot[gen_preds$Zphot>cut1] <- op_lin_2$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut1] * op_lin_2$coefficients[2]
    #gen_preds$corrected_InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] <- op_lin_3$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] * op_lin_3$coefficients[2]
    hist(gen_preds$InvZphot,add=T,col=rgb(0,0,0,0.5))
    hist(gen_preds$corrected_InvZphot,add=T,col=rgb(1,1,1,0.75),breaks = 10)
    return(gen_preds$corrected_InvZphot)
  }
  
}


##### 3WAY BIAS CORRECTION ####
BC_3way <- function(crossvalidated_results
                    ,generalization_set=NA
                    ,cut1=2
                    ,cut2=3.5
                    ,cut3=5){
  
  results=crossvalidated_results
  gen_preds=generalization_set
  
  gen_preds$Zphot = 10^gen_preds$InvZphot-1
  
  s1<-sort(results$InvZphot[results$Zspec<cut1])
  w2<-sort(results$InvZspec[results$Zspec<cut1])
  
  plot(w2~s1)
  
  # THIS LINE SHOWS HOW THE MAX VALUE WE PREDICT COMPARES TO THE MAX VALUE
  # OF THE OBS VALUE.
  # WE TRANSFORM SO THAT THE DISTRIBUTION REMAINS THE SAME.
  op_lin_1 <- lm(w2 ~ s1)
  abline(op_lin_1, col = 'red' , lw = 2)
  
  
  s1<-sort(results$InvZphot[results$Zspec>cut2])
  w2<-sort(results$InvZspec[results$Zspec>cut2])
  
  plot(w2~s1)
  op_lin_2 <- lm(w2 ~ s1)
  abline(op_lin_2, col = 'red' , lw = 2)
  
  
  s1<-sort(results$InvZphot[results$Zspec>cut1 & results$Zspec<cut2])
  w2<-sort(results$InvZspec[results$Zspec>cut1 & results$Zspec<cut2])
  
  plot(w2~s1)
  op_lin_3 <- lm(w2 ~ s1)
  abline(op_lin_3, col = 'red' , lw = 2)
  
  hist(results$InvZphot,ylim = c(0,100),breaks = 20,col=rgb(1,0,0,1))
  hist(results$InvZspec,ylim = c(0,60),breaks = 20,col=rgb(0,1,0,1),add=T)
  
  if(!any(is.na(gen_preds))){
  gen_preds$corrected_InvZphot[gen_preds$Zphot<cut1] <- op_lin_1$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot<cut1] * op_lin_1$coefficients[2]
  gen_preds$corrected_InvZphot[gen_preds$Zphot>cut2] <- op_lin_2$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut2] * op_lin_2$coefficients[2]
  gen_preds$corrected_InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] <- op_lin_3$coefficients[1] + gen_preds$InvZphot[gen_preds$Zphot>cut1 & gen_preds$Zphot<cut2] * op_lin_3$coefficients[2]
  hist(gen_preds$InvZphot,add=T,col=rgb(0,0,0,0.5))
  hist(gen_preds$corrected_InvZphot,add=T,col=rgb(1,1,1,0.75),breaks = 10)
  return(gen_preds$corrected_InvZphot)
  }
  
}
