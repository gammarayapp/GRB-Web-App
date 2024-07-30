
###### STATISTICAL PARAMETERS ####
result_plotter <- function(names,p,o,pred.max=0,pred.min=0,pred.sd = 0, linpred.sd = 0, post_fix = '0'){
  


cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#owMeans(preds)
#invRedshiftVec_WO
#cor(preds,invRedshiftVec_WO)

#mad(rowMeans(preds),center = median(rowMeans(preds)))

#mad(invRedshiftVec_WO,center = median(invRedshiftVec_WO))

#mad(scale(rowMeans(preds),center = T),center = median(rowMeans(preds)))

#mad(scale(invRedshiftVec_WO,center = T),center = median(invRedshiftVec_WO))

#results<-data.frame(mean=rowMeans(preds),Observed=invRedshiftVec_WO)
#Val_results<-data.frame(mean=rowMeans(Validationpreds),Observed=ValidationSet_z$InvRedshift)

results<-data.frame(InvZphot=round(p,4)
                    ,InvZspec=round(o,4)
                    #,Label=TrainData$Label
                    #,LabelNo=TrainData$LabelNo
                    ) # THIS IS NEW CODE. NEED TO BE IMPROVED

rownames(results) <- names

results$Dlogz <- (results$InvZspec - results$InvZphot)
results$Zphot  <- 10^results$InvZphot - 1 #    (1/results$InvZphot)-1
results$Zspec  <- 10^results$InvZspec - 1#  (1/results$InvZspec)-1
results$Dz     <- (results$Zspec - results$Zphot) # DIFFERENCE BETWEEN OBS AND PRED Z
results$normDz <- results$Dz/(1+results$Zspec) # NORMALIZED RESIDUALS
results$SD <- pred.sd #apply(preds) # CALCULATE THE STANDARD DEVIATIONS
results$z_SD <- linpred.sd #rowSds(z_preds) # CALCULATE THE STANDARD DEVIATION IN Z 
results$pred_max <- pred.max
results$pred_min <- pred.min

results$linpred_max <- 10^(pred.max) - 1
results$linpred_min <- 10^(pred.min) - 1

write.csv(results,file = paste(addr,'10fCVResults',plotnames,'.csv',sep = ''))

#results$Label <- factor(results$Label,
                        #levels = c('2','3'),
#                        labels = c('BLL','FSRQ'))

#### CALCULATING SIGMA IN LOG SCALE ###

L_Sigma <- sd(results$Dlogz) # STANDARD DEVIATION IN LOGSCALE
L_2Sigma <-2*L_Sigma

normRMS<-sqrt(mean((results$normDz)^2))
InvRMS<-sqrt(mean((results$Dlogz)^2))

InvBias<-mean(results$Dlogz)
Bias<-mean(results$Dz) #BIAS CALCULATED FROM DELTA Z 
normBias<-mean(results$normDz) # NORMALIZED BIAS CALCULATED FROM NORMALIZED DELTA Z

#BLL_bias <- mean(results$Dz[results$Label=='BLL'])
#FSRQ_bias <- mean(results$Dz[results$Label=='FSRQ'])

plot(results$Zspec,results$Dz,pch='*',main='Redshift vs Bias',xlab='z',ylab='Bias')
abline(h=Bias)

plot(results$Zspec,results$normDz,main='Redshift vs Normalized Bias',xlab='z',ylab='Normalized Bias')
abline(h=normBias)

Sigma <- sqrt(sum((results$Dz-Bias)^2)/(nrow(results))) # SIGMA CALCULATED AS THE ROOT MEAN SQUARE OF DELTA Z - BIAS

normSigma<-sqrt(mean((results$normDz-normBias)^2 )) # NORM SIGMA CALCULATED AS THE ROOT MEAN SQUARE OF NORM DELTA Z - NORM BIAS

MAD<-median(abs(results$Dz)) 
NMAD<-median(abs(results$normDz))




#mad(abs(results$normDz))

CVboxplot<-boxplot(results$InvZphot-results$InvZspec) # BOXPLOT IN Log(z+1)

#Valboxplot<-boxplot(Val_results$InvZphot - Val_results$InvZspec) # BOXPLOT IN Log(z+1)


{ ##### BOX PLOT ####
  png(filename = paste(PLOTaddr,'BoxPlotOfRedshift',post_fix, '.png', sep = ''),width = (750*sz),height = (750*sz),res=rez)
  CVboxplot<-boxplot(results$Dz) # BOXPLOT IN z
  boxplot(results$Dz,
          main = paste('Box plot for Dz (',CVboxplot$n,')  \n',
                       'Median=',signif(CVboxplot$stats[3,],2),
                       '| Outside whisker=',length(CVboxplot$out))
  )
  dev.off()
}

{ ##### BOX PLOT ####
  png(filename = paste(PLOTaddr,'BoxPlotOfNormalizedRedshift',post_fix,'.png', sep = ''),width = (750*sz),height = (750*sz),res=rez)
  CVboxplot<-boxplot(results$normDz) # BOXPLOT IN z
  boxplot(results$normDz,
          main = paste('Box plot for normalized Dz (',CVboxplot$n,')  \n',
                       'Median=',signif(CVboxplot$stats[3,],2),
                       '| Outside whisker=',length(CVboxplot$out))
  )
  dev.off()
}



#Valboxplot<-boxplot(Val_results$Zphot - Val_results$Zspec) # BOXPLOT IN z

# CORRELATIONS IN 1/(Z+1)
cor(results$InvZphot,results$InvZspec)
#cor(Val_results$InvZphot,Val_results$InvZspec)

# CORRELATIONS IN DIRECT REDSHIFT
cor(results$Zphot,results$Zspec)
#cor(Val_results$Zphot,Val_results$Zspec)


#
{
  png(filename = paste(PLOTaddr,'DeltaZSpreadBLL',post_fix,'.png',sep = ''),width = (1000*sz),height = (750*sz),res=rez)
  hist(results$Dz,breaks = 50,main = paste('Histogram of Dz \n Sigma=',signif(Sigma,3),'| Bias=',signif(Bias,3))
       ,xlab='Dz'
  )
  abline(v=c(-Sigma,Sigma),col='blue')
  abline(v=Bias,col='red')
  dev.off()
  hist(results$Dz,breaks = 50,main = paste('Histogram of Dz \n Sigma=',signif(Sigma,3),'| Bias=',signif(Bias,3))
       ,xlab='Dz'
  )
  abline(v=c(-Sigma,Sigma),col='blue')
  abline(v=Bias,col='red')
  
}


{
  png(filename = paste(PLOTaddr,'NormDeltaZSpreadBLL',post_fix,'.png',sep = ''),width = (1000*sz),height = (750*sz),res=rez)
  hist(results$normDz,breaks = 25,main = paste('Histogram of Dz_norm \n Sigma=',signif(normSigma,3),'| Bias=',signif(normBias,3))
       ,xlab='Normalized Dz'
  )
  abline(v=c(-normSigma,normSigma),col='blue')
  abline(v=normBias,col='red')
  dev.off()
  hist(results$normDz,breaks = 25,main = paste('Histogram of Dz_norm \n Sigma=',signif(normSigma,3),'| Bias=',signif(normBias,3))
       ,xlab='Normalized Dz'
  )
  abline(v=c(-normSigma,normSigma),col='blue')
  abline(v=normBias,col='red')
  
}


##### SIGMA CONE CALCULATIONS 

#belowUpperLine<-results[results$Zphot-(1+2*Sigma)*results$Zspec<0,]
#aboveLowerLine<-results[results$Zphot-(1-2*Sigma)*results$Zspec>0,]

#InsideTheCone<-results[intersect(rownames(belowUpperLine),rownames(aboveLowerLine)),]


#Val_belowUpperLine<-Val_results[Val_results$Zphot-(1+Sigma)*Val_results$Zspec<0,]
#Val_aboveLowerLine<-Val_results[Val_results$Zphot-(1-Sigma)*Val_results$Zspec>0,]

#Val_InsideTheCone<-Val_results[intersect(rownames(Val_belowUpperLine),rownames(Val_aboveLowerLine)),]

#### MY INTERPRETATION OF 2Sigma
#belowUpperLine<-results[results$Zphot-(1+Sigma)*results$Zspec < Sigma,]
#aboveLowerLine<-results[results$Zphot-(1-Sigma/(1+Sigma))*results$Zspec > -Sigma/(1+Sigma),]

#InsideTheCone<-results[intersect(rownames(belowUpperLine),rownames(aboveLowerLine)),]

belowUpperLine<-results[results$InvZphot-(1)*results$InvZspec < 2*L_Sigma,]
aboveLowerLine<-results[results$InvZphot-(1)*results$InvZspec > -2*L_Sigma,]

InsideTheCone<-results[intersect(rownames(belowUpperLine),rownames(aboveLowerLine)),]

print(rownames(InsideTheCone))
write.csv(InsideTheCone,file = paste(addr,'Results_wo_catout',plotnames,'.csv',sep = ''))

sigma1_belowUpperLine<-results[results$InvZphot-(1)*results$InvZspec < L_Sigma,]
sigma1_aboveLowerLine<-results[results$InvZphot-(1)*results$InvZspec > -L_Sigma,]

sigma_1InsideTheCone<-results[intersect(rownames(sigma1_belowUpperLine),rownames(sigma1_aboveLowerLine)),]


#abs(results$Zphot-1.2*results$Zspec)>0 & abs(results$Zphot-0.8*results$Zspec)>0
#SlopePoints <- c( 
#  list(c(1 - L_2Sigma*(results$Zphot + 1))/(1 + L_2Sigma)),
#  list(c(1 + L_2Sigma*(results$Zphot + 1))/(1 - L_2Sigma))
#)

#Intercept <- c(-L_2Sigma/( 1 + L_2Sigma ),L_2Sigma/(1 - L_2Sigma)) 
#

#Create a sequence from 0 to max(Zpred) by 0.1
#then calculate the function of the linear scale on this sequence
#then we plot this sequence with the order(Zspec)

#ZSseq<-seq(0,max(results$Zspec),length.out = length(results$Zspec))
#ZSseq<-order(results$Zspec)

#f_of_Zspec <- c(
#  (list(c(ZSseq*((1/(ZSseq+1))/((1/(ZSseq+1)) + L_2Sigma)) - (L_2Sigma/((1/(1+ZSseq)) + L_2Sigma))))),
#  (list(c(ZSseq*((1/(ZSseq+1))/((1/(ZSseq+1)) - L_2Sigma)) + (L_2Sigma/((1/(1+ZSseq)) - L_2Sigma)))))
#)
#s_of_Zspec <- c(
# list(c(ZSseq*((1/ZSseq)/((1/ZSseq) + L_2Sigma)))),
#  list(c(ZSseq*((1/ZSseq)/((1/ZSseq) - L_2Sigma))))
#)
#plot(ZSseq,f_of_Zspec[[1]])


#########Cross Validation Correlation Plot####

sz<-0.8
rez=120

uplim<-max(results$InvZphot,results$InvZspec)
lowlim<-min(results$InvZphot,results$InvZspec)

print(plotnames)

{
  png(filename = paste(PLOTaddr,'logscalez',post_fix,'.png', sep = ""),width = 1000*sz,height = 1000*sz,res=rez)
  plt1<-ggplot(results, aes(x=InvZspec, y=InvZphot,col='blue'))+ xlim(lowlim,uplim)+ylim(lowlim,uplim)+
    geom_point(shape=1)+
    #geom_point(pch=1)+
    #geom_point(col=results$LabelNo,pch=1)+
    #geom_point(aes(x=InvZspec,y=InvZphot,col=Label),data = InsideTheCone, pch=16)+
    geom_point(aes(x=InvZspec,y=InvZphot,col='red'),data = InsideTheCone, shape=1)+
    geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    geom_abline(slope=1,intercept = 2*L_Sigma,col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    geom_abline(slope=1,intercept = -2*L_Sigma,col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope=1+4*Sigma,intercept = 0,col="black")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope=abs(1-4*Sigma),intercept = 0,col="black")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope = )
    
    #geom_errorbar(ymin=results$InvZphot-results$SD,ymax=results$InvZphot+results$SD)+
    geom_errorbar(ymin=results$pred_min,ymax=results$pred_max)+
    
    #geom_abline(slope = Inv_Bias_fitting$coefficients[2],intercept = Inv_Bias_fitting$coefficients[1],col = 1,lwd=1.5,lty=3)+
    #geom_abline(slope = BLL_Inv_Bias_fitting$coefficients[2],intercept = BLL_Inv_Bias_fitting$coefficients[1],col = 2+3,lwd=1.5,lty=2)+
    #geom_abline(slope = FSRQ_Inv_Bias_fitting$coefficients[2],intercept = FSRQ_Inv_Bias_fitting$coefficients[1],col = 3+3,lwd=1.5,lty=2)+
    #geom_abline(slope = CV_BC$coefficients[2],intercept = CV_BC$coefficients[1],col = 1,lwd=1.5,lty=3)+
    
    labs(color='Outliers',
         #title = paste("10fCV of ",plotnames,' | samplesize= ',nrow(results),' | In 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
         title = paste('Samplesize = ',nrow(results),' | Within 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
         subtitle = paste("r = ", signif(cor(results$InvZphot,results$InvZspec),4),
                          # "\t Error =  ", signif(mean(abs(results_cv_log$InvZphot-results_cv_log$InvZspec)),2),
                          #"| RMS = ", signif(sqrt(mean((results$Dz)^2)),2), # RMS VALUE
                          #"| Bias = ", signif(Bias,2), # BIAS VALUE
                          #"| Normalized Bias = ",signif(normBias,2), # NORMALIZED BIAS
                          #"| MAD = ", signif(mad(results$normDz),3), # MAD VALUE FOR normalized residuals
                          #"| NMAD = ", signif((1.48*mad(results$normDz)),3) # NMAD VALUES FOR normalized residuals
                          '| Sigma = ',signif(L_Sigma,3),
                          "| RMS = ", signif(sqrt(mean((results$Dlogz)^2)),3), # RMS VALUE
                          "| Bias = ", signif(InvBias,2), # BIAS VALUE
                          #"| Bias(norm) = ",signif(normBias,3), # NORMALIZED BIAS
                          #"\n| MAD(??z) = ", signif(median(abs(results$Dz)),3), # MAD VALUE FOR normalized residuals
                          #"| NMAD(??z) = ", signif(1.48*median(abs(results$Dz)),3), # NMAD VALUES FOR normalized residuals
                          #"| MAD =",signif(median(abs(results$Dlogz)),2),
                          "| NMAD =",signif(1.48*median(abs(results$Dlogz)),3)
         )) + 
    scale_color_manual(values=cbPalette)+theme_bw()+theme(plot.background = element_rect(color = 'black',size=1))+#theme(axis.text.y = element_text( face ="bold"),axis.text.x = element_text( face ="bold"),panel.border=element_rect(color = 'black',fill = NA))+
    xlab("Observed log(z+1)")+ylab("Predicted log(z+1)")#+geom_tex
  
  print(plt1)
  
  dev.off()
  print(plt1)
}


uplim<-max(results$Zphot,results$Zspec)
lowlim<-min(results$Zphot,results$Zspec)

Yuplim<-max(results$linpred_max)
# lowlim<-min(results$Zspec)

{
  png(filename = paste(PLOTaddr,'logscalez2',post_fix,'.png', sep = ""),width = 1000*sz,height = 1000*sz,res=rez)
  #plt1<-ggplot(results, aes(x=Zspec, y=Zphot,color='Beyond 2 sigma'))+ xlim(lowlim,uplim)+ylim(lowlim,uplim)+
  plt1<-ggplot(results, aes(x=Zspec, y=Zphot),col='black')+xlim(0,8.5)+ylim(0,11)+
    
    geom_point(shape=16)+
    #geom_point(col=results$LabelNo,pch=1,show.legend = T)+
    
    #geom_point(aes(x=Zspec,y=Zphot,col='Inside 2 sigma'),data = InsideTheCone, shape=16)+
    geom_point(aes(x=Zspec,y=Zphot),col='black',data = InsideTheCone, shape=16)+
    
    
    #geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_abline(slope=(SlopePoints[[1]]),intercept = Intercept[1],col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_abline(slope=(SlopePoints[[2]]),intercept = Intercept[2],col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_line(aes(x=sort(SlopePoints)))+
    
    #geom_point(y=f_of_Zspec[[1]] ,x=ZSseq,pch='*',col='blue' )+
    #geom_abline(y=f_of_Zspec[[2]] ,x=ZSseq)+#,pch='*',col='blue' )+
    
    
    
    #geom_line(mapping = aes(x=ZSseq,y=f_of_Zspec[[2]]),col='blue')+
    #geom_line(mapping = aes(x=ZSseq,y=f_of_Zspec[[1]]),col='blue')+
    
    geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
    #geom_errorbar(ymin=results$Zphot-results$z_SD,ymax=results$Zphot+results$z_SD,width=0.1)+
    geom_errorbar(ymin=results$linpred_min,ymax=results$linpred_max,width=0.2,col='black')+
    
    #geom_abline(slope = Lin_Bias_fitting$coefficients[2],intercept = Lin_Bias_fitting$coefficients[1],col = 1,lwd=1.5,lty=3)+
    #geom_abline(slope = BLL_Lin_Bias_fitting$coefficients[2],intercept = BLL_Lin_Bias_fitting$coefficients[1],col = 2+3,lwd=1.5,lty=2)+
    #geom_abline(slope = FSRQ_Lin_Bias_fitting$coefficients[2],intercept = FSRQ_Lin_Bias_fitting$coefficients[1],col = 3+3,lwd=1.5,lty=2)+
    
    
    #geom_abline(slope)
    #geom_abline(slope=SlopePoints,intercept = -1,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
    geom_abline(slope=10^(2*L_Sigma),intercept = (10^(2*L_Sigma) - 1),col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(-2*L_Sigma),intercept =(10^(-2*L_Sigma) - 1),col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(L_Sigma),intercept = (10^(L_Sigma) - 1),col="green")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(-L_Sigma),intercept =(10^(-L_Sigma) - 1),col="green")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
  
    #geom_abline(slope=10^(L_Sigma),intercept = 0.1*(10^(L_Sigma) - 1),col="black")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
  #geom_abline(slope=10^(-L_Sigma),intercept = 0.1*(10^(-L_Sigma) - 1),col="black")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
# 
labs(color = '',
     #title = paste("10fCV of ",plotnames,' | samplesize= ',nrow(results)),#' | In Cone= ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
     title = paste('Samplesize = ',nrow(results)
                   ,' | In 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)'
                   ,' | In sigma =',nrow(sigma_1InsideTheCone),' (',signif(100*(nrow(sigma_1InsideTheCone)/nrow(results)),2),'%)'
                   ,sep=""),
     subtitle = paste(" r = ", signif(cor(results$Zphot,results$Zspec),3),
                      # "\t Error =  ", signif(mean(abs(results_cv_log$InvZphot-results_cv_log$InvZspec)),2),
                      # "\t MSE = ", signif(mean(abs(results$Zphot - results$Zspec)^2),2),
                      #"\t Bias = ", signif(mean((results$Zphot - results$Zspec)),2))) +
                      " | Sigma = ",signif(Sigma,3),
                      " | RMS = ", signif(sqrt(mean((results$Dz)^2)),2), # RMS VALUE
                      " | Bias = ", signif(Bias,2), # BIAS VALUE
                      #"\t| Normalized Bias = ",signif(normBias,2), # NORMALIZED BIAS
                      #"\t| MAD = ", signif(mad(results$normDz),3), # MAD VALUE FOR normalized residuals
                     " | NMAD = ", signif((1.48*mad(results$Dz)),3) # NMAD VALUES FOR normalized residuals

                      # "| RMS(Dz) = ", signif(sqrt(mean((results$Dz)^2)),3), # RMS VALUE
                      #"| RMS(Dz_norm) = ", signif(sqrt(mean((results$normDz)^2)),3), # RMS VALUE
                      # "| Bias(Dz) = ", signif(Bias,3), # BIAS VALUE
                      #" | Bias(Dz_norm) = ",signif(normBias,3), # NORMALIZED BIAS
                      #"| MAD(Dz) = ", signif(median(abs(results$Dz)),3), # MAD VALUE FOR normalized residuals
                      #"| NMAD(Dz) = ", signif(1.48*median(abs(results$Dz)),3), # NMAD VALUES FOR normalized residuals
                      #"\n MAD(Dz_norm)=",signif(median(abs(results$normDz)),3),
                      #"| NMAD(Dz_norm)=",signif(1.48*median(abs(results$normDz)),3)
     )) +
    #scale_color_manual(values=cbPalette)+
    scale_color_manual(values=c('black','black'))+
    theme_bw()+
    #coord_cartesian(xlim=c(0,5),ylim=c(0,5))+
    scale_x_continuous(limits = c(lowlim,uplim),breaks = seq(round(lowlim),round(uplim),1))+
    #scale_y_continuous(limits = c(lowlim,uplim),breaks = seq(round(lowlim),round(uplim),1))+
    scale_y_continuous(limits = c(lowlim,max(results$linpred_max)),breaks = seq(round(lowlim),round(max(results$linpred_max)),1))+
    theme(plot.background = element_rect(color = 'white',size=0,fill=NA)
          ,panel.grid = element_line(colour = 'white')
          ,panel.border = element_rect(colour = 'black',fill=NA,size=0.5)
          ,axis.line = element_line(colour = 'black',size=1)
          ,axis.text.y = element_text( face ="bold",size=10,colour = 'black')
          ,axis.text.x = element_text( face ="bold",size=10,colour = 'black')
          ,axis.ticks = element_line(colour = 'black',size=1))+
  #+theme(plot.background = element_rect(color = 'black',size=2)
   #       ,panel.background = element_rect(colour = 'white'))+theme( axis.text.y = element_text( face ="bold",size=10,colour = 'black'),axis.text.x = element_text( face ="bold",size=10,colour = 'black'))+
    
    xlab("Observed z")+ylab("Predicted z") 
    #geom_abline(slope=0,intercept = 0,lwd=0.75)+
    #geom_abline(slope=100000,intercept = 0,lwd=0.75)
  
  print(plt1)
  dev.off()
  print(plt1)
}


#########Cross Validation Correlation Plot####

sz<-0.8
rez=200

uplim<-max(results$InvZphot,results$InvZspec)
lowlim<-min(results$InvZphot,results$InvZspec)

print(plotnames)

{
  png(filename = paste(PLOTaddr,'logscale3', post_fix,'.png', sep = ""),width = 1000*sz,height = 1000*sz,res=rez)
  plt1<-ggplot(results, aes(x=InvZspec, y=InvZphot,col='blue'))+ xlim(lowlim,uplim)+ylim(lowlim,uplim)+
    geom_point(shape=1)+
    #geom_point(pch=1)+
    #geom_point(col=results$LabelNo,pch=1)+
    #geom_point(aes(x=InvZspec,y=InvZphot,col=Label),data = InsideTheCone, pch=16)+
    geom_point(aes(x=InvZspec,y=InvZphot,col='red'),data = InsideTheCone, shape=1)+
    geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    geom_abline(slope=1,intercept = 2*L_Sigma,col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    geom_abline(slope=1,intercept = -2*L_Sigma,col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope=1+4*Sigma,intercept = 0,col="black")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope=abs(1-4*Sigma),intercept = 0,col="black")+#geom_point(data=lowredshift,aes(x=mean,y=Observed))+
    #geom_abline(slope = )
    
    #geom_errorbar(ymin=results$InvZphot-results$SD,ymax=results$InvZphot+results$SD)+
    geom_errorbar(ymin=results$pred_min,ymax=results$pred_max)+
    
    #geom_abline(slope = Inv_Bias_fitting$coefficients[2],intercept = Inv_Bias_fitting$coefficients[1],col = 1,lwd=1.5,lty=3)+
    #geom_abline(slope = BLL_Inv_Bias_fitting$coefficients[2],intercept = BLL_Inv_Bias_fitting$coefficients[1],col = 2+3,lwd=1.5,lty=2)+
    #geom_abline(slope = FSRQ_Inv_Bias_fitting$coefficients[2],intercept = FSRQ_Inv_Bias_fitting$coefficients[1],col = 3+3,lwd=1.5,lty=2)+
    #geom_abline(slope = CV_BC$coefficients[2],intercept = CV_BC$coefficients[1],col = 1,lwd=1.5,lty=3)+
    
    # labs(color='Outliers',
    #      #title = paste("10fCV of ",plotnames,' | samplesize= ',nrow(results),' | In 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
    #      title = paste('Samplesize = ',nrow(results),' | Within 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
    #      subtitle = paste("r = ", signif(cor(results$InvZphot,results$InvZspec),4),
    #                       # "\t Error =  ", signif(mean(abs(results_cv_log$InvZphot-results_cv_log$InvZspec)),2),
    #                       #"| RMS = ", signif(sqrt(mean((results$Dz)^2)),2), # RMS VALUE
    #                       #"| Bias = ", signif(Bias,2), # BIAS VALUE
    #                       #"| Normalized Bias = ",signif(normBias,2), # NORMALIZED BIAS
    #                       #"| MAD = ", signif(mad(results$normDz),3), # MAD VALUE FOR normalized residuals
    #                       #"| NMAD = ", signif((1.48*mad(results$normDz)),3) # NMAD VALUES FOR normalized residuals
    #                       '| Sigma = ',signif(L_Sigma,3),
    #                       "| RMS = ", signif(sqrt(mean((results$Dlogz)^2)),3), # RMS VALUE
    #                       "| Bias = ", signif(InvBias,2), # BIAS VALUE
    #                       #"| Bias(norm) = ",signif(normBias,3), # NORMALIZED BIAS
    #                       #"\n| MAD(??z) = ", signif(median(abs(results$Dz)),3), # MAD VALUE FOR normalized residuals
    #                       #"| NMAD(??z) = ", signif(1.48*median(abs(results$Dz)),3), # NMAD VALUES FOR normalized residuals
    #                       #"| MAD =",signif(median(abs(results$Dlogz)),2),
    #                       "| NMAD =",signif(1.48*median(abs(results$Dlogz)),3)
    #      )) + 
    scale_color_manual(values=cbPalette)+theme_bw()+theme(plot.background = element_rect(color = 'black',size=1))+#theme(axis.text.y = element_text( face ="bold"),axis.text.x = element_text( face ="bold"),panel.border=element_rect(color = 'black',fill = NA))+
    xlab("Observed log(z+1)")+ylab("Predicted log(z+1)")#+geom_tex
  
  print(plt1)
  
  dev.off()
  print(plt1)
}


uplim<-max(results$Zphot,results$Zspec)
lowlim<-min(results$Zphot,results$Zspec)

Yuplim<-max(results$linpred_max)
# lowlim<-min(results$Zspec)

{
  png(filename = paste(PLOTaddr,'observed_vs_predicted',post_fix,'.png',sep = ""),width = 1000*sz,height = 1000*sz,res=rez)
  #plt1<-ggplot(results, aes(x=Zspec, y=Zphot,color='Beyond 2 sigma'))+ xlim(lowlim,uplim)+ylim(lowlim,uplim)+
  plt1<-ggplot(results, aes(x=Zspec, y=Zphot),col='black')+xlim(0,8.5)+ylim(0,11)+
    
    geom_point(shape=16)+
    #geom_point(col=results$LabelNo,pch=1,show.legend = T)+
    
    #geom_point(aes(x=Zspec,y=Zphot,col='Inside 2 sigma'),data = InsideTheCone, shape=16)+
    geom_point(aes(x=Zspec,y=Zphot),col='black',data = InsideTheCone, shape=16)+
    
    
    #geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_abline(slope=(SlopePoints[[1]]),intercept = Intercept[1],col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_abline(slope=(SlopePoints[[2]]),intercept = Intercept[2],col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_line(aes(x=sort(SlopePoints)))+
    
    #geom_point(y=f_of_Zspec[[1]] ,x=ZSseq,pch='*',col='blue' )+
    #geom_abline(y=f_of_Zspec[[2]] ,x=ZSseq)+#,pch='*',col='blue' )+
    
    
  
  #geom_line(mapping = aes(x=ZSseq,y=f_of_Zspec[[2]]),col='blue')+
  #geom_line(mapping = aes(x=ZSseq,y=f_of_Zspec[[1]]),col='blue')+
  
  geom_abline(slope=1,intercept = 0,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
    #geom_errorbar(ymin=results$Zphot-results$z_SD,ymax=results$Zphot+results$z_SD,width=0.1)+
    geom_errorbar(ymin=results$linpred_min,ymax=results$linpred_max,width=0.2,col='black')+
    
    #geom_abline(slope = Lin_Bias_fitting$coefficients[2],intercept = Lin_Bias_fitting$coefficients[1],col = 1,lwd=1.5,lty=3)+
    #geom_abline(slope = BLL_Lin_Bias_fitting$coefficients[2],intercept = BLL_Lin_Bias_fitting$coefficients[1],col = 2+3,lwd=1.5,lty=2)+
    #geom_abline(slope = FSRQ_Lin_Bias_fitting$coefficients[2],intercept = FSRQ_Lin_Bias_fitting$coefficients[1],col = 3+3,lwd=1.5,lty=2)+
    
    
    #geom_abline(slope)
    #geom_abline(slope=SlopePoints,intercept = -1,col="red")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
    geom_abline(slope=10^(2*L_Sigma),intercept = (10^(2*L_Sigma) - 1),col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(-2*L_Sigma),intercept =(10^(-2*L_Sigma) - 1),col="blue")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(L_Sigma),intercept = (10^(L_Sigma) - 1),col="green")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    geom_abline(slope=10^(-L_Sigma),intercept =(10^(-L_Sigma) - 1),col="green")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    
    
    #geom_abline(slope=10^(L_Sigma),intercept = 0.1*(10^(L_Sigma) - 1),col="black")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    #geom_abline(slope=10^(-L_Sigma),intercept = 0.1*(10^(-L_Sigma) - 1),col="black")+#geom_point(data=lowredshift,aes(x=mean,y=InvZspec))+
    # 
    # labs(color = '',
    #      #title = paste("10fCV of ",plotnames,' | samplesize= ',nrow(results)),#' | In Cone= ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)',sep=""),
    #      title = paste('Samplesize = ',nrow(results)
    #                    ,' | In 2sigma = ',nrow(InsideTheCone),' (',signif(100*(nrow(InsideTheCone)/nrow(results)),2),'%)'
    #                    ,' | In sigma =',nrow(sigma_1InsideTheCone),' (',signif(100*(nrow(sigma_1InsideTheCone)/nrow(results)),2),'%)'
    #                    ,sep=""),
    #      subtitle = paste(" r = ", signif(cor(results$Zphot,results$Zspec),3),
    #                       # "\t Error =  ", signif(mean(abs(results_cv_log$InvZphot-results_cv_log$InvZspec)),2),
    #                       # "\t MSE = ", signif(mean(abs(results$Zphot - results$Zspec)^2),2),
    #                       #"\t Bias = ", signif(mean((results$Zphot - results$Zspec)),2))) +
    #                       " | Sigma = ",signif(Sigma,3),
    #                       " | RMS = ", signif(sqrt(mean((results$Dz)^2)),2), # RMS VALUE
    #                       " | Bias = ", signif(Bias,2), # BIAS VALUE
    #                       #"\t| Normalized Bias = ",signif(normBias,2), # NORMALIZED BIAS
    #                       #"\t| MAD = ", signif(mad(results$normDz),3), # MAD VALUE FOR normalized residuals
    #                       " | NMAD = ", signif((1.48*mad(results$Dz)),3) # NMAD VALUES FOR normalized residuals
    #                       
    #                       # "| RMS(Dz) = ", signif(sqrt(mean((results$Dz)^2)),3), # RMS VALUE
    #                       #"| RMS(Dz_norm) = ", signif(sqrt(mean((results$normDz)^2)),3), # RMS VALUE
    #                       # "| Bias(Dz) = ", signif(Bias,3), # BIAS VALUE
    #                       #" | Bias(Dz_norm) = ",signif(normBias,3), # NORMALIZED BIAS
    #                       #"| MAD(Dz) = ", signif(median(abs(results$Dz)),3), # MAD VALUE FOR normalized residuals
    #                       #"| NMAD(Dz) = ", signif(1.48*median(abs(results$Dz)),3), # NMAD VALUES FOR normalized residuals
    #                       #"\n MAD(Dz_norm)=",signif(median(abs(results$normDz)),3),
    #                       #"| NMAD(Dz_norm)=",signif(1.48*median(abs(results$normDz)),3)
    #      )) +
    #scale_color_manual(values=cbPalette)+
    scale_color_manual(values=c('black','black'))+
    theme_bw()+
    #coord_cartesian(xlim=c(0,5),ylim=c(0,5))+
    scale_x_continuous(limits = c(lowlim,uplim),breaks = seq(round(lowlim),round(uplim),1))+
    #scale_y_continuous(limits = c(lowlim,uplim),breaks = seq(round(lowlim),round(uplim),1))+
    scale_y_continuous(limits = c(lowlim,max(results$linpred_max)),breaks = seq(round(lowlim),round(max(results$linpred_max)),1))+
    theme(plot.background = element_rect(color = 'white',size=0,fill=NA)
          ,panel.grid = element_line(colour = 'white')
          ,panel.border = element_rect(colour = 'black',fill=NA,size=0.5)
          ,axis.line = element_line(colour = 'black',size=1)
          ,axis.text.y = element_text( face ="bold",size=10,colour = 'black')
          ,axis.text.x = element_text( face ="bold",size=10,colour = 'black')
          ,axis.ticks = element_line(colour = 'black',size=1))+
    #+theme(plot.background = element_rect(color = 'black',size=2)
    #       ,panel.background = element_rect(colour = 'white'))+theme( axis.text.y = element_text( face ="bold",size=10,colour = 'black'),axis.text.x = element_text( face ="bold",size=10,colour = 'black'))+
    
    xlab("Observed z")+ylab("Predicted z") 
  #geom_abline(slope=0,intercept = 0,lwd=0.75)+
  #geom_abline(slope=100000,intercept = 0,lwd=0.75)
  
  print(plt1)
  dev.off()
  print(plt1)
}


save.image(file = paste(addr,"Workspace_outputs",plotnames,".Rdata",sep = ""))

return(results)
}

