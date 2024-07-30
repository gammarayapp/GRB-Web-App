Old_data = read.csv("SORTED_FINAL_X-Ray_DATA.csv",header = T,row.names = 1)
dim(Old_data)

New_data = read.csv("combined_data_with_redshift.csv",header = T,row.names = 1)

Old_data = New_data[rownames(Old_data),]

New_data = New_data[!(rownames(New_data)%in%rownames(Old_data)),]

group<-NA
group[row.names(Old_data)]<-1
group[row.names(New_data)]<-2
#dim(group)
#gro
#dim(group)
#group

#Totdata<-GRBPred

#plotnames<-paste(ncol(Totdata),'vars-',nrow(Totdata),'data','_','AllCat',sep = '')

Totdata = rbind(Old_data,New_data)

#Totdata[row.names(Totdata)=="091127A",]
#Totdata_2=Totdata[row.names(Totdata)!="091127A",]

Totdata = Totdata[ Totdata$T90 > 2,]

Totdata$T90 = log10(Totdata$T90)

#Totdata = Totdata[Totdata$T90 > log10(2),]

Totdata = Totdata[Totdata$log10NH > 20,]

{
  png(file = paste("FullScatterPlot-",plotnames,".png",sep = ""),width = 1500,height = 1500,res=120)
  pairs(as.matrix(Totdata[,c(1:11)]),
        #pairs(as.matrix(Totdata[,c(4,6:16)]),
        
        #pairs(as.matrix(fullDatMat_WO[,c('InvRedshift','Frac_Variability','Gaia_G_Magnitude')]),
        horOdd = T ,
        pch=3,
        col=c('red','black')[group],
        cex=0.5,
        cex.labels=1.4,
        #cex.angle=45,
        main=paste('Scatter Plot of',dim(Totdata)[1],' samples')
        #lower.panel = as.matrix(lowredshift[,3:6])
  )
  #legend( title = 'AGN type (frequency)',
  #       'bottomleft',
  #      inset = 0.1,
  #legend = unique(fullDatMat_WO$Label),
  #fill = unique(fullDatMat_WO$LabelNo),
  #col = unique(fullDatMat_WO$LabelNo),
  # legend = paste(plyr::count(Totdata,vars = c('LabelNo','Label'))$Label,
  #               ' (',plyr::count(Totdata,vars = c('LabelNo','Label'))$freq,
  #              ')',sep = ''),
  #fill = plyr::count(Totdata,vars = c('LabelNo','Label'))$LabelNo,
  #col = plyr::count(Totdata,vars = c('LabelNo','Label'))$LabelNo,
  # pch = 16
  #)
  #
  dev.off()
  #par(mar=c(10,10,10,10))
}



hist(Old_data$Redshift_crosscheck,col='red',freq = F)
hist(New_data$Redshift_crosscheck,col=rgb(0,0,1,0.5),add=T,freq = F)

dim(New_data[New_data$Redshift_crosscheck > 3,])

dim(Old_data[Old_data$Redshift_crosscheck > 3,])


########## GOOD PAIRS PLOT ##########

library(ggplot2)
library(GGally)
library(ggpubr)

#Totdata = read.csv("Training_data_MICE_w_NH&Peak_imputed.csv",header = T,row.names = 1)

png(file = paste(PLOTaddr,"/NewScatterPlot.png",sep = ""),width = 1500,height = 1500,res=125)
ggpairs(Totdata[,c(1:11)],
        
        axisLabels = c("show"),
        
        columnLabels = c('log(T90)','log(Fa)','log(Ta)','α','β','log(Fluence)','Photon Index','log(NH)','log(Peak)','z')
        
        ,upper=list(continuous=GGally::wrap("cor", method="pearson", stars=FALSE, size=4,col='blue'))
        ,diag = list(continuous = wrap("barDiag",bins=10,fill='red',col='black')),
        
)+theme_bw()+theme(panel.background = element_rect(colour = 'white')
                   ,panel.grid = element_blank()
                   ,axis.text = element_text(colour = 'black')
                   ,strip.text=ggplot2::element_text(size=11,face="bold")
)
dev.off()

