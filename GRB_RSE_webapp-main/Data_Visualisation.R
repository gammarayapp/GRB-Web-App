source("Load_Imports.R")
#Old_data = read.csv("SORTED_FINAL_X-Ray_DATA.csv",header = T,row.names = 1)
#dim(Old_data)
run_locally = F#Set to true for debugging.

if(run_locally){
  New_data = read.csv("combined_data_with_redshift.csv",header = T,row.names = 1)
  
} else {
  args <- commandArgs(trailingOnly = TRUE)
  input_file <- args[1]
  New_data = read.csv(input_file, header = TRUE, row.names = 1)
}

addr <-paste("Results/") #THIS HOLDS THE ADDRESS AT WHICH THE FILES ARE OUTPUT
PLOTaddr <-paste("Plot_Output/") #THIS HOLDS THE ADDRESS AT WHICH THE PLOTS ARE OUTPUT
## CREATE DIRECTORIES IF THEY DONT EXIST
if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
if(!dir.exists(addr)){dir.create(addr)}

#Old_data = New_data[rownames(Old_data),]

#New_data = New_data[!(rownames(New_data)%in%rownames(Old_data)),]

group<-NA
#group[row.names(Old_data)]<-1
group[row.names(New_data)]<-2
#dim(group)
#gro
#dim(group)
#group

#Totdata<-GRBPred

#plotnames<-paste(ncol(Totdata),'vars-',nrow(Totdata),'data','_','AllCat',sep = '')

Totdata = rbind(New_data)

#Totdata[row.names(Totdata)=="091127A",]
#Totdata_2=Totdata[row.names(Totdata)!="091127A",]

Totdata = Totdata[ Totdata$T90 > 2,]

Totdata$T90 = log10(Totdata$T90)

#Totdata = Totdata[Totdata$T90 > log10(2),]

Totdata = Totdata[Totdata$log10NH > 20,]
Totdata = Totdata[Totdata$Alpha < 3,] 
Totdata = Totdata[Totdata$Beta < 3,]
Totdata = Totdata[Totdata$Gamma < 3,]

#filename = paste(PLOTaddr,'FullScatterPlot-.png',sep = '')

# {
#   # Scatter Plot Section
#   png(file = filename, width = 1500, height = 1500, res = 120)
#   pairs(
#     as.matrix(Totdata[, c(1:11)]),
#     horOdd = T,
#     pch = 3,
#     col = c('black', 'red')[group],  # Black for old data, Red for new data
#     cex = 0.5,
#     cex.labels = 1.4,
#     main = paste('Scatter Plot of', dim(Totdata)[1], ' samples')
#   )
#   dev.off()
#   
#   #par(mar=c(10,10,10,10))
# }



# Histogram Section
#hist(Old_data$Redshift_crosscheck, col = 'black', freq = F, main = "Redshift Distribution", xlab = "Redshift", ylab = "Density")
#hist(New_data$Redshift_crosscheck, col = 'red', add = TRUE, freq = F)
#legend("topright", legend = c("Old Data", "New Data"), fill = c("black", "red"))


dim(New_data[New_data$Redshift_crosscheck > 3,])

#dim(Old_data[Old_data$Redshift_crosscheck > 3,])


########## GOOD PAIRS PLOT ##########

library(ggplot2)
library(GGally)
library(ggpubr)

#Totdata = read.csv("Training_data_MICE_w_NH&Peak_imputed.csv",header = T,row.names = 1)
filename = paste(PLOTaddr,'NewScatterPlot.png',sep = '')
png(file = filename, width = 1500, height = 1500, res = 125)
#Redshift_crosscheck	T90	log10Fluence	log10PeakFlux	PhotonIndex	log10NH	Gamma	log10Fa	log10Ta	Alpha	Beta
ggpairs(
  Totdata[, c(1:11)],
  axisLabels = "show",
  columnLabels = c('z', 'log(T90)', 'log(Fluence)', 'log(Peak)', 'Photon Index','log(NH)', 'Gamma', 'log(Fa)', 'log(Ta)', 'α',  'β' ),
  upper = list(continuous = GGally::wrap("cor", method = "pearson", stars = FALSE, size = 4, col = 'blue')),
  diag = list(continuous = wrap("barDiag", bins = 10, fill = 'red', col = 'black')),
  lower = list(
    continuous = GGally::wrap("points", alpha = 0.6, size = 1, col = 'black')  # Set color for all points to black
  )
) +
  theme_bw() +
  theme(
    panel.background = element_rect(colour = 'white'),
    panel.grid = element_blank(),
    axis.text = element_text(colour = 'black'),
    strip.text = ggplot2::element_text(size = 11, face = "bold")
  )

dev.off()
