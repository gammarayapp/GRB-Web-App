read.table("New_generalization.txt",header = F)
new_GS_1=read.csv("Aleksander_GS_v1.csv",row.names = 1)


GS_0 = read.csv("TOTAL_GENERALIZATION_DATA_v2.csv", header = TRUE, row.names = 1)

head(GS_0)


new_GS_1$Fbest = new_GS_1$flux
new_GS_1$T_abest = new_GS_1$Ta
new_GS_1$F_min = new_GS_1$flux_min
new_GS_1$F_max = new_GS_1$flux_max
new_GS_1$T_amin = new_GS_1$Ta_min
new_GS_1$T_amax = new_GS_1$Ta_max
new_GS_1$logT90 = log10(new_GS_1$T90)
new_GS_1$logPeakFlux = log10(new_GS_1$PeakPhotonFlux)
new_GS_1$errorlogPeakFlux = new_GS_1$PeakPhotonFluxError/ (new_GS_1$PeakPhotonFlux * log(10))
new_GS_1$photon_index = new_GS_1$PhotonIndex
new_GS_1$errorphotonindex = new_GS_1$PhotonIndexError
new_GS_1$logNH = log10( new_GS_1$NH * 10^21 )
new_GS_1$T90_err = 0

GS_1 = rbind(GS_0,new_GS_1[,colnames(GS_0)])

write.csv(GS_1,file = "TOTAL_GENERALIZATION_DATA_v3.csv")



