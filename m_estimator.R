#'
#' Generates the M Estimator for removing outliers in data
#' Should be run from main code

require(MASS)
require(dplyr)


input_dir <- "OutputFiles"
output_dir <- "OutputFiles"

# CREATE DIRECTORIES IF THEY DONT EXIST
if (!dir.exists(input_dir)) {
  dir.create(input_dir)
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

#MEstAddr <- paste(output_dir, "/Plots", sep = "")
MEstAddr <- paste(PLOTaddr, "M_estimator_plots", sep = "")
# creating dir for plots
if (!dir.exists(MEstAddr)) {
  dir.create(MEstAddr)
}

#GRBPred <- read.csv(paste(input_dir, "/grb_xray_imputed.csv", sep = ""), header = TRUE, row.names = 1)

rc <- GRBPred$Redshift_crosscheck


Response = GRBPred$log10z


GRBPred <- subset(GRBPred, select = !colnames(GRBPred) %in% c("Redshift_crosscheck","log10z","invz","X"))

# 80/20 HIGHEST CV CORRELATION

rlm_formula = read.table("Best_formula_GLM.txt")
rlm_form = apply(as.matrix(rlm_formula[,2]),1,as.formula)

#rlm_form <- log10z ~ (log10NHSqr + log10T90Sqr + log10TaSqr + log10FaSqr + log10NH + PhotonIndex + log10T90 + log10Ta)^2 + log10Fa + log10PeakFlux + PhotonIndexSqr + log10PeakFluxSqr

# RLM FORMULA FOR TASK 1
# rlm_form <- log10z ~ (log10NHSqr + log10T90Sqr + log10NH + log10T90)^2 + log10Fa + log10Ta + PhotonIndex + log10PeakFlux + log10FaSqr + log10TaSqr + PhotonIndexSqr + log10PeakFluxSqr


# 80/20 AFTER NEW LOG10NH CUT
#rlm_form <- log10z ~ (PhotonIndex + log10T90 + log10Ta + log10Fa)^2 + log10NH + log10PeakFlux

#rlm_form <- log10z ~ (log10T90Sqr + PhotonIndex + log10Ta + log10Fa)^2 + log10T90 + log10NH + log10PeakFlux + log10FaSqr + log10TaSqr + PhotonIndexSqr + log10NHSqr + log10PeakFluxSqr



# 90/10 HIGHEST CV CORRELATION
#rlm_form <- log10z ~ (log10NHSqr + log10T90Sqr + log10NH + log10T90)^2 + log10Fa + log10Ta + PhotonIndex + log10PeakFlux + log10FaSqr + log10TaSqr + PhotonIndexSqr + log10PeakFluxSqr

grb_cols <- colnames(GRBPred)



# creating a linear combo of all predictors
# all fitting to linear redshift
# for (i in 1:length(grb_cols)-1) {
#   if (i == 1) {
#     assign("rlm_form", paste("log10z ~", grb_cols[i]))
#   } else {
#     assign("rlm_form", paste(rlm_form, "+", grb_cols[i]))
#   }
# }

# creating M Estimate object and showing summary
M_est <- MASS::rlm(rlm_form[[1]], data = GRBPred, method = "M")
summary(M_est)

# saving weight values and threshold
weights <- M_est$w

# the higher the threshold, the more likely a data point 
# will be classified as an outlier and is removed
#weight_threshold <- 0.5

# showing weight cutoff via histogram
{
  png(filename = paste(MEstAddr, "/M_estimator_weights.png", sep = ""), width = 1000, height = 1000, res = 200)
  hist_weights <- hist(weights, breaks = 2 + length(unique(round(weights, 1))))
  abline(v = weight_threshold, col = "red")
  legend("topleft",
    legend = rownames(GRBPred)[weights < weight_threshold]
  )
  dev.off()
}

color_weights <- vector(length = nrow(GRBPred))
names(color_weights) <- rownames(GRBPred)

color_weights <- ifelse(weights > weight_threshold, 1, 2)

# creating pairplot of data once weights have been assigned colors
{
  png(file = paste(MEstAddr, "/ScatterPlot_w_M_est_Weights.png", sep = ""), width = 3000, height = 3000, res = 200)
  pairs(as.matrix(GRBPred), # the excluded columns are the error columns
    horOdd = T,
    pch = 3,
    # col=round(weights*11),
    col = color_weights,
    cex = 0.5,
    cex.labels = 1.4,
    # cex.angle=45,
    main = paste("Scatter Plot of", dim(GRBPred)[1], " samples")
    # lower.panel = as.matrix(lowredshift[,3:6])
  )
  legend(
    title = "Weights",
    "bottomleft",
    inset = 0.1,
    # legend = unique(fullDatMat_WO$Label),
    # fill = sort(unique(round(weights*11))),#unique(fullDatMat_WO$LabelNo),
    # col = sort(unique(round(weights*11))),
    # legend = paste(sort(unique(round(weights,1))))

    fill = unique(color_weights), # unique(fullDatMat_WO$LabelNo),
    col = unique(color_weights),
    legend = c(
      paste("weight more than ", weight_threshold, " (", hist(color_weights, breaks = 2, plot = F)$counts[1], ")", sep = ""),
      paste("weight less than ", weight_threshold, " (", hist(color_weights, breaks = 2, plot = F)$counts[2], ")", sep = "")
    )

    # paste(plyr::count(Totdata,vars = c('LabelNo','Label'))$Label,
    #               ' (',plyr::count(Totdata,vars = c('LabelNo','Label'))$freq,
    #              ')',sep = ''),
    # fill = plyr::count(Totdata,vars = c('LabelNo','Label'))$LabelNo,
    # col = plyr::count(Totdata,vars = c('LabelNo','Label'))$LabelNo,
    # pch = 16
  )

  dev.off()
  # par(mar=c(10,10,10,10))
}

# removing data with weights below the threshold (these are considered outliers)
GRBPred$Redshift_crosscheck <- rc
GRBPred$log10z <- Response

# removing data with weights lower than weight threshold
GRBCut <- GRBPred[M_est$w > weight_threshold, ]

nrow(GRBCut)

print(rownames(GRBPred[M_est$w < weight_threshold, ]))

GRBPred <- GRBCut
write.csv(GRBCut, paste(output_dir, "/grb_xray_m_est.csv", sep = ""))
