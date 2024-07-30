# plot_script.R
png(filename="sample_plot.png", width=700, height=500)
plot(c(1, 2, 3, 4), c(4, 1, 3, 2), main="Sample Plot", xlab="X-axis", ylab="Y-axis", col="blue", pch=19, type="b")
dev.off()