oldPLOTaddr <- PLOTaddr
print(oldPLOTaddr)
PLOTaddr <- paste('Formula_Generation/Formula_Results/',sep='')
print(PLOTaddr)
addr <- paste(PLOTaddr,'Files/',sep='')
if(!dir.exists(PLOTaddr)){dir.create(PLOTaddr)}
if(!dir.exists(addr)){dir.create(addr)}

O1_list <- read.csv("Formula_Generation/O1_formula_list",header = T,row.names = 1)
O2_list <- read.csv("Formula_Generation/O2_formula_list",header = T,row.names = 1)
SO1_list <- read.csv("Formula_Generation/SmoothedO1_formula_list",header = T,row.names = 1)

all_formula_list <- rbind(O1_list, O2_list, SO1_list)
all_formula_list <- subset(all_formula_list, select = c("x"))
all_formula <- apply(as.matrix(all_formula_list), 1, as.formula)

cleaned_list <- all_formula

# INITIALIZE DATA FRAME
col_names <- c("Formula", "LinearCorrel", "LinearRMSE")
formulaScatter_df <- data.frame(matrix(ncol = length(col_names), nrow = 0))

# CV LOOP
loop <- 10
for (i in 1:15) {
  gam_formula = cleaned_list[[i]]
  print(i)
  source("Formula_Generation/Manual10fCVGAM.R")
  formula_str <- paste(deparse(gam_formula), collapse = " ")
  source("Formula_Generation/Plot_Results_GAM_CV.R")

  formulaScatter_df <- rbind(formulaScatter_df, c(formula_str, mean(LinearCorrel), mean(linear_rmse_distribution)))
}

colnames(formulaScatter_df) <- col_names

formulaScatter_df$LinearRMSE <- as.numeric(formulaScatter_df$LinearRMSE)
formulaScatter_df$LinearCorrel <- as.numeric(formulaScatter_df$LinearCorrel)

# Sort the dataframe based on LinearCorrel in descending order and LinearRMSE in ascending order
sorted_df <- formulaScatter_df[order(-formulaScatter_df$LinearCorrel, formulaScatter_df$LinearRMSE), ]

# Subset the top three rows
top_3 <- sorted_df[1:3, ]
top_10 <- sorted_df[1:10, ]
# Plot the scatterplot with the top three formulas highlighted
p <- ggplot(formulaScatter_df, aes(x = LinearRMSE, y = LinearCorrel)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1, 3, 0.1), labels = scales::number_format(accuracy = 0.01)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::number_format(accuracy = 0.01)) +
  geom_point(data = top_3, color = 'red', size = 1) +
  labs(x = "Linear RMSE", y = "Linear Correlation") +
  ggtitle("Linear RMSE vs. Linear Correlation")

ggsave("GAMScatter.pdf", plot = p, device = "pdf", width = 8, height = 6)

PLOTaddr <- oldPLOTaddr