
# using this to export correlation plots since I can't seem to export/ save them properly in markdown

# data
data<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/p450correlationMAN.csv")
sdata<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/sodcorrelationMAN1.csv")
sdata1<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/sodcorrelationMAN2.csv")
sdata2<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/avgSOD_analytes2.csv")

# p450 plot
library(corrplot)
# Spearman correlation tests to extract p-values
corr_results <- sapply(data[ , 3:ncol(data)], function(x) {
  test <- cor.test(data$avg_p450, x, method = "spearman")
  test$p.value  # Extract p-values
})
# Spearman correlation matrix for plotting
spearman_cor_matrix <- cor(data[ , 3:ncol(data)], use = "complete.obs", method = "spearman")
spearman_cor_avg_value <- spearman_cor_matrix["avg_p450", ]
# Filter for significance using corr_results for p-values
significant_analytes <- names(corr_results[corr_results < 0.05])
# New correlation matrix with only significant analytes
significant_cor_matrix <- spearman_cor_matrix[significant_analytes, significant_analytes]
# Plot the correlation matrix for significant analytes
corrplot(significant_cor_matrix, method = "circle", type= "lower", diag= FALSE)

# sod plots 1-3
# plot 1
# Spearman correlation tests to extract p-values
corr_results <- sapply(sdata[ , 3:ncol(sdata)], function(x) {
  test <- cor.test(sdata$avg_sod, x, method = "spearman")
  test$p.value  # Extract p-values
})
# Spearman correlation matrix for plotting
spearman_cor_matrix <- cor(sdata[ , 3:ncol(sdata)], use = "complete.obs", method = "spearman")
spearman_cor_avg_value <- spearman_cor_matrix["avg_sod", ]
# Filter for significance using corr_results for p-values
significant_analytes <- names(corr_results[corr_results < 0.05])
# New correlation matrix with only significant analytes
significant_cor_matrix <- spearman_cor_matrix[significant_analytes, significant_analytes]
# Plot the correlation matrix for significant analytes
corrplot(significant_cor_matrix, method = "circle", type= "lower", diag= FALSE)

# plot 2
# Spearman correlation tests to extract p-values
corr_results <- sapply(sdata1[ , 3:ncol(sdata1)], function(x) {
  test <- cor.test(sdata1$avg_sod, x, method = "spearman")
  test$p.value  # Extract p-values
})
# Spearman correlation matrix for plotting
spearman_cor_matrix <- cor(sdata1[ , 3:ncol(sdata1)], use = "complete.obs", method = "spearman")
spearman_cor_avg_value <- spearman_cor_matrix["avg_sod", ]
# Filter for significance using corr_results for p-values
significant_analytes <- names(corr_results[corr_results < 0.05])
# New correlation matrix with only significant analytes
significant_cor_matrix <- spearman_cor_matrix[significant_analytes, significant_analytes]
# Plot the correlation matrix for significant analytes
corrplot(significant_cor_matrix, method = "circle", type= "lower", diag= FALSE)

#plot3
# Spearman correlation tests to extract p-values
corr_results <- sapply(sdata2[ , 3:ncol(sdata2)], function(x) {
  test <- cor.test(sdata2$avg_value, x, method = "spearman")
  test$p.value  # Extract p-values
})
# Spearman correlation matrix for plotting
spearman_cor_matrix <- cor(sdata2[ , 3:ncol(sdata2)], use = "complete.obs", method = "spearman")
spearman_cor_avg_value <- spearman_cor_matrix["avg_value", ]
# Filter for significance using corr_results for p-values
significant_analytes <- names(corr_results[corr_results < 0.05])
# New correlation matrix with only significant analytes
significant_cor_matrix <- spearman_cor_matrix[significant_analytes, significant_analytes]
# Plot the correlation matrix for significant analytes
corrplot(significant_cor_matrix, method = "circle", type= "lower", diag= FALSE)









