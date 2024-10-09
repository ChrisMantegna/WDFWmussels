
# using this to export correlation plots since I can't seem to export/ save them properly in markdown

# data
data<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/p450correlationMAN.csv")

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

