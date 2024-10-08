
# Load data
data<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/p450data.csv")
indv<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/avgP450_analytes.csv")

library(corrplot)

# Spearman correlation tests to extract p-values
corr_results <- sapply(indv[ , 3:ncol(indv)], function(x) {
  test <- cor.test(indv$avg_value, x, method = "spearman")
  test$p.value  # Extract p-values
})

# Spearman correlation matrix for plotting
spearman_cor_matrix <- cor(indv[ , 3:ncol(indv)], use = "complete.obs", method = "spearman")
spearman_cor_avg_value <- spearman_cor_matrix["avg_value", ]

# Filter for significance using corr_results for p-values
significant_analytes <- names(corr_results[corr_results < 0.05])

# New correlation matrix with only significant analytes
significant_cor_matrix <- spearman_cor_matrix[significant_analytes, significant_analytes]

# Plot the correlation matrix for significant analytes
corrplot(significant_cor_matrix, method = "circle", type= "lower", diag= FALSE)

