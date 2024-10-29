
# using this to export correlation plots since I can't seem to export/ save them properly in markdown

# data
data<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/p450correlationMAN.csv")
sdata<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/sodcorrelationMAN1.csv")
sdata1<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/sodcorrelationMAN2.csv")
sdata2<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/avgSOD_analytes2.csv")

# P450 PLOT
# Load necessary libraries
library(ggplot2)
library(Hmisc)  # for Spearman correlation with p-values

# Function to calculate Spearman correlations and p-values
get_spearman_corr <- function(data, target_col, analyte_columns) {
  corr_results <- sapply(analyte_columns, function(col) {
    result <- rcorr(data[[target_col]], data[[col]], type = "spearman")
    list(correlation = result$r[1, 2], p_value = result$P[1, 2])
  }, simplify = FALSE)
  
  corr_df <- do.call(rbind, lapply(corr_results, as.data.frame))
  rownames(corr_df) <- analyte_columns
  corr_df <- as.data.frame(corr_df)
  
  # Filter for significant correlations (p_value < 0.05) and exclude target column correlation
  significant_corrs <- subset(corr_df, p_value < 0.05 & rownames(corr_df) != target_col)
  return(significant_corrs)
}

# Calculate significant correlations for avg_p450 or another metric
target_metric <- 'avg_p450'  # Change to your desired target
analyte_columns <- grep("^a", names(data), value = TRUE)  # Adjust as needed
significant_corrs <- get_spearman_corr(data, target_metric, analyte_columns)

# Add rownames as a column for plotting and set factor levels to maintain CSV column order
significant_corrs$Analyte <- factor(rownames(significant_corrs), levels = analyte_columns)

# Create the plot for avg_p450 with ordered analytes
ggplot(significant_corrs, aes(x = correlation, y = Analyte)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(correlation, 2)), 
            hjust = ifelse(significant_corrs$correlation > 0, -0.1, 1.1),
            color = "black", size = 3) + 
  labs(title = paste("Significant Spearman Correlations with", target_metric),
       x = "Spearman Correlation", y = "Analyte") +
  xlim(-0.35, 0.35) +  # Set x-axis range
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
