
# keep NA values but exclude them from analyses
mean(df$metric1, na.rm = TRUE)  # Computes mean without NA values
sum(df$metric2, na.rm = TRUE)    # Sums values while ignoring NA

# add a ggsave - I always forget it
# add anything I use regularly but forget
