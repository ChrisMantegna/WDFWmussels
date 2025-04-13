# Old way to create a biomarker index - used for testing against the new way. New way confirmed, will not be moving forward with this method
# DO NOT RUN - Not using chunk from line 75 - 
## Creating the Beliaeff and Burgeot IBR Index with biomarkers and morphometrics

```{r}

# select and scale biomarkers and raw morphometrics
ibr_vars <- df %>%
  select(p450, sod, weight_change_g, weight_final_g, shell, length_mm, height_mm, width_mm)

ibr_scaled <- scale(ibr_vars) # standardize (Z-score)

ibr_scores <- abs(as.data.frame(ibr_scaled)) # convert to absolute Z-scores (deviation) 

df$IBR <- rowSums(ibr_scores) # calculate IBR Index (sum of absolute scores)

# plot
ibr_box<- ggplot(df, aes(x = site_name, y = IBR)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Integrated Biomarker Response (IBR) by Site",
       y = "IBR Index", x = "Site") +
  theme(axis.text.x= element_text(angle= 45, hjust= 1))

#save and view
ggsave(filename= "/Users/cmantegna/Documents/GitHub/WDFWmussels/output/ibr_box.png", plot= ibr_box, width = 16, height = 14, dpi = 300)

print(ibr_box)

```
# Radar Plots
## Test sample
```{r}

#install.packages("fmsb")
library("fmsb")

# Select and scale
ibr_vars <- df %>%
  select(p450, sod, weight_change_g, weight_final_g, shell, length_mm, height_mm, width_mm, ci1, ci2, ci3)

scaled_df <- scale(ibr_vars)
abs_scores <- abs(as.data.frame(scaled_df))

# Add sample/site identifiers back
abs_scores$site_name <- df$site_name
abs_scores$sample_id <- df$sample_id

# Choose one sample (e.g., row 1)
sample <- abs_scores[1, 1:11]  # only the numeric variables
max_min <- rbind(apply(abs_scores[ ,1:11], 2, max),
                 apply(abs_scores[ ,1:11], 2, min))
radar_data <- rbind(max_min, sample)

radarchart(radar_data,
           axistype = 1,
           pcol = "darkblue", pfcol = rgb(0.2, 0.4, 0.8, 0.4),
           plwd = 2,
           title = paste("IBR Radar Plot for Sample", abs_scores$sample_id[1]))

```

## All individual samples
```{r}


# Prepare absolute z-score data
ibr_vars <- df %>%
  select(p450, sod, weight_change_g, weight_final_g, shell, length_mm, height_mm, width_mm, ci1, ci2, ci3)

scaled <- scale(ibr_vars)
abs_scores <- abs(as.data.frame(scaled))

# Add ID columns back
abs_scores$sample_id <- df$sample_id
abs_scores$site_name <- df$site_name

# Loop through each sample and create radar plots
for (i in 1:nrow(abs_scores)) {
  sample_data <- abs_scores[i, 1:11]
  max_min <- rbind(apply(abs_scores[ ,1:11], 2, max),
                   apply(abs_scores[ ,1:11], 2, min))
  radar_data <- rbind(max_min, sample_data)
  
  # Save to file
  png(paste0("radar_sample_", abs_scores$sample_id[i], ".png"), width = 600, height = 600)
  radarchart(radar_data,
             axistype = 1,
             pcol = "darkblue", pfcol = rgb(0.2, 0.8, 0.4, 0.4),
             plwd = 2,
             title = paste("Sample", abs_scores$sample_id[i], "\nSite:", abs_scores$site_name[i]))
  dev.off()
}

```

## Site plots
```{r}

# Calculate site-level mean absolute Z-scores
site_means <- abs_scores %>%
  group_by(site_name) %>%
  summarise(across(1:11, mean, na.rm = TRUE))

# Loop through each site
for (i in 1:nrow(site_means)) {
  site_data <- site_means[i, 2:12]  # skip site_name column
  max_min <- rbind(apply(site_means[ ,2:12], 2, max),
                   apply(site_means[ ,2:12], 2, min))
  radar_data <- rbind(max_min, site_data)
  
  # Save to file
  png(paste0("radar_site_", gsub(" ", "_", site_means$site_name[i]), ".png"), width = 600, height = 600)
  radarchart(radar_data,
             axistype = 1,
             pcol = "darkblue", pfcol = rgb(0.2, 0.4, 0.9, 0.4),
             plwd = 2,
             title = paste("Mean IBR Profile\nSite:", site_means$site_name[i]))
  dev.off()
}

```
