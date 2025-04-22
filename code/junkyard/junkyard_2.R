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
```{r}

# plot with Penn Cove indicated
df$highlight <- ifelse(df$site_name == "Penn Cove Reference", "Penn Cove Reference", "Other")

ibr1_box <- ggplot(df, aes(x = site_name, y = ibr, fill = highlight)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Penn Cove Reference" = "red", "Other" = "white")) +
  theme_minimal() +
  labs(title = "IBR_1 by Site", y = "IBRv2i Index", x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # hides legend if not needed

# plot with Penn Cove indicated
df$highlight <- ifelse(df$site_name == "Penn Cove Reference", "Penn Cove Reference", "Other")

ibr2_box <- ggplot(df, aes(x = site_name, y = IBRv2i, fill = highlight)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Penn Cove Reference" = "red", "Other" = "white")) +
  theme_minimal() +
  labs(title = "IBRv2i by Site", y = "IBRv2i Index", x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # hides legend if not needed

# plot with Penn Cove indicated
df$highlight <- ifelse(df$site_name == "Penn Cove Reference", "Penn Cove Reference", "Other")

ibr3_box <- ggplot(df, aes(x = site_name, y = IBRv2i, fill = highlight)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Penn Cove Reference" = "red", "Other" = "white")) +
  theme_minimal() +
  labs(title = "IBRv2i by Site", y = "IBRv2i Index", x = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # hides legend if not needed

# save and view
#ggsave(filename= "/Users/cmantegna/Documents/GitHub/WDFWmussels/output/ibrv2i_box_reference_indicated.png", plot= ibrv2i_box2, width = 16, height = 14, dpi = 300)

print(ibrv2i_box)
print(ibrv2i_box2)


```
# Visualization
## NWS Poster viz
```{r}

#map
library(sf)
library(readr)
library(patchwork)
library(viridis)
library(rnaturalearth)
library(ggsignif)

df$reporting_area <- as.factor(df$reporting_area)

# Convert to spatial
sites_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

coast <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name == "Washington")

# Major city coordinates for labels
cities <- data.frame(
  city = c("Seattle", "Tacoma", "Olympia", "Bellingham", "Port Angeles", "Hama Hama"),
  lon = c(-122.33, -122.45, -122.90, -122.48, -123.43, -123.16),
  lat = c(47.61, 47.25, 47.00, 48.73, 48.08, 47.63)
)

# Define color palette
area_colors <- viridis::viridis(length(levels(df$reporting_area)), option = "turbo")

# Map
map_plot <- ggplot() +
  geom_sf(data = coast, fill = "gray90", color = "white") +
  geom_sf(data = sites_sf, aes(color = reporting_area), size = 3, alpha = 0.9) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city),
            color = "black", fontface = "bold", size = 4) +
  scale_color_manual(values = area_colors, name = "Reporting Area") + 
  coord_sf(xlim = c(-124, -121), ylim = c(46.5, 49.0)) +
  theme_void() +
  labs(title = "Sampled Sites by Reporting Area")

# Filter data for IBRv2i < 5
df_filtered <- df %>% filter(IBRv2i < 5)
df_filtered <- df_filtered %>%
  mutate(is_reference = ifelse(site_name == "Penn Cove Reference", "Reference", "Other"))

# Boxplot
box_plot <- ggplot(df_filtered, aes(x = reporting_area, y = IBRv2i, fill = reporting_area)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "black", size = 1.5) +
  scale_fill_manual(values = area_colors, guide = "none") + 
  coord_cartesian(ylim = c(-3, 5.5)) +
  geom_signif(comparisons = list(
    c("11", "7"),
    c("13", "7"),
    c("6", "7"),
    c("10", "13")
  ),
  annotations = c("p = 0.0093", "p = 0.0013", "p = 0.0487", "p = 0.0199"),
  y_position = c(5.2, 4.8, 4.5, 4.2),
  tip_length = 0.02,
  textsize = 4.5
  ) +
  theme_minimal(base_size = 13) + 
  theme(
    axis.line = element_line(size = 1.2, color = "black"),
    axis.text = element_text(size= 25, face = "bold"),
    axis.title = element_text(size= 30, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(x = "Reporting Area", y = "IBRv2i Score")

# Combine
combo_plot <- map_plot + box_plot +
  plot_layout(ncol = 2, widths = c(1.3, 1)) +
  plot_annotation(title = "Integrated Biomarker Response Overview")

# Display
print(map_plot)
print(box_plot)

# Export 
ggsave("/Users/cmantegna/Documents/GitHub/WDFWmussels/output/poster_map.png", map_plot, width = 14, height = 8, dpi = 300)

ggsave("/Users/cmantegna/Documents/GitHub/WDFWmussels/output/poster_box.png", box_plot, width = 14, height = 8, dpi = 300)


```

#
```{r}

# Make sure reporting_area is a factor
df$reporting_area <- as.factor(df$reporting_area)

# Run Kruskal-Wallis test
kruskal_result <- kruskal.test(IBRv2i ~ reporting_area, data = df)
print(kruskal_result)

# Run Dunn's post hoc test
dunn_result <- dunnTest(IBRv2i ~ reporting_area, data = df, method = "holm")
print(dunn_result)


```



