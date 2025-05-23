# Plotting
## Box plots, Separate Indices per Site
```{r}

#metrics<- read.csv("../data/cleaned/ibr_1.csv")


# pivot only the index columns - adjusted for each df
ibr3_long <- df3 %>%
  pivot_longer(cols = c(ibr3_biomarkers, ibr3_morphometrics, ibr3_overall),
               names_to = "index_type",
               values_to = "index_value") %>%
  mutate(index_type = case_when(
    index_type == "ibr3_biomarkers" ~ "Biomarkers",
    index_type == "ibr3_morphometrics" ~ "Morphometrics",
    index_type == "ibr3_overall" ~ "Overall",
    TRUE ~ index_type))  # fallback in case of unexpected names

#### plotting ibr1 with Penn Cove reference site
all_index<- ggplot(ibr1_long, aes(x = site_name, y = index_value,
                                  color = site_name == "Penn Cove Reference")) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2) +
  facet_wrap(~ index_type, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") +
  labs(title = "IBRv2i Indices by Site",
       x = "Site", y = "IBRv2i Value")

# save and view
#ggsave(filename= "/Users/cmantegna/Documents/GitHub/WDFWmussels/output/ibr1_all_indices.png", plot= all_index, width = 16, height = 14, dpi = 300)

print(all_index)



```
## Radars, reference overlay
## All Sites, Do Not Run
```{r}

z_scores$sample_id <- df$sample_id
z_scores$site_name <- df$site_name

# Site-averaged Z-scores
site_means <- z_scores %>%
  group_by(site_name) %>%
  summarise(across(1:8, mean, na.rm = TRUE))

# Loop through sites
for (i in 1:nrow(site_means)) {
  site_data <- site_means[i, 2:9]
  max_min <- rbind(apply(site_means[ ,2:9], 2, max),
                   apply(site_means[ ,2:9], 2, min))
  radar_data <- rbind(max_min, site_data)
  
  png(paste0("IBRv2i_radar_site_", gsub(" ", "_", site_means$site_name[i]), ".png"), width = 600, height = 600)
  radarchart(radar_data,
             axistype = 1,
             pcol = "darkblue", pfcol = rgb(0.2, 0.4, 0.9, 0.4),
             plwd = 2,
             title = paste("IBRv2i Radar Plot\nSite:", site_means$site_name[i]))
  dev.off()
}

```

## All Samples,Only run if spot checking
```{r}

library(fmsb)

# Loop through each sample
for (i in 1:nrow(z_scores)) {
  
  # Extract sample data
  sample_data <- z_scores[i, 1:8]
  
  # Get axis limits (min/max across all samples)
  max_min <- rbind(apply(z_scores[, 1:8], 2, max, na.rm = TRUE),
                   apply(z_scores[, 1:8], 2, min, na.rm = TRUE))
  
  # Combine for radar input
  radar_data <- rbind(max_min, sample_data)
  
  # Set output file name (in current directory)
  filename <- paste0("radar_sample_", z_scores$sample_id[i], ".png")
  
  # Save plot
  png(filename, width = 600, height = 600)
  radarchart(radar_data,
             axistype = 1,
             pcol = "darkblue", pfcol = rgb(0.8, 0.2, 0.2, 0.4),
             plwd = 2,
             title = paste("IBRv2i Radar Plot\nSample:", z_scores$sample_id[i],
                           "\nSite:", z_scores$site_name[i]))
  dev.off()
}


```

## All sites w/ reference site overlay
```{r}

#### IBR_1
# Variables to include in radar plots
vars_to_use <- c("p450_ibr1", "sod_ibr1", "shell_ibr1", "weight_initial_g_ibr1", "weight_final_g_ibr1", "weight_change_g_ibr1", "length_mm_ibr1", "height_mm_ibr1", "width_mm_ibr1")

# create the reference profile from Penn Cove 
reference_profile <- z_scores %>%
  filter(sample_id %in% 41:44) %>%
  select(all_of(vars_to_use)) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# create site-level means 
site_means <- z_scores %>%
  group_by(site_name) %>%
  summarise(across(all_of(vars_to_use), mean, na.rm = TRUE)) %>%
  ungroup()

# define radar axis limits (same across all sites)
max_vals <- apply(site_means[, vars_to_use], 2, max, na.rm = TRUE)
min_vals <- apply(site_means[, vars_to_use], 2, min, na.rm = TRUE)

# convert reference profile to a named numeric vector
ref_vec <- unlist(reference_profile[1, ], use.names = TRUE)

# loop through each site to create radar plots
for (i in 1:nrow(site_means)) {
  site <- site_means[i, ]
  site_profile <- unlist(site[, vars_to_use], use.names = TRUE)
  
  # Combine into radar data
  radar_data <- as.data.frame(rbind(max_vals, min_vals, ref_vec, site_profile))
  rownames(radar_data) <- c("Max", "Min", "Reference", as.character(site$site_name))
  
  # Save radar plot
  filename <- paste0("radar_site_with_ref_", gsub(" ", "_", site$site_name), ".png")
  png(filename, width = 600, height = 600)
  
  radarchart(radar_data,
             axistype = 1,
             pcol = c("gray40", "steelblue"),
             pfcol = c(rgb(0.4, 0.4, 0.4, 0.3), rgb(0.2, 0.5, 0.8, 0.4)),
             plwd = 2,
             plty = 1,
             title = paste("IBRv2i Radar Plot with Reference\nSite:", site$site_name))
  
  legend("topright", legend = c("Penn Cove Reference", site$site_name),
         col = c("gray40", "steelblue"), lty = 1, lwd = 2, bty = "n")
  
  dev.off()
}


```

## Sites by Reporting Areas w/ Reference
```{r}

# manually adjusting for each ibr
# Define variables to include (excluding condition indices and weight final)
vars_to_use <- c("p450_ibr3", "sod_ibr3", "shell_ibr3", "weight_initial_g_ibr3", "weight_change_g_ibr3", "length_mm_ibr3", "height_mm_ibr3", "width_mm_ibr3")

# Site-level means (if you have multiple entries per site)
site_means <- df3 %>%
  group_by(site_name, reporting_area) %>%
  summarise(across(all_of(vars_to_use), mean, na.rm = TRUE), .groups = "drop")

# Global min/max for radar chart scaling
global_max <- apply(site_means[, vars_to_use], 2, max, na.rm = TRUE)
global_min <- apply(site_means[, vars_to_use], 2, min, na.rm = TRUE)

# Reference profile from one or more reference sites
reference_sites <- c( "Penn Cove Reference", "Hood Canal Holly", "Broad Spit (Fisherman's Point)")  # or use site_number
reference_profile <- df3 %>%
  filter(site_name %in% reference_sites) %>%
  summarise(across(all_of(vars_to_use), mean, na.rm = TRUE), .groups = "drop") %>%
  unlist(use.names = TRUE)

# Proceed with the rest of the radar chart code
reporting_areas <- unique(site_means$reporting_area)

for (area in reporting_areas) {
  area_sites <- site_means %>% filter(reporting_area == area)
  n_sites <- nrow(area_sites)
  n_pages <- ceiling(n_sites / 6)
  
  for (page in 1:n_pages) {
    start_i <- (page - 1) * 6 + 1
    end_i <- min(start_i + 5, n_sites)
    sites_subset <- area_sites[start_i:end_i, ]
    
    filename <- paste0("radar_group_", gsub(" ", "_", area), "_page_", page, ".png")
    png(filename, width = 1200, height = 800)
    par(mfrow = c(2, 3), mar = c(2, 2, 4, 2), oma = c(0, 0, 4, 0))
    
    for (i in 1:nrow(sites_subset)) {
      site_data <- unlist(sites_subset[i, vars_to_use], use.names = TRUE)
      chart_data <- as.data.frame(rbind(global_max, global_min, reference_profile, site_data))
      rownames(chart_data) <- c("Max", "Min", "Reference", as.character(sites_subset$site_name[i]))
      
      radarchart(chart_data,
                 axistype = 1,
                 pcol = c("gray40", "steelblue"),
                 pfcol = c(rgb(0.4, 0.4, 0.4, 0.3), rgb(0.2, 0.5, 0.8, 0.4)),
                 plwd = 2,
                 plty = 1,
                 cglcol = "grey80",
                 title = sites_subset$site_name[i])
    }
    
    mtext(paste("Reporting Area:", area), side = 3, line = 1, outer = TRUE, cex = 1.0)
    dev.off()
  }
}

```

# Old Spatial code - will delete after i update
# Spatial Autocorrelation - redo this whole workflow, something is wrong
## OLD Moran's I and Spearman
```{r}

ibr_analytes<- read.csv("../data/cleaned/avg_ibr1_analytess.csv")
analytes_long<- read.csv("../data/cleaned/analytes_long.csv")
metals_long<- read.csv("../data/cleaned/metals_long.csv")

# Moran's: ibr - analytes
ibr_sf <- st_as_sf(ibr_analytes, coords = c("longitude", "latitude"), crs = 4326)

coords <- st_coordinates(ibr_sf)
nb <- knn2nb(knearneigh(coords, k = 4))   # adjust k if needed
lw <- nb2listw(nb, style = "W")

bio_vars <- c("p450", "sod", "shell", "ci1", "ci2", "ci3", "ibr1bio", "ibr1morph", "ibr1overall")

bio_moran_results <- lapply(bio_vars, function(var) {
  vals <- ibr_sf[[var]]
  if (sum(!is.na(vals)) >= 4 && length(unique(vals[!is.na(vals)])) > 1) {
    m <- moran.test(vals, lw)
    data.frame(
      metric = var,
      moran_i = m$estimate[["Moran I statistic"]],
      expected_i = m$estimate[["Expectation"]],
      sd = if ("Standard deviate" %in% names(m$estimate)) m$estimate[["Standard deviate"]] else NA,
      p_value = m$p.value
    )
  } else {
    NULL
  }
}) %>% bind_rows()

# add significance stars
bio_moran_results <- bio_moran_results %>%
  mutate(
    sig = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# results
print(bio_moran_results)
#write.csv(bio_moran_results, "/Users/cmantegna/Documents/Github/WDFWmussels/output/moran_metrics.csv", row.names = FALSE)


# Moran's: ibr - metals
ibr_sf <- st_as_sf(ibr_metal, coords = c("longitude", "latitude"), crs = 4326)

coords <- st_coordinates(ibr_sf)
nb <- knn2nb(knearneigh(coords, k = 4))   # adjust k if needed
lw <- nb2listw(nb, style = "W")

bio_vars <- c("p450", "sod", "shell", "ci1", "ci2", "ci3", "ibr1bio", "ibr1morph", "ibr1overall")

bio_moran_results <- lapply(bio_vars, function(var) {
  vals <- ibr_sf[[var]]
  if (sum(!is.na(vals)) >= 4 && length(unique(vals[!is.na(vals)])) > 1) {
    m <- moran.test(vals, lw)
    data.frame(
      metric = var,
      moran_i = m$estimate[["Moran I statistic"]],
      expected_i = m$estimate[["Expectation"]],
      sd = if ("Standard deviate" %in% names(m$estimate)) m$estimate[["Standard deviate"]] else NA,
      p_value = m$p.value
    )
  } else {
    NULL
  }
}) %>% bind_rows()

# add significance stars
bio_moran_results <- bio_moran_results %>%
  mutate(
    sig = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# results
print(bio_moran_results)
write.csv(bio_moran_results, "/Users/cmantegna/Documents/Github/WDFWmussels/output/moran_metals.csv", row.names = FALSE)

# Spearman - analytes
results_latlong <- analytes_long %>%
  group_by(analyte) %>%
  summarise(
    corr_lat = cor(dry_value, latitude, method = "spearman", use = "complete.obs"),
    p_lat = cor.test(dry_value, latitude, method = "spearman")$p.value,
    corr_long = cor(dry_value, longitude, method = "spearman", use = "complete.obs"),
    p_long = cor.test(dry_value, longitude, method = "spearman")$p.value,
    .groups = "drop"
  )

# add significance stars
results_latlong <- results_latlong %>%
  mutate(
    lat_sig = case_when(
      p_lat <= 0.001 ~ "***",
      p_lat <= 0.01 ~ "**",
      p_lat <= 0.05 ~ "*",
      TRUE ~ ""
    ),
    long_sig = case_when(
      p_long <= 0.001 ~ "***",
      p_long <= 0.01 ~ "**",
      p_long <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )

view(results_latlong)
#write.csv(results_latlong, "/Users/cmantegna/Documents/Github/WDFWmussels/output/spatial_correlation_analytes.csv", row.names = FALSE)


# Spearman's - metals
results_latlong <- metals_long %>%
  group_by(analyte) %>%
  summarise(
    corr_lat = cor(dry_value, latitude, method = "spearman", use = "complete.obs"),
    p_lat = cor.test(dry_value, latitude, method = "spearman")$p.value,
    corr_long = cor(dry_value, longitude, method = "spearman", use = "complete.obs"),
    p_long = cor.test(dry_value, longitude, method = "spearman")$p.value,
    .groups = "drop"
  )

# add significance stars
results_latlong <- results_latlong %>%
  mutate(
    lat_sig = case_when(
      p_lat <= 0.001 ~ "***",
      p_lat <= 0.01 ~ "**",
      p_lat <= 0.05 ~ "*",
      TRUE ~ ""
    ),
    long_sig = case_when(
      p_long <= 0.001 ~ "***",
      p_long <= 0.01 ~ "**",
      p_long <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )

view(results_latlong)
#write.csv(results_latlong, "/Users/cmantegna/Documents/Github/WDFWmussels/output/spatial_correlation_metals.csv", row.names = FALSE)

```

## OLD Morans I
```{r}

# biomarkers & ibr's
bio_vars <- c("p450", "sod", "ibr1bio", "ibr1morph", "ibr1overall")

bio_moran_results <- lapply(bio_vars, function(var) {
  vals <- df_sf[[var]]
  if (sum(!is.na(vals)) >= 4 && length(unique(vals[!is.na(vals)])) > 1) {
    m <- moran.test(vals, lw)
    data.frame(
      metric = var,
      moran_i = m$estimate[["Moran I statistic"]],
      expected_i = m$estimate[["Expectation"]],
      sd = if ("Standard deviate" %in% names(m$estimate)) m$estimate[["Standard deviate"]] else NA,
      p_value = m$p.value
    )
  } else {
    NULL
  }
}) %>% bind_rows()


# analytes
df_class_summary <- analytes_long %>%
  group_by(site_name, latitude, longitude, class) %>%
  summarise(class_value = mean(dry_value, na.rm = TRUE), .groups = "drop")

library(sf)
df_sf <- st_as_sf(df_class_summary, coords = c("longitude", "latitude"), crs = 4326)

library(spdep)
moran_results <- list()

for (cl in unique(df_sf$class)) {
  sub_sf <- df_sf %>% filter(class == cl)
  
  # Create spatial neighbors (e.g., 4-nearest)
  coords <- st_coordinates(sub_sf)
  nb <- knn2nb(knearneigh(coords, k = 4))
  lw <- nb2listw(nb, style = "W")
  
  # Run Moran's I on the class_value
  moran <- moran.test(sub_sf$class_value, lw)
  
  moran_results[[cl]] <- data.frame(
    class = cl,
    moran_i = moran$estimate[["Moran I statistic"]],
    expected_i = moran$estimate[["Expectation"]],
    p_value = moran$p.value
  )
}

moran_results_df <- bind_rows(moran_results)

view(moran_results_df)
write.csv(moran_results_df, "/Users/cmantegna/Documents/Github/WDFWmussels/output/moran_analyte.csv", row.names = FALSE)

# metals
metals_wide <- metals_long %>%
  select(site_name, latitude, longitude, analyte, dry_value) %>%
  pivot_wider(names_from = analyte, values_from = dry_value)

df_sf <- st_as_sf(metals_wide, coords = c("longitude", "latitude"), crs = 4326)

# List of metals
metals <- colnames(df_sf)[!(colnames(df_sf) %in% c("site_name", "geometry"))]

# Create neighbor list
coords <- st_coordinates(df_sf)
nb <- knn2nb(knearneigh(coords, k = 4))
lw <- nb2listw(nb, style = "W")

moran_results <- list()

for (metal in metals) {
  values <- df_sf[[metal]]
  
  if (sum(!is.na(values)) >= 4 && length(unique(values[!is.na(values)])) > 1) {
    moran <- moran.test(values, lw)
    
    moran_results[[metal]] <- data.frame(
      metal = metal,
      moran_i = moran$estimate[["Moran I statistic"]],
      expected_i = moran$estimate[["Expectation"]],
      sd = if ("Standard deviate" %in% names(moran$estimate)) moran$estimate[["Standard deviate"]] else NA,
      p_value = moran$p.value
    )
  } else {
    message(paste("Skipping", metal, "- not enough variation or data."))
  }
}

moran_metals_df <- bind_rows(moran_results) %>%
  mutate(
    sig = case_when(
      p_value <= 0.001 ~ "***",
      p_value <= 0.01 ~ "**",
      p_value <= 0.05 ~ "*",
      TRUE ~ ""
    )
  )
view(moran_metals_df)
write.csv(moran_results_df, "/Users/cmantegna/Documents/Github/WDFWmussels/output/moran_metal.csv", row.names = FALSE)

```



