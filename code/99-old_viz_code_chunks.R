# old cleaning steps that don't need to happen anymore
## fixing site names & trimming white space for chem data sets
```{r}

chlordane$site_name[chlordane$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
chlordane$site_name[chlordane$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
chlordane$site_name[chlordane$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
chlordane$site_name[chlordane$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
chlordane$site_name[chlordane$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
chlordane$site_name[chlordane$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
chlordane$site_name[chlordane$site_name == "S of Skunk Island"] <- "South of Skunk Island"
chlordane$site_name[chlordane$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

ddt$site_name[ddt$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
ddt$site_name[ddt$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
ddt$site_name[ddt$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
ddt$site_name[ddt$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
ddt$site_name[ddt$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
ddt$site_name[ddt$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
ddt$site_name[ddt$site_name == "S of Skunk Island"] <- "South of Skunk Island"
ddt$site_name[ddt$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

hch$site_name[hch$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
hch$site_name[hch$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
hch$site_name[hch$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
hch$site_name[hch$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
hch$site_name[hch$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
hch$site_name[hch$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
hch$site_name[hch$site_name == "S of Skunk Island"] <- "South of Skunk Island"
hch$site_name[hch$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

metal$site_name[metal$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
metal$site_name[metal$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
metal$site_name[metal$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
metal$site_name[metal$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
metal$site_name[metal$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
metal$site_name[metal$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
metal$site_name[metal$site_name == "S of Skunk Island"] <- "South of Skunk Island"
metal$site_name[metal$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"
#metal$analyte[metal$analyte == "mercuryTotal"] <- "mercury"
#metal$analyte[metal$analyte == "Zinc"] <- "zinc"

pbde$site_name[pbde$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
pbde$site_name[pbde$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
pbde$site_name[pbde$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
pbde$site_name[pbde$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
pbde$site_name[pbde$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
pbde$site_name[pbde$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
pbde$site_name[pbde$site_name == "S of Skunk Island"] <- "South of Skunk Island"
pbde$site_name[pbde$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

pcb$site_name[pcb$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
pcb$site_name[pcb$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
pcb$site_name[pcb$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
pcb$site_name[pcb$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
pcb$site_name[pcb$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
pcb$site_name[pcb$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
pcb$site_name[pcb$site_name == "S of Skunk Island"] <- "South of Skunk Island"
pcb$site_name[pcb$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

pesticide$site_name[pesticide$site_name == "Blair Waterway #2"] <- "Blair Waterway Two"
pesticide$site_name[pesticide$site_name == "Comm Bay Skookum"] <- "Commencement Bay Skookum"
pesticide$site_name[pesticide$site_name == "Comm Bay, Dick Gilmur Launch"] <- "Commencement Bay, Dick Gilmur Launch"
pesticide$site_name[pesticide$site_name == "Comm Bay, Milwaukee Waterway"] <- "Commencement Bay, Milwaukee Waterway"
pesticide$site_name[pesticide$site_name == "Meyer's Point - Henderson Inlet"] <- "Meyer's Point, Henderson Inlet"
pesticide$site_name[pesticide$site_name == "Purdy - Dexters"] <- "Purdy, Dexters"
pesticide$site_name[pesticide$site_name == "S of Skunk Island"] <- "South of Skunk Island"
pesticide$site_name[pesticide$site_name == "Suquamish, Stormwater Outfall"] <- "Suquamish Stormwater Outfall"

metrics <- metrics %>%
  mutate(across(where(is.character), str_trim))

chlordane <- chlordane %>%
  mutate(across(where(is.character), str_trim))

ddt <- ddt %>%
  mutate(across(where(is.character), str_trim))

hch <- hch %>%
  mutate(across(where(is.character), str_trim))

metal <- metal %>%
  mutate(across(where(is.character), str_trim))

pbde <- pbde %>%
  mutate(across(where(is.character), str_trim))

pcb <- pcb %>%
  mutate(across(where(is.character), str_trim))

pesticide <- pesticide %>%
  mutate(across(where(is.character), str_trim))

```

## Completed 5-5-2025; Only run if rebuilding
### replace undetected analyte values 
```{r}

chlordane<- chlordane %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

ddt<- ddt %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

hch<- hch %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

metal<- metal %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

pbde<- pbde %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

pcb<- pcb %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

pesticide<- pesticide %>%
  mutate(flagged = grepl("U|B|I", qualifier),
         wet_value = ifelse(flagged, 0, wet_value))

```

## Completed 5-5-2025; Only run if rebuilding
### pivoting analytes
```{r}

chlordane <- chlordane %>%
  select(site_name, latitude, longitude, analyte, wet_value)  # keep only what's needed
chlordane_wide <- chlordane %>%
  pivot_wider(
    names_from = analyte,
    values_from = wet_value
  )

# Save the cleaned wide-format table
#write.csv(chlordane_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/chlordane_wide.csv", row.names = FALSE)

ddt <- ddt %>%
  select(site_name, latitude, longitude, analyte, wet_value) 
ddt_wide <- ddt %>%
  pivot_wider(
    names_from = analyte,      
    values_from = wet_value    
  )
#write.csv(ddt_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/ddt_wide.csv", row.names = FALSE)

hch <- hch %>%
  select(site_name, latitude, longitude, analyte, wet_value)  
hch_wide <- hch %>%
  pivot_wider(
    names_from = analyte,      
    values_from = wet_value    
  )
#write.csv(hch_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/hch_wide.csv", row.names = FALSE)

metal <- metal %>%
  select(site_name, latitude, longitude, analyte, wet_value)  
metal_wide <- metal %>%
  pivot_wider(
    names_from = analyte,      
    values_from = wet_value    
  )
#write.csv(metal_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/metal_wide.csv", row.names = FALSE)

pbde <- pbde %>%
  select(site_name, latitude, longitude, analyte, wet_value)  
pbde_wide <- pbde %>%
  pivot_wider(
    names_from = analyte,      
    values_from = wet_value   
  )
#write.csv(pbde_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/pbde_wide.csv", row.names = FALSE)

pcb <- pcb %>%
  select(site_name, latitude, longitude, analyte, wet_value)  
pcb_wide <- pcb %>%
  pivot_wider(
    names_from = analyte,      
    values_from = wet_value    
  )
#write.csv(pcb_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/pcb_wide.csv", row.names = FALSE)

pesticide <- pesticide %>%
  select(site_name, latitude, longitude, analyte, wet_value)  # keep only what's needed
pesticide_wide <- pesticide %>%
  pivot_wider(
    names_from = analyte,      # this becomes the new column names
    values_from = wet_value    # this fills the values
  )
#write.csv(pesticide_wide, "/Users/cmantegna/Documents/Github/WDFWmussels/data/indices/pesticide_wide.csv", row.names = FALSE)

```

## check for outliers, metrics df
```{r}

library(purrr)
library(rlang)

metrics_df<- metrics
metric_cols <- c("p450", "sod", "shell", "length_mm", "height_mm", "width_mm", 
                 "ci1", "ci2", "ci3", "weight_initial_g", "weight_final_g", "weight_change_g") #define what we're assessing

# function to assess & label 'in' or 'out'
flag_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ifelse(x < lower | x > upper, "out", "in")
}

# loop and add columns to df
metrics_df <- metrics_df %>%
  bind_cols(
    map_dfc(metric_cols, function(col) {
      out_flag <- flag_outliers(metrics_df[[col]])
      new_col <- paste0(col, "_quantile")
      tibble(!!new_col := out_flag)
    })
  )

# summary table
outlier_cols <- grep("_quantile$", names(metrics_df), value = TRUE) # pull the columns from metrics_df

# pivot and count
outlier_summary <- metrics_df %>%
  select(all_of(outlier_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "metric",
               values_to = "status") %>%
  mutate(metric = gsub("_quantile", "", metric)) %>%
  group_by(metric, status) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = count, values_fill = 0)

# view & save SAVED on 5.7.25
print(outlier_summary)
#write.csv(outlier_summary, "/Users/cmantegna/Documents/Github/WDFWmussels/data/index_df/metrics_outlier_summary_pre-transformation.csv", row.names = FALSE)

```

## transform, log2 based on the reference site - NOT CORRECT, DO NOT RERUN
```{r}

metric_cols <- c("p450", "sod", "shell", "length_mm", "height_mm", "width_mm", 
                 "ci1", "ci2", "ci3", "weight_initial_g", "weight_final_g", "weight_change_g")

ref_site <- "Penn Cove Reference"

# reference site mean
ref_means <- metrics_df %>%
  filter(site_name == ref_site) %>%
  summarise(across(all_of(metric_cols), mean, na.rm = TRUE))

# log2-transformation table
log2_df <- metrics_df %>%
  mutate(across(
    all_of(metric_cols),
    .fns = ~ log2(.x / ref_means[[cur_column()]]),
    .names = "log2_{.col}"
  ))

# view & save
print(log2_df)
#write.csv(log2_df, "/Users/cmantegna/Documents/Github/WDFWmussels/data/index_df/metrics_outlier_summary_post-transformation.csv", row.names = FALSE)

```

## check for outliers in log2_df
```{r}

library(purrr)
library(rlang)

metric_cols <- c("log2_p450", "log2_sod", "log2_shell", "log2_length_mm", "log2_height_mm", "log2_width_mm", 
                 "log2_ci1", "log2_ci2", "log2_ci3", "log2_weight_initial_g", "log2_weight_final_g", "log2_weight_change_g") 

# function to assess & label 'in' or 'out'
flag_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ifelse(x < lower | x > upper, "out", "in")
}

# loop and add columns to df
log2_df <- log2_df %>%
  bind_cols(
    map_dfc(metric_cols, function(col) {
      out_flag <- flag_outliers(log2_df[[col]])
      new_col <- paste0(col, "_quantile")
      tibble(!!new_col := out_flag)
    })
  )

# summary table
outlier_cols <- grep("_quantile$", names(log2_df), value = TRUE) # pull the columns from log2_df

# pivot and count
outlier_summary <- log2_df %>%
  select(all_of(outlier_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "metric",
               values_to = "status") %>%
  mutate(metric = gsub("_quantile", "", metric)) %>%
  group_by(metric, status) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = status, values_from = count, values_fill = 0)

# view & save. Completed on 5-7-25 
print(outlier_summary)
#write.csv(outlier_summary, "/Users/cmantegna/Documents/Github/WDFWmussels/data/index_df/metrics_outlier_summary_post-transformation.csv", row.names = FALSE)

```








### Correlation, Pearson Correlation

#### Interpretation - the biomarkers show a statistically significant *weak* negative correlation. Where one biomarker increases, the other decreases.

```{r}
# Correlation, Pearson Correlation

#add condition_factor back to the dataframe


#individual correlation test between the biomarkers

cor.test(averaged_data$avg_p450, averaged_data$avg_SOD)

```

```{r}
# Pearson Correlation Plot

library(corrplot)

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- tmp$p.value
      p.mat[j,i] <- tmp$p.value
    }
  }
  list(p = p.mat, conf.level = conf.level)
}

# Assuming 'mdata' and 'data' are defined and 'data' is used to subset 'mdata'
df2 <- averaged_data[sapply(averaged_data, is.numeric)] # Ensure 'mdata' is used here
df2 <- na.omit(df2)
M <- cor(df2)
testRes <- cor.mtest(df2, conf.level = 0.95)

# Visualization with corrplot
corrplot::corrplot(M,
                   method = "circle", type = "lower", insig = "blank",
                   addCoef.col = "black", number.cex = 0.8, order = "AOE", diag = FALSE
)

print(cor)
#ggsave(plot=cor, filename="/Users/cmantegna/Documents/WDFWmussels/output/avgpearson.png", width=15, height=8)

```

### PCA Plot

#### Interpretation - This plot has to be interpreted in space, each axis represents a different variance and the attahced percentage to each axis is the total variance captured by that axis; the x-axis is the max variation in the data and should have the higher percentage. The y-axis is the second most variation. The points are observations, more distance mean more difference. The plot is not particularly helpful.

```{r}

# PCA Plot with biomarkers
#install.packages("FactoMineR")
#install.packages("factoextra")
library('FactoMineR')
library("factoextra")


# Remove NAs from the dataset
df_clean <- na.omit(averaged_data)

# Selecting the relevant variables for PCA
pca_data <- df_clean[, c("avg_SOD", "avg_p450")]

# Performing PCA
pca_res <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

# Plotting the PCA
pcaplot<- fviz_pca_biplot(pca_res, label = "var", col.var = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE)  # Avoid text overlapping (slow if many points)



print(pcaplot)
#ggsave(plot=pcaplot, filename="/Users/cmantegna/Documents/WDFWmussels/output/pca.png", width=15, height=8)

```

### K-means cluster/ relationship test

#### Interpretation- there are three clusters that vary in size and that is an indicator of a significant number of outliers, see above for the code for the plot of the outliers. There are significant outliers for SOD, this may be rendering this test unhelpful. "The means of each cluster give you an idea of the centroid values around which the data points in each cluster are aggregated."

```{r,eval=TRUE}
#fixing data for kmeans

sum(is.na(averaged_data$avg_p450)) # Checks for NA values in p450
sum(is.nan(averaged_data$avg_p450)) # Checks for NaN values in p450
sum(is.infinite(averaged_data$avg_p450)) # Checks for Inf values in p450

sum(is.na(averaged_data$avg_SOD))
sum(is.nan(averaged_data$avg_SOD)) 
sum(is.infinite(averaged_data$avg_SOD)) 

#sum(is.na(mdata$condition_factor)) 
#sum(is.nan(mdata$condition_factor)) 
#sum(is.infinite(mdata$condition_factor)) 

averaged_data$avg_p450 <- as.numeric(averaged_data$avg_p450)
averaged_data$avg_SOD <- as.numeric(averaged_data$avg_SOD)
#mdata$condition_factor <- as.numeric(mdata$condition_factor)

# Remove rows with NA or NaN values in specified columns
clean_data <- averaged_data[complete.cases(averaged_data[, c("avg_p450", "avg_SOD")]), ]

# Ensure all data is numeric for the Inf check to make sense
clean_data <- transform(clean_data,
                        avg_p450 = as.numeric(avg_p450),
                        avg_SOD = as.numeric(avg_SOD))

# Now check for and handle Inf values
clean_data <- clean_data[!is.infinite(clean_data$avg_p450) & !is.infinite(clean_data$avg_SOD), ]

kmeans_result <- kmeans(clean_data[, c("avg_p450", "avg_SOD")], centers = 3)

print(kmeans_result)

```

# Mapping

```{r}
library(tidyr)
#library(tidyverse)
library(ggplot2)
library(vegan)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
```

```{r}
world <- ne_states(country = "united states of america", returnclass = "sf")
washington_map <- world[world$name == "Washington", ]

```

```{r}
# p450 map
pmap<- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_p450), size = 1) +
  scale_color_viridis(option = "C", name = "p450") +
  theme_minimal() +
  labs(title = "Washington State - p450 Data")

print(pmap)
#ggsave(plot=pmap, filename="/Users/cmantegna/Documents/WDFWmussels/output/avgp450map.png", width=15, height=8)
```

```{r}
#zoom into puget sound region
xlim <- c(-124, -122)  # longitude bounds
ylim <- c(47, 49)  # latitude bounds

pmap<- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_p450), size = 1) +
  scale_color_viridis(option = "C", name = "p450") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)+
  theme_minimal() +
  labs(title = "Washington State - p450 Data")

print(pmap)
#ggsave(plot=pmap, filename="/Users/cmantegna/Documents/WDFWmussels/output/avgp450psmap.png", width=15, height=8)
```

```{r}
#SOD map
smap<- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_SOD), size = 1) +
  scale_color_viridis(option = "C", name = "SOD") +
  theme_minimal() +
  labs(title = "Washington State - SOD Data")

print(smap)
#ggsave(plot=smap, filename="/Users/cmantegna/Documents/WDFWmussels/output/avgsodmap.png", width=15, height=8)

```

```{r}
#zoom into puget sound region & note the legend, lighter colors are higher values
xlim <- c(-124, -122)  # longitude bounds
ylim <- c(47, 49)  # latitude bounds

pmap<- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_SOD), size = 1) +
  scale_color_viridis(option = "C", name = "SOD") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)+
  theme_minimal() +
  labs(title = "Washington State - SOD Data")

print(pmap)
#ggsave(plot=pmap, filename="/Users/cmantegna/Documents/WDFWmussels/output/avgSODpsmap.png", width=15, height=8) 
```

```{r}
#mapping both
# Assuming washington_map is your sf object for Washington State
ggplot(data = washington_map) + 
  geom_sf(fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_p450, size = avg_SOD), alpha = 0.6) +
  scale_color_viridis(name = "avg_p450", option = "D") +
  scale_size_continuous(name = "avg_SOD", range = c(2, 6)) +
  theme_minimal() +
  labs(title = "Washington State: p450 and SOD")
```

```{r}
#still not a helpful visualization
xlim <- c(-124, -122)  # longitude bounds
ylim <- c(47, 49)  # latitude bounds

# Assuming washington_map is your sf object for Washington State
ggplot(data = washington_map) + 
  geom_sf(fill = "lightgrey", color = "white") +
  geom_point(data = averaged_data, aes(x = longitude, y = latitude, color = avg_p450, size = avg_SOD), alpha = 0.6) +
  scale_color_viridis(name = "avg_p450", option = "D") +
  scale_size_continuous(name = "avg_SOD", range = c(2, 6)) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)+
  theme_minimal() +
  labs(title = "Washington State: p450 and SOD")
```

# Spatial Analysis

### Note this workflow is used to determine if there are spatial data patterns that wouldn't stand out in the exploratory or relational statistics.

```{r}
# Data prep for geospatial analysis

#install.packages("spatstat")
library(spatstat)

# Create the kind of data frame that works in spatstat package
sites_pp <- ppp(x = averaged_data$longitude, y = averaged_data$latitude, 
                window = owin(xrange = range(averaged_data$longitude), 
                              yrange = range(averaged_data$latitude)))

```

```{r}
#Add biomarker layer to the geographical data
sites_pp$marks <- data.frame(p450 = averaged_data$avg_p450, 
                             SOD = averaged_data$avg_SOD)

```

### K test, 1 of 2.

#### Interpretation- the clustering of the black, red, and green lines (isotropic, and two transformations) lie significanlty above the blue line, the Poisson distribution line. This means that the biomarker data is more clustered than random. The proximity of the black, red and green lines show agreement across the isotropic and transformation analyses. This result is in agreement with the Shapiro- Wilkes tests for normality.

```{r}

# average values result's mirror the results returned in the full biomarker run. see 04-spatial for interpretation.
K_result <- Kest(sites_pp)
print(plot(K_result))

```

### Point Process Model (PPM) model, 2 of 2

#### Interpretation - this model measures the independence and intensity of the occurence of something in space or time based on whatever marks you determine. In this case the occurences are the data point locations, the intensity is the value of the marks and the marks are the biomarkers. This model is confusing to me and will be removed as I do not understand it well enough to interpret the results.

```{r}

# Fit a point process model
#ppm_model <- ppm(sites_pp ~ p450)
#summary(ppm_model)


#option 2
sites_pp$marks <- as.factor(cut(sites_pp$marks$p450, breaks = quantile(sites_pp$marks$p450, probs = 0:3/3), include.lowest = TRUE))

# Now run the ppm model
ppm_model <- ppm(sites_pp ~ marks, covariates = NULL)
print(summary(ppm_model))

```

# Spatial Autocorrelation Analysis, Global

### Note: there are global and local tests that assess the likelihood of the biomarker values to be randomly 'assigned'. My hypothesis is that the biomarkers values are not random.

```{r}
#load packages & prep dataframe for "sf" package

#install.packages(spdep)
#install.packages("/Users/cmantegna/Downloads/spdep_1.2-8.tar.gz", repos = NULL, type = "source")
library(sf)
library(sp)
library(spdep)

sf_data <- st_as_sf(averaged_data, coords = c("longitude", "latitude"), crs = 4326)

# Take a look at your sf object
print(sf_data)

```

```{r}

# Create a spatial weights matrix to assess nearest neighbors and distance-based neighbors to define spatial relationships using Moran's I and Geary's C.
# You can extract the matrix of coordinates using st_coordinates
coords <- st_coordinates(sf_data)

# Now use the knearneigh function from the spdep package directly on the coordinates
neighbors <- knn2nb(knearneigh(coords, k = 4))

# Then convert the neighbors into spatial weights with nb2listw
weights <- nb2listw(neighbors, style = "W")

```

### Moran's I and Geary's C determine if the distribution of the biomarker values across the entire Puget Sound region are random or not- random.

#### Interpretation -\ 

Moran's I statistic's, used for normally distributed data, ranges from 0 - 1. A value close to 0 mean randomness and a value close to 1 means less randomness. A statistic of .04 means points that are close to each other are more likely to have similar *p450* values than you would expect by chance but this is not statistically significant. The *SOD* values are random and not statistically significant.\
Geary's C statistic, used for non- normal data, ranges from 0-2. A value close to 0 is a positive spatial correlation, a value close to 1 is an indication of randomness, and a value close to 2 is an indication of dispersion (negative correlation). A statistic of .8 with a p-value below .05 shows a statistically significant weak indication of negative autocorrelation of *p450* values. The *SOD* values are just above 1 and are not statistically significant. Since Geary's C is influenced by "local" and "global' neighbors, this should not be used".

```{r}

## Run Moran's I and Geary's C for p450

moran_result <- moran.test(sf_data$avg_p450, weights)
print(moran_result)

geary_result <- geary.test(sf_data$avg_p450, weights)
print(geary_result)

```

```{r}

## Run Moran's I and Geary's C for SOD
moran_result <- moran.test(sf_data$avg_SOD, weights)
print(moran_result)

geary_result <- geary.test(sf_data$avg_SOD, weights)
print(geary_result)

```

# Spatial Regression Analysis
### Note: Each of the models analyzes if the observations (biomarker values) are not independent of each other but rather influenced by their location and the spatial arrangement.
```{r}
#library(spdep)
#install.packages("sphet")
library(sphet)

```

```{r}
# Spatial Lag Model. Is the value of one point influencing the value of a neighboring point? 
#Interpretation - No. No statistically significant result.

library(spatialreg)
#accounts for spatial dependence in the dependent variable
slm_model <- lagsarlm(avg_p450 ~ avg_SOD, data = sf_data, listw = weights)
print(summary(slm_model))

```

```{r}
# Spatial Error Model. This accounts for the spatial autocorrelation in the error term. Could statistical error account for results?
# No. There is a trend of a mild negative correlation in the data set but it is not statistically significant.
sem_model <- errorsarlm(avg_p450 ~ avg_SOD, data = sf_data, listw = weights)
print(summary(sem_model))


```

```{r}

# Spatial Durbin Model. An all-in-one model that combines both SLM and SEM. No expected result change.

#install.packages("spatialreg")
library(spatialreg)

#combines SLM and SEM
sdm_model <- spatialreg::lagsarlm(avg_p450 ~ avg_SOD, data = sf_data, listw = weights, type="mixed")
print(summary(sdm_model))

```
# Spatial Autocorrelation Analysis, Local
# Local Indicator of Spatial Association (LISA)
```{r}
# Prep data

#install.packages("spdep")
#install.packages("sf")  # for spatial data handling
#install.packages("tmap")  # for visualization
library(spdep)
library(sf)
library(tmap)

```

```{r}

lisa_values <- localmoran(sf_data$avg_p450, weights)

```

```{r}

# Add LISA values and p-values to your spatial data

sf_data$lisa <- lisa_values[,1]  # Local Moran's I values
sf_data$p.value <- lisa_values[,4]  # p-values for significance

# Use tmap for plotting
library(tmap)

# Define breaks for significance levels, e.g., 0.05 for 95% confidence
sig_breaks <- c(0, 0.05, 1)  # Change according to your significance level

# Create a map
tm_shape(sf_data) +
  tm_dots(col = "lisa", size = 0.5, palette = "-RdBu", title = "LISA Values") +
  tm_layout(legend.position = c("left", "top")) +
  tm_shape(sf_data[sf_data$p.value <= 0.05, ]) +  # Add a layer for significant points only
  tm_dots(col = "red", size = 0.7, title = "Significant Clusters") +
  tm_layout(main.title = "LISA Cluster Map", main.title.position = "center")

```



```{r}
# Plot the LISA data over the map from the base Washington State map found in file: 03-map.rmd

library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

```

#### Interpretation - LISA uses the global application of the Moran's I statistic. This is used to determine any point clustering or significant outliers. There are no sites that are clustered significantly. 
```{r}

world <- ne_states(country = "united states of america", returnclass = "sf")
washington_map <- world[world$name == "Washington", ]

pmap <- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  theme_minimal() +
  labs(title = "Washington State Map")

# Ensure CRS compatibility
sf_data <- st_transform(sf_data, st_crs(washington_map))

# Prepare the base map
pmap <- ggplot() + 
  geom_sf(data = washington_map, fill = "lightgrey", color = "white") +
  theme_minimal() +
  labs(title = "Washington State Map")

# Add the LISA points layer to the map
complete_map <- pmap + 
  geom_sf(data = sf_data, aes(color = lisa), size = 2) +
  scale_color_viridis_c(option = "D", direction = -1, name = "LISA Values")

# Display the combined map
print(complete_map)

```

```{r}
#zoom in on the puget sound region
# Assuming you know the bounding box coordinates you want to zoom in on
# For example: xmin, xmax, ymin, ymax
xlim <- c(-124, -122)  # longitude bounds
ylim <- c(47, 49)  # latitude bounds

complete_map <- pmap + 
  geom_sf(data = sf_data, aes(color = lisa), size = 2) +
  scale_color_viridis_c(option = "D", direction = -1, name = "LISA Values") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

print(complete_map)


```

# Analytes

```{r}
# Have to merge and reshape for a date frame that lets me compare biomarker and analytes in the models and in a PCA

library(reshape2)
#merge data frames and reshape for input.
colnames(pah)[colnames(pah) == "SiteName"] <- "site_name"
merged_df <- merge(averaged_data, pah, by = c("site_name"), all.x = TRUE)


#reshape to get the analytes into their own columns with the DryValue as their values
reshaped_df <- dcast(merged_df, site_name + site_number +latitude.x + longitude.x + avg_p450 + avg_SOD ~ Analyte, value.var = "DryValue")

print(reshaped_df)

```
## Linear Regression - only here to show that the non- normal data passed through this model is unsuccessful.
```{r}

# Linear Regression- p450 as response
lm_model <- lm(avg_p450 ~ avg_SOD + SumPAHs + SumPAHs16 + SumPAHs42_DMNcorrected + SumPAHsHMW + SumPAHsLMW, data = reshaped_df)
print(summary(lm_model))

```
## Generalized Linear Model - used for non- normal data.
#### Interpretation - All categories of PAH's show a statistically significant relationship to the p450 biomarker result.
```{r}

# GLM - p450 as response
glm_model <- glm(avg_p450 ~ avg_SOD + SumPAHs + SumPAHs16 + SumPAHs42_DMNcorrected + SumPAHsHMW + SumPAHsLMW, family = poisson(), data = reshaped_df)
print(summary(glm_model))

```

```{r}
# LR- p450 as response
#### Replicating with individual analytes to see if anything really sticks out.

#merge and reshape just like above
colnames(indv)[colnames(indv) == "SiteName"] <- "site_name"
merged_df2 <- merge(averaged_data, indv, by = c("site_name"), all.x = TRUE)


#reshape to get the analytes into their own columns with the DryValue as their values
reshaped2_df <- dcast(merged_df2, site_name + site_number +latitude + longitude + avg_p450 + avg_SOD ~ Analyte, value.var = "DryValue")

print(reshaped2_df)
```

```{r}
#get the column names so I don't have to individually type each one

all_columns <- names(reshaped2_df)

# Remove the columns you don't want to include in the model
excluded_columns <- c('avg_p450', 'latitude', 'longitude', 'avg_SOD', 'site_name', 'site_number', 'NA')
independent_columns <- all_columns[!all_columns %in% excluded_columns]

# Enclose each column name in backticks to handle special characters
independent_columns <- sapply(independent_columns, function(x) paste0("`", x, "`"))

# Create a string representing the formula
formula_str <- paste("avg_p450 ~", paste(independent_columns, collapse = " + "))

# Convert the string to a formula object
formula <- as.formula(formula_str)

# Now you can use this formula in your model
#lm_model <- lm(formula, data = reshaped_df)
#summary(lm_model)


```

```{r}

indvlm_model <- lm(formula, data = reshaped2_df)
print(summary(indvlm_model))

```

```{r}

# GLM- p450 as response
indvglm_model<- glm(formula, data = reshaped2_df, family = poisson())
print(summary(indvglm_model))

```
## PCA of biomarkers and analyte data
```{r}
#pca for pah and biomarkers, using reshaped_df
#install.packages("FactoMineR")
#install.packages("factoextra")
library('FactoMineR')
library('factoextra')

# Specify the columns you want to use for PCA
pca_columns <- c("avg_SOD", "avg_p450", "SumPAHs", "SumPAHs16", "SumPAHs42_DMNcorrected", "SumPAHsHMW", "SumPAHsLMW")

# Remove rows with NAs in the specified columns only
df_clean <- reshaped_df[complete.cases(reshaped_df[, pca_columns]), ]

# Selecting the relevant variables for PCA
pca_data <- df_clean[, pca_columns]

# Performing PCA
pca_res <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

# Plotting the PCA
analytepcaplot<- fviz_pca_biplot(pca_res, label = "var", col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE)  # Avoid text overlapping (slow if many points)



print(analytepcaplot)
ggsave(plot=analytepcaplot, filename="/Users/cmantegna/Documents/WDFWmussels/output/analytepca.png", width=15, height=8)

```

```{r}
# Clustering based on k-means results. Needs to be revisited to verify it is the best option for this plot.

#different pca option
library(FactoMineR)
library(factoextra)

# Assuming pca_data is prepared for PCA
pca_result <- PCA(pca_data, graph = FALSE)

# If you need to perform clustering (e.g., k-means)
set.seed(123)  # for reproducibility
clusters <- kmeans(pca_data, centers = 3)  # change centers according to your data

# Add the cluster assignments to your PCA data
pca_data$cluster <- as.factor(clusters$cluster)

# Now plot the PCA with fviz_pca_ind
fviz_pca_ind(pca_result,
             col.ind = pca_data$cluster, # color by cluster
             palette = c("#00AFBB", "#E7B800", "#FC4E07"), # customize colors
             addEllipses = TRUE, # add confidence ellipses around clusters
             ellipse.level = 0.95, # 95% confidence ellipse
             legend.title = "Cluster")

```






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



