# Verifying K-Means Cluster Counts - MOVE to 12-viz
```{r}

set.seed(123)
km2 <- kmeans(geo_data, centers = 2, nstart = 25)
km6 <- kmeans(geo_data, centers = 6, nstart = 25)
km9 <- kmeans(geo_data, centers = 9, nstart = 25)


# Add new clusters
data$geo_cluster_2 <- as.factor(km2$cluster)
data$geo_cluster_6 <- as.factor(km6$cluster)
data$geo_cluster_9 <- as.factor(km9$cluster)

# Plot- map without the map underlay
ggplot(data, aes(x = longitude, y = latitude, color = geo_cluster_2)) +
  geom_point(size = 3) + ggtitle("Geographic Clusters (k=4)") +
  xlab("Longitude") + ylab("Latitude")

ggplot(data, aes(x = longitude, y = latitude, color = geo_cluster_6)) +
  geom_point(size = 3) + ggtitle("Geographic Clusters (k=4)") +
  xlab("Longitude") + ylab("Latitude")

ggplot(data, aes(x = longitude, y = latitude, color = geo_cluster_9)) +
  geom_point(size = 3) + ggtitle("Geographic Clusters (k=5)") +
  xlab("Longitude") + ylab("Latitude")


```

# Mapping Clusters - MOVE this to 12-viz
## moving forward with K-Means 4 & 5 + Hierarchical 4 & 5
```{r}

library(sf)
library(ggmap, 'maps')
library("cowplot")

usa <- map_data("usa")
gg1 <-ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) 

states <- map_data("state")
head(states)

wa_df <- subset(states, region== "washington")
head(wa_df)

counties <- map_data("county")
wa_county <- subset(counties, region== "washington")
head(wa_county)

#### troubleshooting why the combined plot is not working
str(wa_df)      # Ensure 'group' is numeric or factor
summary(wa_df)
str(data)  # Ensure 'longitude', 'latitude', and 'geo_hclust3' exist
summary(data)  # Check for missing values
"group" %in% colnames(data)  # Should return FALSE
class(data)  # Should return "data.frame"

print(wa_base + geom_point(data = data, aes(x = longitude, y = latitude, color = geo_hclust3), size = 3))


#data$geo_hclust3<- as.factor(data$geo_hclust3) 
#### end of troubleshooting

wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = as.factor(group))) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
wa_base + theme_nothing()

print(wa_base)

#hc 3
wa_base + 
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(geo_hclust3)), size = 3, inherit.aes = FALSE) +
  ggtitle("Geographic Clusters of Sampling Sites")

#hc 4
wa_base + 
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(geo_hclust4)), size = 3, inherit.aes = FALSE) +
  ggtitle("Geographic Clusters of Sampling Sites")

#group 4
wa_base + 
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(geo_cluster_4)), size = 3, inherit.aes = FALSE) +
  ggtitle("Geographic Clusters of Sampling Sites")

#group 5
wa_base + 
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(geo_cluster_5)), size = 3, inherit.aes = FALSE) +
  ggtitle("Geographic Clusters of Sampling Sites")

# group 6
wa_base + 
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(geo_cluster_6)), size = 3, inherit.aes = FALSE) +
  ggtitle("Geographic Clusters of Sampling Sites")

```

# Write out the data df with all of the geo groups
```{r}

getwd()

write.csv(data, "/Users/cmantegna/Documents/GitHub/WDFWmussels/data/cleaned/full_data_all_geo_groups.csv")

```
