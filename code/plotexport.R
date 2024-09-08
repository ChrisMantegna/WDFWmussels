
# Load data
data<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/p450data.csv")
indv<- read.csv("/Users/cmantegna/Documents/WDFWmussels/data/avgP450_analytes.csv")

library(ggplot2)
library(dplyr)

# Create a unique identifier for each combination of site_name and PAHgroup8
pahbox <- data %>%
  mutate(site_PAHgroup_combined = paste(site_name, PAHgroup8, sep = "_"))

pahbox <- pahbox %>%
  arrange(desc(PAHgroup8), site_name)

# Order site_name by both PAHgroup8 and site_name to ensure no duplication
pahbox <- pahbox %>%
  mutate(site_name_ordered = factor(site_PAHgroup_combined,
                                    levels = unique(site_PAHgroup_combined)))


# Plot using the newly ordered site names
plot<- ggplot(pahbox, aes(x = site_name_ordered, y = p450, fill = as.factor(PAHgroup8))) +
  geom_boxplot() +
  labs(x = bquote("P450 Activity (Unit / mg"^"-1"~"protein)"), title = "P450 Activity by Site with PAH Grouping") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set1", name = "PAH Grouping") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

print(plot)
