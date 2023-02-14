#install.packages("tmap", dependencies= TRUE)
#install.packages("ggspatial")
#install.packages(c("cowplot", "googleway", "ggrepel", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages("rnaturalearth")
#trying not to use Google API, reasoning: https://search.r-project.org/CRAN/refmans/ggmap/html/register_google.html

library("ggplot2")

#haven't figured out how to call and use the shp file, this package is not working
#ps<- tm_shape(shapefile= "CityUGA.shp")

#read in table of values
#2 sites were not documented, CPS_KM Kingston Marina and SAM-1042 Squaxin Island, both sites were marked with a 0 for ease of data plotting
location<- read.csv("mussel.csv")
head(location)

#create plotting df
latitude <- location[,5]
longitude <- location[,6]
cage <- data.frame(latitude, longitude)

#plot my cage retrieval sites
cagePlot<- ggplot(cage, aes(x=latitude, y=longitude)) +
  geom_point() +
  coord_map()
  xlab("Latitude") +
  ylab("Longitude")

#trying to plot Puget Sound using website: https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

#library("rnaturalearth")
#library("rnaturalearthdata")

#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#library("ggspatial")
#ggplot(data = world) +
#  geom_sf() +
#  coord_sf(xlim = c(122.4713, 47.7237), ylim = c(7.65, 33.97), expand = FALSE)

#So far, the primary problem is that the packages/ pieces of packages are not longer supported
#Going to try via: https://r-graph-gallery.com/168-load-a-shape-file-into-r.html#:~:text=Shapefiles%20are%20a%20common%20way,base%20R%20or%20with%20ggplot2%20.&text=If%20you%20did%20not%20find,information%20elsewhere%20on%20the%20web.

#install.packages("rgdal", dependencies= TRUE)
#package does not exist anymore
  
#Trying the ggplot layering method: https://ggplot2.tidyverse.org/reference/coord_map.html
library(maps)

#Puget Sound coordinates: 47.7237° N, 122.4713° W




















