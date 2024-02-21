
# I'm trying not to use Google API, reasoning: https://search.r-project.org/CRAN/refmans/ggmap/html/register_google.html

# These packages have failed to work or require a Google API or require support from other packages listed here that are no longer being supported.

#install.packages("tmap", dependencies= TRUE)
#install.packages("ggspatial")
#install.packages(c("cowplot", "googleway", "ggrepel", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
#install.packages("rnaturalearth")

# Failed attempt: Haven't figured out how to call and use the shp file, this package is not working
#ps<- tm_shape(shapefile= "CityUGA.shp")

library("ggplot2")

# read in table of values
# Note for data adjustment I made: 2 sites were not documented, CPS_KM Kingston Marina and SAM-1042 Squaxin Island, both sites were marked with a 0 for ease of data plotting
location<- read.csv("mussel.csv")
head(location)

# create plotting df
# gotta add site names
latitude <- location[,5]
longitude <- location[,6]
cage <- data.frame(latitude, longitude)

# plot my cage retrieval sites
ggplot(cage, aes(x=latitude, y=longitude)) +
  geom_point() +
  xlab("Latitude") +
  ylab("Longitude")



# Failed attempt: Trying to plot Puget Sound using website: https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

#library("rnaturalearth")
#library("rnaturalearthdata")

#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#library("ggspatial")
#ggplot(data = world) +
#  geom_sf() +
#  coord_sf(xlim = c(122.4713, 47.7237), ylim = c(7.65, 33.97), expand = FALSE)


# Failed attempt: Going to try via: https://r-graph-gallery.com/168-load-a-shape-file-into-r.html#:~:text=Shapefiles%20are%20a%20common%20way,base%20R%20or%20with%20ggplot2%20.&text=If%20you%20did%20not%20find,information%20elsewhere%20on%20the%20web.

#install.packages("rgdal", dependencies= TRUE)
#package does not exist anymore
  
#Trying the ggplot layering method: https://ggplot2.tidyverse.org/reference/coord_map.html

library(maps)

#Puget Sound coordinates: 47.7237° N, 122.4713° W

ggplot(cage, aes(x=latitude, y=longitude)) +
    geom_point() +
    coord_quickmap(xlim = 122.4713, ylim =  47.7237, expand = TRUE, clip = TRUE)
  xlab("Latitude") +
    ylab("Longitude")

map_data(map, region = "Puget Sound", exact = FALSE )
           
maps::map("Washington")

  
  
  if (require("maps")) {
    nz <- map_data("nz")
    # Prepare a map of NZ
    nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = "white", colour = "black")
    
    # Plot it in cartesian coordinates
    nzmap
  }
  
  
  if (require("maps")) {
    # With correct mercator projection
    nzmap + coord_map()
  }
  
  
  if (require("maps")) {
    # With the aspect ratio approximation
    nzmap + coord_quickmap()
  }
  
  
  if (require("maps")) {
    # Other projections
    nzmap + coord_map("azequalarea", orientation = c(-36.92, 174.6, 0))
  }
  
  
  if (require("maps")) {
    states <- map_data("state")
    usamap <- ggplot(states, aes(long, lat, group = group)) +
      geom_polygon(fill = "white", colour = "black")
    
    # Use cartesian coordinates
    usamap
  }
  
  
  if (require("maps")) {
    # With mercator projection
    usamap + coord_map()
  }
  
  
  if (require("maps")) {
    # See ?mapproject for coordinate systems and their parameters
    usamap + coord_map("gilbert")
  }
  
  
  if (require("maps")) {
    # For most projections, you'll need to set the orientation yourself
    # as the automatic selection done by mapproject is not available to
    # ggplot
    usamap + coord_map("orthographic")
  }
  
  
  if (require("maps")) {
    usamap + coord_map("conic", lat0 = 30)
  }
  
  
  if (require("maps")) {
    usamap + coord_map("bonne", lat0 = 50)
  }
  
  
  if (FALSE) {
    if (require("maps")) {
      # World map, using geom_path instead of geom_polygon
      world <- map_data("world")
      worldmap <- ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_path() +
        scale_y_continuous(breaks = (-2:2) * 30) +
        scale_x_continuous(breaks = (-4:4) * 45)

# Orthographic projection with default orientation (looking down at North pole)
      worldmap + coord_map("ortho")
    }
    
    if (require("maps")) {
      # Looking up up at South Pole
      worldmap + coord_map("ortho", orientation = c(-90, 0, 0))
    }
    
    if (require("maps")) {
      # Centered on New York (currently has issues with closing polygons)
      worldmap + coord_map("ortho", orientation = c(41, -74, 0))
    }
  }
  
















