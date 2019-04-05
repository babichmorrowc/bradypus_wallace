# load packages
library(dismo)
library(ggmap)
library(rgeos)
api_key = "AIzaSyBK7lLbqoqnYFdzf-idYYposb-1gwyRAlQ"
register_google(key = api_key)

# Literature data ---------------------------------------------------------

dataDir = '/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Occurrence_Data/'

# Species occurrence coordinates
variegatus_lit <- read.csv(paste0(dataDir,'Bradypus_variegatus_litdata.csv'))
tridactylus_lit <- read.csv(paste0(dataDir,'Bradypus_tridactylus_litdata.csv'))
torquatus_lit <- read.csv(paste0(dataDir,'Bradypus_torquatus_litdata.csv'))


# GBIF data ---------------------------------------------------------------

variegatus_gbif <- gbif(genus = "Bradypus", species = "variegatus", geo = TRUE)
tridactylus_gbif <- gbif(genus = "Bradypus", species = "tridactylus", geo = TRUE)
torquatus_gbif <- gbif(genus = "Bradypus", species = "torquatus", geo = TRUE)

variegatus_gbif <- variegatus_gbif[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")]
tridactylus_gbif <- tridactylus_gbif[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")]
torquatus_gbif <- torquatus_gbif[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")]

names(variegatus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")
names(tridactylus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")
names(torquatus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "institutionCode")

# remove NAs
variegatus_gbif <- variegatus_gbif[!is.na(variegatus_gbif$longitude), ]
tridactylus_gbif <- tridactylus_gbif[!is.na(tridactylus_gbif$longitude), ]
torquatus_gbif <- torquatus_gbif[!is.na(torquatus_gbif$longitude), ]

# Visualize data ----------------------------------------------------------

SA_bbox <- make_bbox(lon = c(-97, -25), lat = c(-25,20), f = 0.1)
SA_map <- get_map(location = SA_bbox, source = "google", maptype = "satellite")

# plot data from literature (black) and points from GBIF (red)

ggmap(SA_map) +
  geom_point(data = variegatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = variegatus_gbif, aes(x = longitude, y = latitude), color = "red")

ggmap(SA_map) +
  geom_point(data = tridactylus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = tridactylus_gbif, aes(x = longitude, y = latitude), color = "red")

ggmap(SA_map) +
  geom_point(data = torquatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = torquatus_gbif, aes(x = longitude, y = latitude), color = "red")

# MCP ---------------------------------------------------------------------

# function to make a minimum convex polygon as SpatialPolygons object
mcp <- function(xy) {
  xy <- as.data.frame(sp::coordinates(xy))
  coords.t <- chull(xy[, 1], xy[, 2])
  xy.bord <- xy[coords.t, ]
  xy.bord <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
  return(sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(as.matrix(xy.bord))), 1))))
}

# create MCP for literature data from all three species
var_mcp <- mcp(variegatus_lit[,2:3])
tri_mcp <- mcp(tridactylus_lit[,2:3])
tor_mcp <- mcp(torquatus_lit[,2:3])

# visualize points and MCP

ggmap(SA_map) +
  geom_point(data = variegatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = variegatus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = fortify(var_mcp), aes(x = long, y = lat), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = tridactylus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = tridactylus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = fortify(tri_mcp), aes(x = long, y = lat), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = torquatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = torquatus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = fortify(tor_mcp), aes(x = long, y = lat), color = "black", alpha = 0)

# Buffered points ---------------------------------------------------------

# make SpatialPoints objects for buffering
var_sp <- SpatialPoints(variegatus_lit[,2:3])
tri_sp <- SpatialPoints(tridactylus_lit[,2:3])
tor_sp <- SpatialPoints(torquatus_lit[,2:3])

var_buffer <- gBuffer(var_sp, width = 4)
tri_buffer <- gBuffer(tri_sp, width = 4)
tor_buffer <- gBuffer(tor_sp, width = 4)

# visualize points and buffered region
plot(var_buffer)
points(variegatus_lit[,2:3], pch = 19, cex = 0.5)
points(variegatus_gbif[,2:3], pch = 19, cex = 0.5, col = "red")

plot(tri_buffer)
points(tridactylus_lit[,2:3], pch = 19, cex = 0.5)
points(tridactylus_gbif[,2:3], pch = 19, cex = 0.5, col = "red")

plot(tor_buffer)
points(torquatus_lit[,2:3], pch = 19, cex = 0.5)
points(torquatus_gbif[,2:3], pch = 19, cex = 0.5, col = "red")

ggmap(SA_map) +
  geom_point(data = torquatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = torquatus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = tor_buffer, aes(x = long, y = lat), color = "black", alpha = 0)

