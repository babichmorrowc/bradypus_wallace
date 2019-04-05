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

variegatus_gbif_og <- gbif(genus = "Bradypus", species = "variegatus", geo = TRUE)
tridactylus_gbif_og <- gbif(genus = "Bradypus", species = "tridactylus", geo = TRUE)
torquatus_gbif_og <- gbif(genus = "Bradypus", species = "torquatus", geo = TRUE)

variegatus_gbif <- variegatus_gbif_og[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")]
tridactylus_gbif <- tridactylus_gbif_og[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")]
torquatus_gbif <- torquatus_gbif_og[,c("species", "lon", "lat", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")]

names(variegatus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")
names(tridactylus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")
names(torquatus_gbif) <- c("name", "longitude", "latitude", "basisOfRecord", "collectionCode", "coordinateUncertaintyInMeters", "datasetName", "dateIdentified", "year", "institutionCode")

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


# Buffered points ---------------------------------------------------------

# make SpatialPoints objects for buffering
var_sp <- SpatialPoints(variegatus_lit[,2:3])
tri_sp <- SpatialPoints(tridactylus_lit[,2:3])
tor_sp <- SpatialPoints(torquatus_lit[,2:3])

var_buffer <- gBuffer(var_sp, width = 1)
tri_buffer <- gBuffer(tri_sp, width = 1)
tor_buffer <- gBuffer(tor_sp, width = 1)

# visualize points and buffered region
ggmap(SA_map) +
  geom_point(data = variegatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = variegatus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = var_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = tridactylus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = tridactylus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = tri_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = torquatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = torquatus_gbif, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = tor_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

# Intersect ---------------------------------------------------------------

# make SpatialPoints objects for intersecting
var_gbif_sp <- SpatialPoints(variegatus_gbif[,2:3])
tri_gbif_sp <- SpatialPoints(tridactylus_gbif[,2:3])
tor_gbif_sp <- SpatialPoints(torquatus_gbif[,2:3])

# intersect GBIF points and buffered region
var_intersect <- over(var_gbif_sp, var_buffer)
tri_intersect <- over(tri_gbif_sp, tri_buffer)
tor_intersect <- over(tor_gbif_sp, tor_buffer)

#find the row numbers of the occurrence points that fall within the polygon:
var_intersect <- as.numeric(which(!(is.na(var_intersect))))
tri_intersect <- as.numeric(which(!(is.na(tri_intersect))))
tor_intersect <- as.numeric(which(!(is.na(tor_intersect))))

#Create a new dataframe of sloth occurrences containing only occurrences within your polygon
variegatus_gbif_buffer <- variegatus_gbif[var_intersect,]
tridactylus_gbif_buffer <- tridactylus_gbif[tri_intersect,]
torquatus_gbif_buffer <- torquatus_gbif[tor_intersect,]

# visualize points and buffered region
ggmap(SA_map) +
  geom_point(data = variegatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = variegatus_gbif_buffer, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = var_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = tridactylus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = tridactylus_gbif_buffer, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = tri_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = torquatus_lit, aes(x = longitude, y = latitude)) +
  geom_point(data = torquatus_gbif_buffer, aes(x = longitude, y = latitude), color = "red") +
  geom_polygon(data = tor_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)


# Further data cleaning ---------------------------------------------------

# visualize points and buffered region
ggmap(SA_map) +
  geom_point(data = variegatus_gbif_buffer[variegatus_gbif_buffer$basisOfRecord == "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "red") +
  geom_point(data = variegatus_gbif_buffer[variegatus_gbif_buffer$basisOfRecord != "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "green") +
  geom_polygon(data = var_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = tridactylus_gbif_buffer[tridactylus_gbif_buffer$basisOfRecord == "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "red") +
  geom_point(data = tridactylus_gbif_buffer[tridactylus_gbif_buffer$basisOfRecord != "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "green") +
  geom_polygon(data = tri_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

ggmap(SA_map) +
  geom_point(data = torquatus_gbif_buffer[torquatus_gbif_buffer$basisOfRecord == "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "red") +
  geom_point(data = torquatus_gbif_buffer[torquatus_gbif_buffer$basisOfRecord != "HUMAN_OBSERVATION",], aes(x = longitude, y = latitude), color = "green") +
  geom_polygon(data = tor_buffer, aes(x = long, y = lat, group = group), color = "black", alpha = 0)

# Find forest cover values ------------------------------------------------

getSPOT <- function(path = "/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/",
                    year, layer = "fcover"){
  # layer can be one of: fcover, fcover_err, nland, ngood, nmeanvalid,
  # ncumvalid, surfaceflag, retrievalflag
  data <- raster(paste0(path, year, "/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__",
                        year, "0110__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4"),
                 varname = layer)
  return(data)
}

getMODIS <- function(path = "/Users/hellenfellows/Desktop/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2001-2017__fv0.02_90W-30W_30S-20N.nc4/",
                     year, layer = "landcover_igbp"){
  # layer can be one of: landcover_igbp, confidence_igbp, water_igbp,
  # evergreen_needleleaf_forest_igbp, evergreen_broadleaf_forest_igbp,
  # deciduous_needleleaf_forest_igbp, deciduous_broadleaf_forest_igbp,
  # mixed_forest_igbp, closed_shrublands_igbp, open_shrublands_igbp,
  # woody_savannas_igbp, savannas_igbp, grasslands_igbp, permanent_wetlands_igbp,
  # croplands_igbp, urban_and_builtup_igbp, cropland_natural_vegetation_mosaic_igbp,
  # snowandice_igbp, barren_sparsely_vegetated_igbp, landcover_lai,
  # confidence_lai, water_lai, grasslands_lai, shrublands_lai,
  # broadleaf_croplands_lai, savannas_lai, evergreen_broadleaf_forests_lai,
  # deciduous_broadleaf_forests_lai, evergreen_needleleaf_forests_lai,
  # deciduous_needleleaf_forests_lai, non_vegetated_lai, urban_and_builtup_lai
  data <- raster(paste0(path, "MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__",
                        year, "_fv0.02_270.000E-330.000E_30.0000S-20.0000N.nc4"),
                 varname = layer)
  return(data)
}





