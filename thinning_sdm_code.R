# load packages
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)
library(ggmap)
library(alphahull)

# Import occurrence data --------------------------------------------------

# Literature data
d.occs <- '/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Occurrence_Data/'
# create path to user occurrences csv file
var_userOccs.path <- file.path(d.occs, "Bradypus_variegatus_litdata.csv")
tri_userOccs.path <- file.path(d.occs, "Bradypus_tridactylus_litdata.csv")
# read in csv
var_userOccs.csv <- read.csv(var_userOccs.path, header = TRUE)
tri_userOccs.csv <- read.csv(tri_userOccs.path, header = TRUE)
# remove rows with duplicate coordinates
var_occs.dups <- duplicated(var_userOccs.csv[c('longitude', 'latitude')])
var_occs <- var_userOccs.csv[!var_occs.dups,]
tri_occs.dups <- duplicated(tri_userOccs.csv[c('longitude', 'latitude')])
tri_occs <- tri_userOccs.csv[!var_occs.dups,]
# remove NAs
var_occs <- var_occs[complete.cases(var_occs$longitude, var_occs$latitude), ]
tri_occs <- tri_occs[complete.cases(tri_occs$longitude, tri_occs$latitude), ]
# give all records a unique ID
var_occs$occID <- row.names(var_occs)
tri_occs$occID <- row.names(tri_occs)

# Mapping -----------------------------------------------------------------

#Use Google maps
api_key = 
register_google(key = api_key)

#Satellite map
bbox <- make_bbox(lon = var_occs$longitude, lat = var_occs$latitude, f = 0.2)
map <- get_map(location = bbox, source = "google", maptype = "hybrid")
ggmap(map)
ggmap(map) +
  geom_point(data = var_occs, aes(x=longitude, y=latitude), color = "darkorange1") +
  geom_point(data = tri_occs, aes(x = longitude, y = latitude), color = "deepskyblue")

# Process occurrence data -------------------------------------------------

#Spatial thinning to 40 km for variegatus, 10 km for tridactylus
var_output <- spThin::thin(var_occs, 'latitude', 'longitude', 'name', thin.par = 40, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)
tri_output <- spThin::thin(tri_occs, 'latitude', 'longitude', 'name', thin.par = 10, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)

# find the iteration that returns the max number of occurrences
var_maxThin <- which(sapply(var_output, nrow) == max(sapply(var_output, nrow)))
tri_maxThin <- which(sapply(tri_output, nrow) == max(sapply(tri_output, nrow)))
# if there's more than one max, pick the first one
var_maxThin <- var_output[[ifelse(length(var_maxThin) > 1, var_maxThin[1], var_maxThin)]]  
tri_maxThin <- tri_output[[ifelse(length(tri_maxThin) > 1, tri_maxThin[1], tri_maxThin)]] 
# subset occs to match only thinned occs
thinned_var_occs <- var_occs[as.numeric(rownames(var_maxThin)),]  
thinned_tri_occs <- tri_occs[as.numeric(rownames(tri_maxThin)),]

#points deleted through thinning
discard_var_occs <- var_occs[-as.numeric(rownames(var_maxThin)),]
discard_tri_occs <- tri_occs[-as.numeric(rownames(tri_maxThin)),]

# visualize the results of thinning

ggmap(map) +
  geom_point(data = thinned_var_occs, aes(x = longitude, y = latitude), color = "darkgoldenrod2") +
  geom_point(data = discard_var_occs, aes(x=longitude, y=latitude), color = "red", alpha = 0.75)

ggmap(map) +
  geom_point(data = thinned_tri_occs, aes(x = longitude, y = latitude), color = "purple1") +
  geom_point(data = discard_tri_occs, aes(x=longitude, y=latitude), color = "red", alpha = 0.75)

# Obtain environmental data -----------------------------------------------

grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
#get extent
combine.lat <- c(var_occs$latitude, tri_occs$latitude)
combine.lon <- c(var_occs$longitude, tri_occs$longitude)
ext_sloths <- extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))
Env_sloths <- crop(envs, ext_sloths)

# extract environmental values at occ grid cells
thinned_var_locs.vals <- raster::extract(envs[[1]], thinned_var_occs[, c('longitude', 'latitude')])
thinned_tri_locs.vals <- raster::extract(envs[[1]], thinned_tri_occs[, c('longitude', 'latitude')])

# remove occs without environmental values
thinned_var_occs <- thinned_var_occs[!is.na(thinned_var_locs.vals), ]
thinned_tri_occs <- thinned_tri_occs[!is.na(thinned_tri_locs.vals), ] 

# Process environmental data ----------------------------------------------


# Background from point buffering -----------------------------------------


# make SpatialPoints object for buffering
thinned_var_occs.xy <- thinned_var_occs[c('longitude', 'latitude')]
thinned_tri_occs.xy <- thinned_tri_occs[c('longitude', 'latitude')]

sp::coordinates(thinned_var_occs.xy) <- ~ longitude + latitude
sp::coordinates(thinned_tri_occs.xy) <- ~ longitude + latitude

#Buffer by 1 degree around points
var_bgExt_1 <- rgeos::gBuffer(thinned_var_occs.xy, width = 1)
tri_bgExt_1 <- rgeos::gBuffer(thinned_tri_occs.xy, width = 1)

# crop the environmental rasters by the background extent shape
var_envsBgCrop_1 <- raster::crop(envs, var_bgExt_1)
tri_envsBgCrop_1 <- raster::crop(envs, tri_bgExt_1)
# mask the background extent shape from the cropped raster
var_envsBgMsk_1 <- raster::mask(var_envsBgCrop_1, var_bgExt_1)
tri_envsBgMsk_1 <- raster::mask(tri_envsBgCrop_1, tri_bgExt_1)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_1, aes(x = long, y = lat, group = group), fill = "darkgoldenrod2", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_1, aes(x = long, y = lat, group = group), fill = "purple1", alpha = 0.7)


#Buffer by 2 degree around points
var_bgExt_2 <- rgeos::gBuffer(thinned_var_occs.xy, width = 2)
tri_bgExt_2 <- rgeos::gBuffer(thinned_tri_occs.xy, width = 2)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_2, aes(x = long, y = lat, group = group), fill = "darkgoldenrod2", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_2, aes(x = long, y = lat, group = group), fill = "purple2", alpha = 0.7)

#Buffer by 3 degree around points
var_bgExt_3 <- rgeos::gBuffer(thinned_var_occs.xy, width = 3)
tri_bgExt_3 <- rgeos::gBuffer(thinned_tri_occs.xy, width = 3)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_3, aes(x = long, y = lat, group = group), fill = "darkgoldenrod3", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_3, aes(x = long, y = lat, group = group), fill = "purple3", alpha = 0.7)


# Background from alpha hull ----------------------------------------------

var_bg_alpha20 <- ahull(x = thinned_var_occs$longitude, y = thinned_var_occs$latitude, alpha = 20)
plot(var_bg_alpha20)



#Visualize the background buffer
ggmap(map)
arc()  
