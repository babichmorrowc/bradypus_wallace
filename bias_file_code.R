#Load packages
library(spocc)
library(plyr)
library(viridis)
library(ggmap)
library(MASS)
library(ENMeval)
library(rgbif)

#provide API key
#api_key = 
register_google(key = api_key)


# Creating the bias layer -------------------------------------------------


#import csv of species list for bias file
#includes primates, sloths, margay, olingos, mountain coatis, coatis, and kinkajous
sp_list <- taxonomy <- read.csv("~/OneDrive - AMNH/Wallace/BiasFile/redlist_species_data_1c60e640-3b63-4897-a3c3-ae6a0c44df70/taxonomy.csv")
View(sp_list)
sp_list <- sp_list[sp_list$genusName != "Homo",]

#Create vector of species names
sp_vec <- paste(sp_list$genusName, sp_list$speciesName, sep = " ")

#Taxonomic resolution
sp_vec[1] <- "Lagothrix lagothricha"

#Get occurrences for the first species:
occ_1 <- occ(query = sp_vec[1], from = "gbif", limit = 1500)
df_1 <- occ2df(occ_1)
bg_locations <- df_1

for(i in 2:length(sp_vec)){
  occ_i <- occ(query = sp_vec[i], from = "gbif", limit = 1000)
  df_i <- occ2df(occ_i)
  if(length(df_i) == 0){
    print(paste(i, ": No observations in GBIF", sep = ""))
  } else{
    bg_locations <- rbind.fill(bg_locations, df_i)
  }
}


#remove rows with duplicated occurrences
bg.dups <- duplicated(bg_locations[c('longitude', 'latitude')])
bg_locations <- bg_locations[!bg.dups,]
#get rid of occurrences with NA values for latitude or longitude
bg_locations <- bg_locations[which(!is.na(bg_locations$latitude) | !is.na(bg_locations$longitude)),]
#remove locations with latitude/longitude values outside of extent
bg_locations <- subset(bg_locations, longitude > -180 & longitude <180 & latitude > -90 & latitude < 90)


#Plot the background points on world map
world_map <- map_data("world")
world <- ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill = "grey", color = "darkgrey")
#map background points on map
world +
  geom_point(data = bg_locations, aes(x = longitude, y = latitude),
             color = "green",
             size = 1)
#plot the background points in region around variegatus points
var_bound_box <- make_bbox(lon = variegatus$longitude, lat = variegatus$latitude, f = .5)
#Get a satellite map at the location of the bounding box:
var_bbox_map <- get_map(location = var_bound_box, maptype = "satellite", source = "google")
var_bbox_map2 <- get_googlemap(location = var_bound_box, style = 'feature:administrative.country|element:labels|visibility:off')
ggmap(var_bbox_map) + 
  geom_point(data = bg_locations, aes(x = longitude, y = latitude), 
             color = "red",
             size =1)

# Create a two column dataframe of the non-NA occurrence points
bg_coords <- bg_locations[c('longitude', 'latitude')]
#Create SpatialPoints object of occurrence points
bg_spatialpoints <- SpatialPoints(bg_coords[c("longitude", "latitude")])

#We need the occurrence raster to have the same resolution as our climate data
#First import climate data
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
brick <- brick(sta)
plot(sta[[1]], col = viridis(99))
points(bg_coords, col = "red", pch = 0.5)

#get extent
combine.lat <- c(variegatus$latitude, tridactylus$latitude)
combine.lon <- c(variegatus$longitude, tridactylus$longitude)
ext_sloths <- extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))
Env_sloths = crop(sta, ext_sloths)
plot(Env_sloths[[1]], col = viridis(99))
brick_sloths <- brick(Env_sloths)

#Make occurrence raster for background points
bg_raster <- rasterize(bg_spatialpoints, brick, field = 1)
length(na.omit(bg_raster@data@values))

#occurrence raster cropped to sloth extent
bg_raster_crop <- rasterize(bg_coords, brick_sloths, field = 1)
length(na.omit(bg_raster_crop@data@values))
plot(bg_raster_crop, col = "black")

#Making KDE
bg_presence <- which(values(bg_raster_crop) == 1)
bg_presence_locs <- coordinates(bg_raster_crop)[bg_presence,]
dens <- kde2d(bg_presence_locs[,1], bg_presence_locs[,2], n = c(nrow(bg_raster_crop), ncol(bg_raster_crop)))
dens_ras <- raster(dens)
#resample to make dens_ras the same resolution of brick_sloths
dens_ras <- resample(dens_ras, brick_sloths, method = "bilinear")
plot(dens_ras)

# mask bias file
#crop bias file to extent of sloth data
bias <- raster::mask(dens_ras, Env_sloths[[1]])
plot(bias)

#Save bias layer
writeRaster(bias, "sloth_bias_file.tif", overwrite = TRUE)

########################################################################################################

# Interrogating the bias layer --------------------------------------------


#Use rgbif::occ_data() to get the locality information with data source
rgbif_occ_1 <- occ_data(scientificName = sp_vec[1], limit = 1500)
rgbif_df_1 <- rgbif_occ_1$data
rgbif_bg_data <- rgbif_df_1

for(i in 2:length(sp_vec)){
  rgbif_occ_i <- occ_data(scientificName = sp_vec[i], limit = 1000)
  rgbif_df_i <- rgbif_occ_i$data
  if(length(rgbif_df_i) == 0){
    print(paste(i, ": No observations in GBIF", sep = ""))
  } else{
    rgbif_bg_data <- rbind.fill(rgbif_bg_data, rgbif_df_i)
  }
}

rgbif_bg_data <- as_data_frame(rgbif_bg_data)

#see datasets and record bases
unique(rgbif_bg_data$datasetName)

unique(rgbif_bg_data$basisOfRecord)
ggplot(data = rgbif_bg_data, aes(x = basisOfRecord)) + 
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90))
ggmap(var_bbox_map) + 
  geom_point(data = rgbif_bg_data, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  scale_color_viridis(option = "B", discrete = T)


#remove rows with duplicated occurrences
rgbif.dups <- duplicated(rgbif_bg_data[c('decimalLongitude', 'decimalLatitude')])
ggplot(data = rgbif_bg_data[rgbif.dups,], aes(x = basisOfRecord)) + 
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90))
rgbif_nodups_data <- rgbif_bg_data[!rgbif.dups,]
#get rid of occurrences with NA values for latitude or longitude
rgbif_nodups_data <- rgbif_nodups_data[which(!is.na(rgbif_nodups_data$decimalLatitude) | !is.na(rgbif_nodups_data$decimalLongitude)),]
#remove locations with latitude/longitude values outside of extent
rgbif_nodups_data <- subset(rgbif_nodups_data, decimalLongitude > -180 & decimalLongitude <180 & decimalLatitude > -90 & decimalLatitude < 90)

unique(rgbif_nodups_data$basisOfRecord)
ggplot(data = rgbif_nodups_data, aes(x = basisOfRecord)) + 
  geom_bar(stat = "count") +
  theme(axis.text.x = element_text(angle = 90))
ggmap(var_bbox_map) + 
  geom_point(data = rgbif_nodups_data, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  scale_color_viridis(option = "B", discrete = T)

ggmap(var_bbox_map) + 
  geom_point(data = rgbif_nodups_data[rgbif_nodups_data$basisOfRecord == "HUMAN_OBSERVATION",], aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  scale_color_viridis(option = "B", discrete = T)
ggmap(var_bbox_map) + 
  geom_point(data = rgbif_nodups_data[rgbif_nodups_data$basisOfRecord == "PRESERVED_SPECIMEN",], aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  scale_color_viridis(option = "B", discrete = T)


unique(rgbif_nodups_data$datasetName)
length(which(rgbif_nodups_data$datasetName == "iNaturalist research-grade observations"))
ggmap(var_bbox_map) + 
  geom_point(data = rgbif_nodups_data, aes(x = decimalLongitude, y = decimalLatitude), color = "black") +
  geom_point(data = rgbif_nodups_data[rgbif_nodups_data$datasetName == "iNaturalist research-grade observations",], aes(x = decimalLongitude, y = decimalLatitude), color = "red", alpha = 0.25)


# Remove iNaturalist data -------------------------------------------------


#remove iNaturalist observations
rgbif_noiNat <- rgbif_nodups_data[which(rgbif_nodups_data$datasetName != "iNaturalist research-grade observations"),]
ggmap(var_bbox_map) +
  geom_point(data = rgbif_noiNat, aes(x = decimalLongitude, y = decimalLatitude), color = "red")


# Create a two column dataframe of the non-NA occurrence points
noiNat_coords <- rgbif_noiNat[c('decimalLongitude', 'decimalLatitude')]
#Create SpatialPoints object of occurrence points
noiNat_spatialpoints <- SpatialPoints(rgbif_noiNat[c('decimalLongitude', 'decimalLatitude')])

#Make occurrence raster for points without iNaturalist
noiNat_raster <- rasterize(noiNat_spatialpoints, brick, field = 1)
length(na.omit(noiNat_raster@data@values))

#occurrence raster cropped to sloth extent
noiNat_raster_crop <- rasterize(noiNat_coords, brick_sloths, field = 1)
length(na.omit(noiNat_raster_crop@data@values))
plot(noiNat_raster_crop, col = "black")

#Making KDE
noiNat_presence <- which(values(noiNat_raster_crop) == 1)
noiNat_presence_locs <- coordinates(noiNat_raster_crop)[noiNat_presence,]
noiNat_dens <- kde2d(noiNat_presence_locs[,1], noiNat_presence_locs[,2], n = c(nrow(noiNat_raster_crop), ncol(noiNat_raster_crop)))
noiNat_dens_ras <- raster(noiNat_dens)
#resample to make dens_ras the same resolution of brick_sloths
noiNat_dens_ras <- resample(noiNat_dens_ras, brick_sloths, method = "bilinear")
plot(noiNat_dens_ras)

# mask bias file
#crop bias file to extent of sloth data
noiNat_bias <- raster::mask(noiNat_dens_ras, Env_sloths[[1]])
plot(noiNat_bias)
points(noiNat_coords)

#Save bias layer
writeRaster(noiNat_bias, "noiNat_bias_file.tif", overwrite = TRUE)
