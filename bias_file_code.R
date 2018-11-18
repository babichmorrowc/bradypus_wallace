#Load packages
library(spocc)
library(plyr)
library(viridis)
library(ggmap)
library(MASS)
library(ENMeval)

#provide API key
#api_key = 
register_google(key = api_key)


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
bias <- raster::mask(bias, Env_sloths[[1]])
plot(bias)

#Save bias layer
writeRaster(dens_ras, "sloth_bias_file.tif")
