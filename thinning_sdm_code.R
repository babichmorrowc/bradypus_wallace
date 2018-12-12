# load packages
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)
library(ggmap)
library(readr)

# Import occurrence data --------------------------------------------------

# Literature data
d.occs <- '/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Occurrence_Data/'
# create path to user occurrences csv file
var_userOccs.path <- file.path(d.occs, "Bradypus_variegatus_litdata.csv")
tri_userOccs.path <- file.path(d.occs, "Bradypus_tridactylus_litdata.csv")
tor_userOccs.path <- file.path(d.occs, "Bradypus_torquatus_litdata.csv")
# read in csv
var_userOccs.csv <- read.csv(var_userOccs.path, header = TRUE)
tri_userOccs.csv <- read.csv(tri_userOccs.path, header = TRUE)
tor_userOccs.csv <- read.csv(tor_userOccs.path, header = TRUE)
# remove rows with duplicate coordinates
var_occs.dups <- duplicated(var_userOccs.csv[c('longitude', 'latitude')])
var_occs <- var_userOccs.csv[!var_occs.dups,]
tri_occs.dups <- duplicated(tri_userOccs.csv[c('longitude', 'latitude')])
tri_occs <- tri_userOccs.csv[!tri_occs.dups,]
tor_occs.dups <- duplicated(tor_userOccs.csv[c('longitude', 'latitude')])
tor_occs <- tor_userOccs.csv[!tor_occs.dups,]
# remove NAs
var_occs <- var_occs[complete.cases(var_occs$longitude, var_occs$latitude), ]
tri_occs <- tri_occs[complete.cases(tri_occs$longitude, tri_occs$latitude), ]
tor_occs <- tor_occs[complete.cases(tor_occs$longitude, tor_occs$latitude), ]
# give all records a unique ID
var_occs$occID <- row.names(var_occs)
tri_occs$occID <- row.names(tri_occs)
tor_occs$occID <- row.names(tor_occs)

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
  geom_point(data = tri_occs, aes(x = longitude, y = latitude), color = "deepskyblue") +
  geom_point(data = tor_occs, aes(x = longitude, y = latitude), color = "mediumorchid2")

# Process occurrence data -------------------------------------------------

#Spatial thinning to 40 km
var_output <- spThin::thin(var_occs, 'latitude', 'longitude', 'name', thin.par = 40, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)
tri_output <- spThin::thin(tri_occs, 'latitude', 'longitude', 'name', thin.par = 40, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)
tor_output <- spThin::thin(tor_occs, 'latitude', 'longitude', 'name', thin.par = 40, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)


# find the iteration that returns the max number of occurrences
var_maxThin <- which(sapply(var_output, nrow) == max(sapply(var_output, nrow)))
tri_maxThin <- which(sapply(tri_output, nrow) == max(sapply(tri_output, nrow)))
tor_maxThin <- which(sapply(tor_output, nrow) == max(sapply(tor_output, nrow)))
# if there's more than one max, pick the first one
var_maxThin <- var_output[[ifelse(length(var_maxThin) > 1, var_maxThin[1], var_maxThin)]]  
tri_maxThin <- tri_output[[ifelse(length(tri_maxThin) > 1, tri_maxThin[1], tri_maxThin)]] 
tor_maxThin <- tor_output[[ifelse(length(tor_maxThin) > 1, tor_maxThin[1], tor_maxThin)]] 
# subset occs to match only thinned occs
thinned_var_occs <- var_occs[as.numeric(rownames(var_maxThin)),]  
thinned_tri_occs <- tri_occs[as.numeric(rownames(tri_maxThin)),]
thinned_tor_occs <- tor_occs[as.numeric(rownames(tor_maxThin)),]

#points deleted through thinning
discard_var_occs <- var_occs[-as.numeric(rownames(var_maxThin)),]
discard_tri_occs <- tri_occs[-as.numeric(rownames(tri_maxThin)),]
discard_tor_occs <- tor_occs[-as.numeric(rownames(tor_maxThin)),]

# visualize the results of thinning

ggmap(map) +
  geom_point(data = thinned_var_occs, aes(x = longitude, y = latitude), color = "darkorange1") +
  geom_point(data = discard_var_occs, aes(x=longitude, y=latitude), color = "red", alpha = 0.75)

ggmap(map) +
  geom_point(data = thinned_tri_occs, aes(x = longitude, y = latitude), color = "deepskyblue") +
  geom_point(data = discard_tri_occs, aes(x=longitude, y=latitude), color = "red", alpha = 0.75)

ggmap(map) +
  geom_point(data = thinned_tor_occs, aes(x = longitude, y = latitude), color = "mediumorchid2") +
  geom_point(data = discard_tor_occs, aes(x=longitude, y=latitude), color = "red", alpha = 0.75)

# Obtain environmental data -----------------------------------------------

grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
#get extent
combine.lat <- c(var_occs$latitude, tri_occs$latitude, tor_occs$latitude)
combine.lon <- c(var_occs$longitude, tri_occs$longitude, tor_occs$longitude)
ext_sloths <- extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))
Env_sloths <- crop(envs, ext_sloths)

# extract environmental values at occ grid cells
thinned_var_locs.vals <- raster::extract(envs[[1]], thinned_var_occs[, c('longitude', 'latitude')])
thinned_tri_locs.vals <- raster::extract(envs[[1]], thinned_tri_occs[, c('longitude', 'latitude')])
thinned_tor_locs.vals <- raster::extract(envs[[1]], thinned_tor_occs[, c('longitude', 'latitude')])

# remove occs without environmental values
thinned_var_occs <- thinned_var_occs[!is.na(thinned_var_locs.vals), ]
thinned_tri_occs <- thinned_tri_occs[!is.na(thinned_tri_locs.vals), ] 
thinned_tor_occs <- thinned_tor_occs[!is.na(thinned_tor_locs.vals), ]

# Process environmental data ----------------------------------------------

# make SpatialPoints object for buffering
thinned_var_occs.xy <- thinned_var_occs[c('longitude', 'latitude')]
thinned_tri_occs.xy <- thinned_tri_occs[c('longitude', 'latitude')]
thinned_tor_occs.xy <- thinned_tor_occs[c('longitude', 'latitude')]

sp::coordinates(thinned_var_occs.xy) <- ~ longitude + latitude
sp::coordinates(thinned_tri_occs.xy) <- ~ longitude + latitude
sp::coordinates(thinned_tor_occs.xy) <- ~ longitude + latitude

#Buffer by 2 degree around points
var_bgExt_2 <- rgeos::gBuffer(thinned_var_occs.xy, width = 2)
tri_bgExt_2 <- rgeos::gBuffer(thinned_tri_occs.xy, width = 2)
tor_bgExt_2 <- rgeos::gBuffer(thinned_tor_occs.xy, width = 2)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_2, aes(x = long, y = lat, group = group), fill = "darkorange1", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_2, aes(x = long, y = lat, group = group), fill = "deepskyblue", alpha = 0.7) +
  geom_polygon(data = tor_bgExt_2, aes(x = long, y = lat, group = group), fill = "mediumorchid2", alpha = 0.7)

#Buffer by 4 degree around points
var_bgExt_4 <- rgeos::gBuffer(thinned_var_occs.xy, width = 4)
tri_bgExt_4 <- rgeos::gBuffer(thinned_tri_occs.xy, width = 4)
tor_bgExt_4 <- rgeos::gBuffer(thinned_tor_occs.xy, width = 4)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_4, aes(x = long, y = lat, group = group), fill = "darkorange1", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_4, aes(x = long, y = lat, group = group), fill = "deepskyblue", alpha = 0.7) +
  geom_polygon(data = tor_bgExt_4, aes(x = long, y = lat, group = group), fill = "mediumorchid2", alpha = 0.7)


# crop the environmental rasters by the background extent shape
var_envsBgCrop <- raster::crop(Env_sloths, var_bgExt_2)
tri_envsBgCrop <- raster::crop(Env_sloths, tri_bgExt_2)
tor_envsBgCrop <- raster::crop(Env_sloths, tor_bgExt_2)
# mask the background extent shape from the cropped raster
var_envsBgMsk <- raster::mask(var_envsBgCrop, var_bgExt_2)
tri_envsBgMsk <- raster::mask(tri_envsBgCrop, tri_bgExt_2)
tor_envsBgMsk <- raster::mask(tor_envsBgCrop, tor_bgExt_2)
# sample random background points
buffer_var_bg.xy <- dismo::randomPoints(var_envsBgMsk, 10000)
buffer_tri_bg.xy <- dismo::randomPoints(tri_envsBgMsk, 10000)
buffer_tor_bg.xy <- dismo::randomPoints(tor_envsBgMsk, 10000)
# convert matrix output to data frame
buffer_var_bg.xy <- as.data.frame(buffer_var_bg.xy)
buffer_tri_bg.xy <- as.data.frame(buffer_tri_bg.xy)
buffer_tor_bg.xy <- as.data.frame(buffer_tor_bg.xy)


# crop the environmental rasters by the background extent shape
var_envsBgCrop_4 <- raster::crop(Env_sloths, var_bgExt_4)
tri_envsBgCrop_4 <- raster::crop(Env_sloths, tri_bgExt_4)
tor_envsBgCrop_4 <- raster::crop(Env_sloths, tor_bgExt_4)
# mask the background extent shape from the cropped raster
var_envsBgMsk_4 <- raster::mask(var_envsBgCrop_4, var_bgExt_4)
tri_envsBgMsk_4 <- raster::mask(tri_envsBgCrop_4, tri_bgExt_4)
tor_envsBgMsk_4 <- raster::mask(tor_envsBgCrop_4, tor_bgExt_4)
# sample random background points
buffer_var_bg.xy_4 <- dismo::randomPoints(var_envsBgMsk_4, 10000)
buffer_tri_bg.xy_4 <- dismo::randomPoints(tri_envsBgMsk_4, 10000)
buffer_tor_bg.xy_4 <- dismo::randomPoints(tor_envsBgMsk_4, 10000)
# convert matrix output to data frame
buffer_var_bg.xy_4 <- as.data.frame(buffer_var_bg.xy_4)
buffer_tri_bg.xy_4 <- as.data.frame(buffer_tri_bg.xy_4)
buffer_tor_bg.xy_4 <- as.data.frame(buffer_tor_bg.xy_4)

# Partition occurrence data -----------------------------------------------

# variegatus: Partition using block method
thinned_var_group.data <- ENMeval::get.block(occ = thinned_var_occs.xy, bg.coords = buffer_var_bg.xy)

# tridactylus: Partition using jackknife
thinned_tri_group.data <- ENMeval::get.jackknife(occ = thinned_tri_occs.xy, bg.coords = buffer_tri_bg.xy)

# torquatus: Partition using jackknife
thinned_tor_group.data <- ENMeval::get.jackknife(occ = thinned_tor_occs.xy, bg.coords = buffer_tor_bg.xy)

# pull out the occurrence and background partition group numbers from the list
thinned_var_occs.grp <- thinned_var_group.data[[1]]
thinned_var_bg.grp <- thinned_var_group.data[[2]]
thinned_tri_occs.grp <- thinned_tri_group.data[[1]]
thinned_tri_bg.grp <- thinned_tri_group.data[[2]]
thinned_tor_occs.grp <- thinned_tor_group.data[[1]]
thinned_tor_bg.grp <- thinned_tor_group.data[[2]]

# 4 degree buffer

# variegatus: Partition using block method
thinned_var_group.data <- ENMeval::get.block(occ = thinned_var_occs.xy, bg.coords = buffer_var_bg.xy)

# tridactylus: Partition using jackknife
thinned_tri_group.data <- ENMeval::get.jackknife(occ = thinned_tri_occs.xy, bg.coords = buffer_tri_bg.xy)

# torquatus: Partition using jackknife
thinned_tor_group.data <- ENMeval::get.jackknife(occ = thinned_tor_occs.xy, bg.coords = buffer_tor_bg.xy)

# pull out the occurrence and background partition group numbers from the list
thinned_var_occs.grp <- thinned_var_group.data[[1]]
thinned_var_bg.grp <- thinned_var_group.data[[2]]
thinned_tri_occs.grp <- thinned_tri_group.data[[1]]
thinned_tri_bg.grp <- thinned_tri_group.data[[2]]
thinned_tor_occs.grp <- thinned_tor_group.data[[1]]
thinned_tor_bg.grp <- thinned_tor_group.data[[2]]

# Build and evaluate niche model ------------------------------------------

# define the vector of regularization multipliers to test
rms <- seq(1, 5, 1)

# Bradypus variegatus

# iterate model building over all chosen parameter settings
thinned_var_e <- ENMeval::ENMevaluate(thinned_var_occs.xy, var_envsBgMsk, bg.coords = buffer_var_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'block', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_var_evalTbl <- thinned_var_e@results
thinned_var_evalTbl <- thinned_var_evalTbl[with(thinned_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_var_evalTbl)
write_csv(thinned_var_evalTbl, "./maxentoutputs/thinned_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
thinned_var_evalMods <- thinned_var_e@models
names(thinned_var_evalMods) <- thinned_var_e@results$settings
thinned_var_evalPreds <- thinned_var_e@predictions
# Select your model from the models list
thinned_var_mod <- thinned_var_evalMods[["L_5"]]
# generate cloglog prediction
thinned_var_pred <- ENMeval::maxnet.predictRaster(thinned_var_mod, var_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_var_pred)
#project to entire extent
thinned_var_proj <- ENMeval::maxnet.predictRaster(thinned_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_var_proj)

#save cloglog prediction
writeRaster(thinned_var_proj, "thinned_variegatus_L_5_cloglog.tif")

# iterate model building over all chosen parameter settings
# thinned_var_e_4 <- ENMeval::ENMevaluate(thinned_var_occs.xy, var_envsBgMsk_4, bg.coords = buffer_var_bg.xy_4, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
#                                       method = 'block', clamp = TRUE, algorithm = "maxnet")
thinned_var_e_4 <- ENMeval::ENMevaluate(thinned_var_occs.xy, var_envsBgMsk_4, bg.coords = buffer_var_bg.xy_4, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                        method = 'block', clamp = TRUE, algorithm = "maxent.jar")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_var_evalTbl_4 <- thinned_var_e_4@results
thinned_var_evalTbl_4 <- thinned_var_evalTbl_4[with(thinned_var_evalTbl_4, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_var_evalTbl_4)
write_csv(thinned_var_evalTbl_4, "./maxentoutputs/thinned_var_evalTbl_4.csv")
#evaluation table for variegatus with spatial thinning and bias file:
thinned_var_evalMods_4 <- thinned_var_e_4@models
names(thinned_var_evalMods_4) <- thinned_var_e_4@results$settings
thinned_var_evalPreds_4 <- thinned_var_e_4@predictions
# Select your model from the models list
thinned_var_mod_4 <- thinned_var_evalMods_4[["L_1"]]
# generate cloglog prediction
thinned_var_pred_4 <- ENMeval::maxnet.predictRaster(thinned_var_mod_4, var_envsBgMsk_4, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_var_pred_4)
#project to entire extent
thinned_var_proj_4 <- ENMeval::maxnet.predictRaster(thinned_var_mod_4, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_var_proj_4)

#save cloglog prediction
writeRaster(thinned_var_proj_4, "thinned_variegatus_L_5_cloglog.tif")


# Bradypus tridactylus

# iterate model building over all chosen parameter settings
thinned_tri_e <- ENMeval::ENMevaluate(thinned_tri_occs.xy, tri_envsBgMsk, bg.coords = buffer_tri_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
 # unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tri_evalTbl <- thinned_tri_e@results
thinned_tri_evalTbl <- thinned_tri_evalTbl[with(thinned_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_tri_evalTbl)
write_csv(thinned_tri_evalTbl, "./maxentoutputs/thinned_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
thinned_tri_evalMods <- thinned_tri_e@models
names(thinned_tri_evalMods) <- thinned_tri_e@results$settings
thinned_tri_evalPreds <- thinned_tri_e@predictions
# Select your model from the models list
thinned_tri_mod <- thinned_tri_evalMods[["H_3"]]
# generate cloglog prediction
thinned_tri_pred <- ENMeval::maxnet.predictRaster(thinned_tri_mod, tri_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tri_pred)
#project to entire extent
thinned_tri_proj <- ENMeval::maxnet.predictRaster(thinned_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tri_proj)

#save cloglog prediction
writeRaster(thinned_tri_proj, "thinned_tridactylus_L_5_cloglog.tif")

# iterate model building over all chosen parameter settings
thinned_tri_e_4 <- ENMeval::ENMevaluate(thinned_tri_occs.xy, tri_envsBgMsk_4, bg.coords = buffer_tri_bg.xy_4, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tri_evalTbl_4 <- thinned_tri_e_4@results
thinned_tri_evalTbl_4 <- thinned_tri_evalTbl_4[with(thinned_tri_evalTbl_4, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_tri_evalTbl_4)
write_csv(thinned_tri_evalTbl_4, "./maxentoutputs/thinned_tri_evalTbl_4.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
thinned_tri_evalMods_4 <- thinned_tri_e_4@models
names(thinned_tri_evalMods_4) <- thinned_tri_e_4@results$settings
thinned_tri_evalPreds_4 <- thinned_tri_e_4@predictions
# Select your model from the models list
thinned_tri_mod_4_H5 <- thinned_tri_evalMods_4[["H_5"]]
thinned_tri_mod_4_LQH5 <- thinned_tri_evalMods_4[["LQH_5"]]
# generate cloglog prediction
thinned_tri_pred_4_H5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_4_H5, tri_envsBgMsk_4, type = 'cloglog', clamp = TRUE)
thinned_tri_pred_4_LQH5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_4_LQH5, tri_envsBgMsk_4, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tri_pred_4_H5)
plot(thinned_tri_pred_4_LQH5)
#project to entire extent
thinned_tri_proj_4_H5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_4_H5, Env_sloths, type = 'cloglog', clamp = TRUE)
thinned_tri_proj_4_LQH5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_4_LQH5, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tri_proj_4_H5)
plot(thinned_tri_proj_4_LQH5)

#save cloglog prediction
#writeRaster(thinned_tri_proj_4, "thinned_tridactylus_H_5_cloglog.tif")

# iterate model building over all chosen parameter settings
thinned_tri_e_6 <- ENMeval::ENMevaluate(thinned_tri_occs.xy, tri_envsBgMsk_6, bg.coords = buffer_tri_bg.xy_6, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                        method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tri_evalTbl_6 <- thinned_tri_e_6@results
thinned_tri_evalTbl_6 <- thinned_tri_evalTbl_6[with(thinned_tri_evalTbl_6, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_tri_evalTbl_6)
write_csv(thinned_tri_evalTbl_6, "./maxentoutputs/thinned_tri_evalTbl_6.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
thinned_tri_evalMods_6 <- thinned_tri_e_6@models
names(thinned_tri_evalMods_6) <- thinned_tri_e_6@results$settings
thinned_tri_evalPreds_6 <- thinned_tri_e_6@predictions
# Select your model from the models list
thinned_tri_mod_6_H5 <- thinned_tri_evalMods_6[["H_5"]]
thinned_tri_mod_6_LQH5 <- thinned_tri_evalMods_6[["LQH_5"]]
# generate cloglog prediction
thinned_tri_pred_6_H5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_6_H5, tri_envsBgMsk_6, type = 'cloglog', clamp = TRUE)
thinned_tri_pred_6_LQH5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_6_LQH5, tri_envsBgMsk_6, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tri_pred_6_H5)
plot(thinned_tri_pred_6_LQH5)
#project to entire extent
thinned_tri_proj_6_H5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_6_H5, Env_sloths, type = 'cloglog', clamp = TRUE)
thinned_tri_proj_6_LQH5 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_6_LQH5, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tri_proj_6_H5)
plot(thinned_tri_proj_6_LQH5)

#save cloglog prediction
#writeRaster(thinned_tri_proj_6, "thinned_tridactylus_H_5_cloglog.tif")



# Bradypus torquatus

# iterate model building over all chosen parameter settings
thinned_tor_e <- ENMeval::ENMevaluate(thinned_tor_occs.xy, tor_envsBgMsk, bg.coords = buffer_tor_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tor_evalTbl <- thinned_tor_e@results
thinned_tor_evalTbl <- thinned_tor_evalTbl[with(thinned_tor_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_tor_evalTbl)
write_csv(thinned_tor_evalTbl, "./maxentoutputs/thinned_tor_evalTbl.csv")
#evaluation table for torquatus with spatial thinning and bias file:
thinned_tor_evalMods <- thinned_tor_e@models
names(thinned_tor_evalMods) <- thinned_tor_e@results$settings
thinned_tor_evalPreds <- thinned_tor_e@predictions
# Select your model from the models list
thinned_tor_mod <- thinned_tor_evalMods[["L_2"]]
# generate cloglog prediction
thinned_tor_pred <- ENMeval::maxnet.predictRaster(thinned_tor_mod, tor_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tor_pred)
#project to entire extent
thinned_tor_proj <- ENMeval::maxnet.predictRaster(thinned_tor_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tor_proj)

#save cloglog prediction
writeRaster(thinned_tor_proj, "thinned_torquatus_L_2_cloglog.tif")

# iterate model building over all chosen parameter settings
thinned_tor_e_4 <- ENMeval::ENMevaluate(thinned_tor_occs.xy, tor_envsBgMsk_4, bg.coords = buffer_tor_bg.xy_4, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tor_evalTbl_4 <- thinned_tor_e_4@results
thinned_tor_evalTbl_4 <- thinned_tor_evalTbl_4[with(thinned_tor_evalTbl_4, order(avg.test.or10pct, -avg.test.AUC)), ]
View(thinned_tor_evalTbl_4)
write_csv(thinned_tor_evalTbl_4, "./maxentoutputs/thinned_tor_evalTbl_4.csv")
#evaluation table for torquatus with spatial thinning and bias file:
thinned_tor_evalMods_4 <- thinned_tor_e_4@models
names(thinned_tor_evalMods_4) <- thinned_tor_e_4@results$settings
thinned_tor_evalPreds_4 <- thinned_tor_e_4@predictions
# Select your model from the models list
thinned_tor_mod_4 <- thinned_tor_evalMods_4[["H_3"]]
# generate cloglog prediction
thinned_tor_pred_4 <- ENMeval::maxnet.predictRaster(thinned_tor_mod_4, tor_envsBgMsk_4, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tor_pred_4)
#project to entire extent
thinned_tor_proj_4 <- ENMeval::maxnet.predictRaster(thinned_tor_mod_4, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tor_proj_4)

#save cloglog prediction
writeRaster(thinned_tor_proj_4, "thinned_torquatus_L_2_cloglog.tif")


# Response curves ---------------------------------------------------------

thinned_var_mod$betas
plot(thinned_var_mod, vars = c('bio10', 'bio13', 'bio18', 'bio2', 'bio3', 'bio4', 'bio8'), type = "cloglog")

thinned_tri_mod$betas
plot(thinned_tri_mod, vars = c('bio16', 'bio4', 'bio6'))

thinned_tor_mod$betas
plot(thinned_tor_mod, vars = c('bio12', 'bio2', 'bio3'))

