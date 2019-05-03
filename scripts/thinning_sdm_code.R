# load packages
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)
library(ggmap)
library(readr)
library(sp)

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


# Create bounding boxes ---------------------------------------------------

#get extent
ext_var <- extent(c(min(var_occs$longitude)-2, max(var_occs$longitude)+2, min(var_occs$latitude)-2, max(var_occs$latitude)+2))
ext_tri <- extent(c(min(tri_occs$longitude)-2, max(tri_occs$longitude)+2, min(tri_occs$latitude)-2, max(tri_occs$latitude)+2))
ext_tor <- extent(c(min(tor_occs$longitude)-2, max(tor_occs$longitude)+2, min(tor_occs$latitude)-2, max(tor_occs$latitude)+2))


# Mapping -----------------------------------------------------------------

#Use Google maps
api_key = 
register_google(key = api_key)

#Satellite map
bbox <- make_bbox(lon = var_occs$longitude, lat = var_occs$latitude, f = 0.05)
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

grids_2.5 <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
envs_2.5 <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids_2.5))
grids <- list.files("/Users/hellenfellows/Desktop/wc2.0_30s_bio")
envs <- stack(paste0("/Users/hellenfellows/Desktop/wc2.0_30s_bio/", grids))

#get extent
combine.lat <- c(var_occs$latitude, tri_occs$latitude, tor_occs$latitude)
combine.lon <- c(var_occs$longitude, tri_occs$longitude, tor_occs$longitude)
ext_sloths <- extent(c(min(combine.lon)-2, max(combine.lon)+2, min(combine.lat)-2, max(combine.lat)+2))
Env_sloths <- crop(envs, ext_sloths)
Env_sloths_2.5 <- crop(envs_2.5, ext_sloths)
Env_var <- crop(envs, ext_var)
Env_tri <- crop(envs, ext_tri)
Env_tor <- crop(envs, ext_tor)

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

#Buffer by 4 degree around points
var_bgExt <- rgeos::gBuffer(thinned_var_occs.xy, width = 4)
tri_bgExt <- rgeos::gBuffer(thinned_tri_occs.xy, width = 4)
tor_bgExt <- rgeos::gBuffer(thinned_tor_occs.xy, width = 4)

#Visualize the background buffer
ggmap(map) +
  geom_polygon(data = var_bgExt_4, aes(x = long, y = lat, group = group), fill = "darkorange1", alpha = 0.7) +
  geom_polygon(data = tri_bgExt_4, aes(x = long, y = lat, group = group), fill = "deepskyblue", alpha = 0.7) +
  geom_polygon(data = tor_bgExt_4, aes(x = long, y = lat, group = group), fill = "mediumorchid2", alpha = 0.7)

# crop the environmental rasters by the background extent shape
var_envsBgCrop <- raster::crop(Env_sloths, var_bgExt)
tri_envsBgCrop <- raster::crop(Env_sloths, tri_bgExt)
tor_envsBgCrop <- raster::crop(Env_sloths, tor_bgExt)
# mask the background extent shape from the cropped raster
var_envsBgMsk <- raster::mask(var_envsBgCrop, var_bgExt)
tri_envsBgMsk <- raster::mask(tri_envsBgCrop, tri_bgExt)
tor_envsBgMsk <- raster::mask(tor_envsBgCrop, tor_bgExt)
# sample random background points
buffer_var_bg.xy <- dismo::randomPoints(var_envsBgMsk, 10000)
buffer_tri_bg.xy <- dismo::randomPoints(tri_envsBgMsk, 10000)
buffer_tor_bg.xy <- dismo::randomPoints(tor_envsBgMsk, 10000)
# convert matrix output to data frame
buffer_var_bg.xy <- as.data.frame(buffer_var_bg.xy)
buffer_tri_bg.xy <- as.data.frame(buffer_tri_bg.xy)
buffer_tor_bg.xy <- as.data.frame(buffer_tor_bg.xy)

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

# Build and evaluate niche model ------------------------------------------

# define the vector of regularization multipliers to test
rms <- seq(0.5, 5, 0.5)

# Bradypus variegatus
# iterate model building over all chosen parameter settings
thinned_var_e <- ENMeval::ENMevaluate(thinned_var_occs.xy, var_envsBgMsk, bg.coords = buffer_var_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'block', clamp = TRUE, algorithm = "maxnet")

# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_var_evalTbl <- thinned_var_e@results
View(thinned_var_evalTbl)
write_csv(thinned_var_evalTbl, "./maxentoutputs/thinned_var_evalTbl.csv")
# get all models with avg.test.or10pct != 0
sel_var_evalTbl <- thinned_var_evalTbl[thinned_var_evalTbl$avg.test.or10pct != 0, ]
sel_var_evalTbl <- sel_var_evalTbl[with(sel_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_var_evalTbl)
# get all models with delta AICc <= 2
aic_var_evalTbl <- thinned_var_evalTbl[thinned_var_evalTbl$delta.AICc <= 2.0, ]
aic_var_evalTbl <- aic_var_evalTbl[with(aic_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(aic_var_evalTbl)
#evaluation table for variegatus with spatial thinning and bias file:
thinned_var_evalMods <- thinned_var_e@models
names(thinned_var_evalMods) <- thinned_var_e@results$settings
thinned_var_evalPreds <- thinned_var_e@predictions
# Select your model from the models list
thinned_var_mod <- thinned_var_evalMods[["LQH_2.5"]]
thinned_var_mod_new <- thinned_var_evalMods[["L_4.5"]]
# generate cloglog prediction
thinned_var_pred <- ENMeval::maxnet.predictRaster(thinned_var_mod, var_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_var_pred)
#project to entire extent
thinned_var_proj <- ENMeval::maxnet.predictRaster(thinned_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
thinned_var_proj_new <- ENMeval::maxnet.predictRaster(thinned_var_mod_new, Env_sloths, type = 'cloglog', clamp = TRUE)
plot(thinned_var_proj_new)
#project to variegatus extent
thinned_var_proj_bbox <- ENMeval::maxnet.predictRaster(thinned_var_mod, Env_var, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_var_proj)
plot(thinned_var_proj_bbox)

# going back to look at the 2.5 resolution with new eval criteria
# get all models with avg.test.or10pct != 0
sel_var_evalTbl_2.5 <- thinned_var_evalTbl_4[thinned_var_evalTbl_4$avg.test.or10pct != 0, ]
sel_var_evalTbl_2.5 <- sel_var_evalTbl_2.5[with(sel_var_evalTbl_2.5, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_var_evalTbl_2.5)


#save cloglog prediction
writeRaster(thinned_var_proj, "thinned_variegatus_LQH_2.5_cloglog.tif")
writeRaster(thinned_var_proj_bbox, "thinned_variegatus_bbox.tif")


# Bradypus tridactylus
# iterate model building over all chosen parameter settings
thinned_tri_e <- ENMeval::ENMevaluate(thinned_tri_occs.xy, tri_envsBgMsk, bg.coords = buffer_tri_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tri_evalTbl <- thinned_tri_e@results
View(thinned_tri_evalTbl)
write_csv(thinned_tri_evalTbl, "./maxentoutputs/thinned_tri_evalTbl.csv")
# get all models with avg.test.or10pct != 0
sel_tri_evalTbl <- thinned_tri_evalTbl[thinned_tri_evalTbl$avg.test.or10pct != 0, ]
sel_tri_evalTbl <- sel_tri_evalTbl[with(sel_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_tri_evalTbl)
# get all models with delta AICc <= 2
aic_tri_evalTbl <- thinned_tri_evalTbl[thinned_tri_evalTbl$delta.AICc <= 2.0, ]
aic_tri_evalTbl <- aic_tri_evalTbl[with(aic_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(aic_tri_evalTbl)
#evaluation table for tridactylus with spatial thinning and bias file:
thinned_tri_evalMods <- thinned_tri_e@models
names(thinned_tri_evalMods) <- thinned_tri_e@results$settings
thinned_tri_evalPreds <- thinned_tri_e@predictions
# Select your model from the models list
thinned_tri_mod <- thinned_tri_evalMods[["LQH_3.5"]]
thinned_tri_mod_new <- thinned_tri_evalMods[["LQH_4"]]
# generate cloglog prediction
thinned_tri_pred <- ENMeval::maxnet.predictRaster(thinned_tri_mod, tri_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tri_pred)
#project to entire extent
thinned_tri_proj <- ENMeval::maxnet.predictRaster(thinned_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
thinned_tri_proj_new <- ENMeval::maxnet.predictRaster(thinned_tri_mod_new, Env_sloths, type = 'cloglog', clamp = TRUE)
plot(thinned_tri_proj_new)
#project to tridactylus extent
thinned_tri_proj_bbox <- ENMeval::maxnet.predictRaster(thinned_tri_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tri_proj, zlim = c(0,1))
plot(thinned_tri_proj_bbox, zlim = c(0,1))

#save cloglog prediction
writeRaster(thinned_tri_proj, "thinned_tridactylus_H_4_cloglog.tif")
writeRaster(thinned_tri_proj_bbox, "thinned_tridactylus_bbox.tif")

# going back to look at the 2.5 resolution with new eval criteria
# get all models with avg.test.or10pct != 0
sel_tri_evalTbl_2.5 <- thinned_tri_evalTbl_4[thinned_tri_evalTbl_4$avg.test.or10pct != 0, ]
sel_tri_evalTbl_2.5 <- sel_tri_evalTbl_2.5[with(sel_tri_evalTbl_2.5, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_tri_evalTbl_2.5)
# Select your model from the models list
thinned_tri_mod_4_LQH4 <- thinned_tri_evalMods_4[["LQH_4"]]
#project to entire extent
thinned_tri_proj_4_LQH4 <- ENMeval::maxnet.predictRaster(thinned_tri_mod_4_LQH4, Env_sloths_2.5, type = 'cloglog', clamp = TRUE)
plot(thinned_tri_proj_4_LQH4, zlim = c(0,1))

# Bradypus torquatus
# iterate model building over all chosen parameter settings
thinned_tor_e <- ENMeval::ENMevaluate(thinned_tor_occs.xy, tor_envsBgMsk, bg.coords = buffer_tor_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'jackknife', clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tor_evalTbl <- thinned_tor_e@results
View(thinned_tor_evalTbl)
write_csv(thinned_tor_evalTbl, "./maxentoutputs/thinned_tor_evalTbl.csv")
# get all models with avg.test.or10pct != 0
sel_tor_evalTbl <- thinned_tor_evalTbl[thinned_tor_evalTbl$avg.test.or10pct != 0, ]
sel_tor_evalTbl <- sel_tor_evalTbl[with(sel_tor_evalTbl, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_tor_evalTbl)
# get all models with delta AICc <= 2
aic_tor_evalTbl <- thinned_tor_evalTbl[thinned_tor_evalTbl$delta.AICc <= 2.0, ]
aic_tor_evalTbl <- aic_tor_evalTbl[with(aic_tor_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
View(aic_tor_evalTbl)
#evaluation table for torquatus with spatial thinning and bias file:
thinned_tor_evalMods <- thinned_tor_e@models
names(thinned_tor_evalMods) <- thinned_tor_e@results$settings
thinned_tor_evalPreds <- thinned_tor_e@predictions
# Select your model from the models list
thinned_tor_mod <- thinned_tor_evalMods[["H_4.5"]]
# generate cloglog prediction
thinned_tor_pred <- ENMeval::maxnet.predictRaster(thinned_tor_mod, tor_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tor_pred)
#project to entire extent
thinned_tor_proj <- ENMeval::maxnet.predictRaster(thinned_tor_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#project to torquatus extent
thinned_tor_proj_bbox <- ENMeval::maxnet.predictRaster(thinned_tor_mod, Env_tor, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tor_proj)
plot(thinned_tor_proj_bbox)

# going back to look at the 2.5 resolution with new eval criteria
# get all models with avg.test.or10pct != 0
sel_tor_evalTbl_2.5 <- thinned_tor_evalTbl_4[thinned_tor_evalTbl_4$avg.test.or10pct != 0, ]
sel_tor_evalTbl_2.5 <- sel_tor_evalTbl_2.5[with(sel_tor_evalTbl_2.5, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(sel_tor_evalTbl_2.5)

#save cloglog prediction
writeRaster(thinned_tor_proj, "thinned_torquatus_H_4.5_cloglog.tif")
writeRaster(thinned_tor_proj_bbox, "thinned_torquatus_bbox.tif")

# Response curves ---------------------------------------------------------

thinned_var_mod$betas
response.plot(thinned_var_mod, v = "wc2.0_bio_30s_06", type = "cloglog")
plot(thinned_var_mod, vars = paste0('wc2.0_bio_30s_', c("02", "04", "06", "07", "10", "11", "12", "13", "16", "17", "18", "19")))

thinned_tri_mod$betas
plot(thinned_tri_mod, vars = c('wc2.0_bio_30s_06'))

thinned_tor_mod$betas
plot(thinned_tor_mod, vars = c('wc2.0_bio_30s_14'))

# Threshold models --------------------------------------------------------

source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/scripts/sdm_threshold.R")

# thresholded models at MTP and 10th percentile
# variegatus
thinned_var_proj_mtp <- sdm_threshold(thinned_var_proj, thinned_var_occs[,2:3], "mtp")
plot(thinned_var_proj_mtp, zlim = c(0, 1))
thinned_var_proj_p10 <- sdm_threshold(thinned_var_proj, thinned_var_occs[,2:3], "p10")
plot(thinned_var_proj_p10, zlim = c(0, 1))
# tridactylus
thinned_tri_proj_mtp <- sdm_threshold(thinned_tri_proj, thinned_tri_occs[,2:3], "mtp")
plot(thinned_tri_proj_mtp, zlim = c(0, 1))
thinned_tri_proj_p10 <- sdm_threshold(thinned_tri_proj, thinned_tri_occs[,2:3], "p10")
plot(thinned_tri_proj_p10, zlim = c(0, 1))
# torquatus
thinned_tor_proj_mtp <- sdm_threshold(thinned_tor_proj, thinned_tor_occs[,2:3], "mtp")
plot(thinned_tor_proj_mtp, zlim = c(0, 1))
thinned_tor_proj_p10 <- sdm_threshold(thinned_tor_proj, thinned_tor_occs[,2:3], "p10")
plot(thinned_tor_proj_p10, zlim = c(0, 1))

# thresholded models projected only to bounding box for each species
# variegatus
thinned_var_proj_bbox_mtp <- sdm_threshold(thinned_var_proj_bbox, thinned_var_occs[,2:3], "mtp")
plot(thinned_var_proj_bbox_mtp, zlim = c(0, 1))
thinned_var_proj_bbox_p10 <- sdm_threshold(thinned_var_proj_bbox, thinned_var_occs[,2:3], "p10")
plot(thinned_var_proj_bbox_p10, zlim = c(0, 1))
# tridactylus
thinned_tri_proj_bbox_mtp <- sdm_threshold(thinned_tri_proj_bbox, thinned_tri_occs[,2:3], "mtp")
plot(thinned_tri_proj_bbox_mtp, zlim = c(0, 1))
thinned_tri_proj_bbox_p10 <- sdm_threshold(thinned_tri_proj_bbox, thinned_tri_occs[,2:3], "p10")
plot(thinned_tri_proj_bbox_p10, zlim = c(0, 1))
# torquatus
thinned_tor_proj_bbox_mtp <- sdm_threshold(thinned_tor_proj_bbox, thinned_tor_occs[,2:3], "mtp")
plot(thinned_tor_proj_bbox_mtp, zlim = c(0, 1))
thinned_tor_proj_bbox_p10 <- sdm_threshold(thinned_tor_proj_bbox, thinned_tor_occs[,2:3], "p10")
plot(thinned_tor_proj_bbox_p10, zlim = c(0, 1))


# Misclassification rates -------------------------------------------------

# Omission rate
# percentage of variegatus points outside variegatus range
length(which(is.na(extract(thinned_var_proj_mtp, var_occs[,2:3]))))/nrow(var_occs[,2:3])
# percentage of tridactylus points outside tridactylus range
length(which(is.na(extract(thinned_tri_proj_bbox_mtp, tri_occs[,2:3]))))/nrow(tri_occs[,2:3])
# percentage of torquatus points outside torquatus range
length(which(is.na(extract(thinned_tor_proj_bbox_mtp, tor_occs[,2:3]))))/nrow(tor_occs[,2:3])

# False inclusion
# number of tridactylus & torquatus points classified as variegatus
length(which(!is.na(extract(thinned_var_proj_mtp, tri_occs[,2:3]))))
length(which(!is.na(extract(thinned_var_proj_mtp, tor_occs[,2:3]))))
# number of variegatus points classified as tridactylus
length(which(!is.na(extract(thinned_tri_proj_bbox_mtp, var_occs[,2:3]))))
# number of variegatus points classified as torquatus
length(which(!is.na(extract(thinned_tor_proj_bbox_mtp, var_occs[,2:3]))))

