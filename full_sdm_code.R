# load packages
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)


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


# Process occurrence data -------------------------------------------------

#Spatial thinning to 40 km
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


# Obtain environmental data -----------------------------------------------

grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
#get extent
combine.lat <- c(var_occs$latitude, tri_occs$latitude)
combine.lon <- c(var_occs$longitude, tri_occs$longitude)
ext_sloths <- extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))
Env_sloths <- crop(envs, ext_sloths)

# extract environmental values at occ grid cells
var_locs.vals <- raster::extract(envs[[1]], var_occs[, c('longitude', 'latitude')])
thinned_var_locs.vals <- raster::extract(envs[[1]], thinned_var_occs[, c('longitude', 'latitude')])
# remove occs without environmental values
var_occs <- var_occs[!is.na(var_locs.vals), ]
thinned_var_occs <- thinned_var_occs[!is.na(thinned_var_locs.vals), ]
# extract environmental values at occ grid cells
tri_locs.vals <- raster::extract(envs[[1]], tri_occs[, c('longitude', 'latitude')])
thinned_tri_locs.vals <- raster::extract(envs[[1]], thinned_tri_occs[, c('longitude', 'latitude')])
# remove occs without environmental values
tri_occs <- tri_occs[!is.na(tri_locs.vals), ] 
thinned_tri_occs <- thinned_tri_occs[!is.na(thinned_tri_locs.vals), ] 


# Process environmental data ----------------------------------------------

# make SpatialPoints object for buffering
var_occs.xy <- var_occs[c('longitude', 'latitude')]
thinned_var_occs.xy <- thinned_var_occs[c('longitude', 'latitude')]
tri_occs.xy <- tri_occs[c('longitude', 'latitude')]
thinned_tri_occs.xy <- thinned_tri_occs[c('longitude', 'latitude')]

sp::coordinates(thinned_var_occs.xy) <- ~ longitude + latitude
sp::coordinates(thinned_tri_occs.xy) <- ~ longitude + latitude

#Buffer by 1 degree around points
var_bgExt <- rgeos::gBuffer(thinned_var_occs.xy, width = 1)
tri_bgExt <- rgeos::gBuffer(thinned_tri_occs.xy, width = 1)

# crop the environmental rasters by the background extent shape
var_envsBgCrop <- raster::crop(envs, var_bgExt)
tri_envsBgCrop <- raster::crop(envs, tri_bgExt)
# mask the background extent shape from the cropped raster
var_envsBgMsk <- raster::mask(var_envsBgCrop, var_bgExt)
tri_envsBgMsk <- raster::mask(tri_envsBgCrop, tri_bgExt)
# sample random background points
buffer_var_bg.xy <- dismo::randomPoints(var_envsBgMsk, 10000)
buffer_tri_bg.xy <- dismo::randomPoints(tri_envsBgMsk, 10000)
# convert matrix output to data frame
buffer_var_bg.xy <- as.data.frame(buffer_var_bg.xy)
buffer_tri_bg.xy <- as.data.frame(buffer_tri_bg.xy)

# Bias file ------------------------------------------

# load in bias file
bias <- raster("sloth_bias_file.tif")

bias_bg.xy <- randomPoints(bias, 10000, prob=TRUE)
plot(bias)
points(bias_bg.xy, pch = 20, cex = 0.25)

# load in bias file without iNat
noiNat_bias <- raster("noiNat_bias_file.tif")

noiNat_bias_bg.xy <- randomPoints(noiNat_bias, 10000, prob=TRUE)
plot(noiNat_bias)
points(noiNat_bias_bg.xy, pch = 20, cex = 0.25)

# Partition occurrence data -----------------------------------------------

#Partition using block method
var_group_data <- ENMeval::get.block(occ=var_occs.xy, bg.coords=bias_bg.xy)
tri_group_data <- ENMeval::get.block(occ=tri_occs.xy, bg.coords=bias_bg.xy)

thinned_var_group.data <- ENMeval::get.block(occ=thinned_var_occs.xy, bg.coords=buffer_var_bg.xy)
thinned_tri_group.data <- ENMeval::get.block(occ=thinned_tri_occs.xy, bg.coords=buffer_tri_bg.xy)

# pull out the occurrence and background partition group numbers from the list
var_occs.grp <- var_group_data[[1]]
var_bg.grp <- var_group_data[[2]]
tri_occs.grp <- tri_group_data[[1]]
tri_bg.grp <- tri_group_data[[2]]

thinned_var_occs.grp <- thinned_var_group.data[[1]]
thinned_var_bg.grp <- thinned_var_group.data[[2]]
thinned_tri_occs.grp <- thinned_tri_group.data[[1]]
thinned_tri_bg.grp <- thinned_tri_group.data[[2]]



# Build and evaluate niche model ------------------------------------------

# define the vector of regularization multipliers to test
rms <- seq(1, 5, 1)


# Spatial thinning only ---------------------------------------------------

# iterate model building over all chosen parameter settings
thinned_var_e <- ENMeval::ENMevaluate(thinned_var_occs.xy, var_envsBgMsk, bg.coords = buffer_var_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                          method = 'user', thinned_var_occs.grp, thinned_var_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_var_evalTbl <- thinned_var_e@results
thinned_var_evalTbl <- thinned_var_evalTbl[with(thinned_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(thinned_var_evalTbl, "./maxentoutputs/thinned_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
thinned_var_evalMods <- thinned_var_e@models
names(thinned_var_evalMods) <- thinned_var_e@results$settings
thinned_var_evalPreds <- thinned_var_e@predictions
# Select your model from the models list
thinned_var_mod <- thinned_var_evalMods[["L_3"]]
# generate cloglog prediction
thinned_var_pred <- ENMeval::maxnet.predictRaster(thinned_var_mod, var_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_var_pred)
#project to entire extent
thinned_var_proj <- ENMeval::maxnet.predictRaster(thinned_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_var_proj)

#save cloglog prediction
writeRaster(thinned_var_proj, "thinned_variegatus_L_3_cloglog.tif")

# iterate model building over all chosen parameter settings
thinned_tri_e <- ENMeval::ENMevaluate(thinned_tri_occs.xy, tri_envsBgMsk, bg.coords = buffer_tri_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'user', thinned_tri_occs.grp, thinned_tri_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
thinned_tri_evalTbl <- thinned_tri_e@results
#get rid of models with no parameters
thinned_tri_evalTbl <- thinned_tri_evalTbl[thinned_tri_evalTbl[,16] > 0,]
thinned_tri_evalTbl <- thinned_tri_evalTbl[with(thinned_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(thinned_tri_evalTbl, "./maxentoutputs/thinned_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
thinned_tri_evalMods <- thinned_tri_e@models
names(thinned_tri_evalMods) <- thinned_tri_e@results$settings
thinned_tri_evalPreds <- thinned_tri_e@predictions
# Select your model from the models list
thinned_tri_mod <- thinned_tri_evalMods[["LQ_3"]]
# generate cloglog prediction
thinned_tri_pred <- ENMeval::maxnet.predictRaster(thinned_tri_mod, tri_envsBgMsk, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(thinned_tri_pred)
#project to entire extent
thinned_tri_proj <- ENMeval::maxnet.predictRaster(thinned_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(thinned_tri_proj)

#save cloglog prediction
writeRaster(thinned_tri_proj, "thinned_variegatus_LQ_3_cloglog.tif")



# Bias file only ----------------------------------------------------------


# iterate model building over all chosen parameter settings
bias_var_e <- ENMeval::ENMevaluate(var_occs.xy, Env_sloths, bg.coords = bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                   method = 'user', var_occs.grp, var_bg.grp, clamp = TRUE, algorithm = "maxent.jar")
# some kind of error in ENMeval / maxnet has occurred here when I set algorithm = "maxnet"
# switched algorithm to "maxent.jar" to see if things go better for me
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
bias_var_evalTbl <- bias_var_e@results
bias_var_evalTbl <- bias_var_evalTbl[with(bias_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(bias_var_evalTbl, "./maxentoutputs/bias_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
bias_var_evalMods <- bias_var_e@models
names(bias_var_evalMods) <- bias_var_e@results$settings
bias_var_evalPreds <- bias_var_e@predictions
# Select your model from the models list
bias_var_mod <- bias_var_evalMods[["LQH_2"]]
# generate cloglog prediction
bias_var_pred <- ENMeval::maxnet.predictRaster(bias_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(bias_var_pred)

#save cloglog prediction
writeRaster(bias_var_pred, "bias_variegatus_LQH_2_cloglog.tif")


# iterate model building over all chosen parameter settings
bias_tri_e <- ENMeval::ENMevaluate(tri_occs.xy, Env_sloths, bg.coords = bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                   method = 'user', tri_occs.grp, tri_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
bias_tri_evalTbl <- bias_tri_e@results
bias_tri_evalTbl <- bias_tri_evalTbl[with(bias_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(bias_tri_evalTbl, "./maxentoutputs/bias_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
bias_tri_evalMods <- bias_tri_e@models
names(bias_tri_evalMods) <- bias_tri_e@results$settings
bias_tri_evalPreds <- bias_tri_e@predictions
# Select your model from the models list
bias_tri_mod <- bias_tri_evalMods[["L_4"]]
# generate cloglog prediction
bias_tri_pred <- ENMeval::maxnet.predictRaster(bias_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(bias_tri_pred)

#save cloglog prediction
writeRaster(bias_tri_pred, "bias_tridactylus_L_4_cloglog.tif")

# Bias file w/o iNat --------------------------------------------------

# iterate model building over all chosen parameter settings
noiNat_bias_var_e <- ENMeval::ENMevaluate(var_occs.xy, Env_sloths, bg.coords = noiNat_bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                   method = 'user', var_occs.grp, var_bg.grp, clamp = TRUE, algorithm = "maxnet")
# Warning message:
#   In paste0(if (use.fC) formatC(x, format = "fg", width = 1, digits = digits) else format(x,  :
#                                                                                             restarting interrupted promise evaluation

# unpack the results data frame, the list of models, and the RasterStack of raw predictions
noiNat_bias_var_evalTbl <- noiNat_bias_var_e@results
noiNat_bias_var_evalTbl <- noiNat_bias_var_evalTbl[with(noiNat_bias_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(noiNat_bias_var_evalTbl, "./maxentoutputs/noiNat_bias_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
noiNat_bias_var_evalMods <- noiNat_bias_var_e@models
names(noiNat_bias_var_evalMods) <- noiNat_bias_var_e@results$settings
noiNat_bias_var_evalPreds <- noiNat_bias_var_e@predictions
# Select your model from the models list
noiNat_bias_var_mod <- noiNat_bias_var_evalMods[["L_1"]]
# generate cloglog prediction
noiNat_bias_var_pred <- ENMeval::maxnet.predictRaster(noiNat_bias_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(noiNat_bias_var_pred)

#save cloglog prediction
writeRaster(bias_tri_pred, "noiNat_bias_variegatus_L_1_cloglog.tif")

# iterate model building over all chosen parameter settings
noiNat_bias_tri_e <- ENMeval::ENMevaluate(tri_occs.xy, Env_sloths, bg.coords = noiNat_bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                          method = 'user', tri_occs.grp, tri_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
noiNat_bias_tri_evalTbl <- noiNat_bias_tri_e@results
noiNat_bias_tri_evalTbl <- noiNat_bias_tri_evalTbl[with(noiNat_bias_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(noiNat_bias_tri_evalTbl, "./maxentoutputs/noiNat_bias_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
noiNat_bias_tri_evalMods <- noiNat_bias_tri_e@models
names(noiNat_bias_tri_evalMods) <- noiNat_bias_tri_e@results$settings
noiNat_bias_tri_evalPreds <- noiNat_bias_tri_e@predictions
# Select your model from the models list
noiNat_bias_tri_mod <- noiNat_bias_tri_evalMods[["L_3"]]
# generate cloglog prediction
noiNat_bias_tri_pred <- ENMeval::maxnet.predictRaster(noiNat_bias_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(noiNat_bias_tri_pred)

# Spatial thinning + Bias file --------------------------------------------


# iterate model building over all chosen parameter settings
bias_thinned_var_e <- ENMeval::ENMevaluate(thinned_var_occs.xy, Env_sloths, bg.coords = bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                      method = 'user', thinned_var_occs.grp, thinned_var_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
bias_thinned_var_evalTbl <- bias_thinned_var_e@results
bias_thinned_var_evalTbl <- bias_thinned_var_evalTbl[with(bias_thinned_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(bias_thinned_var_evalTbl, "./maxentoutputs/bias_thinned_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
bias_thinned_var_evalMods <- bias_thinned_var_e@models
names(bias_thinned_var_evalMods) <- bias_thinned_var_e@results$settings
bias_thinned_var_evalPreds <- bias_thinned_var_e@predictions
# Select your model from the models list
bias_thinned_var_mod <- bias_thinned_var_evalMods[["H_5"]]
# generate cloglog prediction
bias_thinned_var_pred <- ENMeval::maxnet.predictRaster(bias_thinned_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(bias_thinned_var_pred)

#save cloglog prediction
writeRaster(bias_thinned_var_pred, "bias_thinned_variegatus_H_5_cloglog.tif")


# iterate model building over all chosen parameter settings
bias_thinned_tri_e <- ENMeval::ENMevaluate(thinned_tri_occs.xy, Env_sloths, bg.coords = bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                           method = 'user', thinned_tri_occs.grp, thinned_tri_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
bias_thinned_tri_evalTbl <- bias_thinned_tri_e@results
bias_thinned_tri_evalTbl <- bias_thinned_tri_evalTbl[with(bias_thinned_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(bias_thinned_tri_evalTbl, "./maxentoutputs/bias_thinned_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
bias_thinned_tri_evalMods <- bias_thinned_tri_e@models
names(bias_thinned_tri_evalMods) <- bias_thinned_tri_e@results$settings
bias_thinned_tri_evalPreds <- bias_thinned_tri_e@predictions
# Select your model from the models list
bias_thinned_tri_mod <- bias_thinned_tri_evalMods[["H_5"]]
# generate cloglog prediction
bias_thinned_tri_pred <- ENMeval::maxnet.predictRaster(bias_thinned_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(bias_thinned_tri_pred)

#save cloglog prediction
writeRaster(bias_thinned_tri_pred, "bias_thinned_tridactylus_H_5_cloglog.tif")


# Spatial thinning + Bias file w/o iNat -----------------------------------

# iterate model building over all chosen parameter settings
noiNat_bias_thinned_var_e <- ENMeval::ENMevaluate(thinned_var_occs.xy, Env_sloths, bg.coords = noiNat_bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                           method = 'user', thinned_var_occs.grp, thinned_var_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
noiNat_bias_thinned_var_evalTbl <- noiNat_bias_thinned_var_e@results
noiNat_bias_thinned_var_evalTbl <- noiNat_bias_thinned_var_evalTbl[with(noiNat_bias_thinned_var_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(noiNat_bias_thinned_var_evalTbl, "./maxentoutputs/noiNat_bias_thinned_var_evalTbl.csv")
#evaluation table for variegatus with spatial thinning and bias file:
noiNat_bias_thinned_var_evalMods <- noiNat_bias_thinned_var_e@models
names(noiNat_bias_thinned_var_evalMods) <- noiNat_bias_thinned_var_e@results$settings
noiNat_bias_thinned_var_evalPreds <- noiNat_bias_thinned_var_e@predictions
# Select your model from the models list
noiNat_bias_thinned_var_mod <- noiNat_bias_thinned_var_evalMods[["L_5"]]
# generate cloglog prediction
noiNat_bias_thinned_var_pred <- ENMeval::maxnet.predictRaster(noiNat_bias_thinned_var_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(noiNat_bias_thinned_var_pred)

#save cloglog prediction
writeRaster(noiNat_bias_thinned_var_pred, "noiNat_bias_thinned_variegatus_L_5_cloglog.tif")


# iterate model building over all chosen parameter settings
noiNat_bias_thinned_tri_e <- ENMeval::ENMevaluate(thinned_tri_occs.xy, Env_sloths, bg.coords = noiNat_bias_bg.xy, RMvalues = rms, fc = c('L', 'LQ', 'H', 'LQH'), 
                                           method = 'user', thinned_tri_occs.grp, thinned_tri_bg.grp, clamp = TRUE, algorithm = "maxnet")
# unpack the results data frame, the list of models, and the RasterStack of raw predictions
noiNat_bias_thinned_tri_evalTbl <- noiNat_bias_thinned_tri_e@results
noiNat_bias_thinned_tri_evalTbl <- noiNat_bias_thinned_tri_evalTbl[with(noiNat_bias_thinned_tri_evalTbl, order(avg.test.or10pct, -avg.test.AUC)), ]
write_csv(noiNat_bias_thinned_tri_evalTbl, "./maxentoutputs/noiNat_bias_thinned_tri_evalTbl.csv")
#evaluation table for tridactylus with spatial thinning and bias file:
noiNat_bias_thinned_tri_evalMods <- noiNat_bias_thinned_tri_e@models
names(noiNat_bias_thinned_tri_evalMods) <- noiNat_bias_thinned_tri_e@results$settings
noiNat_bias_thinned_tri_evalPreds <- noiNat_bias_thinned_tri_e@predictions
# Select your model from the models list
noiNat_bias_thinned_tri_mod <- noiNat_bias_thinned_tri_evalMods[["H_5"]]
# generate cloglog prediction
noiNat_bias_thinned_tri_pred <- ENMeval::maxnet.predictRaster(noiNat_bias_thinned_tri_mod, Env_sloths, type = 'cloglog', clamp = TRUE)
# plot the model prediction
plot(noiNat_bias_thinned_tri_pred)

#save cloglog prediction
writeRaster(noiNat_bias_thinned_tri_pred, "noiNat_bias_thinned_tridactylus_H_5_cloglog.tif")

# Response curves ---------------------------------------------------------

plot(thinned_var_mod, vars = c('bio10', 'bio13', 'bio18', 'bio2', 'bio3', 'bio4'), type = "cloglog")
# Warnings:
# Warning messages:
#   1: In `[<-.data.frame`(`*tmp*`, , v, value = list(251.742424242424,  ... :
#                                                       provided 100 variables to replace 1 variables

plot(thinned_tri_mod, vars = c('bio8'), type = "cloglog")


# plot(bias_var_mod, vars = c('bio10', 'bio13', 'bio18', 'bio2', 'bio3', 'bio4'), type = "cloglog")
# Need a different format here because I used maxent.jar

plot(bias_tri_mod, vars = c('bio12', 'bio18', 'bio6', 'bio7'), type = "cloglog")


plot(bias_thinned_var_mod, vars = c('bio11', 'bio14', 'bio19', 'bio2', 'bio8'), type = "cloglog")


plot(bias_thinned_tri_mod, vars = c('bio18', 'bio6'), type = "cloglog")


