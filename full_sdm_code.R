# load packages
library(spocc)
library(spThin)
library(dismo)
library(rgeos)
library(ENMeval)
library(dplyr)


# Import occurrence data --------------------------------------------------

# Variegatus data
d.occs <- '/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Occurrence_Data/'
# create path to user occurrences csv file
userOccs.path <- file.path(d.occs, "Bradypus_variegatus_litdata.csv")
# read in csv
userOccs.csv <- read.csv(userOccs.path, header = TRUE)
# remove rows with duplicate coordinates
occs.dups <- duplicated(userOccs.csv[c('longitude', 'latitude')])
occs <- userOccs.csv[!occs.dups,]
# remove NAs
occs <- occs[complete.cases(occs$longitude, occs$latitude), ]
# give all records a unique ID
occs$occID <- row.names(occs)


# Process occurrence data -------------------------------------------------

#Spatial thinning to 40 km
output <- spThin::thin(occs, 'latitude', 'longitude', 'name', thin.par = 40, reps = 100, locs.thinned.list.return = TRUE, write.files = FALSE, verbose = FALSE)

# find the iteration that returns the max number of occurrences
maxThin <- which(sapply(output, nrow) == max(sapply(output, nrow)))
# if there's more than one max, pick the first one
maxThin <- output[[ifelse(length(maxThin) > 1, maxThin[1], maxThin)]]  
# subset occs to match only thinned occs
occs <- occs[as.numeric(rownames(maxThin)),]  


# Obtain environmental data -----------------------------------------------

grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))

# extract environmental values at occ grid cells
locs.vals <- raster::extract(envs[[1]], occs[, c('longitude', 'latitude')])
# remove occs without environmental values
occs <- occs[!is.na(locs.vals), ] 


# Process environmental data ----------------------------------------------

# make SpatialPoints object for buffering
occs.xy <- occs[c('longitude', 'latitude')]
sp::coordinates(occs.xy) <- ~ longitude + latitude
# #Buffer by 1 degree around points
# bgExt <- rgeos::gBuffer(occs.xy, width = 1)
# 
# # crop the environmental rasters by the background extent shape
# envsBgCrop <- raster::crop(envs, bgExt)
# # mask the background extent shape from the cropped raster
# envsBgMsk <- raster::mask(envsBgCrop, bgExt)
# # sample random background points
# bg.xy <- dismo::randomPoints(envsBgMsk, 10000)
# # convert matrix output to data frame
# bg.xy <- as.data.frame(bg.xy) 


# Partition occurrence data -----------------------------------------------

#Partition using block method
occs.xy <- occs[c('longitude', 'latitude')]
group.data <- ENMeval::get.block(occ=occs.xy, bg.coords=bg.xy)

# pull out the occurrence and background partition group numbers from the list
occs.grp <- group.data[[1]]
bg.grp <- group.data[[2]]


# Bias file ------------------------------------------

# load in bias file
bias <- raster("sloth_bias_file.tif")
# mask bias file
#get extent of occurrence data
ext_occs <- extent(c(min(occs$longitude)-5, max(occs$longitude)+5, min(occs$latitude)-5, max(occs$latitude)+5))
env_occs = crop(envs, ext_sloths)
#crop bias file to extent of sloth data
bias <- raster::mask(bias, env_occs[[1]])

bg.xy <- randomPoints(bias, 10000, prob=TRUE)
plot(bias)
points(bg.xy, pch = 20, cex = 0.25)


# Build and evaluate niche model ------------------------------------------



