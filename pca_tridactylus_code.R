library(raster)
library(RStoolbox)
library(ggbiplot)

#grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
#envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))

#get extent
tri.lat <- tri_occs$latitude
tri.lon <- tri_occs$longitude
ext_tri <- extent(c(min(tri.lon)-5, max(tri.lon)+5, min(tri.lat)-5, max(tri.lat)+5))
Env_tridactylus <- crop(envs, ext_tri)

pcamap<-rasterPCA(Env_tridactylus,spca=TRUE)
summary(pcamap$model)


# A different approach

thinned_tri_locs.env <- raster::extract(envs, thinned_tri_occs[, c('longitude', 'latitude')])
thinned_tri_locs.env <- cbind(thinned_tri_locs.env, thinned_tri_occs[, c('longitude', 'latitude')])

tri_pca <- prcomp(thinned_tri_locs.env[, 1:19], center = TRUE, scale. = TRUE)
summary(tri_pca)
ggbiplot(tri_pca)


#Pete's code

####create bg coordinates, extract env values for bg coords and locs
bgvals<-extract(tri_envsBgMsk, buffer_tri_bg.xy)
locvals<-extract(tri_envsBgMsk, thinned_tri_occs[, c('longitude', 'latitude')])
## put them together
bothvals<-rbind(locvals, bgvals)
# Run pca
tri_pca<-prcomp(bothvals)
# get scores only
scores <- tri_pca$x
# plotting
plot(scores)
points(scores[1:15,], col = "red") # change 15 to the number of occs you have






