library(raster)
library(RStoolbox)
library(ggbiplot)
library(alphahull)

#grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
#envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))

#Pete's code

####create bg coordinates, extract env values for bg coords and locs
bgvals<-extract(tri_envsBgMsk, buffer_tri_bg.xy)
locvals<-extract(tri_envsBgMsk, thinned_tri_occs[, c('longitude', 'latitude')])
## put them together
bothvals<-rbind(locvals, bgvals)
# Run pca
tri_pca<-prcomp(bothvals)
summary(tri_pca)
# ggbiplot(tri_pca)
# get scores only
scores <- tri_pca$x
# plotting
plot(scores)
points(scores[1:15,], col = "red") # change 15 to the number of occs you have

####create bg coordinates, extract env values for bg coords and locs
bgvals_4 <- extract(tri_envsBgMsk_4, buffer_tri_bg.xy_4)
locvals_4 <- extract(tri_envsBgMsk_4, thinned_tri_occs[, c('longitude', 'latitude')])
## put them together
bothvals_4<-rbind(locvals_4, bgvals_4)
# Run pca
tri_pca_4 <- prcomp(bothvals_4)
summary(tri_pca_4)
# ggbiplot(tri_pca)
# get scores only
scores_4 <- tri_pca_4$x
# plotting
plot(scores_4)
points(scores_4[1:15,], col = "red") # change 15 to the number of occs you have



####create bg coordinates, extract env values for bg coords and locs
var_bgvals<-extract(var_envsBgMsk, buffer_var_bg.xy)
var_locvals<-extract(var_envsBgMsk, thinned_var_occs[, c('longitude', 'latitude')])
## put them together
var_bothvals<-rbind(var_locvals, var_bgvals)
# Run pca
var_pca<-prcomp(var_bothvals)
summary(var_pca)
# ggbiplot(tri_pca)
# get scores only
var_scores <- var_pca$x
# plotting
plot(var_scores)
points(var_scores[1:132,], col = "red", pch = 19)


# Alpha hull --------------------------------------------------------------

tri_bg_alpha20 <- ahull(x = thinned_tri_occs$longitude, y = thinned_tri_occs$latitude, alpha = 20)
plot(tri_bg_alpha20)
tri_bg_alpha20 <- ahull2poly(tri_bg_alpha20)
plot(tri_bg_alpha20)
colnames(tri_bg_alpha10@polygons[[1]]@Polygons[[1]]@coords) <- c("long", "lat")

tri_bg_alpha20_buffer <- rgeos::gBuffer(tri_bg_alpha20, width = 1)
plot(tri_bg_alpha20_buffer)

hull_tri_envsBgCrop <- raster::crop(Env_sloths, tri_bg_alpha20_buffer)
hull_tri_envsBgMsk <- raster::mask(hull_tri_envsBgCrop, tri_bg_alpha20_buffer)

tri_bg_alpha20 <- dismo::randomPoints(hull_tri_envsBgMsk, 10000)

####create bg coordinates, extract env values for bg coords and locs
alpha_bgvals<-extract(hull_tri_envsBgMsk, tri_bg_alpha20)
locvals<-extract(hull_tri_envsBgMsk, thinned_tri_occs[, c('longitude', 'latitude')])
## put them together
alpha_bothvals<-rbind(locvals, alpha_bgvals)
# Run pca
alpha_tri_pca<-prcomp(alpha_bothvals)
summary(alpha_tri_pca)
# ggbiplot(tri_pca)
# get scores only
alpha_scores <- alpha_tri_pca$x
# plotting
plot(alpha_scores)
points(alpha_scores[1:15,], col = "red") # change 15 to the number of occs you have

