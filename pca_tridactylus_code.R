library(raster)
library(RStoolbox)

#grids <- list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
#envs <- stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))

#get extent
tri.lat <- tri_occs$latitude
tri.lon <- tri_occs$longitude
ext_tri <- extent(c(min(tri.lon)-5, max(tri.lon)+5, min(tri.lat)-5, max(tri.lat)+5))
Env_tridactylus <- crop(envs, ext_tri)

pcamap<-rasterPCA(Env_tridactylus,spca=TRUE)
summary(pcamap$model)



