#Based on R tutorial using ecospat package
#https://rsh249.github.io/spatial_bioinformatics/niche_overlap.html

#Load required packages
library(readr)
library(spocc)
library(raster)
library(viridis)


# Sample Tutorial Data ----------------------------------------------------


## Getting Started ---------------------------------------------------------

#Search for species occurrence data
d1 = occ('Ambystoma texanum') #some kinda salamander
d2 = occ('Ambystoma tigrinum')

#convert occ calls to a dataframe
occd1 = data.frame(occ2df(d1))
head(occd1)
occd2 = data.frame(occ2df(d2))

#We *know* these should be in North America so clip the distributions
occd1 = occd1[occd1$lon <= -40,]
occd1 = na.omit(occd1)
occd2 = occd2[occd2$lon <= -40,]
occd2 = na.omit(occd2)
#get extent
combine.lat = c(occd1$lat, occd2$lat)
combine.lon = c(occd1$lon, occd2$lon)
ext=extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))

#Get environmental data and visualize
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
Env = crop(sta, ext)
#Plots annual mean temperature
plot(Env[[1]], col =viridis(99))
points(occd1[,2:3], pch=20, col = 'darkred')
points(occd2[,2:3], pch=20, col = 'grey')


## Library ecospat - Niche similarity/equivalency testing ------------------

library(ecospat)
library(ENMTools)

#PCA-env

#background by radius
#returns (10 times the number of rows in occd1) randomly sampled points
#within a 200000 m radius around the points in occd1
bg1 = background.points.buffer(occd1[,2:3], radius = 200000, n = 10*nrow(occd1), mask = Env[[1]])
bg2 = background.points.buffer(occd2[,2:3], radius = 200000, n = 10*nrow(occd2), mask = Env[[1]])

#get environmental data for occurrence points
#with a column of 1's to indicate presence
extract1 = na.omit(cbind(occd1[,2:3], extract(Env, occd1[,2:3]), rep(1, nrow(occd1))))
colnames(extract1)[ncol(extract1)] = 'occ'
extract2 = na.omit(cbind(occd2[,2:3], extract(Env, occd2[,2:3]), rep(1, nrow(occd2))))
colnames(extract2)[ncol(extract2)] = 'occ'
#get environmental data for background points
#with a column of 0's to indicate absence
extbg1 = na.omit(cbind(bg1, extract(Env, bg1), rep(0, nrow(bg1))))
colnames(extbg1)[ncol(extbg1)] = 'occ'
extbg2 = na.omit(cbind(bg2, extract(Env, bg2), rep(0, nrow(bg2))))
colnames(extbg2)[ncol(extbg2)] = 'occ'

#merge occ and background data
dat1 = rbind(extract1, extbg1)
dat2 = rbind(extract2, extbg2)

#run pca
pca.env <- dudi.pca(
  rbind(dat1, dat2)[,3:21],
  scannf=FALSE,
  nf=2
)

#plot pca
ecospat.plot.contrib(contrib=pca.env$co, eigen = pca.env$eig)

scores.globclim<-pca.env$li # PCA scores for the whole study area

scores.globclim<-pca.env$li # PCA scores for the whole study area (all points)

scores.sp1 <- suprow(pca.env,
                     extract1[which(extract1[,22]==1),3:21])$li # PCA scores for the species 1 distribution

scores.sp2 <- suprow(pca.env,
                     extract2[which(extract2[,22]==1),3:21])$li # PCA scores for the species 1 distribution

scores.clim1 <- suprow(pca.env,dat1[,3:21])$li # PCA scores for the whole native study area

scores.clim2 <- suprow(pca.env,dat2[,3:21])$li # PCA scores for the whole native study area


grid.clim1 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim1,
  sp = scores.sp1,
  R = 100,
  th.sp = 0
)
grid.clim2 <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim2,
  sp = scores.sp2,
  R = 100,
  th.sp = 0
)

D.overlap <- ecospat.niche.overlap (grid.clim1, grid.clim2, cor=T)$D 
D.overlap




