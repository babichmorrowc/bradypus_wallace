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

#convert occ calls to a dataframe
occd1 = data.frame(occ2df(d1))
head(occd1)

#We *know* these should be in North America so clip the distributions
occd1 = occd1[occd1$lon <= -40,]
occd1 = na.omit(occd1)
#get extent
ext=extent(c(min(occd1$longitude)-5, max(occd1$longitude)+5, min(occd1$latitude)-5, max(occd1$latitude)+5))

#Get environmental data and visualize
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
Env = crop(sta, ext)
#Plots annual mean temperature
plot(Env[[1]], col =viridis(99))
points(occd1[,2:3], pch=20, col = 'darkred')


## Library ecospat - Niche similarity/equivalency testing ------------------

library(ecospat)
library(ENMTools)

#PCA-env

#background by radius
#returns (10 times the number of rows in occd1) randomly sampled points
#within a 200000 m radius around the points in occd1
bg1 = background.points.buffer(occd1[,2:3], radius = 200000, n = 10*nrow(occd1), mask = Env[[1]])

#get environmental data for occurrence points
#with a column of 1's to indicate presence
extract1 = na.omit(cbind(occd1[,2:3], extract(Env, occd1[,2:3]), rep(1, nrow(occd1))))
colnames(extract1)[ncol(extract1)] = 'occ'
#get environmental data for background points
#with a column of 0's to indicate absence
extbg1 = na.omit(cbind(bg1, extract(Env, bg1), rep(0, nrow(bg1))))
colnames(extbg1)[ncol(extbg1)] = 'occ'

#merge occ and background data
dat1 = rbind(extract1, extbg1)

#run pca
pca.env <- dudi.pca(
  dat1[,3:21],
  scannf=F,
  nf=2
)

#plot pca
ecospat.plot.contrib(contrib=pca.env$co, eigen = pca.env$eig)





