#Based R tutorial using ecospat package
#https://rsh249.github.io/spatial_bioinformatics/niche_overlap.html

#Load required packages
library(spocc)
library(raster)
library(viridis)


# Sample Tutorial Data ----------------------------------------------------

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
plot(Env[[1]], col =viridis(99))
points(occd1[,2:3], pch=20, col = 'darkred')
points(occd2[,2:3], pch=20, col = 'grey')