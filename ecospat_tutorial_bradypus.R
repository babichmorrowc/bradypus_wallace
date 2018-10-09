#Based on R tutorial using ecospat package
#https://rsh249.github.io/spatial_bioinformatics/niche_overlap.html

#Load required packages
library(readr)
library(spocc)
library(raster)
library(viridis)
library(ecospat)
library(ENMTools)

# Bradypus data -----------------------------------------------------------

#import data from csvs (cleaned version from literature)
variegatus = read_csv("~/OneDrive - AMNH/Wallace/Data/Bradypus_variegatus_Anderson_Handley_plus_Moraes_Barros_2011.csv",
                      col_types = cols(X4 = col_skip(), X5 = col_skip(),
                                       X6 = col_skip(), X7 = col_skip()))

tridactylus <- read_csv("~/OneDrive - AMNH/Wallace/Data/Bradypus_tridactylus_Anderson_Handley_Moraes_Barros_2011.csv")

#get extent
combine.lat = c(variegatus$latitude, tridactylus$latitude)
combine.lon = c(variegatus$longitude, tridactylus$longitude)
ext_sloths=extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))

#Get environmental data and visualize
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
Env_sloths = crop(sta, ext_sloths)
#Plots annual mean temperature
plot(Env_sloths[[1]], col =viridis(99))
points(variegatus[,2:3], pch = 20, col = "darkorchid4")
points(tridactylus[,2:3], pch = 20, col = "deeppink3")

#background by radius
bg_var = background.points.buffer(variegatus[,2:3], radius = 200000, n = 10*nrow(variegatus), mask = Env_sloths[[1]])
bg_tri = background.points.buffer(tridactylus[,2:3], radius = 200000, n = 10*nrow(tridactylus), mask = Env_sloths[[1]])

# Get environmental data
extract_var = na.omit(cbind(variegatus[,2:3], extract(Env_sloths, variegatus[,2:3]), rep(1, nrow(variegatus))))
extract_tri = na.omit(cbind(tridactylus[,2:3], extract(Env_sloths, tridactylus[,2:3]), rep(1, nrow(tridactylus))))

colnames(extract_var)[ncol(extract_var)] = 'occ'
colnames(extract_tri)[ncol(extract_tri)] = 'occ'

extbg_var = na.omit(cbind(bg_var, extract(Env_sloths, bg_var), rep(0, nrow(bg_var))))
extbg_tri = na.omit(cbind(bg_tri, extract(Env_sloths, bg_tri), rep(0, nrow(bg_tri))))

colnames(extbg_var)[ncol(extbg_var)] = 'occ'
colnames(extbg_tri)[ncol(extbg_tri)] = 'occ'

#merge occ and bg data 
dat_var = rbind(extract_var, extbg_var)
dat_tri = rbind(extract_tri, extbg_tri)

#run pca
pca.env_sloth <- dudi.pca(
  rbind(dat_var, dat_tri)[,3:21],
  scannf=FALSE,
  nf=2
)

#Variable contribution
ecospat.plot.contrib(contrib=pca.env_sloth$co, eigen=pca.env_sloth$eig)

#Calculating various pca scores
scores.globclim<-pca.env_sloth$li # PCA scores for the whole study area

scores.var <- suprow(pca.env_sloth,
                     extract_var[which(extract_var[,22]==1),3:21])$li # PCA scores for the species 1 distribution

scores.tri <- suprow(pca.env_sloth,
                     extract_tri[which(extract_tri[,22]==1),3:21])$li # PCA scores for the species 1 distribution

scores.clim_var <- suprow(pca.env_sloth,dat_var[,3:21])$li # PCA scores for the whole native study area

scores.clim_tri <- suprow(pca.env_sloth,dat_tri[,3:21])$li # PCA scores for the whole native study area

#ecospat.grid.clim.dyn creates a grid with occurrence densities along one or two environmental gradients
#think this is the Broennimann method?
grid.clim_var <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim_var,
  sp = scores.var,
  R = 100
)


grid.clim_tri <- ecospat.grid.clim.dyn(
  glob = scores.globclim,
  glob1 = scores.clim_tri,
  sp = scores.tri,
  R = 100,
  th.sp = 0
)

#Calculate Schoener's overlap metric
D.overlap <- ecospat.niche.overlap (grid.clim_var, grid.clim_tri, cor=T)$D 
D.overlap

#Niche equivalence test
eq.test <- ecospat.niche.equivalency.test(grid.clim_var, grid.clim_tri,
                                          rep=10, alternative = "greater") ##rep = 1000 recommended for operational runs
sim.test <- ecospat.niche.similarity.test(grid.clim_var, grid.clim_tri,
                                          rep=1000, alternative = "greater",
                                          rand.type=2) 
ecospat.plot.overlap.test(eq.test, "D", "Equivalency")
ecospat.plot.overlap.test(sim.test, "D", "Similarity")
