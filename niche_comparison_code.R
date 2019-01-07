#Based on code from session6_tutorial_bradypus.R and ecospat_tutorial_bradypus.R

#Load required packages
library(dismo)
library(ENMeval)
library(ENMTools)
library(phyloclim)
library(sp)
library(rgdal)
library(rgeos)
library(spocc)
library(raster)
library(ecospat)
library(readr)


# Bradypus data -----------------------------------------------------------

#import data from csvs (cleaned version from literature)
variegatus <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv")
tridactylus <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")
torquatus <- read_csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_torquatus_litdata.csv")

# Change the species columns to just the species' names
variegatus[1] <- as.factor('variegatus')
tridactylus[1] <- as.factor('tridactylus')
torquatus[1] <- as.factor('torquatus')
# Change column name of first column
names(variegatus)[1] <- "species"
names(tridactylus)[1] <- "species"
names(torquatus)[1] <- "species"

#get extent
combine.lat <- c(variegatus$latitude, tridactylus$latitude, torquatus$latitude)
combine.lon <- c(variegatus$longitude, tridactylus$longitude, torquatus$longitude)
ext_sloths <- extent(c(min(combine.lon)-5, max(combine.lon)+5, min(combine.lat)-5, max(combine.lat)+5))

#Get environmental data
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
Env_sloths = crop(sta, ext_sloths)


# Get Maxent rasters ------------------------------------------------------

#Load rasters

var_raster <- raster("thinned_variegatus_L_1_cloglog.tif")
plot(var_raster)

tri_raster_H5 <- raster("thinned_tridactylus_H_5_cloglog.tif")
plot(tri_raster_H5)
tri_raster_LQH5 <- raster("thinned_tridactylus_LQH_5_cloglog.tif")
plot(tri_raster_LQH5)

tor_raster <- raster("thinned_torquatus_H_3_cloglog.tif")
plot(tor_raster)

#Check that extents are the same
var_raster@extent
tri_raster_H5@extent
tri_raster_LQH5@extent
tor_raster@extent

#Calculate Schoener's D using dismo
var_triH5_overlap <- nicheOverlap(var_raster, tri_raster_H5, stat='D', mask=TRUE, checkNegatives=TRUE)
var_triH5_overlap

var_triLQH5_overlap <- nicheOverlap(var_raster, tri_raster_LQH5, stat='D', mask=TRUE, checkNegatives=TRUE)
var_triLQH5_overlap

var_tor_overlap <- nicheOverlap(var_raster, tor_raster, stat='D', mask=TRUE, checkNegatives=TRUE)
var_tor_overlap

tor_triH5_overlap <- nicheOverlap(tor_raster, tri_raster_H5, stat='D', mask=TRUE, checkNegatives=TRUE)
tor_triH5_overlap

tor_triLQH5_overlap <- nicheOverlap(tor_raster, tri_raster_LQH5, stat='D', mask=TRUE, checkNegatives=TRUE)
tor_triLQH5_overlap

triH5_triLQH5_overlap <- nicheOverlap(tri_raster_H5, tri_raster_LQH5, stat='D', mask=TRUE, checkNegatives=TRUE)
triH5_triLQH5_overlap


var_triH5_ENMOverlap<-raster.overlap(var_raster,tri_raster_H5)
var_triH5_ENMOverlap

# Make environmental PCA --------------------------------------------------

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


# Niche equivalency test --------------------------------------------------

#Broennimann method

#alternative = "greater" => testing for niche conservatism
#alternative = "lower" => testing for niche divergence
#Running only lower

eq.test.lower <- ecospat.niche.equivalency.test(grid.clim_var, grid.clim_tri,
                                                rep=1000, alternative = "lower") ##rep = 1000 recommended for operational runs
ecospat.plot.overlap.test(eq.test.lower, "D", "Equivalency")

#Warren method

# row bind the occurrence data so all occurrences are in 3 rows of Species, X, Y
sites<-rbind(variegatus[,1:3], tridactylus[,1:3])
View(sites)
species <- c('variegatus','tridactylus')

# Change the column names of sites
colnames(sites)<-c("species","longitude","latitude")
samples <- sites[grep(paste(species, collapse = "|"), sites$species), ] # don't think I need this bit
samples <- as.data.frame(samples)

# Tell R where maxent is (the copy that is with dismo).
maxent.exe <- paste(system.file(package="dismo"),"/java/maxent.jar", sep = "")

# Perform niche equivalency test using phyloclim
nicheEquivalency<-niche.equivalency.test(p = samples, env = env, app=maxent.exe, dir = 'NicheEquivalence')
plot(nicheEquivalency)


# Niche similarity test ---------------------------------------------------

#Broennimann method

#compares empirical overlap to overlap between variegatus and random niches in range of tridactylus
sim.test.greater1 <- ecospat.niche.similarity.test(grid.clim_var, grid.clim_tri,
                                                   rep=1000, alternative = "greater",
                                                   rand.type=2)
ecospat.plot.overlap.test(sim.test.greater1, "D", "Similarity")

#compares empirical overlap to overlap between tridactylus and random niches in range of variegatus
sim.test.greater2 <- ecospat.niche.similarity.test(grid.clim_tri, grid.clim_var,
                                                   rep=1000, alternative = "greater",
                                                   rand.type=2)
ecospat.plot.overlap.test(sim.test.greater2, "D", "Similarity")

#Warren method

#Perform niche similarity test using phyloclim
bg.test <- bg.similarity.test(p = samples, env = env, app = maxent.exe, dir = 'background')
plot(bg.test)

