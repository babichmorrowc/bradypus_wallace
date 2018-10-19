#Based on Session 6 R tutorial (sent by Pete)

#Load required packages
require(dismo)
require(ENMeval)
require(phyloclim)
require(sp)
require(rgdal)
require(rgeos)

#ENMTools package
require(devtools)
#install_github("danlwarren/ENMTools")
library(ENMTools)


# Calculate Niche Overlap -------------------------------------------------

#Load rasters

var_raster <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/variegatus_layer.asc.txt")
plot(var_raster)

tri_raster <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/tridactylus_layer.asc.txt")
plot(tri_raster)

#crop rasters to get same extent
var_raster_crop <- crop(var_raster, tri_raster@extent)
tri_raster_crop <- crop(tri_raster, var_raster_crop@extent)
#Check that extents are the same
var_raster_crop@extent
tri_raster_crop@extent


#Calculate Schoener's D using dismo
sloth_overlap <- nicheOverlap(var_raster_crop, tri_raster_crop, stat='D', mask=TRUE, checkNegatives=TRUE)
sloth_overlap

ENMOverlap<-raster.overlap(var_raster_crop,tri_raster_crop)
ENMOverlap


# Identity Test -----------------------------------------------------------

# Load environmental variables
#get extent
ext_sloth_raster = extent(c(var_raster_crop@extent@xmin-5, var_raster_crop@extent@xmax+5, var_raster_crop@extent@ymin-5, var_raster_crop@extent@ymax+5))
grids = list.files("/Users/hellenfellows/Desktop/bio_2-5m_bil", pattern = "*.bil$")
sta = stack(paste0("/Users/hellenfellows/Desktop/bio_2-5m_bil/", grids))
env = crop(sta, ext_sloth_raster)

# Load occurrence records for both species
# These should be csv files of records where columns are: "Species, X, Y".
#variegatus <- read_csv("~/OneDrive - AMNH/Wallace/Data/Bradypus_variegatus_Anderson_Handley_plus_Moraes_Barros_2011.csv",
#                      col_types = cols(X4 = col_skip(), X5 = col_skip(),
#                                       X6 = col_skip(), X7 = col_skip()))

#tridactylus <- read_csv("~/OneDrive - AMNH/Wallace/Data/Bradypus_tridactylus_Anderson_Handley_Moraes_Barros_2011.csv")

# Change the species columns to just the species' names
variegatus[1] <- as.factor('variegatus')
tridactylus[1] <- as.factor('tridactylus')
# Change column name of first column
names(variegatus)[1] <- "species"
names(tridactylus)[1] <- "species"

# row bind them so all occurrences are in 3 rows of Species, X, Y
sites<-rbind(variegatus, tridactylus)
View(sites)
species <- c('variegatus','tridactylus')

# Change the column names of sites
colnames(sites)<-c("species","longitude","latitude")
samples <- sites[grep(paste(species, collapse = "|"), sites$species), ] # don't think I need this bit

# Tell R where maxent is (the copy that is with dismo).
maxent.exe <- paste(system.file(package="dismo"),"/java/maxent.jar", sep = "")

# Perform niche equivalency test using phyloclim
nicheEquivalency<-niche.equivalency.test(p = samples, env = env, app=maxent.exe, dir = 'NicheEquivalence')
# Error in file(fname, "r") : cannot open the connection
# In addition: Warning messages:
#   1: In niche.equivalency.test(p = samples, env = env, app = maxent.exe,  :
#                                  2 presence points with missing environmental data removed
#                                2: In file(fname, "r") :
#                                  cannot open file 'NicheEquivalence/out/_proj.asc': No such file or directory

# Perform niche equivalency test using ENMTools

var_enm<-enmtools.species()
var_enm$species.name <- "variegatus"
var_enm$presence.points <- variegatus[,2:3]
var_enm$range <- background.raster.buffer(var_enm$presence.points, 50000, mask = env)
var_enm$background.points <- background.points.buffer(points = var_enm$presence.points,
                                                   radius = 20000, n = 1000, mask = env[[1]])

tri_enm<-enmtools.species()
tri_enm$species.name <- "tridactylus"
tri_enm$presence.points <- tridactylus[,2:3]
tri_enm$range <- background.raster.buffer(tri_enm$presence.points, 50000, mask = env)
tri_enm$background.points <- background.points.buffer(points = tri_enm$presence.points,
                                                      radius = 20000, n = 1000, mask = env[[1]])

#Run identity test using ENMTools
id.glm <- identity.test(species.1 = var_enm, species.2 = tri_enm, env = env, type = "glm", nreps = 4)
id.glm

id.mx <- identity.test(species.1 = var_enm, species.2 = tri_enm, env = env, type = "mx", nreps = 99)
id.mx
saveRDS(id.mx, file = "id_test_mx.rds")
