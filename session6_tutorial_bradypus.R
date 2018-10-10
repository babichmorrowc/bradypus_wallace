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

#Calculate Schoener's D
sloth_overlap <- nicheOverlap(var_raster_crop, tri_raster_crop, stat='D', mask=TRUE, checkNegatives=TRUE)
sloth_overlap

