library(raster)

# load data
S00_W40 <- raster("/Users/hellenfellows/Desktop/forest_cover/Hansen_ForestLoss/Hansen_GFC-2017-v1.5_lossyear_00N_040W.tif")
plot(S20_W80)
S20_W70 <- raster("/Users/hellenfellows/Desktop/Hansen_ForestLoss/Hansen_GFC-2017-v1.5_lossyear_20S_070W.tif")
plot(S20_W70)

merge <- merge(S20_W80, S20_W70)
plot(merge)

#list files of the tiles
tile_files <- paste("/Users/hellenfellows/Desktop/forest_cover/Hansen_ForestLoss/", list.files("/Users/hellenfellows/Desktop/forest_cover/Hansen_ForestLoss/"), sep = "")

tile_rasters <- lapply(tile_files, raster)
forest_loss <- do.call("merge", tile_rasters)
plot(forest_loss)
writeRaster(forest_loss, "/Users/hellenfellows/Desktop/forest_cover/forest_loss_year.tif")

#crop var_sdm to same extent as forest_loss
var_crop <- crop(var_sdm, forest_loss@extent)

#list files of the data mask tiles
mask_files <- paste("/Users/hellenfellows/Desktop/forest_cover/Hansen_Datamask/", list.files("/Users/hellenfellows/Desktop/forest_cover/Hansen_Datamask/"), sep = "")

mask_rasters <- lapply(mask_files, raster)
datamask <- do.call("merge", mask_rasters)
plot(datamask)
writeRaster(datamask, "/Users/hellenfellows/Desktop/forest_cover/datamask.tif")



