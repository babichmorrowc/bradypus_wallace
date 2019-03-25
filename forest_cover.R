library(raster)


# Hansen Forest Loss ------------------------------------------------------


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


# SPOT/PROBA-V ------------------------------------------------------------

test_ncdf <- raster("/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/1999/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__19990110__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4")
test2_ncdf <- raster("/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/2017/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__20171231__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4")

