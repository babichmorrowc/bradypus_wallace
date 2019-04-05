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
getSPOT <- function(path = "/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/",
                    year, layer = "fcover"){
  # layer can be one of: fcover, fcover_err, nland, ngood, nmeanvalid,
  # ncumvalid, surfaceflag, retrievalflag
  data <- raster(paste0(path, year, "/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__",
                        year, "0110__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4"),
                 varname = layer)
  return(data)
}

test_ncdf1999 <- raster("/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/1999/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__19990110__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4")
test2_ncdf2017 <- raster("/Users/hellenfellows/Desktop/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__1999-2017__UHAM-ICDC__v01.0_90W-30W_30S-20N.nc4/2017/c_gls_FCOVER__RT6_global_V2.0.1__0.5deg__20171231__UHAM-ICDC__v01.0_270.000E-330.000E_30.0000S-20.0000N.nc4")

plot(test_ncdf1999)
plot(test2_ncdf2017)


# MODIS -------------------------------------------------------------------

getMODIS <- function(path = "/Users/hellenfellows/Desktop/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2001-2017__fv0.02_90W-30W_30S-20N.nc4/",
                     year, layer = "landcover_igbp"){
  # layer can be one of: landcover_igbp, confidence_igbp, water_igbp,
  # evergreen_needleleaf_forest_igbp, evergreen_broadleaf_forest_igbp,
  # deciduous_needleleaf_forest_igbp, deciduous_broadleaf_forest_igbp,
  # mixed_forest_igbp, closed_shrublands_igbp, open_shrublands_igbp,
  # woody_savannas_igbp, savannas_igbp, grasslands_igbp, permanent_wetlands_igbp,
  # croplands_igbp, urban_and_builtup_igbp, cropland_natural_vegetation_mosaic_igbp,
  # snowandice_igbp, barren_sparsely_vegetated_igbp, landcover_lai,
  # confidence_lai, water_lai, grasslands_lai, shrublands_lai,
  # broadleaf_croplands_lai, savannas_lai, evergreen_broadleaf_forests_lai,
  # deciduous_broadleaf_forests_lai, evergreen_needleleaf_forests_lai,
  # deciduous_needleleaf_forests_lai, non_vegetated_lai, urban_and_builtup_lai
  data <- raster(paste0(path, "MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__",
                        year, "_fv0.02_270.000E-330.000E_30.0000S-20.0000N.nc4"),
                 varname = layer)
  return(data)
}


test_modis2017 <- raster("/Users/hellenfellows/Desktop/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2001-2017__fv0.02_90W-30W_30S-20N.nc4/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2017_fv0.02_270.000E-330.000E_30.0000S-20.0000N.nc4", varname = "evergreen_broadleaf_forest_igbp")
plot(test_modis2017)

test_modis2001 <- raster("/Users/hellenfellows/Desktop/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2001-2017__fv0.02_90W-30W_30S-20N.nc4/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg__2001_fv0.02_270.000E-330.000E_30.0000S-20.0000N.nc4", varname = "evergreen_broadleaf_forest_igbp")
plot(test_modis2001)

# need to mask out the ocean

# extract raster value for given coordinate
value <- extract(test_modis2001, variegatus_gbif_buffer[1,2:3])



