library(raster)
source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/maskRangeR\ testing/maskRangeR/R/svm.R")


#load occurrence data
# variegatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv")
# tridactylus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")
# torquatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_torquatus_litdata.csv")

# Use occurrence dataframes from `thinning_sdm_code.R`
# var_occs, tri_occs, and tor_occs

#load SDMs
# var_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_variegatus_LQH_2.5_cloglog.tif")
# plot(var_sdm)
# tri_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_tridactylus_H_4_cloglog.tif")
# plot(tri_sdm)
# thinned_tor_proj <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_torquatus_H_4.5_cloglog.tif")
# plot(tor_sdm)

# Use SDMs from `thinning_sdm_code.R`
# thinned_var_proj, thinned_tri_proj, thinned_tor_proj
plot(thinned_var_proj)
plot(thinned_tri_proj)
plot(thinned_tor_proj)

# SDMs restricted to bounding box
thinned_tri_proj_bbox2 <- mask(thinned_tri_proj, as(ext_tri, "SpatialPolygons"))
thinned_tor_proj_bbox2 <- mask(thinned_tor_proj, as(ext_tor, "SpatialPolygons"))

#MTP threshold bounding box SDMs
source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/scripts/sdm_threshold.R")
# thinned_var_proj_mtp
thinned_tri_proj_bbox_mtp2 <- mask(thinned_tri_proj_mtp, as(ext_tri, "SpatialPolygons"))
thinned_tor_proj_bbox_mtp2 <- mask(thinned_tor_proj_mtp, as(ext_tor, "SpatialPolygons"))


# Unweighted SVM - occurrence only ---------------------------------------------

#create svm
unweighted_svmSP <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], nrep = 100)
sloth_unweighted_svmSP <- rangeSVM_predict(svm = unweighted_svm_occ, r = thinned_var_proj)
plot(sloth_unweighted_svmSP, col=c("khaki1","darkseagreen1","lightblue"))
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")

#mask sdm predictions
var_unweighted_svmSP <- sloth_unweighted_svmSP == 1
var_unweighted_svmSP[var_unweighted_svmSP == 0] <- NA
plot(var_unweighted_svmSP)
var_unweighted_svmSP_mask <- mask(thinned_var_proj, var_unweighted_svmSP)
plot(var_unweighted_svmSP_mask)

tri_unweighted_svmSP <- sloth_unweighted_svmSP == 2
tri_unweighted_svmSP[tri_unweighted_svmSP == 0] <- NA
plot(tri_unweighted_svmSP)
tri_unweighted_svmSP_mask <- mask(thinned_tri_proj_bbox2, tri_unweighted_svmSP)
plot(tri_unweighted_svmSP_mask, xlim = c(-70, -49), ylim = c(-6, 11))

tor_unweighted_svmSP <- sloth_unweighted_svmSP == 3
tor_unweighted_svmSP[tor_unweighted_svmSP == 0] <- NA
plot(tor_unweighted_svmSP)
tor_unweighted_svmSP_mask <- mask(thinned_tor_proj_bbox2, tor_unweighted_svmSP)
plot(tor_unweighted_svmSP_mask, xlim = c(-46, -34), ylim = c(-24, -8))

# mask the thresholded SDMs (MTP)
var_unweighted_svmSP_thresh_mask <- mask(thinned_var_proj_mtp, var_unweighted_svmSP)
plot(var_unweighted_svmSP_thresh_mask, zlim = c(0,1))

tri_unweighted_svmSP_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_unweighted_svmSP)
plot(tri_unweighted_svmSP_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_unweighted_svmSP_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_unweighted_svmSP)
plot(tor_unweighted_svmSP_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))


# Weighted SVM - occurrence only ------------------------------------------

#create svm
weighted_svmSP <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], nrep = 100, weight = TRUE)
sloth_weighted_svmSP <- rangeSVM_predict(svm = weighted_svmSP, r = thinned_var_proj)
plot(sloth_weighted_svmSP, col=c("khaki1","darkseagreen1","lightblue"))
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")

#mask sdm predictions
var_weighted_svmSP <- sloth_weighted_svmSP == 1
var_weighted_svmSP[var_weighted_svmSP == 0] <- NA
plot(var_weighted_svmSP)
var_weighted_svmSP_mask <- mask(thinned_var_proj, var_weighted_svmSP)
plot(var_weighted_svmSP_mask)

tri_weighted_svmSP <- sloth_weighted_svmSP == 2
tri_weighted_svmSP[tri_weighted_svmSP == 0] <- NA
plot(tri_weighted_svmSP)
tri_weighted_svmSP_mask <- mask(thinned_tri_proj_bbox2, tri_weighted_svmSP)
plot(tri_weighted_svmSP_mask, xlim = c(-70, -49), ylim = c(-6, 11))

tor_weighted_svmSP <- sloth_weighted_svmSP == 3
tor_weighted_svmSP[tor_weighted_svmSP == 0] <- NA
plot(tor_weighted_svmSP)
tor_weighted_svmSP_mask <- mask(thinned_tor_proj_bbox2, tor_weighted_svmSP)
plot(tor_weighted_svmSP_mask, xlim = c(-46, -34), ylim = c(-24, -8))

# mask the thresholded SDMs (MTP)
var_weighted_svmSP_thresh_mask <- mask(thinned_var_proj_mtp, var_weighted_svmSP)
plot(var_weighted_svmSP_thresh_mask, zlim = c(0,1))

tri_weighted_svmSP_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_weighted_svmSP)
plot(tri_weighted_svmSP_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_weighted_svmSP_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_weighted_svmSP)
plot(tor_weighted_svmSP_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))



# Unweighted SVM - hybrid -------------------------------------------------

#create svm
unweighted_svmHYB <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj), nrep = 100)
sloth_unweighted_svmHYB <- rangeSVM_predict(svm = unweighted_svmHYB, r = thinned_var_proj, sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj))
plot(sloth_unweighted_svmHYB, col=c("khaki1","darkseagreen1","lightblue"))
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")

#mask sdm predictions
var_unweighted_svmHYB <- sloth_unweighted_svmHYB == 1
var_unweighted_svmHYB[var_unweighted_svmHYB == 0] <- NA
plot(var_unweighted_svmHYB)
var_unweighted_svmHYB_mask <- mask(thinned_var_proj, var_unweighted_svmHYB)
plot(var_unweighted_svmHYB_mask)

tri_unweighted_svmHYB <- sloth_unweighted_svmHYB == 2
tri_unweighted_svmHYB[tri_unweighted_svmHYB == 0] <- NA
plot(tri_unweighted_svmHYB)
tri_unweighted_svmHYB_mask <- mask(thinned_tri_proj_bbox2, tri_unweighted_svmHYB)
plot(tri_unweighted_svmHYB_mask, xlim = c(-70, -49), ylim = c(-6, 11))

tor_unweighted_svmHYB <- sloth_unweighted_svmHYB == 3
tor_unweighted_svmHYB[tor_unweighted_svmHYB == 0] <- NA
plot(tor_unweighted_svmHYB)
tor_unweighted_svmHYB_mask <- mask(thinned_tor_proj_bbox2, tor_unweighted_svmHYB)
plot(tor_unweighted_svmHYB_mask, xlim = c(-46, -34), ylim = c(-24, -8))

# mask the thresholded SDMs (MTP)
var_unweighted_svmHYB_thresh_mask <- mask(thinned_var_proj_mtp, var_unweighted_svmHYB)
plot(var_unweighted_svmHYB_thresh_mask, zlim = c(0,1))

tri_unweighted_svmHYB_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_unweighted_svmHYB)
plot(tri_unweighted_svmHYB_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_unweighted_svmHYB_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_unweighted_svmHYB)
plot(tor_unweighted_svmHYB_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))


# Weighted SVM - hybrid ---------------------------------------------------

#create svm
weighted_svmHYB <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj), nrep = 100, weight = TRUE)
sloth_weighted_svmHYB <- rangeSVM_predict(svm = weighted_svmHYB, r = thinned_var_proj, sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj))
plot(sloth_weighted_svmHYB, col=c("khaki1","darkseagreen1","lightblue"))
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")

#mask sdm predictions
var_weighted_svmHYB <- sloth_weighted_svmHYB == 1
var_weighted_svmHYB[var_weighted_svmHYB == 0] <- NA
plot(var_weighted_svmHYB)
var_weighted_svmHYB_mask <- mask(thinned_var_proj, var_weighted_svmHYB)
plot(var_weighted_svmHYB_mask)

tri_weighted_svmHYB <- sloth_weighted_svmHYB == 2
tri_weighted_svmHYB[tri_weighted_svmHYB == 0] <- NA
plot(tri_weighted_svmHYB)
tri_weighted_svmHYB_mask <- mask(thinned_tri_proj_bbox2, tri_weighted_svmHYB)
plot(tri_weighted_svmHYB_mask)

tor_weighted_svmHYB <- sloth_weighted_svmHYB == 3
tor_weighted_svmHYB[tor_weighted_svmHYB == 0] <- NA
plot(tor_weighted_svmHYB)
tor_weighted_svmHYB_mask <- mask(thinned_tor_proj, tor_weighted_svmHYB)
plot(tor_weighted_svmHYB_mask)
