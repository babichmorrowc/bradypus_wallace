library(raster)
source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/maskRangeR\ testing/maskRangeR/R/svm.R")


#load occurrence data
# variegatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv")
# tridactylus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")
# torquatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_torquatus_litdata.csv")

# Use occurrence dataframes from `thinning_sdm_code.R`
# variegatus_occs, tridactylus_occs, and torquatus_occs

#load SDMs
# var_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_variegatus_LQH_2.5_cloglog.tif")
# plot(var_sdm)
# tri_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_tridactylus_H_4_cloglog.tif")
# plot(tri_sdm)
# thinned_tor_proj <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_torquatus_H_4.5_cloglog.tif")
# plot(tor_sdm)

# Use SDMs from `thinning_sdm_code.R`
# thinned_var_proj, thinned_tri_proj, thinned_tor_proj
# plot(thinned_var_proj)
# plot(thinned_tri_proj)
# plot(thinned_tor_proj)

# Use Checkerboard 1, aggregation 10 sdms
plot(var_checkerboard1.10_proj)
plot(tri_checkerboard1.10_proj2)
plot(tor_checkerboard1.10_proj2)

# SDMs restricted to bounding box
# thinned_tri_proj_bbox2 <- mask(thinned_tri_proj, as(ext_tri, "SpatialPolygons"))
# thinned_tor_proj_bbox2 <- mask(thinned_tor_proj, as(ext_tor, "SpatialPolygons"))
plot(tri_checkerboard1.10_proj)
plot(tor_checkerboard1.10_proj)

#MTP threshold bounding box SDMs
source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/scripts/sdm_threshold.R")
# thinned_var_proj_mtp
# thinned_tri_proj_bbox_mtp2 <- mask(thinned_tri_proj_mtp, as(ext_tri, "SpatialPolygons"))
# thinned_tor_proj_bbox_mtp2 <- mask(thinned_tor_proj_mtp, as(ext_tor, "SpatialPolygons"))


# Unweighted SVM - occurrence only ---------------------------------------------

#create svm
unweighted_svmSP <- rangeSVM(variegatus_occs[,2:3], tridactylus_occs[,2:3], torquatus_occs[,2:3], nrep = 100)
sloth_unweighted_svmSP <- rangeSVM_predict(svm = unweighted_svmSP, r = var_checkerboard1.10_proj)
# png("sp_unweighted_svm.png", units = "in", width = 5, height = 4, res = 300)
plot(sloth_unweighted_svmSP, col=c("khaki1","darkseagreen1", "lightblue"))
points(variegatus_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tridactylus_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(torquatus_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")
# dev.off()

#mask sdm predictions
var_unweighted_svmSP <- sloth_unweighted_svmSP == 1
var_unweighted_svmSP[var_unweighted_svmSP == 0] <- NA
plot(var_unweighted_svmSP)
var_unweighted_svmSP_mask <- mask(var_checkerboard1.10_proj, var_unweighted_svmSP)
plot(var_checkerboard1.10_proj, col = gray.colors(1000, start = 0, end = 1), legend = FALSE)
plot(var_unweighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE)
points(variegatus_occs[,2:3], col = "black", bg = "darkorange1", pch = 21, cex = 0.5)

tri_unweighted_svmSP <- sloth_unweighted_svmSP == 2
tri_unweighted_svmSP[tri_unweighted_svmSP == 0] <- NA
plot(tri_unweighted_svmSP)
tri_unweighted_svmSP_mask <- mask(tri_checkerboard1.10_proj2, tri_unweighted_svmSP)
png("tri_unweightedSP_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tri_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-70, -49), ylim = c(-6, 11))
plot(tri_unweighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-70, -49), ylim = c(-6, 11))
points(tridactylus_occs[,2:3], pch = 21, cex = 0.5, col = "black", bg = "springgreen3")
dev.off()

tor_unweighted_svmSP <- sloth_unweighted_svmSP == 3
tor_unweighted_svmSP[tor_unweighted_svmSP == 0] <- NA
plot(tor_unweighted_svmSP)
tor_unweighted_svmSP_mask <- mask(tor_checkerboard1.10_proj2, tor_unweighted_svmSP)
png("tor_unweightedSP_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tor_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-46, -34), ylim = c(-24, -8))
plot(tor_unweighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-46, -34), ylim = c(-24, -8))
points(torquatus_occs[,2:3], col = "black", bg = "deepskyblue", pch = 21, cex = 0.5)
dev.off()

# mask the thresholded SDMs (MTP)
var_unweighted_svmSP_thresh_mask <- mask(thinned_var_proj_mtp, var_unweighted_svmSP)
plot(var_unweighted_svmSP_thresh_mask, zlim = c(0,1))

tri_unweighted_svmSP_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_unweighted_svmSP)
plot(tri_unweighted_svmSP_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_unweighted_svmSP_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_unweighted_svmSP)
plot(tor_unweighted_svmSP_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))


# Weighted SVM - occurrence only ------------------------------------------

#create svm
weighted_svmSP <- rangeSVM(variegatus_occs[,2:3], tridactylus_occs[,2:3], torquatus_occs[,2:3], nrep = 100, weight = TRUE)
sloth_weighted_svmSP <- rangeSVM_predict(svm = weighted_svmSP, r = var_checkerboard1.10_proj)
png("sp_weighted_svm.png", units = "in", width = 5, height = 4, res = 300)
plot(sloth_weighted_svmSP, col=c("khaki1","darkseagreen1", "lightblue"))
points(variegatus_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tridactylus_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(torquatus_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")
dev.off()

#mask sdm predictions
var_weighted_svmSP <- sloth_weighted_svmSP == 1
var_weighted_svmSP[var_weighted_svmSP == 0] <- NA
plot(var_weighted_svmSP)
var_weighted_svmSP_mask <- mask(var_checkerboard1.10_proj, var_weighted_svmSP)
png("var_weightedSP_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(var_checkerboard1.10_proj, col = gray.colors(1000, start = 0, end = 1), legend = FALSE)
plot(var_weighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE)
points(variegatus_occs[,2:3], col = "black", bg = "darkorange1", pch = 21, cex = 0.5)
dev.off()

tri_weighted_svmSP <- sloth_weighted_svmSP == 2
tri_weighted_svmSP[tri_weighted_svmSP == 0] <- NA
plot(tri_weighted_svmSP)
tri_weighted_svmSP_mask <- mask(tri_checkerboard1.10_proj2, tri_weighted_svmSP)
png("tri_weightedSP_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tri_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-70, -49), ylim = c(-6, 11))
plot(tri_weighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-70, -49), ylim = c(-6, 11))
points(tridactylus_occs[,2:3], pch = 21, cex = 0.5, col = "black", bg = "springgreen3")
dev.off()

tor_weighted_svmSP <- sloth_weighted_svmSP == 3
tor_weighted_svmSP[tor_weighted_svmSP == 0] <- NA
plot(tor_weighted_svmSP)
tor_weighted_svmSP_mask <- mask(tor_checkerboard1.10_proj2, tor_weighted_svmSP)
png("tor_weightedSP_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tor_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-46, -34), ylim = c(-24, -8))
plot(tor_weighted_svmSP_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-46, -34), ylim = c(-24, -8))
points(torquatus_occs[,2:3], col = "black", bg = "deepskyblue", pch = 21, cex = 0.5)
dev.off()

# mask the thresholded SDMs (MTP)
var_weighted_svmSP_thresh_mask <- mask(thinned_var_proj_mtp, var_weighted_svmSP)
plot(var_weighted_svmSP_thresh_mask, zlim = c(0,1))

tri_weighted_svmSP_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_weighted_svmSP)
plot(tri_weighted_svmSP_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_weighted_svmSP_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_weighted_svmSP)
plot(tor_weighted_svmSP_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))



# Unweighted SVM - hybrid -------------------------------------------------

#create svm
unweighted_svmHYB <- rangeSVM(variegatus_occs[,2:3], tridactylus_occs[,2:3], torquatus_occs[,2:3],
                              sdm = raster::stack(var_checkerboard1.10_proj, tri_checkerboard1.10_proj2, tor_checkerboard1.10_proj2), nrep = 100)
sloth_unweighted_svmHYB <- rangeSVM_predict(svm = unweighted_svmHYB, r = var_checkerboard1.10_proj, sdm = raster::stack(var_checkerboard1.10_proj, tri_checkerboard1.10_proj2, tor_checkerboard1.10_proj2))
png("hyb_unweighted_svm.png", units = "in", width = 5, height = 4, res = 300)
plot(sloth_unweighted_svmHYB, col=c("khaki1","darkseagreen1","lightblue"))
points(variegatus_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tridactylus_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(torquatus_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")
dev.off()

#mask sdm predictions
var_unweighted_svmHYB <- sloth_unweighted_svmHYB == 1
var_unweighted_svmHYB[var_unweighted_svmHYB == 0] <- NA
plot(var_unweighted_svmHYB)
var_unweighted_svmHYB_mask <- mask(var_checkerboard1.10_proj, var_unweighted_svmHYB)
plot(var_unweighted_svmHYB_mask)
png("var_unweightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(var_checkerboard1.10_proj, col = gray.colors(1000, start = 0, end = 1), legend = FALSE)
plot(var_unweighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE)
points(variegatus_occs[,2:3], col = "black", bg = "darkorange1", pch = 21, cex = 0.5)
dev.off()

tri_unweighted_svmHYB <- sloth_unweighted_svmHYB == 2
tri_unweighted_svmHYB[tri_unweighted_svmHYB == 0] <- NA
plot(tri_unweighted_svmHYB)
tri_unweighted_svmHYB_mask <- mask(tri_checkerboard1.10_proj2, tri_unweighted_svmHYB)
plot(tri_unweighted_svmHYB_mask, xlim = c(-70, -49), ylim = c(-6, 11))
png("tri_unweightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tri_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-70, -49), ylim = c(-6, 11))
plot(tri_unweighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-70, -49), ylim = c(-6, 11))
points(tridactylus_occs[,2:3], pch = 21, cex = 0.5, col = "black", bg = "springgreen3")
dev.off()

tor_unweighted_svmHYB <- sloth_unweighted_svmHYB == 3
tor_unweighted_svmHYB[tor_unweighted_svmHYB == 0] <- NA
plot(tor_unweighted_svmHYB)
tor_unweighted_svmHYB_mask <- mask(tor_checkerboard1.10_proj2, tor_unweighted_svmHYB)
png("tor_unweightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tor_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-46, -34), ylim = c(-24, -8))
plot(tor_unweighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-46, -34), ylim = c(-24, -8))
points(torquatus_occs[,2:3], col = "black", bg = "deepskyblue", pch = 21, cex = 0.5)
dev.off()

# mask the thresholded SDMs (MTP)
var_unweighted_svmHYB_thresh_mask <- mask(thinned_var_proj_mtp, var_unweighted_svmHYB)
plot(var_unweighted_svmHYB_thresh_mask, zlim = c(0,1))

tri_unweighted_svmHYB_thresh_mask <- mask(thinned_tri_proj_bbox_mtp2, tri_unweighted_svmHYB)
plot(tri_unweighted_svmHYB_thresh_mask, xlim = c(-70, -49), ylim = c(-6, 11), zlim = c(0,1))

tor_unweighted_svmHYB_thresh_mask <- mask(thinned_tor_proj_bbox_mtp2, tor_unweighted_svmHYB)
plot(tor_unweighted_svmHYB_thresh_mask, xlim = c(-46, -34), ylim = c(-24, -8), zlim = c(0,1))


# Weighted SVM - hybrid ---------------------------------------------------

#create svm
weighted_svmHYB <- rangeSVM(variegatus_occs[,2:3], tridactylus_occs[,2:3], torquatus_occs[,2:3],
                            sdm = raster::stack(var_checkerboard1.10_proj, tri_checkerboard1.10_proj2, tor_checkerboard1.10_proj2), nrep = 100, weight = TRUE)
sloth_weighted_svmHYB <- rangeSVM_predict(svm = weighted_svmHYB, r = var_checkerboard1.10_proj, sdm = raster::stack(var_checkerboard1.10_proj, tri_checkerboard1.10_proj2, tor_checkerboard1.10_proj2))
png("hyb_weighted_svm.png", units = "in", width = 5, height = 4, res = 300)
plot(sloth_weighted_svmHYB, col=c("khaki1","darkseagreen1","lightblue"))
points(variegatus_occs[,2:3], pch = 20, cex = 0.75, col = "orange1")
points(tridactylus_occs[,2:3], pch = 20, cex = 0.75, col = "springgreen3")
points(torquatus_occs[,2:3], pch = 20, cex = 0.75, col = "deepskyblue3")
dev.off()

#mask sdm predictions
var_weighted_svmHYB <- sloth_weighted_svmHYB == 1
var_weighted_svmHYB[var_weighted_svmHYB == 0] <- NA
plot(var_weighted_svmHYB)
var_weighted_svmHYB_mask <- mask(var_checkerboard1.10_proj, var_weighted_svmHYB)
png("var_weightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(var_checkerboard1.10_proj, col = gray.colors(1000, start = 0, end = 1), legend = FALSE)
plot(var_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE)
points(variegatus_occs[,2:3], col = "black", bg = "darkorange1", pch = 21, cex = 0.5)
dev.off()

png("var_weightedHYB_mask_nopts.png", units = "in", width = 5, height = 4, res = 300)
plot(var_checkerboard1.10_proj, col = gray.colors(1000, start = 0, end = 1), legend = FALSE)
plot(var_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE)
dev.off()

tri_weighted_svmHYB <- sloth_weighted_svmHYB == 2
tri_weighted_svmHYB[tri_weighted_svmHYB == 0] <- NA
plot(tri_weighted_svmHYB)
tri_weighted_svmHYB_mask <- mask(tri_checkerboard1.10_proj2, tri_weighted_svmHYB)
png("tri_weightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tri_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-70, -49), ylim = c(-6, 11))
plot(tri_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-70, -49), ylim = c(-6, 11))
points(tridactylus_occs[,2:3], pch = 21, cex = 0.5, col = "black", bg = "springgreen3")
dev.off()

png("tri_weightedHYB_mask_nopts.png", units = "in", width = 5, height = 4, res = 300)
plot(tri_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-70, -49), ylim = c(-6, 11))
plot(tri_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-70, -49), ylim = c(-6, 11))
dev.off()

tor_weighted_svmHYB <- sloth_weighted_svmHYB == 3
tor_weighted_svmHYB[tor_weighted_svmHYB == 0] <- NA
plot(tor_weighted_svmHYB)
tor_weighted_svmHYB_mask <- mask(tor_checkerboard1.10_proj2, tor_weighted_svmHYB)
png("tor_weightedHYB_mask.png", units = "in", width = 5, height = 4, res = 300)
plot(tor_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-46, -34), ylim = c(-24, -8))
plot(tor_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-46, -34), ylim = c(-24, -8))
points(torquatus_occs[,2:3], col = "black", bg = "deepskyblue", pch = 21, cex = 0.5)
dev.off()

png("tor_weightedHYB_mask_nopts.png", units = "in", width = 5, height = 4, res = 300)
plot(tor_checkerboard1.10_proj2, col = gray.colors(1000, start = 0, end = 1), legend = FALSE, xlim = c(-46, -34), ylim = c(-24, -8))
plot(tor_weighted_svmHYB_mask, col = inferno(1000), zlim = c(0,1), add = TRUE, xlim = c(-46, -34), ylim = c(-24, -8))
dev.off()


# Misclassification rates -------------------------------------------------


# Spatial unweighted ------------------------------------------------------

# Omission rate
length(which(extract(sloth_unweighted_svmSP, variegatus_occs[,2:3]) != 1))/nrow(variegatus_occs[,2:3])
length(which(extract(sloth_unweighted_svmSP, tridactylus_occs[,2:3]) != 2))/nrow(tridactylus_occs[,2:3])
length(which(extract(sloth_unweighted_svmSP, torquatus_occs[,2:3]) != 3))/nrow(torquatus_occs[,2:3])

# False inclusion
# number of tridactylus & torquatus points classified as variegatus
length(which(extract(sloth_unweighted_svmSP, tridactylus_occs[,2:3]) == 1))
length(which(extract(sloth_unweighted_svmSP, torquatus_occs[,2:3]) == 1))
# number of variegatus points classified as tridactylus
length(which(extract(sloth_unweighted_svmSP, variegatus_occs[,2:3]) == 2))
# number of variegatus points classified as torquatus
length(which(extract(sloth_unweighted_svmSP, variegatus_occs[,2:3]) == 3))

# Spatial weighted --------------------------------------------------------

# Omission rate
length(which(extract(sloth_weighted_svmSP, variegatus_occs[,2:3]) != 1))/nrow(variegatus_occs[,2:3])
length(which(extract(sloth_weighted_svmSP, tridactylus_occs[,2:3]) != 2))/nrow(tridactylus_occs[,2:3])
length(which(extract(sloth_weighted_svmSP, torquatus_occs[,2:3]) != 3))/nrow(torquatus_occs[,2:3])

# False inclusion
# number of tridactylus & torquatus points classified as variegatus
length(which(extract(sloth_weighted_svmSP, tridactylus_occs[,2:3]) == 1))
length(which(extract(sloth_weighted_svmSP, torquatus_occs[,2:3]) == 1))
# number of variegatus points classified as tridactylus
length(which(extract(sloth_weighted_svmSP, variegatus_occs[,2:3]) == 2))
# number of variegatus points classified as torquatus
length(which(extract(sloth_weighted_svmSP, variegatus_occs[,2:3]) == 3))

# Hybrid unweighted -------------------------------------------------------

# Omission rate
# percentage of variegatus points outside variegatus range
length(which(extract(sloth_unweighted_svmHYB, variegatus_occs[,2:3]) != 1))/nrow(variegatus_occs[,2:3])
# percentage of tridactylus points outside tridactylus range
length(which(extract(sloth_unweighted_svmHYB, tridactylus_occs[,2:3]) != 2))/nrow(tridactylus_occs[,2:3])
# percentage of torquatus points outside torquatus range
length(which(extract(sloth_unweighted_svmHYB, torquatus_occs[,2:3]) != 3))/nrow(torquatus_occs[,2:3])

# False inclusion
# number of tridactylus & torquatus points classified as variegatus
length(which(extract(sloth_unweighted_svmHYB, tridactylus_occs[,2:3]) == 1))
length(which(extract(sloth_unweighted_svmHYB, torquatus_occs[,2:3]) == 1))
# number of variegatus points classified as tridactylus
length(which(extract(sloth_unweighted_svmHYB, variegatus_occs[,2:3]) == 2))
# number of variegatus points classified as torquatus
length(which(extract(sloth_unweighted_svmHYB, variegatus_occs[,2:3]) == 3))


# Hybrid weighted -------------------------------------------------------

# Omission rate
# percentage of variegatus points outside variegatus range
length(which(extract(sloth_weighted_svmHYB, variegatus_occs[,2:3]) != 1))/nrow(variegatus_occs[,2:3])
# percentage of tridactylus points outside tridactylus range
length(which(extract(sloth_weighted_svmHYB, tridactylus_occs[,2:3]) != 2))/nrow(tridactylus_occs[,2:3])
# percentage of torquatus points outside torquatus range
length(which(extract(sloth_weighted_svmHYB, torquatus_occs[,2:3]) != 3))/nrow(torquatus_occs[,2:3])

# False inclusion
# number of tridactylus & torquatus points classified as variegatus
length(which(extract(sloth_weighted_svmHYB, tridactylus_occs[,2:3]) == 1))
length(which(extract(sloth_weighted_svmHYB, torquatus_occs[,2:3]) == 1))
# number of variegatus points classified as tridactylus
length(which(extract(sloth_weighted_svmHYB, variegatus_occs[,2:3]) == 2))
# number of variegatus points classified as torquatus
length(which(extract(sloth_weighted_svmHYB, variegatus_occs[,2:3]) == 3))
