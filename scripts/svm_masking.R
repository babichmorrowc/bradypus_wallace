library(raster)
source("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/maskRangeR/R/svm.R")


#load occurrence data
variegatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_variegatus_litdata.csv")
tridactylus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_tridactylus_litdata.csv")
torquatus <- read.csv("~/OneDrive - AMNH/Wallace/Occurrence_Data/Bradypus_torquatus_litdata.csv")

#load SDMs
var_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_variegatus_L_1_cloglog.tif")
plot(var_sdm)
tri_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_tridactylus_LQ_2_cloglog.tif")
plot(tri_sdm)
tor_sdm <- raster("/Users/hellenfellows/OneDrive\ -\ AMNH/Wallace/Wallace_code/thinned_torquatus_H_3_cloglog.tif")
plot(tor_sdm)


# Unweighted SVM - occurrence only ---------------------------------------------

#create svm
unweighted_svm_occ <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], torquatus[,2:3], nrep = 100)
sloth_unweighted_svm_occ <- rangeSVM_predict(svm = unweighted_svm_occ, r = var_sdm)
plot(sloth_unweighted_svm_occ)
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "black")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_unweighted_svm_occ <- sloth_unweighted_svm_occ == 1
var_unweighted_svm_occ[var_unweighted_svm_occ == 0] <- NA
plot(var_unweighted_svm_occ)
var_unweighted_svm_occ_mask <- mask(var_sdm, var_unweighted_svm_occ)
plot(var_unweighted_svm_occ_mask)

tri_unweighted_svm_occ <- sloth_unweighted_svm_occ == 2
tri_unweighted_svm_occ[tri_unweighted_svm_occ == 0] <- NA
plot(tri_unweighted_svm_occ)
tri_unweighted_svm_occ_mask <- mask(tri_sdm, tri_unweighted_svm_occ)
plot(tri_unweighted_svm_occ_mask)

tor_unweighted_svm_occ <- sloth_unweighted_svm_occ == 3
tor_unweighted_svm_occ[tor_unweighted_svm_occ == 0] <- NA
plot(tor_unweighted_svm_occ)
tor_unweighted_svm_occ_mask <- mask(tor_sdm, tor_unweighted_svm_occ)
plot(tor_unweighted_svm_occ_mask)


# Weighted SVM - occurrence only ------------------------------------------

#create svm
weighted_svm_occ <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], torquatus[,2:3], nrep = 200, weight = TRUE)
sloth_weighted_svm_occ <- rangeSVM_predict(svm = weighted_svm_occ, r = var_sdm)
plot(sloth_weighted_svm_occ)
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "black")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_weighted_svm_occ <- sloth_weighted_svm_occ == 1
var_weighted_svm_occ[var_weighted_svm_occ == 0] <- NA
plot(var_weighted_svm_occ)
var_weighted_svm_occ_mask <- mask(var_sdm, var_weighted_svm_occ)
plot(var_weighted_svm_occ_mask)

tri_weighted_svm_occ <- sloth_weighted_svm_occ == 2
tri_weighted_svm_occ[tri_weighted_svm_occ == 0] <- NA
plot(tri_weighted_svm_occ)
tri_weighted_svm_occ_mask <- mask(tri_sdm, tri_weighted_svm_occ)
plot(tri_weighted_svm_occ_mask)

tor_weighted_svm_occ <- sloth_weighted_svm_occ == 3
tor_weighted_svm_occ[tor_weighted_svm_occ == 0] <- NA
plot(tor_weighted_svm_occ)
tor_weighted_svm_occ_mask <- mask(tor_sdm, tor_weighted_svm_occ)
plot(tor_weighted_svm_occ_mask)


# Unweighted SVM - hybrid -------------------------------------------------

#create svm
unweighted_svm_hyb <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], torquatus[,2:3], sdm = raster::stack(var_sdm, tri_sdm, tor_sdm), nrep = 100)
sloth_unweighted_svm_hyb <- rangeSVM_predict(svm = unweighted_svm_hyb, r = var_sdm, sdm = raster::stack(var_sdm, tri_sdm, tor_sdm))
plot(sloth_unweighted_svm_hyb)
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "black")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 1
var_unweighted_svm_hyb[var_unweighted_svm_hyb == 0] <- NA
plot(var_unweighted_svm_hyb)
var_unweighted_svm_hyb_mask <- mask(var_sdm, var_unweighted_svm_hyb)
plot(var_unweighted_svm_hyb_mask)

tri_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 2
tri_unweighted_svm_hyb[tri_unweighted_svm_hyb == 0] <- NA
plot(tri_unweighted_svm_hyb)
tri_unweighted_svm_hyb_mask <- mask(tri_sdm, tri_unweighted_svm_hyb)
plot(tri_unweighted_svm_hyb_mask)

tor_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 3
tor_unweighted_svm_hyb[tor_unweighted_svm_hyb == 0] <- NA
plot(tor_unweighted_svm_hyb)
tor_unweighted_svm_hyb_mask <- mask(tor_sdm, tor_unweighted_svm_hyb)
plot(tor_unweighted_svm_hyb_mask)


# Weighted SVM - hybrid ---------------------------------------------------

#create svm
weighted_svm_hyb <- rangeSVM(variegatus[,2:3], tridactylus[,2:3], torquatus[,2:3], sdm = raster::stack(var_sdm, tri_sdm, tor_sdm), nrep = 100, weight = TRUE)
sloth_weighted_svm_hyb <- rangeSVM_predict(svm = weighted_svm_hyb, r = var_sdm, sdm = raster::stack(var_sdm, tri_sdm, tor_sdm))
plot(sloth_weighted_svm_hyb)
points(variegatus[,2:3], pch = 20, cex = 0.75, col = "black")
points(tridactylus[,2:3], pch = 20, cex = 0.75, col = "red")
points(torquatus[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_weighted_svm_hyb <- sloth_weighted_svm_hyb == 1
var_weighted_svm_hyb[var_weighted_svm_hyb == 0] <- NA
plot(var_weighted_svm_hyb)
var_weighted_svm_hyb_mask <- mask(var_sdm, var_weighted_svm_hyb)
plot(var_weighted_svm_hyb_mask)

tri_weighted_svm_hyb <- sloth_weighted_svm_hyb == 2
tri_weighted_svm_hyb[tri_weighted_svm_hyb == 0] <- NA
plot(tri_weighted_svm_hyb)
tri_weighted_svm_hyb_mask <- mask(tri_sdm, tri_weighted_svm_hyb)
plot(tri_weighted_svm_hyb_mask)

tor_weighted_svm_hyb <- sloth_weighted_svm_hyb == 3
tor_weighted_svm_hyb[tor_weighted_svm_hyb == 0] <- NA
plot(tor_weighted_svm_hyb)
tor_weighted_svm_hyb_mask <- mask(tor_sdm, tor_weighted_svm_hyb)
plot(tor_weighted_svm_hyb_mask)
