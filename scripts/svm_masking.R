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


# Unweighted SVM - occurrence only ---------------------------------------------

#create svm
unweighted_svm_occ <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], nrep = 100)
sloth_unweighted_svm_occ <- rangeSVM_predict(svm = unweighted_svm_occ, r = thinned_var_proj)
plot(sloth_unweighted_svm_occ)
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "black")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "red")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_unweighted_svm_occ <- sloth_unweighted_svm_occ == 1
var_unweighted_svm_occ[var_unweighted_svm_occ == 0] <- NA
plot(var_unweighted_svm_occ)
var_unweighted_svm_occ_mask <- mask(thinned_var_proj, var_unweighted_svm_occ)
plot(var_unweighted_svm_occ_mask)

tri_unweighted_svm_occ <- sloth_unweighted_svm_occ == 2
tri_unweighted_svm_occ[tri_unweighted_svm_occ == 0] <- NA
plot(tri_unweighted_svm_occ)
tri_unweighted_svm_occ_mask <- mask(thinned_tri_proj, tri_unweighted_svm_occ)
plot(tri_unweighted_svm_occ_mask)

tor_unweighted_svm_occ <- sloth_unweighted_svm_occ == 3
tor_unweighted_svm_occ[tor_unweighted_svm_occ == 0] <- NA
plot(tor_unweighted_svm_occ)
tor_unweighted_svm_occ_mask <- mask(thinned_tor_proj, tor_unweighted_svm_occ)
plot(tor_unweighted_svm_occ_mask)


# Weighted SVM - occurrence only ------------------------------------------

#create svm
weighted_svm_occ <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], nrep = 200, weight = TRUE)
sloth_weighted_svm_occ <- rangeSVM_predict(svm = weighted_svm_occ, r = thinned_var_proj)
plot(sloth_weighted_svm_occ)
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "black")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "red")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_weighted_svm_occ <- sloth_weighted_svm_occ == 1
var_weighted_svm_occ[var_weighted_svm_occ == 0] <- NA
plot(var_weighted_svm_occ)
var_weighted_svm_occ_mask <- mask(thinned_var_proj, var_weighted_svm_occ)
plot(var_weighted_svm_occ_mask)

tri_weighted_svm_occ <- sloth_weighted_svm_occ == 2
tri_weighted_svm_occ[tri_weighted_svm_occ == 0] <- NA
plot(tri_weighted_svm_occ)
tri_weighted_svm_occ_mask <- mask(thinned_tri_proj, tri_weighted_svm_occ)
plot(tri_weighted_svm_occ_mask)

tor_weighted_svm_occ <- sloth_weighted_svm_occ == 3
tor_weighted_svm_occ[tor_weighted_svm_occ == 0] <- NA
plot(tor_weighted_svm_occ)
tor_weighted_svm_occ_mask <- mask(thinned_tor_proj, tor_weighted_svm_occ)
plot(tor_weighted_svm_occ_mask)


# Unweighted SVM - hybrid -------------------------------------------------

#create svm
unweighted_svm_hyb <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj), nrep = 100)
sloth_unweighted_svm_hyb <- rangeSVM_predict(svm = unweighted_svm_hyb, r = thinned_var_proj, sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj))
plot(sloth_unweighted_svm_hyb)
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "black")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "red")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 1
var_unweighted_svm_hyb[var_unweighted_svm_hyb == 0] <- NA
plot(var_unweighted_svm_hyb)
var_unweighted_svm_hyb_mask <- mask(thinned_var_proj, var_unweighted_svm_hyb)
plot(var_unweighted_svm_hyb_mask)

tri_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 2
tri_unweighted_svm_hyb[tri_unweighted_svm_hyb == 0] <- NA
plot(tri_unweighted_svm_hyb)
tri_unweighted_svm_hyb_mask <- mask(thinned_tri_proj, tri_unweighted_svm_hyb)
plot(tri_unweighted_svm_hyb_mask)

tor_unweighted_svm_hyb <- sloth_unweighted_svm_hyb == 3
tor_unweighted_svm_hyb[tor_unweighted_svm_hyb == 0] <- NA
plot(tor_unweighted_svm_hyb)
tor_unweighted_svm_hyb_mask <- mask(thinned_tor_proj, tor_unweighted_svm_hyb)
plot(tor_unweighted_svm_hyb_mask)


# Weighted SVM - hybrid ---------------------------------------------------

#create svm
weighted_svm_hyb <- rangeSVM(var_occs[,2:3], tri_occs[,2:3], tor_occs[,2:3], sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj), nrep = 100, weight = TRUE)
sloth_weighted_svm_hyb <- rangeSVM_predict(svm = weighted_svm_hyb, r = thinned_var_proj, sdm = raster::stack(thinned_var_proj, thinned_tri_proj, thinned_tor_proj))
plot(sloth_weighted_svm_hyb)
points(var_occs[,2:3], pch = 20, cex = 0.75, col = "black")
points(tri_occs[,2:3], pch = 20, cex = 0.75, col = "red")
points(tor_occs[,2:3], pch = 20, cex = 0.75, col = "blue")

#mask sdm predictions
var_weighted_svm_hyb <- sloth_weighted_svm_hyb == 1
var_weighted_svm_hyb[var_weighted_svm_hyb == 0] <- NA
plot(var_weighted_svm_hyb)
var_weighted_svm_hyb_mask <- mask(thinned_var_proj, var_weighted_svm_hyb)
plot(var_weighted_svm_hyb_mask)

tri_weighted_svm_hyb <- sloth_weighted_svm_hyb == 2
tri_weighted_svm_hyb[tri_weighted_svm_hyb == 0] <- NA
plot(tri_weighted_svm_hyb)
tri_weighted_svm_hyb_mask <- mask(thinned_tri_proj, tri_weighted_svm_hyb)
plot(tri_weighted_svm_hyb_mask)

tor_weighted_svm_hyb <- sloth_weighted_svm_hyb == 3
tor_weighted_svm_hyb[tor_weighted_svm_hyb == 0] <- NA
plot(tor_weighted_svm_hyb)
tor_weighted_svm_hyb_mask <- mask(thinned_tor_proj, tor_weighted_svm_hyb)
plot(tor_weighted_svm_hyb_mask)
