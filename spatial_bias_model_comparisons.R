# Comparison of models created using:
# spatial thinning, bias file, and spatial thinning + bias file

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

# For variegatus models

#Calculate Schoener's D using dismo
# between spatial thinning and bias file models
plot(thinned_var_proj)
plot(bias_var_pred)
#Calculate Schoener's D using dismo
thinning.vs.bias_var_overlap <- nicheOverlap(thinned_var_proj, bias_var_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
thinning.vs.bias_var_overlap
# 0.6396672

# between spatial thinning and thinning+bias file models
plot(thinned_var_proj)
plot(bias_thinned_var_pred)
#Calculate Schoener's D using dismo
thinning.vs.thinbias_var_overlap <- nicheOverlap(thinned_var_proj, bias_thinned_var_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
thinning.vs.thinbias_var_overlap
# 0.8678816

# between bias and thinning+bias file models
plot(bias_var_pred)
plot(bias_thinned_var_pred)
#Calculate Schoener's D using dismo
bias.vs.thinbias_var_overlap <- nicheOverlap(bias_var_pred, bias_thinned_var_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
bias.vs.thinbias_var_overlap
# 0.7113986


# For tridactylus models

#Calculate Schoener's D using dismo
# between spatial thinning and bias file models
thinning.vs.bias_tri_overlap <- nicheOverlap(thinned_tri_proj, bias_tri_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
thinning.vs.bias_tri_overlap
# 0.4876976

# between spatial thinning and thinning+bias file models
thinning.vs.thinbias_tri_overlap <- nicheOverlap(thinned_tri_proj, bias_thinned_tri_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
thinning.vs.thinbias_tri_overlap
# 0.7089353

# between bias and thinning+bias file models
bias.vs.thinbias_tri_overlap <- nicheOverlap(bias_tri_pred, bias_thinned_tri_pred, stat='D', mask=TRUE, checkNegatives=TRUE)
bias.vs.thinbias_tri_overlap
# 0.7034757



