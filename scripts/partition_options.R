# Use build_enm.R function to try a range of partition methods


# VARIEGATUS --------------------------------------------------------------


# Checkerboard 1 ----------------------------------------------------------


# var_checkerboard1 <- lapply(X = c(2,4), build_enm, occs = thinned_var_occs[,2:3], env = var_envsBgMsk,
#        bg.coords = buffer_var_bg.xy, partition = "checkerboard1")

var_checkerboard1.2 <- build_enm(occs = thinned_var_occs[,2:3], env = var_envsBgMsk,
                                 bg.coords = buffer_var_bg.xy, partition = "checkerboard1", aggregation.factor = 2)
var_checkerboard1.2_eval <- var_checkerboard1.2@results
var_checkerboard1.2_eval <- var_checkerboard1.2_eval[with(var_checkerboard1.2_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(var_checkerboard1.2_eval)
View(var_checkerboard1.2_eval[var_checkerboard1.2_eval$avg.test.or10pct < 0.13, ])
# get model
var_checkerboard1.2_evalMods <- var_checkerboard1.2@models
names(var_checkerboard1.2_evalMods) <- var_checkerboard1.2@results$settings
var_checkerboard1.2_Preds <- var_checkerboard1.2@predictions
# Select your model from the models list
var_checkerboard1.2_mod <- var_checkerboard1.2_evalMods[["LH_2"]]
var_checkerboard1.2_mod2 <- var_checkerboard1.2_evalMods[["LQH_3"]]
var_checkerboard1.2_mod3 <- var_checkerboard1.2_evalMods[["LH_4.5"]]
var_checkerboard1.2_proj <- ENMeval::maxnet.predictRaster(var_checkerboard1.2_mod, Env_var, type = 'cloglog', clamp = TRUE)
var_checkerboard1.2_proj2 <- ENMeval::maxnet.predictRaster(var_checkerboard1.2_mod2, Env_var, type = 'cloglog', clamp = TRUE)
var_checkerboard1.2_proj3 <- ENMeval::maxnet.predictRaster(var_checkerboard1.2_mod3, Env_var, type = 'cloglog', clamp = TRUE)

#plot the model prediction
plot(var_checkerboard1.2_proj)
plot(var_checkerboard1.2_proj2)
plot(var_checkerboard1.2_proj3)

var_checkerboard1.4 <- build_enm(occs = thinned_var_occs[,2:3], env = var_envsBgMsk,
                                 bg.coords = buffer_var_bg.xy, partition = "checkerboard1", aggregation.factor = 4)
var_checkerboard1.4_eval <- var_checkerboard1.4@results
var_checkerboard1.4_eval <- var_checkerboard1.4_eval[with(var_checkerboard1.4_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(var_checkerboard1.4_eval)

# Checkerboard 2 ----------------------------------------------------------


var_checkerboard2 <- lapply(X = c(2,4), build_enm, occs = thinned_var_occs[,2:3], env = var_envsBgMsk,
                            bg.coords = buffer_var_bg.xy, partition = "checkerboard2")

var_checkerboard2.2 <- var_checkerboard2[[1]]
var_checkerboard2.4 <- var_checkerboard2[[2]]

var_checkerboard2.2_eval <- var_checkerboard2.2@results
var_checkerboard2.2_eval <- var_checkerboard2.2_eval[with(var_checkerboard2.2_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(var_checkerboard2.2_eval)
# get model
var_checkerboard2.2_evalMods <- var_checkerboard2.2@models
names(var_checkerboard2.2_evalMods) <- var_checkerboard2.2@results$settings
var_checkerboard2.2_Preds <- var_checkerboard2.2@predictions
# Select your model from the models list
var_checkerboard2.2_mod <- var_checkerboard2.2_evalMods[["L_2"]]
var_checkerboard2.2_proj <- ENMeval::maxnet.predictRaster(var_checkerboard2.2_mod, Env_var, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(var_checkerboard2.2_proj)


var_checkerboard2.4_eval <- var_checkerboard2.4@results
var_checkerboard2.4_eval <- var_checkerboard2.4_eval[with(var_checkerboard2.4_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(var_checkerboard2.4_eval)
# get model
var_checkerboard2.4_evalMods <- var_checkerboard2.4@models
names(var_checkerboard2.4_evalMods) <- var_checkerboard2.4@results$settings
var_checkerboard2.4_Preds <- var_checkerboard2.4@predictions
# Select your model from the models list
var_checkerboard2.4_mod <- var_checkerboard2.4_evalMods[["LH_5"]]
var_checkerboard2.4_proj <- ENMeval::maxnet.predictRaster(var_checkerboard2.4_mod, Env_var, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(var_checkerboard2.4_proj)


# TRIDACTYLUS -------------------------------------------------------------

# Block -------------------------------------------------------------------


tri_block <- build_enm(occs = thinned_tri_occs[,2:3], env = tri_envsBgMsk,
                                 bg.coords = buffer_tri_bg.xy, partition = "block")
tri_block_eval <- tri_block@results
tri_block_eval <- tri_block_eval[with(tri_block_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(tri_block_eval)
# get model
tri_block_evalMods <- tri_block@models
names(tri_block_evalMods) <- tri_block@results$settings
tri_block_Preds <- tri_block@predictions
tri_block_mod <- tri_block_evalMods[["LQH_3"]]
tri_block_proj <- ENMeval::maxnet.predictRaster(tri_block_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(tri_block_proj)

# Checkerboard 1 ----------------------------------------------------------


tri_checkerboard1 <- lapply(X = c(1, 2), build_enm, occs = thinned_tri_occs[,2:3], env = tri_envsBgMsk,
                            bg.coords = buffer_tri_bg.xy, partition = "checkerboard1")

tri_checkerboard1.1 <- tri_checkerboard1[[1]]
tri_checkerboard1.2 <- tri_checkerboard1[[2]]

tri_checkerboard1.1_eval <- tri_checkerboard1.1@results
tri_checkerboard1.1_eval <- tri_checkerboard1.1_eval[with(tri_checkerboard1.1_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(tri_checkerboard1.1_eval)
# get model
tri_checkerboard1.1_evalMods <- tri_checkerboard1.1@models
names(tri_checkerboard1.1_evalMods) <- tri_checkerboard1.1@results$settings
tri_checkerboard1.1_Preds <- tri_checkerboard1.1@predictions
tri_checkerboard1.1_mod <- tri_checkerboard1.1_evalMods[["LQ_5"]]
tri_checkerboard1.1_proj <- ENMeval::maxnet.predictRaster(tri_checkerboard1.1_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(tri_checkerboard1.1_proj)
  
tri_checkerboard1.2_eval <- tri_checkerboard1.2@results
tri_checkerboard1.2_eval <- tri_checkerboard1.2_eval[with(tri_checkerboard1.2_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(tri_checkerboard1.2_eval)
# get model
tri_checkerboard1.2_evalMods <- tri_checkerboard1.2@models
names(tri_checkerboard1.2_evalMods) <- tri_checkerboard1.2@results$settings
tri_checkerboard1.2_Preds <- tri_checkerboard1.2@predictions
tri_checkerboard1.2_mod <- tri_checkerboard1.2_evalMods[["L_4.5"]]
tri_checkerboard1.2_proj <- ENMeval::maxnet.predictRaster(tri_checkerboard1.2_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(tri_checkerboard1.2_proj)

# Checkerboard 2 ----------------------------------------------------------


tri_checkerboard2 <- lapply(X = c(1, 2), build_enm, occs = thinned_tri_occs[,2:3], env = tri_envsBgMsk,
                            bg.coords = buffer_tri_bg.xy, partition = "checkerboard2")

tri_checkerboard2.1 <- tri_checkerboard2[[1]]
tri_checkerboard2.2 <- tri_checkerboard2[[2]]

tri_checkerboard2.1_eval <- tri_checkerboard2.1@results
tri_checkerboard2.1_eval <- tri_checkerboard2.1_eval[with(tri_checkerboard2.1_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(tri_checkerboard2.1_eval)
# get model
tri_checkerboard2.1_evalMods <- tri_checkerboard2.1@models
names(tri_checkerboard2.1_evalMods) <- tri_checkerboard2.1@results$settings
tri_checkerboard2.1_Preds <- tri_checkerboard2.1@predictions
tri_checkerboard2.1_mod <- tri_checkerboard2.1_evalMods[["LQ_5"]]
tri_checkerboard2.1_proj <- ENMeval::maxnet.predictRaster(tri_checkerboard2.1_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(tri_checkerboard2.1_proj)

tri_checkerboard2.2_eval <- tri_checkerboard2.2@results
tri_checkerboard2.2_eval <- tri_checkerboard2.2_eval[with(tri_checkerboard2.2_eval, order(avg.test.or10pct, -avg.test.AUC, delta.AICc)), ]
View(tri_checkerboard2.2_eval)
# get model
tri_checkerboard2.2_evalMods <- tri_checkerboard2.2@models
names(tri_checkerboard2.2_evalMods) <- tri_checkerboard2.2@results$settings
tri_checkerboard2.2_Preds <- tri_checkerboard2.2@predictions
tri_checkerboard2.2_mod <- tri_checkerboard2.2_evalMods[["L_4.5"]]
tri_checkerboard2.2_proj <- ENMeval::maxnet.predictRaster(tri_checkerboard2.2_mod, Env_tri, type = 'cloglog', clamp = TRUE)
#plot the model prediction
plot(tri_checkerboard2.2_proj)

  