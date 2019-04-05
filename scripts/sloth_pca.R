library(raster)
library(RStoolbox)
library(ggbiplot)

#Background values from 4 degree buffer:
tri_bgvals_4 <- extract(tri_envsBgMsk_4, buffer_tri_bg.xy_4)
var_bgvals_4 <- extract(var_envsBgMsk_4, buffer_var_bg.xy_4)

#Values at (thinned) occurrence points:
tri_locvals_4 <- extract(tri_envsBgMsk_4, thinned_tri_occs[, c('longitude', 'latitude')])
var_locvals_4 <- extract(var_envsBgMsk_4, thinned_var_occs[, c('longitude', 'latitude')])

#rbind them all together
trivals_4 <- rbind(tri_bgvals_4, tri_locvals_4)
varvals_4 <- rbind(var_bgvals_4, var_locvals_4)
allvals_4<-rbind(tri_bgvals_4, var_bgvals_4, tri_locvals_4, var_locvals_4)
#tri_bgvals_4: 1:10000
#var_bgvals_4: 10001:20000
#tri_locvals_4: 20001:20015
#var_locvals_4: 20016:20146 (I think)

# Run pca
tri_pca_4 <- prcomp(trivals_4)
ggbiplot(tri_pca_4) +
  ggtitle("Tridactylus PCA (background and occurrences)")

var_pca_4 <- prcomp(varvals_4)
ggbiplot(var_pca_4) +
  ggtitle("Variegatus PCA (background and occurrences)")

sloth_pca_4 <- prcomp(allvals_4)
summary(sloth_pca_4)
ggbiplot(sloth_pca_4) +
  ggtitle("Variegatus and tridactylus PCA (background and occurrences)")

# get scores only
sloth_scores_4 <- sloth_pca_4$x
# plotting
plot(sloth_scores_4[10001:20000,], col = alpha("darkorange1", 0.10))
points(sloth_scores_4[1:10000,], col = alpha("deepskyblue", 0.10))
points(sloth_scores_4[20016:20147,], col = "darkorange1", pch = 21, bg = "black")
points(sloth_scores_4[20001:20015,], col = "deepskyblue", pch = 21, bg = "black")

