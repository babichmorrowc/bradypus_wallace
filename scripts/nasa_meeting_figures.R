maxent_pal <- colorRampPalette(c("blue", "green", "yellow", "red"))

tri_sdm_bbox <- crop(thinned_tri_proj_4_LQ2, ext_tri)
tor_sdm_bbox <- crop(thinned_tor_proj_4, ext_tor)

plot(thinned_var_proj_4, col = maxent_pal(n=1000), zlim = c(0,1))
plot(tri_sdm_bbox, col = maxent_pal(n=1000), zlim = c(0,1))
plot(tor_sdm_bbox, col = maxent_pal(n=1000), zlim = c(0,1))

