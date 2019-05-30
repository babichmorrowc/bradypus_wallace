build_enm <- function(occs, env, bg, partition, aggregation.factor){
  # partition the occurrence data
  if(partition == "block"){
    group.data <- ENMeval::get.block(occ = occs, bg = bg)
  } else if(partition == "checkerboard1"){
    group.data <- ENMeval::get.checkerboard1(occ = occs, env = env, bg = bg, aggregation.factor = aggregation.factor)
  } else if(partition == "checkerboard2"){
    group.data <- ENMeval::get.checkerboard2(occ = occs, env = env, bg = bg, aggregation.factor = aggregation.factor)
  }
  occs.grp <- group.data[[1]]
  bg.grp <- group.data[[2]]
  rms <- seq(0.5, 5, 0.5)
  enm <- ENMeval::ENMevaluate(occs, env, bg = bg, RMvalues = rms, fc = c('L', 'LQ', 'LH', 'LQH'), 
                                        occ.grp = occs.grp, bg.grp = bg.grp, method = 'user', clamp = TRUE, algorithm = "maxnet")
  return(enm)
}
