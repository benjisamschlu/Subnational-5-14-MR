


#######################################################################################
##      PROJECT: Space-time smoothing of 5-14 mortality estimates in SSA
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
##      Dec. 2020
#######################################################################################
#
#       Save R objects associated to a country
#
#######################################################################################
#
#
# Notes:
# 1) 

########################################################################################################################################################


# Store outputs
results <- list(u5, a5, out.sub_u5, out.sub_a5, sim.sub_u5, sim.sub_a5, variance, geo, Amat, years, ctry, exp_a5, exp_u5, dth_a5, dth_u5)
names(results) <- c("u5", "a5", "ST est sub_u5", "ST est sub_a5", "sim u5", "sim a5", "variance", "geo", "Amat", "years", "ctry", "exp_a5", "exp_u5", "dth_a5", "dth_u5")
saveRDS(results, file = paste("./data/tidy/", ctry, ".rds", sep = ""))

