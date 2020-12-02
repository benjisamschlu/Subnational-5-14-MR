########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: Save R objects associated to a country

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 12-3-2020

# Notes:
# 1) 

########################################################################################################################################################

# Store outputs
results <- list(u5, a5, out.sub_u5, out.sub_a5, sim.sub_u5, sim.sub_a5, variance, geo, Amat, years, ctry)
names(results) <- c("u5", "a5", "ST est sub_u5", "ST est sub_a5", "sim u5", "sim a5", "variance", "geo", "Amat", "years", "ctry")
saveRDS(results, file = paste("./data/tidy/", ctry, ".rds", sep = ""))

# Store figures.pdf
fig <- list(fig.smooth_dhs_u5, fig.smooth_dhs_a5, fig.sub_u5, fig.sub_a5, fig.log, fig.rtime_u5, fig.rspace_u5, fig.rspacetime_u5,fig.rtime_a5, fig.rspace_a5, fig.rspacetime_a5)
pdf(paste("./figure/tidy/", ctry, "-final", ".pdf", sep = ""))
fig
dev.off()

# Usefull to reload important outputs of a given country 

# reload <- readRDS("./data/tidy/Kenya.rds")
# u5 <- reload[["u5"]]
# a5 <- reload[["a5"]]
# ti <- reload[["ti"]]
# arr_all <- reload[["arr_all"]]
# arr <- reload[["arr"]]
# years <- reload[["years"]]
# ctry <- "Nigeria"
