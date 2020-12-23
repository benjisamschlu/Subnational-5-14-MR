


#######################################################################################
##      PROJECT: Space-time smoothing of 5-14 mortality estimates in SSA
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
##      Dec. 2020
#######################################################################################
#
#       Create some figures to check that overall fitting is ok
#
#######################################################################################
#
#
# Notes:
# 1) 
 

########################################################################################################################################################


#--- Load packages ----------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr", "demogsurv", "sp", "RColorBrewer")
invisible( lapply(packages, library, character.only = TRUE) )


# ----- Plot estimates of each DHS and Space-Time estimates ----------------------------------------------------------------------------------------------------

fig.smooth_dhs_u5 <- ggplot(data = u5[u5$region == "All" & u5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean, group = 1)) +
        geom_point() +
        geom_line() +
        geom_point(data = data.multi_u5[data.multi_u5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears)) +
        geom_line(data = data.multi_u5[data.multi_u5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears), linetype = "dashed") +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        theme_bw() +
        ggtitle(paste(ctry, "national period model: 0-5", sep = " ")) +
        ylab("5q0 (deaths per 1000)") +
        xlab("Period") +
        labs(color = "DHS Survey")

fig.smooth_dhs_a5 <- ggplot(data = a5[a5$region == "All" & a5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean, group = 1)) +
        geom_point() +
        geom_line() +
        geom_point(data = data.multi_a5[data.multi_a5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears)) +
        geom_line(data = data.multi_a5[data.multi_a5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears), linetype = "dashed") +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        theme_bw() +
        ggtitle(paste(ctry, "national period model: 5-15", sep = " ")) +
        ylab("10q5 (deaths per 1000)") +
        xlab("Period")  +
        labs(color = "DHS Survey")


# ----- Diagnostics -----------------------------------------------------------------------------------------------------------------------------------------

# 0-5

# Extract random effects
random.time <- getDiag(fit.sub_u5, field = "time", year_label = years)
random.time$years <- factor(random.time$years, levels = years, labels = years)
random.space <- getDiag(fit.sub_u5, field = "space", Amat = Amat)
random.spacetime <- getDiag(fit.sub_u5, field = "spacetime", year_label = years, Amat = Amat)
random.spacetime$years <- factor(random.spacetime$years, levels = years, labels = years)

# Plots
fig.rtime_u5 <- ggplot(data = random.time, aes(x = years, y = median, group = 1)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~label, ncol = 2) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ylim(c(-0.8,0.8)) +
        ggtitle(paste("Compare temporal random effects: 0-5", ctry, sep = "-")) +
        ylab("Random Effects") +
        xlab("Period") 

fig.rspace_u5 <- mapPlot(random.space, geo = geo, by.data = "region", by.geo = "REGNAME", variables = "label",
                         values = c("median"), ncol = 2, is.long = TRUE)

fig.rspacetime_u5 <- ggplot(data = random.spacetime, aes(x = years, y = median, group = 1)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~region, ncol = 4) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle(paste("Compare space-time interaction random effects: 0-5", ctry, sep = "-")) +
        ylab("Random Effects") +
        xlab("Period")


# 5-15

# Extract random effects
random.time <- getDiag(fit.sub_a5, field = "time", year_label = years)
random.time$years <- factor(random.time$years, levels = years, labels = years)
random.space <- getDiag(fit.sub_a5, field = "space", Amat = Amat)
random.spacetime <- getDiag(fit.sub_a5, field = "spacetime", year_label = years, Amat = Amat)
random.spacetime$years <- factor(random.spacetime$years, levels = years, labels = years)

# Plots
fig.rtime_a5 <- ggplot(data = random.time, aes(x = years, y = median, group = 1)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~label, ncol = 2) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ylim(c(-0.8,0.8)) +
        ggtitle(paste("Compare temporal random effects: 5-15", ctry, sep = "-")) +
        ylab("Random Effects") +
        xlab("Period") 

fig.rspace_a5 <- mapPlot(random.space, geo = geo, by.data = "region", by.geo = "REGNAME", variables = "label",
                         values = c("median"), ncol = 2, is.long = TRUE)

fig.rspacetime_a5 <- ggplot(data = random.spacetime, aes(x = years, y = median, group = 1)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~region, ncol = 4) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        ggtitle(paste("Compare space-time interaction random effects: 5-15", ctry, sep = "-")) +
        ylab("Random Effects") +
        xlab("Period")





