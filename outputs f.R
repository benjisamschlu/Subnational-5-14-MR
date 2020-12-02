########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: Create result outputs

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 5-3-2020

# Notes:
# 1) 

########################################################################################################################################################


#--- Load packages ----------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr", "demogsurv", "sp", "RColorBrewer")
invisible( lapply(packages, library, character.only = TRUE) )


#--- Load functions ----------------------------------------------------------------------------------------------------------------------


#--- Plot Meta-analysis and Space-Time estimates ----------------------------------------------------------------------------------------------------


# Subnational

fig.sub_u5 <- ggplot(data = u5[u5$region != "All", ], aes(x = years, y = mean, group = Type, colour = Type)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_line(data = u5[u5$region != "All" & u5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean)) +
        geom_errorbar(position = position_dodge(width = 0.2), aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~region, ncol = 3) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_manual(values = c("orange", "black")) + 
        ggtitle(paste(ctry, "subnational period model: 0-5", sep = " ")) +
        ylab("5q0 (deaths per 1000)") +
        xlab("Period") 

fig.sub_a5 <- ggplot(data = a5[a5$region != "All", ], aes(x = years, y = mean, group = Type, colour = Type)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_line(data = a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean)) +
        geom_errorbar(position = position_dodge(width = 0.2), aes(ymin=lower, ymax=upper), width=.1) +
        facet_wrap(~region, ncol = 3) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_manual(values = c("orange", "black")) + 
        ggtitle(paste(ctry, "subnational period model: 5-15", sep = " ")) +
        ylab("10q5 (deaths per 1000)") +
        xlab("Period") 


#--- Plot estimates of each DHS and Space-Time estimates ----------------------------------------------------------------------------------------------------

fig.smooth_dhs_u5 <- ggplot(data = u5[u5$region == "All" & u5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean, group = 1)) +
        geom_point() +
        geom_line() +
        geom_point(data = data.multi_u5[data.multi_u5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears)) +
        geom_line(data = data.multi_u5[data.multi_u5$region == "All", ], aes(x = years, y = mean*1000, group = surveyYears, color = surveyYears), linetype = "dashed") +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
        theme_bw() +
        #ylim(range(data.multi_u5$mean[data.multi_u5$region == "All"], na.rm = TRUE)*1000) +
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
        #ylim(range(data.multi_a5$mean[data.multi_a5$region == "All"], na.rm = TRUE)*1000) +
        ggtitle(paste(ctry, "national period model: 5-15", sep = " ")) +
        ylab("10q5 (deaths per 1000)") +
        xlab("Period")  +
        labs(color = "DHS Survey")


#--- Plot log q of Space-Time estimates ----------------------------------------------------------------------------------------------------

# had to play with the labels and color to get the same color order in the legend as other figures
fig.log <- ggplot(data = u5[u5$region != "All" & u5$Type == "Space-Time Smoothing", ], aes(x = years, y = log(exp(logit.median)/(1+exp(logit.median))), group = 1)) +
        geom_point() +
        geom_line(aes(color = "10q5")) +
        geom_errorbar(aes(ymin=log(exp(logit.lower)/(1+exp(logit.lower))), ymax=log(exp(logit.upper)/(1+exp(logit.upper)))), width=.1) +
        geom_point(data = a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ], aes(x = years, y = log(exp(logit.median)/(1+exp(logit.median))))) +
        geom_line(data = a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ], aes(x = years, y = log(exp(logit.median)/(1+exp(logit.median))), color = "5q0") ) +
        geom_errorbar(data = a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ], aes(x = years, ymin=log(exp(logit.lower)/(1+exp(logit.lower))), ymax=log(exp(logit.upper)/(1+exp(logit.upper)))), width=.1) +
        facet_wrap(~region, ncol = 3) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_color_discrete(name = "Age Group", labels = c("5q0", "10q5")) +
        #scale_color_manual(values = c("orange", "black"), labels = c("5q0", "10q5"), name = "Age Group") + 
        ggtitle(paste(ctry, "regions", sep = " ")) +
        ylab("log(nqx)") +
        xlab("Period") 


#--- Diagnostics -----------------------------------------------------------------------------------------------------------------------------------------

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





