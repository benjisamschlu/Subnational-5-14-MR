########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: Comparison of all countries

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 23-7-2020

# Notes:
# 1) 

########################################################################################################################################################


rm(list=ls())

#--- Load packages ----------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr", "demogsurv", "sp", "purrr", "lme4", "plyr", "grid", "forcats", "cowplot")
# "hrbrthemes"
invisible( lapply(packages, library, character.only = TRUE) )


#--- Load functions ----------------------------------------------------------------------------------------------------------------------

source("./code/functions/cv.R")
source("./code/functions/arr f.R")
source("./code/functions/midperiod.R")


#--- Load data ----------------------------------------------------------------------------------------------------------------------------------------------

ctry.ls <- c("Malawi", "Benin", "Ethiopia", "Ghana", "Guinea", "Kenya", "Namibia", "Nigeria", "Rwanda", "Zimbabwe")

df <- lapply(ctry.ls, function(x) {readRDS(paste("./data/tidy/", x, ".rds", sep = ""))})
names(df) <- ctry.ls




#--- Plot q of Space-Time estimates on log10 scale ----------------------------------------------------------------------------------------------------

# had to play with the labels and color to get the same color order in the legend as other figures
list.plot <- lapply(ctry.ls, function(x) {
        
        u5 <- df[[x]]$'u5'
        u5 <- u5[u5$region != "All" & u5$Type == "Space-Time Smoothing", ]
        a5 <- df[[x]]$'a5'
        a5 <- a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ]
        
        ggplot(data = u5, aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000, group = 1)) +
                geom_point() +
                geom_line(aes(linetype = "10q5")) +
                geom_errorbar(aes(ymin=(exp(logit.lower)/(1+exp(logit.lower)))*1000, ymax=(exp(logit.upper)/(1+exp(logit.upper)))*1000), width=.1) +
                geom_point(data = a5, aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000)) +
                geom_line(data = a5, aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000, linetype = "5q0") ) +
                geom_errorbar(data = a5, aes(x = years, ymin=(exp(logit.lower)/(1+exp(logit.lower)))*1000, ymax=(exp(logit.upper)/(1+exp(logit.upper)))*1000), width=.1) +
                scale_y_log10() +
                facet_wrap(~region, ncol = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                scale_linetype_discrete(name = "Age Group", labels = c("5q0", "10q5")) +
                #scale_color_manual(values = c("orange", "black"), labels = c("5q0", "10q5"), name = "Age Group") + 
                ggtitle(paste(x, "regions", sep = " ")) +
                ylab("Mortality rates (per 1000)") +
                xlab("Period")
        
        # for submission
        # ggsave(paste("./submission/fig plosone/logq ", x, ".eps", sep = ""))
}
)
list.plot


#--- Robust regional estimates ----------------------------------------------------------------------

# Load info to be able to plot CVs
cv <- list()
for (i in seq_along(ctry.ls)) {
        data <- df[[ctry.ls[i]]]$'sim a5'
        data$mean <- unlist(lapply(data$draws, function(x) mean(x)))
        data$median <- unlist(lapply(data$draws, function(x) median(x)))
        data$upper <- unlist(lapply(data$draws, function(x) quantile(x, probs = .975)))
        data$lower <- unlist(lapply(data$draws, function(x) quantile(x, probs = .025)))
        data$sd <- unlist(lapply(data$draws, function(x) sd(x)))
        data$cv <- data$sd/data$mean
        data$age <- "5-14"
        data$ctry <- ctry.ls[i]
        cv[[ctry.ls[i]]] <- data[, !names(data) %in% c("draws")]
}
df.cv_a5 <- do.call("rbind", cv)

cv <- list()
for (i in seq_along(ctry.ls)) {
        data <- df[[ctry.ls[i]]]$'sim u5'
        data$mean <- unlist(lapply(data$draws, function(x) mean(x)))
        data$median <- unlist(lapply(data$draws, function(x) median(x)))
        data$upper <- unlist(lapply(data$draws, function(x) quantile(x, probs = .975)))
        data$lower <- unlist(lapply(data$draws, function(x) quantile(x, probs = .025)))
        data$sd <- unlist(lapply(data$draws, function(x) sd(x)))
        data$cv <- data$sd/data$mean
        data$age <- "0-5"
        data$ctry <- ctry.ls[i]
        cv[[ctry.ls[i]]] <- data[, !names(data) %in% c("draws")]
}
df.cv_u5 <- do.call("rbind", cv)

# Order dates in factor for plot
gp1 <- c("Malawi", "Ethiopia", "Zimbabwe")
date1 <- df[["Zimbabwe"]]$years
gp2 <- c("Benin", "Guinea", "Namibia")
date2 <- c("90-93", df[["Benin"]]$years)
gp3 <- c("Ghana", "Kenya", "Nigeria", "Rwanda")
date3 <- c(df[["Ghana"]]$years, "15-18")
df.cv_a5$order <- NA

# 5-14 MR
for (i in 1:dim(df.cv_a5)[1]) {
        if (df.cv_a5$ctry[i] %in% gp1) {
                df.cv_a5$order[i] <- c(1:8)[which(df.cv_a5$years[i] == date1)]
        }
        else if (df.cv_a5$ctry[i] %in% gp2) {
                df.cv_a5$order[i] <- c(9:15)[which(df.cv_a5$years[i] == date2)]
        }
        else {
                df.cv_a5$order[i] <- c(16:24)[which(df.cv_a5$years[i] == date3)]
        }
}
df.cv_a5$years <- factor(df.cv_a5$years, levels = unique(df.cv_a5$years[order(df.cv_a5$order)]))

# Make first regional letter big
substr(df.cv_a5$region, 1, 1) <- toupper(substr(df.cv_a5$region, 1, 1))
substr(df.cv_u5$region, 1, 1) <- toupper(substr(df.cv_u5$region, 1, 1))

# Plot of all regional est. over periods
list.plot <- lapply(ctry.ls, function(x) {df.cv_a5 %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = years, y = cv*100, color = region)) +
                geom_point() +
                geom_hline(yintercept = 20, linetype = "dashed") +
                ylim(c(0, 100)) +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
                      legend.position = "none", axis.title.y = element_text(size = 12)) +
                ylab("CV (%)") +
                labs(color = "Region") +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot, nrow = 2 )

# for submission
p1 <- ggplot(df.cv_a5, aes(x = years, y = cv*100, color = region)) +
        geom_point() +
        geom_hline(yintercept = 20, linetype = "dashed") +
        ylim(c(0, 100)) +
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
              legend.position = "none", axis.title.y = element_text(size = 12),
              plot.margin = unit(c(0,0.5,1,0), "cm")) +
        ylab("CV (%)") +
        labs(color = "Region") +
        facet_wrap(~ ctry, ncol = 5, scales = "free")

# U5MR
for (i in 1:dim(df.cv_u5)[1]) {
        if (df.cv_u5$ctry[i] %in% gp1) {
                df.cv_u5$order[i] <- c(1:8)[which(df.cv_u5$years[i] == date1)]
        }
        else if (df.cv_u5$ctry[i] %in% gp2) {
                df.cv_u5$order[i] <- c(9:15)[which(df.cv_u5$years[i] == date2)]
        }
        else {
                df.cv_u5$order[i] <- c(16:24)[which(df.cv_u5$years[i] == date3)]
        }
}
df.cv_u5$years <- factor(df.cv_u5$years, levels = unique(df.cv_u5$years[order(df.cv_u5$order)]))

# Plot of all regional est. over periods
list.plot <- lapply(ctry.ls, function(x) {df.cv_u5 %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = years, y = cv*100, color = region)) +
                geom_point() +
                geom_hline(yintercept = 20, linetype = "dashed") +
                ylim(c(0, 100)) +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
                      legend.position = "none", axis.title.y = element_text(size = 12)) +
                ylab("CV (%)") +
                labs(color = "Region") +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot, nrow = 2 )

# for submission
p2 <- ggplot(df.cv_u5, aes(x = years, y = cv*100, color = region)) +
        geom_point() +
        geom_hline(yintercept = 20, linetype = "dashed") +
        ylim(c(0, 100)) +
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
              legend.position = "none", axis.title.y = element_text(size = 12),
              plot.margin = unit(c(0.5,0.5,1,0), "cm")) +
        ylab("CV (%)") +
        labs(color = "Region") +
        facet_wrap(~ ctry, ncol = 5, scales = "free")

p <- plot_grid(p1, p2, labels = c('(a) 10q5', '(b) 5q0'), 
               label_x = .45, label_y = .1, label_size = 12, nrow = 2)
# ggsave(filename = "./submission/fig plosone/cv.eps",
#        plot = p,
#        device = cairo_ps)

# Highlight robust est.
df.rob <- merge(df.cv_a5, df.cv_u5, 
                by = c("ctry", "region", "years"))
df.rob$robust <- ifelse(df.rob$cv.x <0.2, TRUE, FALSE)
df.rob$robust_u5 <- ifelse(df.rob$cv.y <0.2, TRUE, FALSE)

# Amount of robust est. by country
prop.rob.est <- df.rob %>% 
        dplyr::group_by(ctry) %>% 
        dplyr::summarize(prop.rob = (sum(robust)/n())*100)
# prop of robust estimates for text
df.rob %>% 
        dplyr::summarize(prop.rob = (sum(robust)/n())*100)
df.rob %>% 
        dplyr::summarize(prop.rob = (sum(robust_u5)/n())*100)

# Keep period if:
# >=75% of regional estimates are below 0.2
# Kept periods:
kept.periods <- df.rob %>% 
        dplyr::group_by(ctry, years) %>% 
        dplyr::summarize(prop.rob = (sum(robust)/length(unique(region)))*100) %>% 
        dplyr::filter(prop.rob > 75)

# Subset robust est.

df.rob.only <- df.rob %>% 
        dplyr::filter(ctry == "Malawi" & years %in% kept.periods$years[kept.periods$ctry == "Malawi"] |
               ctry == "Benin" & years %in% kept.periods$years[kept.periods$ctry == "Benin"] |
               ctry == "Ethiopia" & years %in% kept.periods$years[kept.periods$ctry == "Ethiopia"] |
               ctry == "Ghana" & years %in% kept.periods$years[kept.periods$ctry == "Ghana"] |
               ctry == "Guinea" & years %in% kept.periods$years[kept.periods$ctry == "Guinea"] |
               ctry == "Nigeria" & years %in% kept.periods$years[kept.periods$ctry == "Nigeria"] |
               ctry == "Rwanda" & years %in% kept.periods$years[kept.periods$ctry == "Rwanda"] |
               ctry == "Zimbabwe" & years %in% kept.periods$years[kept.periods$ctry == "Zimbabwe"] ) 

# Plot kept est. over kept periods
# Zimbabwe should be added but only one period... not added
list.plot.rob <- lapply(ctry.ls[-c(6,7,10)], function(x) {df.rob.only %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = years, y = cv.x*100, color = region)) +
                geom_point() +
                geom_hline(yintercept = 15, linetype = "dashed") +
                ylim(c(0, 100)) +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
                      legend.position = "none", axis.title.y = element_text(size = 12) ) +
                ylab("Coefficient of Variation (%)") +
                labs(color = "Region") +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot.rob, nrow = 2 )

# for submission (need to remove Ghana)
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot.rob, nrow = 2 )
# ggsave(filename = "./submission/fig plosone/rob15.eps",
#        plot = p,
#        device = cairo_ps)


#--- Ridge plots ------------------------------------------------------------------------------------------------------------------------------------------

# Comparison of countries
df.test <- NULL
x <- lapply(c("Nigeria", "Ethiopia"), function(x) {
        
        test <- df[[x]]$'sim a5'
        test$ctry <- x
        df.test <- rbind(test, df.test)
})

y <- do.call("rbind", x)
x <- y %>% 
        filter(ctry == "Nigeria" & years %in% c("07-10", "15-18") |
               ctry == "Ethiopia" & years == "08-11") %>% 
        mutate(lab = ifelse(ctry == "Nigeria" & years == "07-10", "Nigeria, 07-10",
                            ifelse(ctry == "Nigeria" & years == "15-18", "Nigeria, 15-18",
                            "Ethiopia, 08-11"))) # easier to plot with facet_wrap()

df.test <- do.call("rbind", x$draws)

df.end <- NULL
for (i in 1:dim(x)[1]) {
        
        df.it <- data.frame(region = as.character(rep(x$region[i], 4000)),
                            years = as.character(rep(x$years[i], 4000)),
                            ctry = as.character(rep(x$ctry[i], 4000)),
                            lab = as.character(rep(x$lab[i], 4000)),
                            qdraws = df.test[i ,])
        df.end <- rbind(df.it, df.end) 
}
df.end$lab <- factor(df.end$lab, levels = c("Ethiopia, 08-11", "Nigeria, 07-10", "Nigeria, 15-18"), 
                     labels = c("Ethiopia, 08-11", "Nigeria, 07-10", "Nigeria, 15-18"))

# Plot
ggplot(df.end, aes(y=region, x=qdraws*1000, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 0.9, size = 0.3, rel_min_height = 0.01) +
        scale_fill_viridis_c(name = "10q5") +
        xlim(c(0, 45)) +
        theme_bw() +
        theme(axis.title.y = element_blank()) +
        xlab("10q5") +
        facet_wrap(~ lab, scale = "free", ncol = 1)

# for submission
# ggsave("./submission/fig plosone/ridgeplot.eps")



#--- Relationship between 5q0 and 10q5 ------------------------------------------------------------------------------------------------------------------------

# Plot
list.plot.rob.only <- lapply(ctry.ls[-c(6, 7, 10)], function(x) {df.rob.only %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = median.x*1000, y = median.y*1000, shape = years)) +
                geom_point(size = 2, alpha = 1) +
                geom_errorbarh(aes(xmin = lower.x*1000, xmax = upper.x*1000), alpha = .2) +
                geom_errorbar(aes(ymin = lower.y*1000, ymax = upper.y*1000), alpha = .2) +
                theme_bw() +
                labs(shape = "Period") +
                ylab("5q0") +
                xlab("10q5") +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot.rob.only, nrow = 3 )

# for submission
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot.rob.only, nrow = 3 )
# ggsave(filename = "./submission/fig plosone/5q0vs10q5.eps",
#        plot = p,
#        device = cairo_ps)



# Overall relationship due to decreasing mortality over time in both age group (Pearson correlation)
# However, by period we see no relationship: region with high 5q0 are not the one having high 10q5.
# See Spearman rank corr. above 


#--- Pearson correlation with credible int. --------------------------------------------------------------------------------------------------------------

# account for uncertainty in correlation
rob.ctry.ls <- c("Malawi", "Benin", "Ethiopia", "Ghana", "Guinea", "Nigeria", "Rwanda")
lower <- (0.025*4000)+1 # defines the quantiles
upper <- (0.975*4000)-1
df.cor.ci <- NULL # dataset that will store correlations and associated CI by period for each ctry

for (l in seq_along(rob.ctry.ls)){
        
        
        # load data for a ctry
        db1 <- df[[rob.ctry.ls[l]]]$'sim a5'
        db2 <- df[[rob.ctry.ls[l]]]$'sim u5'
        
        # period considered as robust
        periods <- as.character(kept.periods$years[kept.periods$ctry == rob.ctry.ls[l]])
        
        # only keep robust periods
        db1<- db1 %>% 
                dplyr::filter(years %in% periods)
        db2<- db2 %>% 
                dplyr::filter(years %in% periods)
        # empty matrix to store corr associated to all simulations (lines) and periods (columns)
        sim_cor <- vector()
        
        # loop on all simulations
        for (j in 1:4000) {
                
                sim_x <- vector()
                sim_y <- vector()
                # given a simulation, loop on regions and periods
                for (k in 1:dim(db1)[1]) {
                        # given a period, store simluations (j) for various regions (k)
                        sim_x[k] <- db1[k, 3][[1]][j]
                        sim_y[k] <- db2[k, 3][[1]][j] 
                }
                # corr between the two age groups
                sim_cor[j] <- cor(sim_x, sim_y)
        }
        
        
        # order the columns to obtain quantiles
        sim_cor <- sort(sim_cor)
        # build data set of interest
        df.it <- data.frame(ctry = rob.ctry.ls[l],
                            median = median(sim_cor),
                            lower = sim_cor[lower],
                            upper = sim_cor[upper])
        
        df.cor.ci <- rbind(df.cor.ci, df.it)
        
        
}

# Plot
# order by highest value
df.cor.ci$ctry <- factor(df.cor.ci$ctry, levels = unique(df.cor.ci$ctry[order(df.cor.ci$median)]))
df.cor.ctry <- df.cor.ci

df.cor.ci %>% 
        ggplot(aes(x = ctry, y = median)) +
        geom_point(size = 2) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "blue") +
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 17), 
              axis.text.y = element_text(size = 17), axis.title.y = element_text(size = 18)) +
        ylim(c(-1,1)) +
        ylab("Pearson Corr.")





#--- Pearson correlation by period with credible int. --------------------------------------------------------------------------------------------------------------



# account for uncertainty in correlation
df.cor.ci <- NULL # dataset that will store correlations and associated CI by period for each ctry

for (l in seq_along(rob.ctry.ls)){
        
        
        # load data for a ctry
        db1 <- df[[rob.ctry.ls[l]]]$'sim a5'
        db2 <- df[[rob.ctry.ls[l]]]$'sim u5'
        
        # period considered as robust
        periods <- as.character(kept.periods$years[kept.periods$ctry == rob.ctry.ls[l]])
        
        # only keep robust periods
        db1<- db1 %>% 
                dplyr::filter(years %in% periods)
        db2<- db2 %>% 
                dplyr::filter(years %in% periods)
        # empty matrix to store corr associated to all simulations (lines) and periods (columns)
        sim_cor <- matrix(NA, nrow = 4000, ncol = length(periods))
        
        # loop on periods
        for (i in seq_along(periods)) { 
                
                x <- db1[db1$years == periods[i], ]
                y <- db2[db2$years == periods[i], ]
                
                # loop within period on all simulations
                for (j in 1:4000) {
                        
                        sim_x <- vector()
                        sim_y <- vector()
                         # given a period and simulation, loop on regions
                        for (k in 1:dim(x)[1]) {
                                # given a period, store simluations (j) for various regions (k)
                                sim_x[k] <- x[k, 3][[1]][j]
                                sim_y[k] <- y[k, 3][[1]][j] 
                        }
                        # corr between the two age groups
                        sim_cor[j, i] <- cor(sim_x, sim_y)
                }
                
        }
        # order the columns to obtain quantiles
        sim_cor <- apply(sim_cor,2,sort)
        # build data set of interest
        df.it <- data.frame(ctry = rep(rob.ctry.ls[l], length(periods)),
                           period = periods,
                           median = apply(sim_cor, 2, function(x) median(x)),
                           lower = sim_cor[lower, ],
                           upper = sim_cor[upper, ])
        
        df.cor.ci <- rbind(df.cor.ci, df.it)
        
        
}

# order periods for plotting
df.cor.ci$order <- NA

for (i in 1:dim(df.cor.ci)[1]) {
        if (df.cor.ci$ctry[i] %in% gp1) {
                df.cor.ci$order[i] <- c(1:8)[which(df.cor.ci$period[i] == date1)]
        }
        else if (df.cor.ci$ctry[i] %in% gp2) {
                df.cor.ci$order[i] <- c(9:14)[which(df.cor.ci$period[i] == date2)]
        }
        else {
                df.cor.ci$order[i] <- c(15:23)[which(df.cor.ci$period[i] == date3)]
        }
}
df.cor.ci$period <- factor(df.cor.ci$period, levels = unique(df.cor.ci$period[order(df.cor.ci$order)]))

# Plot
# Create a text

list.plot <- lapply(rob.ctry.ls[-c(1)], function(x) {
        
        r <- df.cor.ctry$median[df.cor.ctry == x]
        lo <- df.cor.ctry$lower[df.cor.ctry == x]
        up <- df.cor.ctry$upper[df.cor.ctry == x]
        grob <- grobTree(textGrob(paste("r=", round(r,2), " (", round(lo,2), "-", round(up,2), ")", sep=""), 
                                  x=0.3,  y=0.1, hjust=0,
                                  gp=gpar(col="black", fontsize=15, fontface="bold")))
        
        df.cor.ci <- df.cor.ci[df.cor.ci$ctry == x, ]
        ggplot(data = df.cor.ci, aes(x = period, y = median)) +
                geom_point(size = 2) +
                geom_pointrange(aes(ymin = lower, ymax = upper)) +
                geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "blue") +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16, angle = -90), 
                      axis.text.y = element_text(size = 16), axis.title.y = element_text(size = 17)) +
                ylim(c(-1,1)) +
                ylab("Pearson Corr.") +
                annotation_custom(grob) +
                ggtitle(x)
})
gridExtra::grid.arrange( grobs = list.plot, nrow = 2 )


# for submission
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot, nrow = 2 )
# ggsave(filename = "./submission/fig plosone/corr.eps",
#        plot = p,
#        device = cairo_ps)




#--- Variances of the different components ------------------------------------------------------------------------------------------------------------------------

df.var <- NULL
for (i in seq_along(ctry.ls)) {
        data <- df[[ctry.ls[i]]]$variance
        data$ctry <- ctry.ls[i]
        df.var <- rbind(data, df.var)
}
df.var_u5 <- df.var[df.var$Age == "<5", ]
df.var_u5 <- df.var_u5[, names(df.var_u5) %in% c("Proportion", "Name", "ctry")]
df.var_a5 <- df.var[df.var$Age == ">=5", ]
df.var_a5 <- df.var_a5[, names(df.var_a5) %in% c("Proportion", "Name", "ctry")]

t.var_u5 <- setNames(reshape(df.var_u5, idvar = "ctry", timevar = "Name", direction = "wide"), 
         c("Country", "RW2", "ICAR", "Space (S)", "Time (T)", "RW2xICAR"))
t.var_u5 <- t.var_u5[, c(1, 2, 3, 6, 5, 4)]

t.var_a5 <- setNames(reshape(df.var_a5, idvar = "ctry", timevar = "Name", direction = "wide"), 
                     c("Country", "RW2", "ICAR", "Space (S)", "Time (T)", "RW2xICAR"))
t.var_a5 <- t.var_a5[, c(1, 2, 3, 6, 5, 4)]

t.var <- cbind(t.var_u5, t.var_a5)
t.var <- t.var[, c(1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 12)]
t.var <- t.var %>%
        arrange(factor(Country, levels = ctry.ls))

print(xtable::xtable(t.var, digits=c(0, 0,rep(1, 10))), include.rownames = FALSE)


# data for text
mean(t.var_u5$RW2)
mean(t.var_a5$RW2)

mean(t.var_u5$ICAR)
mean(t.var_a5$ICAR)

mean(t.var_u5$RW2xICAR)
mean(t.var_a5$RW2xICAR)

mean(t.var_u5$'Space (S)')
mean(t.var_a5$'Space (S)')

mean(t.var_u5$'Time (T)')
mean(t.var_a5$'Time (T)')




