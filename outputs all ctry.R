


#######################################################################################
##      PROJECT: Space-time smoothing of 5-14 mortality estimates in SSA
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
##      Dec. 2020
#######################################################################################
#
#       Comparison of all countries and creation of manusript figures and tables    
#
#######################################################################################
#
#
# Notes:
# 1) 

########################################################################################################################################################


rm(list=ls())

# ----- Load packages ----------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr", "demogsurv", "sp", "purrr", "lme4", "plyr", "grid", "forcats", "cowplot")
# "hrbrthemes"
invisible( lapply(packages, library, character.only = TRUE) )


# ----- Load functions ----------------------------------------------------------------------------------------------------------------------

source("./code/functions/cv.R") # fct to compute coefficient of variation from posterior distribution
source("./code/functions/firstup.R") # fct to transform 1st letter of factor to capital
source("./code/functions/midperiod.R") # fct defines mid period from 4y period


# ----- Load data ----------------------------------------------------------------------------------------------------------------------------------------------

ctry.ls <- c("Benin", "Burkina Faso", "Cameroon", "Ethiopia", "Ghana", "Guinea", "Kenya", "Lesotho", "Madagascar", "Malawi",
             "Mali", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

df <- lapply(ctry.ls, function(x) {readRDS(paste("./data/tidy/", x, ".rds", sep = ""))})
names(df) <- ctry.ls


# ----- Descriptive stat on dth and person month -----------------------------------------------------------------------------------------------------------------------------------------------

# For manuscript text: count person-month and deaths for all countries
n <- lapply(ctry.ls, function(x) {
        list(df[[x]]$exp_u5, df[[x]]$exp_a5, df[[x]]$dth_u5, df[[x]]$dth_a5)
})
sum(unlist(sapply(n,"[",1)))/12 # person-year <5
sum(unlist(sapply(n,"[",2)))/12 # person-year 5-14
sum(unlist(sapply(n,"[",3))) # deaths <5
sum(unlist(sapply(n,"[",4))) # deaths 5-14


# ----- Selectivity assessment: SSA, Li and our sample ----------------------------------------------------------------------------------------------------------------------------------------

sub.ctry <- c("Angola", "Burundi", "Democratic Republic of the Congo", "Cameroon", "Central African Republic", "Chad",
              "Congo", "Equatorial Guinea", "Gabon", "Kenya", "Nigeria", "Rwanda", "Sao Tome and Principe", "United Republic of Tanzania",
              "Uganda", "South Sudan", "Eritrea", "Ethiopia", "Somalia", "Botswana", "Comoros", "Lesotho", 
              "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Seychelles", "South Africa", "Eswatini", "Zambia", "Benin", 
              "Mali", "Burkina Faso", "Cabo Verde", "CÃ´te d'Ivoire", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mauritania", 
              "Niger", "Senegal", "Sierra Leone", "Togo", "Zimbabwe")
li.ctry <- c("Angola", "Burundi", "Chad", "Comoros", "Congo", "CÃ´te d'Ivoire", "Democratic Republic of the Congo", "Gabon",
             "Gambia", "Liberia", "Mozambique", "Sierra Leone", "Togo", "United Republic of Tanzania")
# Load UN IGME data
df.un <- read.csv("./data/auxiliary/un_igme.csv", header = TRUE)
df.un <- df.un[df.un$Indicator == "Mortality rate age 5-14", ]
df.un <- df.un[df.un$Geographic.area %in% sub.ctry, ]
df.un <- df.un[df.un$Series.Name == "UN IGME estimate", ]
df.un <- df.un[, c("Geographic.area", "OBS_VALUE", "REF_DATE", "LOWER_BOUND", "UPPER_BOUND")]
names(df.un) <- c("ctry", "mean", "p.mid", "lower", "upper")

df.context <- lapply(sub.ctry, function(x) {
  
        last.y <- max(df.un$p.mid[as.character(df.un$ctry) == x])
        q <- df.un$mean[as.character(df.un$ctry) == x & df.un$p.mid == last.y]
        data.frame(ctry = x, 
                   q = q,
                   year = last.y)
})
df.context <- do.call("rbind", df.context)
df.context$li <- ifelse(as.character(df.context$ctry) %in% c(li.ctry, ctry.ls), "yes", "no")
df.context$own<- ifelse(as.character(df.context$ctry) %in% c(ctry.ls, "United Republic of Tanzania"), "yes", "no")

# Fig S64 Supp Info
# pdf("./submission/final/zip Bruno/zip Benjamin/selectivity_assessment.pdf")
plot(density(df.context$q, from = min(df.context$q), to = max(df.context$q)),
     ylim = c(0, 0.08),
     lty = 1,
     xlab = "Latest UN IGME 10q5 estimates", 
     main = "")
lines(density(df.context$q[df.context$li == "yes"], from = min(df.context$q[df.context$li == "yes"]), to = max(df.context$q[df.context$li == "yes"])),
      lty = 2)
lines(density(df.context$q[df.context$own == "yes"], from = min(df.context$q[df.context$own == "yes"]), to = max(df.context$q[df.context$own == "yes"])),
      lty = 3)
legend("topright", legend = c("Sub-Saharan (SSA) countries", "Li et al. SSA sample", "Our sample"), 
       lty = c(1, 2, 3))
# dev.off()


# ----- Impact of smoothing on precision ----------------------------------------------------------------------------------------------------------------------------------------

# Fig S2-S21 Supp Info
list.plot <- lapply(ctry.ls, function(x) {
        
        a5 <- df[[x]]$'a5' # load estimates on 10q5 (a5)
        a5 <- a5[a5$region != "All", ]
        levels(a5$Type)[levels(a5$Type)=="Horvitz-Thompson"] <- "Meta-Analysis"
        a5$region <- firstup(a5$region)
        
        ggplot(data = a5, aes(x = years, y = mean, group = Type, colour = Type)) +
                geom_point(aes(shape = Type), position = position_dodge(width = 0.4), size = 1.5) +
                geom_line(data = a5[a5$Type == "Space-Time Smoothing", ], aes(x = years, y = mean)) +
                geom_errorbar(position = position_dodge(width = 0.2), aes(ymin=lower, ymax=upper), width=.1) +
                facet_wrap(~region, ncol = 3) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90)) +
                scale_color_manual(values = c("orange", "black")) + 
                ylab("10q5 (deaths per 1000)") +
                xlab("Period") 
        
        # for submission
         # ggsave(paste("./submission/final/smoothing_impact_ ", x, ".pdf", sep = ""))
}
)
list.plot


# ----- National-level model and IGME comparison ----------------------------------------------------------------------------------------------------------------

# Fig S22-S41 Supp. Info
df.un <- df.un[as.character(df.un$ctry) %in% c(ctry.ls, "United Republic of Tanzania"), ]
levels(df.un$ctry)[levels(df.un$ctry) == "United Republic of Tanzania"] <- "Tanzania"

ltype <- c("UN IGME" = "dashed")
shp <- c("National-level model" = 16)
list.plot <- lapply(ctry.ls, function(x) {
        
        un <- df.un[df.un$ctry == x, ]
        min <- min(un$p.mid)
        a5 <- df[[x]]$'a5' # load estimates on 10q5 (a5)
        a5 <- a5[a5$region == "All" & a5$Type == "Space-Time Smoothing", ]
        a5 <- midperiod(a5)
        a5 <- a5[a5$p.mid >= min, ]
        
        ggplot(un, aes(x = p.mid, y = mean)) +
                geom_line(aes(x = p.mid, y = mean, linetype = "UN IGME")) +
                geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
                geom_point(data = a5, aes(shape = "National-level model")) +
                geom_pointrange(data = a5, aes(ymin=lower, ymax=upper)) +
                theme_bw() +
                scale_linetype_manual(name="",values=ltype) +
                scale_shape_manual(name="", values=shp) +
                ylab("10q5 (deaths per 1000)") +
                xlab("Period") 
        
        
        # for submission
        # ggsave(paste("./submission/final/comparisonUN_ ", x, ".pdf", sep = ""))
})
list.plot


# ----- Plot q of Space-Time estimates on log10 scale ----------------------------------------------------------------------------------------------------

# Fig 1, Fig 2 and Fig S42-S61
list.plot <- lapply(ctry.ls, function(x) {
        
        u5 <- df[[x]]$'u5' # load estimates on 5q0 (u5)
        u5 <- u5[u5$region != "All" & u5$Type == "Space-Time Smoothing", ]
        u5$region <- firstup(u5$region)
        a5 <- df[[x]]$'a5' # load estimates on 10q5 (a5)
        a5 <- a5[a5$region != "All" & a5$Type == "Space-Time Smoothing", ]
        a5$region <- firstup(a5$region)
        
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
                scale_linetype_discrete(name = "Age Group", labels = c(expression(paste(""[5], "q"[0], "")), expression(paste(""[10], "q"[5], "")))) +
                ylab("Mortality rates (per 1000)") +
                xlab("Period")
        
        # for submission
        # ggsave(paste("./submission/final/zip Bruno/zip Benjamin/logq_", x, ".eps", sep = ""))
}
)
list.plot

# For manuscript text: % significant decrease in 10q5 and 5q0
list.decline <- lapply(ctry.ls, function(x) {
  
  y <- df[[x]]$years
  lo.y <- head(y, 1)
  up.y <- tail(y, 1)
  u5 <- df[[x]]$'u5'
  u5 <- u5[u5$region != "All" & u5$Type == "Space-Time Smoothing" & u5$years %in% c(lo.y, up.y), ]
  a5 <- df[[x]]$'a5'
  a5 <- a5[a5$region != "All" & a5$Type == "Space-Time Smoothing" & a5$years %in% c(lo.y, up.y), ]
  
  ls.reg <- unique(a5$region) # get sub-national units
  n.reg <- length(ls.reg) # get number of sub-national units
  
  n <- lapply(ls.reg, function(y) { # count of sub-national unit that had a significant improvement in 5q0
    if( u5$upper[u5$region == y & u5$years == up.y] < u5$lower[u5$region == y & u5$years == lo.y] ) {
      1
    } else {0}
  })
  prop_u5 <- (sum(do.call("rbind", n))/n.reg)*100 # obtain proportion
  n <- lapply(ls.reg, function(y) { # count of sub-national unit that had a significant improvement in 10q5
    if( a5$upper[a5$region == y & a5$years == up.y] < a5$lower[a5$region == y & a5$years == lo.y] ) {
      1
    } else {0}
  })
  prop_a5 <- (sum(do.call("rbind", n))/n.reg)*100 # obtain proportion
  list(u5 = prop_u5, a5 = prop_a5)
}
)
sum(unlist(lapply(list.decline, `[[`, 1)))/20
sum(unlist(lapply(list.decline, `[[`, 2)))/20


# ----- Reliable sub-national estimates according to CV ----------------------------------------------------------------------

# Load sample from posterior, compute summary measures and create data.frame (1st loop 5-14, 2nd <5)
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
gp1 <- c("Malawi", "Ethiopia", "Zimbabwe", "Tanzania", "Senegal", "Niger")
date1 <- c("80-83", df[["Senegal"]]$years)
gp2 <- c("Benin", "Guinea", "Namibia")
date2 <- df[["Benin"]]$years
gp3 <- c("Ghana", "Kenya", "Nigeria", "Rwanda", "Burkina Faso", "Cameroon", "Lesotho", "Mali", "Zambia")
date3 <- df[["Nigeria"]]$years
gp4 <- c("Madagascar", "Uganda")
date4 <- c("81-84", df[["Uganda"]]$years)

df.cv_a5$order <- NA
for (i in 1:dim(df.cv_a5)[1]) {
        if (df.cv_a5$ctry[i] %in% gp1) {
                df.cv_a5$order[i] <- c(1:10)[which(df.cv_a5$years[i] == date1)]
                df.cv_u5$order[i] <- c(1:10)[which(df.cv_u5$years[i] == date1)]
                
        }
        else if (df.cv_a5$ctry[i] %in% gp2) {
                df.cv_a5$order[i] <- c(11:18)[which(df.cv_a5$years[i] == date2)]
                df.cv_u5$order[i] <- c(11:18)[which(df.cv_u5$years[i] == date2)]
                
        } 
        else if (df.cv_a5$ctry[i] %in% gp3) {
                df.cv_a5$order[i] <- c(19:27)[which(df.cv_a5$years[i] == date3)]
                df.cv_u5$order[i] <- c(19:27)[which(df.cv_u5$years[i] == date3)]
                
        } 
        else {
                df.cv_a5$order[i] <- c(28:36)[which(df.cv_a5$years[i] == date4)]
                df.cv_u5$order[i] <- c(28:36)[which(df.cv_u5$years[i] == date4)]
                
        }
}

df.cv_a5$years <- factor(df.cv_a5$years, levels = unique(df.cv_a5$years[order(df.cv_a5$order)]))
df.cv_u5$years <- factor(df.cv_u5$years, levels = unique(df.cv_u5$years[order(df.cv_u5$order)]))

# Make first regional letter big
substr(df.cv_a5$region, 1, 1) <- toupper(substr(df.cv_a5$region, 1, 1))
substr(df.cv_u5$region, 1, 1) <- toupper(substr(df.cv_u5$region, 1, 1))

# Fig 3 Manuscript
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
gridExtra::grid.arrange( grobs = list.plot, nrow = 4 )
# for submission
# p <- gridExtra::grid.arrange( grobs = list.plot, nrow = 4 )
# ggsave(filename = "./submission/final/zip Bruno/zip Benjamin/Fig3.eps",
#        plot = p,
#        device = cairo_ps)

# Same as Fig 3 but for <5
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
gridExtra::grid.arrange( grobs = list.plot, nrow = 4 )

# Highlight reliable estimates
df.rob <- merge(df.cv_a5, df.cv_u5, 
                by = c("ctry", "region", "years"))
df.rob$robust <- ifelse(df.rob$cv.x <0.2, TRUE, FALSE)
df.rob$robust_u5 <- ifelse(df.rob$cv.y <0.2, TRUE, FALSE)

# Amount of robust est. by country
prop.rob.est <- df.rob %>% 
        dplyr::group_by(ctry) %>% 
        dplyr::summarize(prop.rob = (sum(robust)/n())*100)

# For manuscript text: % significant decrease in 10q5 and 5q0
df.rob %>% 
        dplyr::summarize(prop.rob = (sum(robust)/n())*100)
df.rob %>% 
        dplyr::summarize(prop.rob = (sum(robust_u5)/n())*100)

# Keep period if:
# >75% of regional estimates are below 0.2
kept.periods <- df.rob %>% 
        dplyr::group_by(ctry, years) %>% 
        dplyr::summarize(prop.rob = (sum(robust)/length(unique(region)))*100) %>% 
        dplyr::filter(prop.rob > 75)

# Subset estimates based on reliable years
ctry.ls <- unique(kept.periods$ctry)
df.rob.only <- lapply(ctry.ls, function(x) {
        rob.y <- kept.periods$years[kept.periods$ctry == x]
        df.rob[df.rob$ctry == x & df.rob$years %in% rob.y, ]
})
df.rob.only <- do.call("rbind", df.rob.only)

# Code below can be used to build Fig S63
list.plot.rob <- lapply(ctry.ls, function(x) {df.rob.only %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = years, y = cv.x*100, color = region)) +
                geom_point() +
                geom_hline(yintercept = 20, linetype = "dashed") +
                ylim(c(0, 100)) +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = -90, size = 11),
                      legend.position = "none", axis.title.y = element_text(size = 12) ) +
                ylab("Coefficient of Variation (%)") +
                labs(color = "Region") +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot.rob, nrow = 3 )

# for submission Manuscript Fig
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot.rob, nrow = 2 )
# ggsave(filename = "./submission/fig plosone/rob15.eps",
#        plot = p,
#        device = cairo_ps)

# for submission Supp Info Fig
# p <- gridExtra::grid.arrange( grobs = list.plot.rob, nrow = 3 )
# ggsave(paste("./submission/final/cv 10q5(conservative).pdf", sep = ""),
#        plot = p)


# ----- Ridge plots ------------------------------------------------------------------------------------------------------------------------------------------

# Comparison of Nigeria and Ethiopia
df.test <- NULL
x <- lapply(c("Nigeria", "Ethiopia"), function(x) {
        
        test <- df[[x]]$'sim a5' # load random sample from posterior
        test$ctry <- x
        df.test <- rbind(test, df.test) # merge data on two countries together
})

y <- do.call("rbind", x)
x <- y %>% 
        filter(ctry == "Nigeria" & years %in% c("07-10", "15-18") |
               ctry == "Ethiopia" & years == "08-11") %>% 
        mutate(lab = ifelse(ctry == "Nigeria" & years == "07-10", "Nigeria, 07-10",
                            ifelse(ctry == "Nigeria" & years == "15-18", "Nigeria, 15-18",
                            "Ethiopia, 08-11"))) # easier to plot with facet_wrap()

df.test <- do.call("rbind", x$draws)

# create data set used for building ridge plot
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
# Fig 4
ggplot(df.end, aes(y=region, x=qdraws*1000, fill = stat(x))) +
        ggridges::geom_density_ridges_gradient(scale = 0.9, size = 0.3, rel_min_height = 0.01) +
        scale_fill_viridis_c(name = expression(paste(""[10], "q"[5], ""))) +
        xlim(c(0, 45)) +
        theme_bw() +
        theme(axis.title.y = element_blank()) +
        xlab("10q5") +
        facet_wrap(~ lab, scale = "free", ncol = 1)

# for submission
# ggsave("./submission/final/zip Bruno/zip Benjamin/Fig4.eps")


# ----- Relationship between 5q0 and 10q5 ------------------------------------------------------------------------------------------------------------------------

# Fig 5
list.plot.rob.only <- lapply(ctry.ls, function(x) {df.rob.only %>% 
                filter(ctry == x) %>% 
                ggplot(aes(x = median.x*1000, y = median.y*1000, shape = years)) +
                geom_point(size = 2, alpha = 1) +
                geom_errorbarh(aes(xmin = lower.x*1000, xmax = upper.x*1000), alpha = .2) +
                geom_errorbar(aes(ymin = lower.y*1000, ymax = upper.y*1000), alpha = .2) +
                theme_bw() +
                theme(legend.position = "none") +
                scale_shape_manual(values=1:8) +
                        labs(shape = "Period") +
                ylab(expression(paste(""[5], "q"[0], ""))) +
                xlab(expression(paste(""[10], "q"[5], ""))) +
                ggtitle(x)})
gridExtra::grid.arrange( grobs = list.plot.rob.only, nrow = 3 )
# ggsave("./submission/fig plosone/review/5q0vs10q5.pdf", plot = p)

# for submission
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot.rob.only, nrow = 3 )
# ggsave(filename = "./submission/final/zip Bruno/zip Benjamin/Fig5.eps",
#        plot = p,
#        device = cairo_ps)


# ----- Pearson correlation with credible int. --------------------------------------------------------------------------------------------------------------

# Zimbabwe & Lesotho have only one period -> not added
rob.ctry.ls <- ctry.ls[!ctry.ls %in% c("Lesotho", "Zimbabwe")]
lower <- (0.025*4000)+1 # defines the quantiles
upper <- (0.975*4000)-1
df.cor.ci <- NULL # dataset that will store correlations and associated CI by period for each ctry

for (l in seq_along(rob.ctry.ls)){
        
        # load data for a ctry
        db1 <- df[[rob.ctry.ls[l]]]$'sim a5'
        db2 <- df[[rob.ctry.ls[l]]]$'sim u5'
        
        # period considered as reliable
        periods <- as.character(kept.periods$years[kept.periods$ctry == rob.ctry.ls[l]])
        
        # only keep reliable periods
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
                # given a simulation, loop on sub-national units and periods
                for (k in 1:dim(db1)[1]) {
                        # given a period, store simluations (j) for various sub-national units (k)
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

# order by highest value
df.cor.ci$ctry <- factor(df.cor.ci$ctry, levels = unique(df.cor.ci$ctry[order(df.cor.ci$median)]))
df.cor.ctry <- df.cor.ci

# Plot correlation of each country
df.cor.ci %>% 
        ggplot(aes(x = ctry, y = median)) +
        geom_point(size = 2) +
        geom_pointrange(aes(ymin = lower, ymax = upper)) +
        geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "blue") +
        theme_bw() +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 17, angle = 90), 
              axis.text.y = element_text(size = 17), axis.title.y = element_text(size = 18)) +
        ylim(c(-1,1)) +
        ylab("Pearson Corr.")


# ----- Pearson correlation by period with credible int. --------------------------------------------------------------------------------------------------------------

# dataset that will store correlations and associated CI by period for each ctry
df.cor.ci <- NULL 

for (l in seq_along(rob.ctry.ls)){
        
        # load data for a ctry
        db1 <- df[[rob.ctry.ls[l]]]$'sim a5'
        db2 <- df[[rob.ctry.ls[l]]]$'sim u5'
        
        # period considered as robust
        periods <- as.character(kept.periods$years[kept.periods$ctry == rob.ctry.ls[l]])
        
        # only keep reliable periods
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
                         # given a period and simulation, loop on sub-national unit
                        for (k in 1:dim(x)[1]) {
                                # given a period, store simluations (j) for various sub-national unit (k)
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
                df.cor.ci$order[i] <- c(1:10)[which(df.cor.ci$period[i] == date1)]
        }
        else if (df.cor.ci$ctry[i] %in% gp2) {
                df.cor.ci$order[i] <- c(11:18)[which(df.cor.ci$period[i] == date2)]
        }
        else if (df.cor.ci$ctry[i] %in% gp3) {
                df.cor.ci$order[i] <- c(19:27)[which(df.cor.ci$period[i] == date3)]
        }
        else {
                df.cor.ci$order[i] <- c(28:36)[which(df.cor.ci$period[i] == date4)]
        }
}
df.cor.ci$period <- factor(df.cor.ci$period, levels = unique(df.cor.ci$period[order(df.cor.ci$order)]))

# Fig 6
# Does not include Malawi as it contains only 3 regions
list.plot <- lapply(rob.ctry.ls[rob.ctry.ls != "Malawi"], function(x) {
        
        r <- df.cor.ctry$median[df.cor.ctry == x]
        lo <- df.cor.ctry$lower[df.cor.ctry == x]
        up <- df.cor.ctry$upper[df.cor.ctry == x]
        grob <- grobTree(textGrob(paste("r=", round(r,2), " (", round(lo,2), "; ", round(up,2), ")", sep=""), 
                                  x=0.33,  y=0.1, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="bold")))
        
        df.cor.ci <- df.cor.ci[df.cor.ci$ctry == x, ]
        ggplot(data = df.cor.ci, aes(x = period, y = median)) +
                geom_point(size = 2) +
                geom_pointrange(aes(ymin = lower, ymax = upper)) +
                geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
                theme_bw() +
                theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 11, angle = -90), 
                      axis.text.y = element_text(size = 13), axis.title.y = element_text(size = 14)) +
                ylim(c(-1,1)) +
                ylab("Pearson Corr.") +
                annotation_custom(grob) +
                ggtitle(x)
})
gridExtra::grid.arrange( grobs = list.plot, nrow = 5 )
# ggsave("./submission/fig plosone/review/corr.pdf", plot = p)

# for submission
# library("Cairo")
# p <- gridExtra::arrangeGrob( grobs = list.plot, nrow = 5 )
# ggsave(filename = "./submission/final/zip Bruno/zip Benjamin/Fig6.eps",
#        plot = p,
#        device = cairo_ps)

# For manuscript text: number of significant correlation
df.cor.ci$sign <- ifelse(df.cor.ci$lower <= 0 & df.cor.ci$upper >= 0, "not significantly != 0", "significantly != 0") 
sum(df.cor.ci$sig[df.cor.ci$ctry != "Malawi"] == "not significantly != 0")
dim(df.cor.ci[df.cor.ci$ctry != "Malawi", ])[1]


# ----- Variance decomposition into the different components ------------------------------------------------------------------------------------------------------------------------

ctry.ls <- c("Benin", "Burkina Faso", "Cameroon", "Ethiopia", "Ghana", "Guinea", "Kenya", "Lesotho", "Madagascar", "Malawi",
             "Mali", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
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

# create code for Latex table
# print(xtable::xtable(t.var, digits=c(0, 0,rep(1, 10))), include.rownames = FALSE)

# For manuscript text: % of explained variance from each component
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

# Help interpret variance analysis
list.plot <- lapply(ctry.ls, function(x) {

        a5 <- df[[x]]$'a5'
        a5 <- a5[a5$Type == "Space-Time Smoothing", ]

        ggplot(data = a5[a5$region != "All", ], aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000, group = region)) +
                geom_point(data = a5[a5$region != "All", ], aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000, colour = region), position = position_dodge(width = 0.3)) +
                geom_pointrange(data = a5[a5$region != "All", ], aes(x = years, ymin=(exp(logit.lower)/(1+exp(logit.lower)))*1000, ymax=(exp(logit.upper)/(1+exp(logit.upper)))*1000, colour = region), position = position_dodge(width = 0.3)) +
                geom_line(data = a5[a5$region == "All", ], aes(x = years, y = (exp(logit.median)/(1+exp(logit.median)))*1000)) +
                theme_bw() +
                theme(legend.position = "none") +
                ylab("Mortality rates (per 1000)") +
                xlab("Period") + 
                ggtitle(x)

}
)
list.plot[1]


