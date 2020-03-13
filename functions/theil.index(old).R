

# FUNCTION: Compute Theil Index

# Code by: Benjamin-Samuel Schlüter
# Date: 11/3/2020

# Notes:
# 1) Theil Indexes are computed for (the first and) last period available. 
#    Choice of fixed proportions for comparison.
#    First period is max two periods away from the oldest DHS 
#    (defined in periodbreaks used with propchildren_by_region). 
# 2) IMPROVE to be able to select one period for children proportions

#-------------------------------------------------------------

theil.index <- function(data, prop) {
        
        # prop <- c(head(prop, 1), tail(prop, 1)) # select first and last period of the list of proportion of children by region
        prop <- c(tail(prop, 1), tail(prop, 1)) 
        period <- lapply(prop, function(x) x$period[1]) # get the period that correspond to the first and last
        period <- lapply(period, function(x) { x <- gsub("^|\\d{2}(\\d{2})(\\-)|\\d{2}(\\d{2})", "\\1\\2\\3", x); x } ) # transform four digits date into two
        
        reg_estimates <- lapply(period, function(x) data[data$years == x & data$Type == "Space-Time Smoothing" & data$region != "All", ]) # get regional estimates fro both periods
        reg_estimates <- lapply(reg_estimates, function(x) { x$region <- droplevels(x$region); x }) # region level "All" has to be dropped
        nat_estimates <- lapply(period, function(x) data[data$years == x & data$Type == "Space-Time Smoothing" & data$region == "All", ]) # get national estimate for both periods
        region <- levels(reg_estimates[[1]]$region) # extract region for TI function
        
        TI <- function(prop, reg_est, nat_est, period) { # function that loop on region to compute Theil Index
                
                T.i <- lapply(region, function(x) { (prop$prop[prop$region == x]/100)* (reg_est$mean[reg_est$region == x]/nat_est$mean)*log(reg_est$mean[reg_est$region == x]/nat_est$mean)})  
                list(Theil = sum(unlist(T.i)), period = period) # output is the theil index and associated period
        }
        
        mapply(FUN =  TI,
                       prop = prop, # all the arguments consist of a list with two elements
                       reg_est = reg_estimates,
                       nat_est = nat_estimates,
                       period = period, 
                       SIMPLIFY = FALSE)
}