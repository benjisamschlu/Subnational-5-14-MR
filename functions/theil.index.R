

# FUNCTION: Compute Theil Index

# Code by: Benjamin-Samuel Schlüter
# Date: 11/3/2020

# Notes:
# 1) Last version use the regional estimates to get the mean
#    used in theil index formula.
#-------------------------------------------------------------

theil.index <- function(data, prop, period, age.lab = "0-5") {
        
        prop <- data.frame( tail(prop, 1) )
        ref.p <- prop$period[1] # set the reference period for proportion of children
        ref.p <- gsub("^|\\d{2}(\\d{2})(\\-)|\\d{2}(\\d{2})", "\\1\\2\\3", ref.p) # transform four digits date into two
        
        reg_est <- data[data$Type == "Space-Time Smoothing" & data$region != "All" & data$years == period, ] # subset regional estimates for a given period
        # nat_est <- data[data$Type == "Space-Time Smoothing" & data$region == "All" & data$years == period, ] # subset national estimate for a given period
        nat_est <- sum(reg_est$mean*(prop$prop/100)) # mean computed with regional estimates
        region <- levels(reg_est$region)[levels(reg_est$region) != "All"] # extract region for TI function
        
        ti <- lapply(region, 
                     function(x) {(prop$prop[prop$region == x]/100)*(reg_est$mean[reg_est$region == x]/nat_est)*log(reg_est$mean[reg_est$region == x]/nat_est)})
        
        return(list(sum(unlist(ti)), period, age.lab) )

        
}


