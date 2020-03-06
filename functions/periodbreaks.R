

# FUNCTION: Create period breaks for multiple DHS surveys

# Code by: Benjamin-Samuel Schlüter
# Date: 4/3/2020
#-------------------------------------------------------------

periodbreaks <- function(data, nber.p = 3, length.p = 4) {
        
        enddate_DHS <- 1900 + max(data[[last_DHS]][,"v008"])/12
        upper <- ifelse(abs(enddate_DHS - floor(enddate_DHS)) == 0, enddate_DHS, # if enddate_DHS after September (3/4 of a year) use it as highest year
                        ifelse(abs(enddate_DHS - floor(enddate_DHS)) >= 0.75, floor(enddate_DHS) + 1,
                               floor(enddate_DHS)))
        
        startdate_DHS <- 1900 + max(data[[head(filename1, 1)]]$v008)/12
        lower <- ifelse(abs(startdate_DHS - floor(startdate_DHS)) == 0, startdate_DHS, 
                        ifelse(abs(startdate_DHS - floor(startdate_DHS)) >= 0.75, floor(startdate_DHS) + 1,
                               floor(startdate_DHS)))
        lower <- lower - (nber.p*length.p) # defines lowest year for year.cut
        breaks <- sort( seq(upper, lower, by = -length.p) ) # start from latest year and go back in time (reason to have sort())
        breaks
}
