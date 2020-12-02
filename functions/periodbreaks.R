

# FUNCTION: Create period breaks for multiple DHS surveys

# Code by: Benjamin-Samuel Schlüter
# Date: 4/3/2020

# Notes:
# 1) set common period breaks but only keep nber.p periods for each DHS
# 2) amount.info allows to define if 1 year of data is sufficient to consider a period

#-------------------------------------------------------------

periodbreaks <- function(data, nber.p = 4, length.p = 4, amount.info = 1) {
        
        enddate_DHS <- 1900 + max(data[[last_DHS]][,"v008"])/12 # last interview date
        upper <- ifelse(abs(enddate_DHS - floor(enddate_DHS)) == 0, floor(enddate_DHS), # if enddate_DHS after September (3/4 of a year) use it as highest year
                        ifelse(abs(enddate_DHS - floor(enddate_DHS)) >= 0.75, floor(enddate_DHS) + 1,
                               floor(enddate_DHS)))
        lower <- upper - (10*length.p) # defines lowest year for year.cut
        breaks <- sort( seq(upper, lower, by = -length.p) ) # start from latest year and go back in time (reason to have sort())
        breaks
        
        upper_date <- lapply(data, function(x) {1900 + range(x$v008)[2]/12}) # upper date DHS surveyS
        y_cut <- list() # container for periods of each DHS
        for (j in seq_along(filename1)) {
                
                time_lapse <- upper_date[[ filename1[j] ]] - head(breaks, -1) # time between lower bound period and upper date DHS
                if (any(time_lapse >= 0 & time_lapse <= amount.info)) { # if upper date to close to lower bound of period, start periods one period below
                        k <- which(time_lapse >= 0 & time_lapse <= amount.info)
                        y_cut[[ filename1[j] ]] <- tail(breaks[1:k], (nber.p+1))
                } else {                                                # start periods in the period within which the upper date is
                        k <- which.min(time_lapse[time_lapse>0])
                        y_cut[[ filename1[j] ]] <- tail(breaks[1:k+1], (nber.p+1))
                }
        }
        y_cut
        
}
