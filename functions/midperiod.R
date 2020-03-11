

# FUNCTION: Find mid period from a date format: "2 digits-2 digits"

# Code by: Benjamin-Samuel Schlüter
# Date: 11/3/2020

# Notes:

#-------------------------------------------------------------

midperiod <- function(data) {
        
        data$p.mid <- ifelse(as.numeric(substr(data$years,1,2))+as.numeric(substr(data$years,4,5)) < 40, ( (2000+as.numeric(substr(data$years,1,2))) + (2001+as.numeric(substr(data$years,4,5)))) / 2,
                             ifelse(as.numeric(substr(data$years,1,2))+as.numeric(substr(data$years,4,5)) > 160, ( (1900+as.numeric(substr(data$years,1,2))) + (1901+as.numeric(substr(data$years,4,5)))) / 2,
                                    ( (1900+as.numeric(substr(data$years,1,2))) + (2001+as.numeric(substr(data$years,4,5)))) / 2))
        data
        
}