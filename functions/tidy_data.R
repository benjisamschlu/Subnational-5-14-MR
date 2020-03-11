

# FUNCTION: Tidy the data to estimate the number of children in a region 

# Code by: Benjamin-Samuel Schlüter
# Date: 10/3/2020

# Notes:

#-------------------------------------------------------------

library("dplyr")

tidy_data <- function(data) {
        
        df <- lapply(data, function(x) x[, c("b3", "v005", "v024", "v008", "b5", "b7")]) # keep variables required to estimate proportion of children in a region
        df <- lapply(df, setNames, nm = c("dob_cmc", "weights", "region", "intw", "alive", "age")) # set names of columns
        df <- lapply(df, function(x) { x$dob <- 1900 + x$dob_cmc/12; x }) # convert cmc to dates
        df <- lapply(df, function(x) { x$weights <- x$weights/1000000; x }) # rescale weights
        df
}
