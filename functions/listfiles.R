

# FUNCTION: Search for file(s) by pattern in a given directory

# Code by: Benjamin-Samuel Schlüter
# Date: 3/3/2020
#-------------------------------------------------------------


listfiles <- function(path = getwd(), start = TRUE,  match = NULL) {
        
        if (is.null(match)) { stop("Select a matching pattern") }
        
        if (start == TRUE) { pattern <- paste("^",match, sep = "") } # add an option for just having the pattern somewhere
        else { pattern <- paste(match, "$", sep = "") }
        
        dir( path = path, pattern = pattern ) # check for file
        
}