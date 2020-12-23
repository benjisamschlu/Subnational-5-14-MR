


# FUNCTION: Convert first letter of levels to capital

# Code by: Benjamin-Samuel Schlüter
# Date: 23/12/2020

# Notes:
# 1) # for gsub (end code):
#
#

#-------------------------------------------------------------

firstup <- function(x) { 
        end <- nchar(levels(x))
        levels(x) <- paste(toupper(substr(levels(x), 1, 1)), substr(levels(x), 2, end), sep = "")
        x
}