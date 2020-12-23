


# FUNCTION: Convert first letter of string to capital

# Code by: Benjamin-Samuel Schlüter
# Date: 23/12/2020

# Notes:
# 1) 
#
#

#-------------------------------------------------------------

CapStr <- function(y) {
        c <- strsplit(y, " ")[[1]]
        paste(toupper(substring(c, 1,1)), substring(c, 2),
              sep="", collapse=" ")
}