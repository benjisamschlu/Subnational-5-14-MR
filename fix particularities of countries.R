########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: 

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 4-3-2020

# Notes:
# 1) 


########################################################################################################################################################


# RWANDA
########

# RW2005DHS has region name in another variable, hence had to run: data[[1]][, "v024"] <- data[[1]][, "sregnat"] 
# before running childmonth()

# Which columns contain actual region names for oldest DHS

# admin <- lapply(data[[1]], function(x) print(sum(grep("west", x))))
# cond <- lapply(admin, function(x) x > 0)
# admin[unlist(cond)]
# unique(data[[1]][, "sregnat"]) # region for rwanda

data[[1]][, "v024"] <- data[[1]][, "sregnat"] 


# ETHIOPIA
##########

# Convert ethiopian date into gregorian date

# data[[1]][, "b3"]
# lapply(data, dim)
# unique(data[[4]][, "v024"])

data <- lapply(data, function(x) {x$b3 <- x$b3+92; x})
data <- lapply(data, function(x) {x$v008 <- x$v008+92; x})
