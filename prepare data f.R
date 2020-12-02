########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: Store the data for all DHS of a given country in a list (5q0 or 10q5)

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 23-7-2020

# Notes:
# 1) Check code "fix particularities of countries" for Rwanda and Ethiopia before running the different do files

########################################################################################################################################################


rm(list=ls())


#--- Load packages ----------------------------------------------------------------------------------------------------------------------


packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr","demogsurv", "sp", "maptools", "tidyverse", "plyr")
invisible( lapply(packages, library, character.only = TRUE) )


#--- Load functions ----------------------------------------------------------------------------------------------------------------------

source("./code/functions/listfiles.R") 
source("./code/functions/periodbreaks.R") 
source("./code/functions/childmonth.R") 
source("./code/functions/same_order.R")


#--- Find the data available for a country ------------------------------------------------------------------------------------

ID <- "BJ" # CHANGE country ID
ctry <- "Benin" # CHANGE ACCORDING TO THE COUNTRY
nber_dhs <- 5

filename1 <- listfiles(path = "./data/fbh", start = TRUE, match = ID) 
filename2 <- listfiles(path = paste("./data/fbh", filename1, sep = "/"), start = FALSE, match = ".(DTA|dta)")
data_to_load <- paste("./data/fbh", filename1, filename2, sep = "/") ; rm(filename2)


#--- Load FBH data ------------------------------------------------------------------------------------------------------------

data <- lapply(data_to_load, function(x) read.dta13(x, generate.factors = TRUE)) ; rm(data_to_load) # load the data
names(data) <- filename1


#--- Load GEO data ------------------------------------------------------------------------------------------------------------

last_DHS <- tail(filename1,1) # Only load geo info of last DHS
if ( file.exists( paste("./data/shapefiles", last_DHS, "shps/sdr_subnational_boundaries2.shp", sep = "/")) ) {
        
        mapfilename <- paste("./data/shapefiles", last_DHS, "shps/sdr_subnational_boundaries2.shp", sep = "/")
        mapfilename0 <- paste("./data/shapefiles",last_DHS, "shps/sdr_subnational_boundaries.shp", sep = "/")
        geo <- readOGR(mapfilename, verbose = FALSE)
        geo0 <- readOGR(mapfilename0, verbose = FALSE)
        
        if (length(geo) > length(geo0)) { # to assess if "..2.shp" is admin 1 or 2 as it is not constant over DHS surveys
                geo <- readOGR(mapfilename0, verbose = FALSE)
                geo0 <- readOGR(mapfilename, verbose = FALSE)
        }
        
        Amat <- getAmat(geo, geo$REGNAME)
        Amat0 <- getAmat(geo0, geo0$REGNAME)
        rm(mapfilename, mapfilename0) # clean environment
        
} else {
        mapfilename <- paste("./data/shapefiles", last_DHS, "shps/sdr_subnational_boundaries.shp", sep = "/")
        geo <- readOGR(mapfilename, verbose = FALSE)
        Amat <- getAmat(geo, geo$REGNAME)  
        rm(mapfilename) # clean environment
        
}


#--- Compute child-month data ---------------------------------------------------------------------------------------------------

# Run these lines separately to assess region's order
data_u5 <- childmonth(u5mr = TRUE, data = data, nber.p = 6) # verify that region names printed have same order

data_a5 <- childmonth(u5mr = FALSE, data = data, nber.p = 3) 

# If FALSE above, set common region names manually:
data_u5[[3]][, "region"] <- factor(data_u5[[3]][, "region"], levels = c("caprivi", "erongo", "hardap", "karas", "kavango", "khomas", "kunene", "ohangwena", "omaheke", "omusati", "oshana", "oshikoto", "otjozondjupa"), labels = geo$REGNAME)
data_a5[[3]][, "region"] <- factor(data_a5[[3]][, "region"], levels = c("caprivi", "erongo", "hardap", "karas", "kavango", "khomas", "kunene", "ohangwena", "omaheke", "omusati", "oshana", "oshikoto", "otjozondjupa"), labels = geo$REGNAME)
# Check
levels(data_u5[[2]][, "region"])
levels(data_a5[[3]][, "region"])


# Count person-month and deaths
lapply(data_a5, function(x) dim(x))
lapply(data_u5, function(x) dim(x))
lapply(data_a5, function(x) sum(x$died))
lapply(data_u5, function(x) sum(x$died))
