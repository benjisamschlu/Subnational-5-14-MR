


#######################################################################################
##      PROJECT: Space-time smoothing of 5-14 mortality estimates in SSA
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
##      Dec. 2020
#######################################################################################
#
#       Run main R scripts for tidying data, estimation and storing results.
#       Then you can open "outputs all ctry.R" and run it to obtain main outputs of 
#       analysis.
#
#######################################################################################
#
#
# Notes:
# 1) Takes approx. 10min per country
 

########################################################################################################################################################


rm(list=ls())


# ----- Load packages ---------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr","demogsurv", "sp", "maptools", "tidyverse", "plyr", "INLA")
invisible( lapply(packages, library, character.only = TRUE) )


#--- Load functions ----------------------------------------------------------------------------------------------------------------------

source("./code/functions/listfiles.R") # list data files associated to a country ID
source("./code/functions/periodbreaks.R") # defines period breaks according to various DHS surveys
source("./code/functions/childmonth.R") # compute child-month using GetBirth() fct from SUMMER pkg
source("./code/functions/CapStr.R") # Make 1st letter of string capital
source("./code/functions/getSmooth2.R") # function that keeps the random samples from the posterior using getSmooth() 
# fct from SUMMER pkg to be able to obtain credible interval around other metrics (i.e correlation)
source("./code/functions/create_df.R") # simple function that creates a data.frame of interest for later analysis steps


# ----- Meta data -------------------------------------------------------------------------------------------------------------------------

ctry.ls <- c("Benin", "Burkina Faso", "Cameroon", "Ethiopia", "Ghana", "Guinea", "Kenya", "Lesotho", "Madagascar", "Malawi",
             "Mali", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
ctry.id <- c("BJ", "BF", "CM", "ET", "GH", "GN", "KE", "LS", "MD", "MW", "ML", "NM", "NI", "NG", "RW", "SN", "TZ", "UG", "ZM", "ZW")

keep.obj <- c("ctry.ls", "ctry.id", "listfiles", "periodbreaks", "childmonth", "CapStr", "getSmooth2", "create_df", "keep.obj")


# ----- Run main scripts for all countries -------------------------------------------------------------------------------------------------

for (c in seq_along(ctry.ls)) {
        
        ID <- ctry.id[c]
        ctry <- ctry.ls[c]
        cat(paste(ctry, "\n"))

        source("./code/prepare data.R", echo = TRUE)

        source("./code/get estimates.R", echo = TRUE)

        source("./code/store results.R", echo = TRUE)
        
        rm(list=setdiff(ls(), keep.obj))
        
}
