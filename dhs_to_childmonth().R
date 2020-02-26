
# DHS TO CHILD-MONTH BY COUNTRY FUNCTION
########################################

# Notes: This functions take all DHS survey available in the folder data/fbh for a given country
#        and store them in a list. Then, all data sets are converted into child-month format. 
#        u5mr argument allows to do that for 5q0 or 10q5
#        loc.cluster.rm.na allows to remove cluster that have incorrect location
#        mutli.survey useful when merging all direct estimates --> need common time variable

# BLOCK TO LOAD AVAILABLE DHS DATA OF A GIVEN COUNTRY

dhs_to_childmonth <- function(ctry.id, u5mr = TRUE, period.length = 5, nber.period = 3, child_month = TRUE, loc.cluster.rm.na = TRUE,
                              multi.survey = FALSE) {
        
        dhs <- dir( path = "./data/fbh", pattern = paste("^",ctry.id, sep = "") ) # check DHS surveys of ctry.id
        filename <- vector("list", length(dhs))
        
        
        for (i in seq_along(dhs)) { # store the different fbh data name available for the country
                
                df <- dir(path = paste("./data/fbh", dhs[i], sep = "/"), pattern = ".(DTA|dta)")
                filename[[i]] <- paste("./data/fbh", dhs[i], df, sep = "/")
        }
        
        data <- vector("list", length(filename))
        
        
        for (i in seq_along(filename)) { # stores data sets into a list
                
                data[[i]] <- read.dta13(filename[[i]], generate.factors = TRUE)
        }
        
        
        
        
        
        # BLOCK TO LOCATE CLUSTERS IN ADMIN 1/2 AND EXCLUDE CLUSTERS WITHOUT LOCATION
        # Not compulsory step because region variable is already available in fbh data --> of interest if cluster-model
        
        geo <- vector("list", length(dhs)) # container for geo objects of each dhs
        Amat <- vector("list", length(dhs)) # container for adjacency matrix
        
        if ( file.exists( paste("./data/shapefiles", dhs[i], "shps/sdr_subnational_boundaries2.shp", sep = "/")) ) {
                geo0 <- vector("list", length(dhs)) # container for geo objects of each dhs
                Amat0 <- vector("list", length(dhs)) # container for adjacency matrix 
        }
        
        for (i in seq_along(dhs)) { # store the different gps cluster data name available for the country
                
                gpsfilename <- dir(path = paste("./data/gps cluster", dhs[i], sep = "/"), pattern = ".shp$") # get file names
                gpsfilename <- paste("./data/gps cluster", dhs[i], gpsfilename, sep = "/")
                
                # If admin 2 available, has to be added to the fbh data. O here are the admin 2 level
                if ( file.exists( paste("./data/shapefiles", dhs[i], "shps/sdr_subnational_boundaries2.shp", sep = "/")) ) {
                        
                        mapfilename <- paste("./data/shapefiles", dhs[i], "shps/sdr_subnational_boundaries2.shp", sep = "/")
                        mapfilename0 <- paste("./data/shapefiles", dhs[i], "shps/sdr_subnational_boundaries.shp", sep = "/")
                        geo[[i]] <- readOGR(mapfilename, verbose = FALSE)
                        geo0[[i]] <- readOGR(mapfilename0, verbose = FALSE)
                        
                        if (length(geo[[i]]) > length(geo0[[i]])) { # to assess if "..2.shp" is admin 1 or 2 as it is not constant over DHS surveys
                                geo[[i]] <- readOGR(mapfilename0, verbose = FALSE)
                                geo0[[i]] <- readOGR(mapfilename, verbose = FALSE)
                                
                        }
                        
                        Amat[[i]] <- getAmat(geo[[i]], geo[[i]]$REGNAME)
                        Amat0[[i]] <- getAmat(geo0[[i]], geo0[[i]]$REGNAME)
                        
                        if (loc.cluster.rm.na == TRUE) { # Only drop clusters if asked
                                
                                # Build data set with gps info of clusters to merge this info with fbh data
                                loc <- readOGR(gpsfilename, verbose = FALSE)
                                loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                                gps <- mapPoints(loc.dat, geo = geo[[i]], long = "long", lat = "lat", names = c("REGNAME"))
                                gps0 <- mapPoints(loc.dat, geo = geo0[[i]], long = "long", lat = "lat", names = c("REGNAME"))
                                colnames(gps)[4] <- "admin1"
                                colnames(gps0)[4] <- "admin2"
                                gps <- merge(gps, gps0[, c("cluster", "admin2")]) 
                                
                                # print the number of cluster with missing location info
                                print( paste(dhs[i], "Nber of cluster missing", sum(is.na(gps$admin1)), "|", sum(is.na(gps$admin2)), sep = " ") ) # print the number of cluster with missing location info
                                
                                # Exclude clusters with no location info
                                unknown_cluster <- gps$cluster[which(is.na(gps$admin1))]
                                gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                                data[[i]] <- data[[i]][ data[[i]]$v001 %in% unknown_cluster == FALSE, ]
                                data[[i]] <- merge(data[[i]], gps[, c("cluster", "admin1", "admin2")], by.x = "v001", 
                                                   by.y = "cluster", all.x = TRUE)
                                data[[i]]$v024 <- data[[i]]$admin1 # v024 will have the same admin1 labels as Amat since geo[[]] is used in mapPoints() 
                                
                        }
                        
                }                        
                
                # If only admin 1 available
                else {
                        
                        mapfilename <- paste("./data/shapefiles", dhs[i], "shps/sdr_subnational_boundaries.shp", sep = "/")
                        geo[[i]] <- readOGR(mapfilename, verbose = FALSE)
                        Amat[[i]] <- getAmat(geo[[i]], geo[[i]]$REGNAME)
                        
                        if (loc.cluster.rm.na == TRUE) { # Only drop clusters if asked
                                
                                # Build data set with gps info of clusters to merge this info with fbh data
                                loc <- readOGR(gpsfilename, verbose = FALSE)
                                loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                                gps <- mapPoints(loc.dat, geo = geo[[i]], long = "long", lat = "lat", names = c("REGNAME"))
                                colnames(gps)[4] <- "admin1"
                                print( paste(dhs[i], "Nber of cluster missing", sum(is.na(gps$admin1)), sep = " ") ) # print the number of cluster with missing location info
                                
                                # Exclude clusters with no location info
                                unknown_cluster <- gps$cluster[which(is.na(gps$admin1))]
                                gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                                data[[i]] <- data[[i]][ data[[i]]$v001 %in% unknown_cluster == FALSE, ]
                                data[[i]] <- merge(data[[i]], gps[, c("cluster", "admin1")], by.x = "v001", 
                                                   by.y = "cluster", all.x = TRUE)
                                data[[i]]$v024 <- data[[i]]$admin1 # v024 will have the same admin1 labels as Amat!   
                        }
                        
                         
                        
                }
                
                
                
                
        }
        
                
                
        
        
        # BLOCK TO COMPUTE CHILD MONTH DATA (ADD ARG FOR MULTIPLE DHS: multi.survey =+ TRUE/FALSE)
        
        if (child_month == TRUE) { # if false, only store the dhs data without transforming into child-month
                
                if (multi.survey == TRUE) { # when working with multiple surveys, time range common to all dhs
                        
                        up_srvyear = 1900 + floor(range(data[[length(dhs)]]$v008)[2]/12) # get date of last dhs interview
                        upper_y = up_srvyear # defines highest year for year.cut 
                        lo_srvyear = 1900 + floor(range(data[[1]]$v008)[2]/12) # get date of first dhs interview
                        lower_y = lo_srvyear - (nber.period*period.length) # defines lowest year for year.cut
                        y_cut = seq(upper_y, lower_y, by = -period.length)
                        y_cut = sort(y_cut)
                }
                
                for (i in seq_along(data)) { # convert each data set of the list to child-month 
                        
                        srvyear = 1900 + floor(range(data[[i]]$v008)[2]/12) # get date of last interview
                        
                        if (multi.survey == FALSE) { # when working with only one survey, time range change for each dhs
                                
                                upper_y = srvyear # defines highest year for year.cut 
                                lower_y = upper_y - (nber.period*period.length) # defines lowest year for year.cut
                                y_cut = seq(lower_y, upper_y, by = period.length)
                        }
                        
                        if (u5mr == TRUE) {m_cut <- c(1, 12, 24, 36, 48, 60)}
                        else {m_cut <- seq(60, 180, by = 12)}
                        
                        # if admin 2 variable exist, has to be kept in the child month data
                        if ("admin2" %in% colnames(data[[i]])) {
                              
                                data[[i]] <- getBirths(data = data[[i]], surveyyear = srvyear, strata = c("v023"), dob = "b3",
                                                       alive = "b5", age = "b7", date.interview = "v008", 
                                                       variables = c("v001", "v002", "v004", "v005", "v021", "v022", "v023", "v024", "v025", "v139", "admin2"), #v139 are regions
                                                       month.cut = m_cut, 
                                                       year.cut = y_cut)  
                                data[[i]] <- data[[i]][, c("v001", "v002", "v024", "admin2", "time", "age", "v005", "strata", "died")] # keep columns of interest
                                colnames(data[[i]]) <- c("clustid", "id", "region", "admin2", "time", "age", "weights", "strata", "died") # rename them
                        }
                        else {
                                data[[i]] <- getBirths(data = data[[i]], surveyyear = srvyear, strata = c("v023"), dob = "b3",
                                                       alive = "b5", age = "b7", date.interview = "v008", 
                                                       variables = c("v001", "v002", "v004", "v005", "v021", "v022", "v023", "v024", "v025", "v139"), #v139 are regions
                                                       month.cut = m_cut, 
                                                       year.cut = y_cut)
                                data[[i]] <- data[[i]][, c("v001", "v002", "v024", "time", "age", "v005", "strata", "died")]
                                colnames(data[[i]]) <- c("clustid", "id", "region", "time", "age", "weights", "strata", "died")
                        }
                        
                        if (multi.survey == TRUE) { # such that all surveys have the same region names
                                
                                data[[i]][, "region"] <- tolower(data[[i]][, "region"]) 
                        }
                        
                        
                        if (u5mr == FALSE) {
                                data[[i]] <- data[[i]][data[[i]]$age != "0-59", ] 
                                data[[i]][, "age"] <- droplevels( data[[i]][, "age"])
                        }
                }
                
        }
        names(data) <- dhs
        
        # Output
        if (loc.cluster.rm.na == TRUE) {
                list("dhs" = dhs, "childmonth" = data, "geo" = geo, "geo0" = geo0, "Amat" = Amat, "Amat0" = Amat0)
        }
        else {
                list("dhs" = dhs, "childmonth" = data, "geo" = geo, "Amat" = Amat)
                }
}
