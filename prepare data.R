


#######################################################################################
##      PROJECT: Space-time smoothing of 5-14 mortality estimates in SSA
##      -- by Benjamin-Samuel Schlüter --
##      UCLouvain
##      Dec. 2020
#######################################################################################
#
#       Load multiple DHS for a given country into a list
#       Set a given shapefile for each country
#       Tidy the data for each country:
#       - Re-locate clusters in coherent sub-national areas DHS surveys using the shapefile
#         and the clusters GPS location
#       - Set the same writting for sub-national areas over DHS surveys
#       Convert to child-month data
#
#######################################################################################
#
#
# Notes:
# 1) 
# 2) 

########################################################################################################################################################


# ----- Load FBH data -------------------------------------------------------------------------------------------------------------------

nber_dhs <- as.numeric(length(listfiles(path = "./data/fbh", start = TRUE, match = ID)))

filename1 <- listfiles(path = "./data/fbh", start = TRUE, match = ID) 
filename2 <- listfiles(path = paste("./data/fbh", filename1, sep = "/"), start = FALSE, match = ".(DTA|dta)")
data_to_load <- paste("./data/fbh", filename1, filename2, sep = "/") ; rm(filename2)


# ----- Load FBH data ------------------------------------------------------------------------------------------------------------------

data <- lapply(data_to_load, function(x) read.dta13(x, generate.factors = TRUE)) ; rm(data_to_load) # load the data
names(data) <- filename1
 

# ----- Load GEO data ------------------------------------------------------------------------------------------------------------------

# set shapefile used for each country
if (ctry == "Benin") { selected_DHS <- "BJ2001DHS" 
} else if (ctry == "Burkina Faso") { selected_DHS <- "BF1999DHS"
} else if (ctry == "Cameroon") { selected_DHS <- "CM1998DHS"
} else if (ctry == "Guinea") { selected_DHS <- "GN1999DHS"
} else if (ctry == "Madagascar") { selected_DHS <- "MD2004DHS" 
} else if (ctry == "Mali") {  selected_DHS <- "ML1996DHS" 
} else if (ctry == "Niger") { selected_DHS <- "NI1998DHS"
} else if (ctry == "Senegal") { selected_DHS <- "SN2017DHS"
} else if (ctry == "Tanzania") { selected_DHS <- "TZ1996DHS"
} else if (ctry == "Uganda") { selected_DHS <- "UG1995DHS"
} else if (ctry == "Zambia") { selected_DHS <- "ZM2002DHS"
} else { selected_DHS <- tail(filename1,1) } # geo info of last DHS 

if ( file.exists( paste("./data/shapefiles", selected_DHS, "shps/sdr_subnational_boundaries2.shp", sep = "/")) ) {
        
        mapfilename <- paste("./data/shapefiles", selected_DHS, "shps/sdr_subnational_boundaries2.shp", sep = "/")
        mapfilename0 <- paste("./data/shapefiles",selected_DHS, "shps/sdr_subnational_boundaries.shp", sep = "/")
        geo <- readOGR(mapfilename, verbose = FALSE)
        geo0 <- readOGR(mapfilename0, verbose = FALSE)
        
        if (length(geo) > length(geo0)) { # to assess if "..2.shp" is admin 1 or 2 as it is not constant over DHS surveys
                geo <- readOGR(mapfilename0, verbose = FALSE) # using the geo with fewer units to pool more information
                geo0 <- readOGR(mapfilename, verbose = FALSE)
        }
        Amat <- getAmat(geo, geo$REGNAME)
        Amat0 <- getAmat(geo0, geo0$REGNAME)
        rm(mapfilename, mapfilename0) 
} else {
        mapfilename <- paste("./data/shapefiles", selected_DHS, "shps/sdr_subnational_boundaries.shp", sep = "/")
        geo <- readOGR(mapfilename, verbose = FALSE)
        Amat <- getAmat(geo, geo$REGNAME)  
        rm(mapfilename) 
}


# ----- Harmonize the subnational unit and define strata if not done -----------------------------------------------------------------------------------------------------------

data <- lapply( data, function(x) {x$v024 <- tolower(x$v024) ; x} )

# BENIN
if (ctry == "Benin") { 
        # redefine strata 
        data[["BJ1996DHS"]]$v023 <- paste(data[["BJ1996DHS"]]$v024, "-", data[["BJ1996DHS"]]$v025)
        # harmonize subnational units
        # needs to include all potential writting in pattern
        # set names to the ones of geo$NAMES and aggregates geo units for latest DHS
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("alibori", "borgou"), "borgou",
                                                            ifelse(x$v024 %in% c("atacora", "donga"), "atacora",
                                                                   ifelse(x$v024 %in% c("zou", "collines"), "zou",
                                                                          ifelse(x$v024 %in% c("ouémé", "quémé", "plateau", "oueme"), "oueme",
                                                                                 ifelse(x$v024 %in% c("mono", "couffo"), "mono",
                                                                                        ifelse(x$v024 %in% c("atlantique", "atlantic", "littoral"), "atlantique", NA)))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = c("atacora", "atlantique", "borgou", "mono", "oueme",  "zou"),
                                                            labels = geo$REGNAME); x})
}

# BURKINA FASO 
if (ctry == "Burkina Faso") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # locate clusters (2010, 2003) in older administrative units (1999)
        data <- lapply(c("BF1993DHS", "BF1999DHS", "BF2003DHS", "BF2010DHS"), function(x) {
                if (x == "BF2003DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/BFGE43FL.shp", sep=""), verbose = FALSE)
                } else if (x == "BF2010DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/BFGE61FL.shp", sep=""), verbose = FALSE)
                }
                if (x %in% c("BF2010DHS", "BF2003DHS")) {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                data[[x]]
        })
        # rename list item for future functions
        names(data) <- c("BF1993DHS", "BF1999DHS", "BF2003DHS", "BF2010DHS")
        
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("ouagadougou"), "ouagadougou",
                                                            ifelse(x$v024 %in% c("north"), "north",
                                                                   ifelse(x$v024 %in% c("east"), "east",
                                                                          ifelse(x$v024 %in% c("west"), "west",
                                                                                 ifelse(x$v024 %in% c("central /south", "central/south"), "central /south", NA))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# CAMEROON
if (ctry == "Cameroon") {
        # Issue with geo label
        levels(geo@data$REGNAME)[levels(geo@data$REGNAME)=="NULL"] <- "yaounde/douala"
        levels(geo@data$REGNAME)[levels(geo@data$REGNAME)=="north / extreme north/ ad"] <- "north/ext north/adamoua"
        colnames(Amat)[colnames(Amat) == "NULL"] <- "yaounde/douala"
        rownames(Amat)[rownames(Amat) == "NULL"] <- "yaounde/douala"
        colnames(Amat)[colnames(Amat) == "north / extreme north/ ad"] <- "north/ext north/adamoua"
        rownames(Amat)[rownames(Amat) == "north / extreme north/ ad"] <- "north/ext north/adamoua"
        # add yaounde/douala as region in v024 for 1998
        data[["CM1998DHS"]]$v024[data[["CM1998DHS"]]$v023 == "yaoundé/douala"] <- "yaounde/douala"
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("adam/nord/ext-nord", "north/ extreme north/ ad", "nord", "north", "extreme nor", "extrême-nord", "far-north", "adamaoua", "adamawa"), "north/ext north/adamoua",
                                                            ifelse(x$v024 %in% c("centre/sud/est", "central, south, & east", "centre", "sud", "est", "centre (without yaounde)", "south", "east"), "central, south, & east",
                                                                   ifelse(x$v024 %in% c("ouest/littoral", "west & littoral", "ouest", "littoral", "west", "littoral (without douala)"), "west & littoral",
                                                                          ifelse(x$v024 %in% c("nord-ouest/sud-ouest", "northwest & southwest", "nord ouest", "sud ouest", "nord-ouest", "sud-ouest", "north-west", "south-west"), "northwest & southwest",
                                                                                 ifelse(x$v024 %in% c("yaoundé/douala", "yaounde/douala", "douala", "yaounde", "yaoundé"), "yaounde/douala", NA ))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# ETHIOPIA
if (ctry == "Ethiopia") {
        # Convert ethiopian date into gregorian date
        data <- lapply(data, function(x) {x$b3 <- x$b3+92; x})
        data <- lapply(data, function(x) {x$v008 <- x$v008+92; x})
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("tigray"), "Tigray",
                                                            ifelse(x$v024 %in% c("affar", "afar"), "Afar",
                                                                   ifelse(x$v024 %in% c("amhara"), "Amhara",
                                                                          ifelse(x$v024 %in% c("oromiya", "oromia"), "Oromia",
                                                                                 ifelse(x$v024 %in% c("somali"), "Somali",
                                                                                        ifelse(x$v024 %in% c("ben-gumz", "benishangul-gumuz", "benishangul"), "Benishangul",
                                                                                               ifelse(x$v024 %in% c("snnp", "snnpr"), "SNNPR",
                                                                                                      ifelse(x$v024 %in% c("gambela"), "Gambela",
                                                                                                             ifelse(x$v024 %in% c("harari"), "Harari",
                                                                                                                    ifelse(x$v024 %in% c("addis", "addis abeba", "addis ababa"), "Addis Adaba", 
                                                                                                                           ifelse(x$v024 %in% c("dire dawa"), "Dire Dawa", NA ))))))))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# GHANA
if (ctry == "Ghana") {
        # redefine strata 
        data[["GH1993DHS"]]$v023 <- paste(data[["GH1993DHS"]]$v024, "-", data[["GH1993DHS"]]$v025)
        data[["GH2003DHS"]]$v023 <- paste(data[["GH2003DHS"]]$v024, "-", data[["GH2003DHS"]]$v025)
        data[["GH2008DHS"]]$v023 <- paste(data[["GH2008DHS"]]$v024, "-", data[["GH2008DHS"]]$v025)
        
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("western", "western region"), "Western",
                                                            ifelse(x$v024 %in% c("central", "central region"), "Central",
                                                                   ifelse(x$v024 %in% c("greater accra", "greater accra region"), "Greater Accra",
                                                                          ifelse(x$v024 %in% c("eastern", "eastern region"), "Eastern",
                                                                                 ifelse(x$v024 %in% c("volta", "volta region"), "Volta",
                                                                                        ifelse(x$v024 %in% c("ashanti", "ashanti region"), "Ashanti",
                                                                                               ifelse(x$v024 %in% c("brong ahafo", "brong ahafo region", "brong-ahafo"), "Brong Ahafo",
                                                                                                      ifelse(x$v024 %in% c("northern", "northern region"), "Northern", 
                                                                                                             ifelse(x$v024 %in% c("upper east", "upper east region"), "Upper East",
                                                                                                                    ifelse(x$v024 %in% c("upper west", "upper west region"), "Upper West", NA )))))))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# GUINEA
if (ctry == "Guinea") {
        # redefine strata
        data[["GN2005DHS"]]$v023 <- paste(data[["GN2005DHS"]]$v024, "-", data[["GN2005DHS"]]$v025)
        data[["GN2018DHS"]]$v023 <- paste(data[["GN2018DHS"]]$v024, "-", data[["GN2018DHS"]]$v025)
        
        # locate clusters (2018, 2012, 2005) in older administrative units (1999)
        data <- lapply(c("GN1999DHS", "GN2005DHS", "GN2012DHS", "GN2018DHS"), function(x) {
                if (x == "GN2005DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/GNGE52FL.shp", sep=""), verbose = FALSE)
                } else if (x == "GN2012DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/GNGE61FL.shp", sep=""), verbose = FALSE)
                } else if (x == "GN2018DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/GNGE71FL.shp", sep=""), verbose = FALSE)
                } 
                
                if (x != "GN1999DHS") {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        print(sum(is.na(gps$region)))
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                
                data[[x]]
        })
        names(data) <- c("GN1999DHS", "GN2005DHS", "GN2012DHS", "GN2018DHS")
                
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                                    levels = geo$REGNAME,
                                                                    labels = geo$REGNAME); x})
}

# KENYA
if (ctry == "Kenya") {
        # redefine strata
        data[["KE2003DHS"]]$v023 <- paste(data[["KE2003DHS"]]$v024, "-", data[["KE2003DHS"]]$v025)
        data[["KE2008DHS"]]$v023 <- paste(data[["KE2008DHS"]]$v024, "-", data[["KE2008DHS"]]$v025)
        data[["KE2014DHS"]]$v023 <- paste(data[["KE2014DHS"]]$v024, "-", data[["KE2014DHS"]]$v025)
        
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("central"), "central",
                                                            ifelse(x$v024 %in% c("coast"), "coast",
                                                                   ifelse(x$v024 %in% c("eastern"), "eastern",
                                                                          ifelse(x$v024 %in% c("nairobi"), "nairobi",
                                                                                 ifelse(x$v024 %in% c("north eastern", "northeastern"), "north eastern",
                                                                                        ifelse(x$v024 %in% c("nyanza"), "nyanza",
                                                                                               ifelse(x$v024 %in% c("rift valley"), "rift valley",
                                                                                                      ifelse(x$v024 %in% c("western"), "western", NA )))))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# LESOTHO
if (ctry == "Lesotho") {
         # use the correct admin level
        geo <- geo0
        Amat <- Amat0
        # redefine strata 
        data[["LS2004DHS"]]$v023 <- paste(data[["LS2004DHS"]]$v024, "-", data[["LS2004DHS"]]$v025)
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("butha-buthe", "butha-bothe", "botha-bothe"), "botha-bothe",
                                                            ifelse(x$v024 %in% c("mafeteng"), "mafeteng",
                                                                   ifelse(x$v024 %in% c("leribe"), "leribe",
                                                                          ifelse(x$v024 %in% c("mokhotlong"), "mokhotlong",
                                                                                 ifelse(x$v024 %in% c("thaba tseka", "thaba-tseka"), "thaba tseka",
                                                                                        ifelse(x$v024 %in% c("maseru"), "maseru",
                                                                                               ifelse(x$v024 %in% c("berea"), "berea",
                                                                                                      ifelse(x$v024 %in% c("mohale's hoek"), "mohale's hoek",
                                                                                                             ifelse(x$v024 %in% c("qacha's-nek", "qasha's nek"), "qacha's-nek",
                                                                                                                    ifelse(x$v024 %in% c("quthing"), "quthing", NA )))))))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
        
        
}

# MADAGASCAR
if (ctry == "Madagascar") {
        # redefine strata 
        data[["MD1992DHS"]]$v023 <- paste(data[["MD1992DHS"]]$v024, "-", data[["MD1992DHS"]]$v025)
        
        # locate clusters (2009) in older administrative units (2004)
        loc <- readOGR("./data/gps cluster/MD2009DHS/MDGE53FL.shp", verbose = FALSE)
        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
        colnames(gps)[4] <- "region"
        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
        data[["MD2009DHS"]] <- data[["MD2009DHS"]][data[["MD2009DHS"]]$v001 %in% unknown_cluster == FALSE, ]
        data[["MD2009DHS"]] <- merge(data[["MD2009DHS"]], gps[, c("cluster", "region")], by.x = "v001",
                                     by.y = "cluster", all.x = TRUE)
        data[["MD2009DHS"]]$v024 <- data[["MD2009DHS"]]$region
        
        # this step needs to be done for all ctries, even the ones not subject to change in regions
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# MALAWI
if (ctry == "Malawi") {
        # redefine strata 
        data[["MW2000DHS"]]$v023 <- paste(data[["MW2000DHS"]]$v024, "-", data[["MW2000DHS"]]$v025)
        data[["MW2004DHS"]]$v023 <- paste(data[["MW2004DHS"]]$v024, "-", data[["MW2004DHS"]]$v025)
        data[["MW2010DHS"]]$v023 <- paste(data[["MW2010DHS"]]$v024, "-", data[["MW2010DHS"]]$v025)
        
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("northern region", "north", "northern"), "Northern region",
                                                            ifelse(x$v024 %in% c("central region", "central"), "Central region",
                                                                   ifelse(x$v024 %in% c("southern region", "south", "southern"), "Southern region", NA ))); x } )
        # this step needs to be done for all ctries, even the ones not subject to change in regions
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
        
}

# MALI
if (ctry == "Mali") {
        # redefine strata 
        data[["ML2001DHS"]]$v023 <- paste(data[["ML2001DHS"]]$v024, "-", data[["ML2001DHS"]]$v025)
        data[["ML2006DHS"]]$v023 <- paste(data[["ML2006DHS"]]$v024, "-", data[["ML2006DHS"]]$v025)
        
        # locate clusters (2018, 2013, 2006, 2001) in older administrative units (1996)
        # Issue with one of the geo label coded as NULL
        levels(geo@data$REGNAME)[levels(geo@data$REGNAME)=="NULL"] <- "gao/timbouctou"
        colnames(Amat)[colnames(Amat) == "NULL"] <- "gao/timbouctou"
        rownames(Amat)[rownames(Amat) == "NULL"] <- "gao/timbouctou"
        
        data <- lapply(c("ML1996DHS", "ML2001DHS", "ML2006DHS", "ML2013DHS", "ML2018DHS"), function(x) {
                if (x == "ML1996DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/MLGE33FL.shp", sep=""), verbose = FALSE)
                } else if (x == "ML2001DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/MLGE42FL.shp", sep=""), verbose = FALSE)
                } else if (x == "ML2006DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/MLGE52FL.shp", sep=""), verbose = FALSE)
                } else if (x == "ML2013DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/MLGE6BFL.shp", sep=""), verbose = FALSE)
                } else if (x == "ML2018DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/MLGE7AFL.shp", sep=""), verbose = FALSE)
                }
                
                loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                colnames(gps)[4] <- "region"
                print(sum(is.na(gps$region)))
                unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                   by.y = "cluster", all.x = TRUE)
                data[[x]]$v024 <- data[[x]]$region
                data[[x]]
        })
        # need to rename list item for future functions
        names(data) <- c("ML1996DHS", "ML2001DHS", "ML2006DHS", "ML2013DHS", "ML2018DHS")
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# NAMIBIA
if (ctry == "Namibia") {
        # redefine strata 
        data[["NM2007DHS"]]$v023 <- paste(data[["NM2007DHS"]]$v024, "-", data[["NM2007DHS"]]$v025)
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x}) 
}

# NIGER
if (ctry == "Niger") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("niamey"), "niamey",
                                                            ifelse(x$v024 %in% c("dosso"), "dosso",
                                                                   ifelse(x$v024 %in% c("maradi"), "maradi",
                                                                          ifelse(x$v024 %in% c("tahoua", "agadez", "tahoua/agadez"), "tahoua/agadez",
                                                                                 ifelse(x$v024 %in% c("tillaberi", "tillabéri"), "tillaberi",
                                                                                        ifelse(x$v024 %in% c("zinder", "diffa", "zinda/diffa"), "zinda/diffa", NA )))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
 
}

# NIGERIA
if (ctry == "Nigeria") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # harmonise subnational units
        # locate clusters (1990) in older administrative units (2018)
        loc <- readOGR("./data/gps cluster/NG1990DHS/NGGE23FL.shp", verbose = FALSE)
        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
        colnames(gps)[4] <- "region"
        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
        data[["NG1990DHS"]] <- data[["NG1990DHS"]][data[["NG1990DHS"]]$v001 %in% unknown_cluster == FALSE, ]
        data[["NG1990DHS"]] <- merge(data[["NG1990DHS"]], gps[, c("cluster", "region")], by.x = "v001",
                                     by.y = "cluster", all.x = TRUE)
        data[["NG1990DHS"]]$v024 <- data[["NG1990DHS"]]$region
        
        # upper case letter to coincide with geo
        data <- lapply(data, function(x) {x$v024 <- sapply(as.character(x$v024), CapStr); x})
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024, 
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# RWANDA
if (ctry == "Rwanda") {
        # RW2005DHS has region name in another variable
        data[[1]][, "v024"] <- data[[1]][, "sregnat"]
        # redefine strata 
        data[["RW2005DHS"]]$v023 <- paste(data[["RW2005DHS"]]$v024, "-", data[["RW2005DHS"]]$v025)
        data[["RW2008DHS"]]$v023 <- paste(data[["RW2008DHS"]]$v024, "-", data[["RW2008DHS"]]$v025)
        data[["RW2010DHS"]]$v023 <- paste(data[["RW2010DHS"]]$v024, "-", data[["RW2010DHS"]]$v025)
        # harmonizesubnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("city of kigali", "ville de kigali", "kigali city"), "kigali city",
                                                            ifelse(x$v024 %in% c("south", "sud"), "south",
                                                                   ifelse(x$v024 %in% c("west", "ouest"), "west",
                                                                          ifelse(x$v024 %in% c("north", "nord"), "north",
                                                                                 ifelse(x$v024 %in% c("est", "east"), "east", NA))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# SENEGAL
if (ctry == "Senegal") {
        # check if it would be possible to use latest years (14 R) otherwise 2005
        data[["SN1993DHS"]]$v023 <- paste(data[["SN1993DHS"]]$v024, "-", data[["SN1993DHS"]]$v025)
        data[["SN1997DHS"]]$v023 <- paste(data[["SN1997DHS"]]$v024, "-", data[["SN1997DHS"]]$v025)
        data[["SN2005DHS"]]$v023 <- paste(data[["SN2005DHS"]]$v024, "-", data[["SN2005DHS"]]$v025)
        data[["SN2011DHS"]]$v023 <- paste(data[["SN2011DHS"]]$v024, "-", data[["SN2011DHS"]]$v025)
        
        # Issue with one of the geo label coded as NULL
        levels(geo@data$REGNAME)[levels(geo@data$REGNAME)=="ThiÃ¨s"] <- "Thiès"
        colnames(Amat)[colnames(Amat) == "ThiÃ¨s"] <- "Thiès"
        rownames(Amat)[rownames(Amat) == "ThiÃ¨s"] <- "Thiès"
        # locate clusters (2019, 2018, 2016, 2014, 2011, 2005, 1997, 1993) in other administrative units (2017)
        # Issue with one of the geo label coded as NULL
        data <- lapply(c("SN1993DHS", "SN1997DHS", "SN2005DHS", "SN2011DHS", "SN2013DHS", "SN2014DHS", "SN2015DHS", "SN2016DHS", "SN2017DHS", "SN2018DHS", "SN2019DHS"), function(x) {
                if (x == "SN1993DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE23FL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN1997DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE32FL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2005DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE4BFL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2011DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE61FL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2013DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE6AFL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2014DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE71FL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2015DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE7AFL.shp", sep=""), verbose = FALSE)
                } else if (x == "SN2016DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/SNGE7IFL.shp", sep=""), verbose = FALSE)
                } 
                if (!(x %in% c("SN2017DHS", "SN2018DHS", "SN2019DHS"))) {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        print(sum(is.na(gps$region)))
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                
                data[[x]]
        })
        # need to rename list item for future functions
        names(data) <- c("SN1993DHS", "SN1997DHS", "SN2005DHS", "SN2011DHS", "SN2013DHS", "SN2014DHS", "SN2015DHS", "SN2016DHS", "SN2017DHS", "SN2018DHS", "SN2019DHS")
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("thi?s"), "Thiès",
                                                            ifelse(x$v024 %in% c("saint-louis"), "Saint-Louis", as.character(x$v024))); x } )
        data <- lapply(data, function(x) {x$v024 <- sapply(as.character(x$v024), CapStr); x})
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
        
}

# TANZANIA
if (ctry == "Tanzania") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # locate clusters (2016, 2010, 2005, 1999) in other administrative units (1996)
        # Issue with one of the geo label coded as NULL
        data <- lapply(c("TZ1996DHS", "TZ1999DHS", "TZ2005DHS", "TZ2010DHS", "TZ2016DHS"), function(x) {
                if (x == "TZ1999DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/TZGE43FL.shp", sep=""), verbose = FALSE)
                } else if (x == "TZ2010DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/TZGE61FL.shp", sep=""), verbose = FALSE)
                } else if (x == "TZ2016DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/TZGE7AFL.shp", sep=""), verbose = FALSE)
                } 
                if (!(x %in% c("TZ1996DHS", "TZ2005DHS"))) {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        print(sum(is.na(gps$region)))
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                
                data[[x]]
        })
        # need to rename list item for future functions
        names(data) <- c("TZ1996DHS", "TZ1999DHS", "TZ2005DHS", "TZ2010DHS", "TZ2016DHS")
        
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 %in% c("central", "singida", "dodoma"), "central",
                                                            ifelse(x$v024 %in% c("lake", "mara", "mwanza", "kagera", "shinyanga", "tabora", "kigoma"), "lake", 
                                                                   ifelse(x$v024 %in% c("southern highlands", "rukwa", "mbeya", "iringa"), "southern highlands",
                                                                          ifelse(x$v024 %in% c("southern", "ruvuma", "mtwara", "lindi"), "southern",
                                                                                 ifelse(x$v024 %in% c("coastal", "morogoro", "dar es salaam", "dar es salam", "coast", "pemba", "pwani", "pemba south", "pemba north", "tanga", "rest zanzibar", "zanzibar north", "zanziba south", "town west"), "coastal",
                                                                                        ifelse(x$v024 %in% c("northern highlands", "arusha", "kilimanjaro", "manyara"), "northern highlands", NA )))))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# UGANDA
if (ctry == "Uganda") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # locate clusters (2016, 2011, 2006, 1999) in other administrative units (1995)
        # Issue with one of the geo label coded as NULL
        data <- lapply(c("UG1995DHS", "UG2001DHS", "UG2006DHS", "UG2011DHS", "UG2016DHS"), function(x) {
                if (x == "UG2006DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/UGGE53FL.shp", sep=""), verbose = FALSE)
                } else if (x == "UG2011DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/UGGE61FL.shp", sep=""), verbose = FALSE)
                } else if (x == "UG2016DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/UGGE7AFL.shp", sep=""), verbose = FALSE)
                } 
                if (!(x %in% c("UG2001DHS", "UG1995DHS"))) {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        print(sum(is.na(gps$region)))
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                
                data[[x]]
        })
        # need to rename list item for future functions
        names(data) <- c("UG1995DHS", "UG2001DHS", "UG2006DHS", "UG2011DHS", "UG2016DHS")
        
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# ZAMBIA
if (ctry == "Zambia") {
        # redefine strata 
        data <- lapply(data, function(x) {x$v023 <- paste(x$v024, "-", x$v025); x})
        # locate clusters (2016, 2011, 2006, 1999) in other administrative units (1995)
        # Issue with one of the geo label coded as NULL
        data <- lapply(c("ZM1992DHS", "ZM1996DHS", "ZM2002DHS", "ZM2007DHS", "ZM2014DHS", "ZM2018DHS"), function(x) {
                if (x == "ZM2014DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/ZMGE61FL.shp", sep=""), verbose = FALSE)
                } else if (x == "ZM2018DHS") {
                        loc <- readOGR(paste("./data/gps cluster/", x, "/ZMGE71FL.shp", sep=""), verbose = FALSE)
                } 
                if (!(x %in% c("ZM1992DHS", "ZM1996DHS", "ZM2002DHS", "ZM2007DHS"))) {
                        loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
                        gps <- mapPoints(loc.dat, geo = geo, long = "long", lat = "lat", names = c("REGNAME"))
                        colnames(gps)[4] <- "region"
                        print(sum(is.na(gps$region)))
                        unknown_cluster <- gps$cluster[which(is.na(gps$region))]
                        gps <- gps[gps$cluster %in% unknown_cluster == FALSE, ]
                        data[[x]] <- data[[x]][data[[x]]$v001 %in% unknown_cluster == FALSE, ]
                        data[[x]] <- merge(data[[x]], gps[, c("cluster", "region")], by.x = "v001",
                                           by.y = "cluster", all.x = TRUE)
                        data[[x]]$v024 <- data[[x]]$region
                }
                
                data[[x]]
        })
        # need to rename list item for future functions
        names(data) <- c("ZM1992DHS", "ZM1996DHS", "ZM2002DHS", "ZM2007DHS", "ZM2014DHS", "ZM2018DHS")
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 == c("northwestern"), "north-western", as.character(x$v024)); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                            levels = geo$REGNAME,
                                                            labels = geo$REGNAME); x})
}

# ZIMBABWE
if (ctry == "Zimbabwe") {
        # redefine strata 
        data[["ZW2006DHS"]]$v023 <- paste(data[["ZW2006DHS"]]$v024, "-", data[["ZW2006DHS"]]$v025)
        # upper case letter to coincide with geo
        data <- lapply(data, function(x) {x$v024 <- sapply(as.character(x$v024), CapStr); x})
        
        # harmonize subnational units
        data <- lapply(data, function(x) { x$v024 <- ifelse(x$v024 == c("Harare/chitungwiza"), "Harare", 
                                                            ifelse(x$v024 == c("Matebeleland North"), "Matabeleland North", 
                                                                   ifelse(x$v024 == c("Matebeleland South"), "Matabeleland South", as.character(x$v024)))); x } )
        data <- lapply(data, function(x) { x$v024 <- factor(x$v024,
                                                    levels = geo$REGNAME,
                                                    labels = geo$REGNAME); x})
}
        

# ----- Compute child-month data ---------------------------------------------------------------------------------------------------

last_DHS <- tail(filename1,1) # used in periodbreaks() within childmonth()

# Verify that region names printed have the same order
data_u5 <- childmonth(u5mr = TRUE, data = data, nber.p = 6) 

data_a5 <- childmonth(u5mr = FALSE, data = data, nber.p = 3) 

# For manuscript text: count person-month and deaths
exp_a5 <- sum(unlist(lapply(data_a5, function(x) dim(x)[1])))
exp_u5 <- sum(unlist(lapply(data_u5, function(x) dim(x)[1])))
dth_a5 <- sum(unlist(lapply(data_a5, function(x) sum(x$died))))
dth_u5 <- sum(unlist(lapply(data_u5, function(x) sum(x$died))))
