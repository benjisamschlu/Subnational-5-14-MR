
# DHS TO CHILD-MONTH BY COUNTRY FUNCTION
########################################

# Notes: This functions take all DHS survey available in the folder data/fbh for a given country
#        and store them in a list. Then, all data sets are converted into child-month format. 
#        u5mr argument allows to do that for 5q0 or 10q5

dhs_to_childmonth <- function(ctry.id, u5mr = TRUE, child_month = TRUE) {
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
        
        if (child_month == TRUE) { # if false, only store the dhs data without transforming into child-month
                
                for (i in seq_along(data)) { # convert to child-month each data set of the list
                        
                        srvyear <- 1900 + floor(range(data[[i]]$v008)[2]/12) # get date of last interview
                        upper_y = srvyear + 1 # defines highest year for year.cut
                        lower_y = upper_y - 3*5 # defines lowest year for year.cut
                        
                        if (u5mr == TRUE) {m_cut <- c(1, 12, 24, 36, 48, 60)}
                        else {m_cut <- seq(60, 180, by = 12)}
                        
                        data[[i]] <- getBirths(data = data[[i]], surveyyear = srvyear, strata = c("v023"), dob = "b3",
                                               alive = "b5", age = "b7", date.interview = "v008", 
                                               variables = c("v001", "v002", "v004", "v005", "v021", "v022", "v023", "v024", "v025", "v139"), #v139 are regions
                                               month.cut = m_cut, 
                                               year.cut = seq(lower_y, upper_y, by = 5))
                        if (u5mr == FALSE) {
                                data[[i]] <- data[[i]][data[[i]]$age != "0-59", ] 
                                data[[i]][, "age"] <- droplevels( data[[i]][, "age"])
                        }
                }
                
        }
        names(data) <- dhs
        data
}
