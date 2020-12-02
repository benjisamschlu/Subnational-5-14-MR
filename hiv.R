########################################################################################################################################################
# PROJECT: Comparison of 10q5 to 5q0 at a subnational scale

# Aim of code: HIV correction

# Code constructed by: Benjamin-Samuel Schlüter
# Date of last revision: 9-10-2020

# Notes:
# 1) 

########################################################################################################################################################


#--- Load packages ----------------------------------------------------------------------------------------------------------------------

packages <- c("SUMMER", "ggplot2", "gridExtra", "rdhs", "rgdal", "readstata13", "dplyr", "demogsurv", "sp", "openxlsx")
invisible( lapply(packages, library, character.only = TRUE) )


# SOME OBJECT CREATED AT THE BEGINNING OF PREPARE DATA.R

# Added to make code work
data <- data.multi_u5[,c("region","years","mean","lower","upper", # ADDED
                "logit.est","var.est","region_num","survey", "logit.prec")]
names(data)[3] <- "u5m"
years.all <- years

info <- read.csv(paste0("./richardli_github/AfricaU5MR-master/CountryMain/CountryInfo/", ctry, ".csv"))
filenames <- as.character(info[, 1])[which(as.character(info[, 1]) != "")]
surveylabels <- as.character(info[, 2])[which(as.character(info[, 2]) != "")]


# input: 5q0, variance of logit, and adjustment factor c
# output: 5q0, logit, variance of logit, and prec (adjusted)
adjust <- function(p, v, c){
        f.prime <- 1 - (c - 1) * (p/(1-p)) / (c + (c-1) * (p/(1-p)))
        p <- p / c
        v <- v * f.prime^2
        return(c(p, log(p/(1-p)), v, 1/v))
}
# input: current year, per5 label, adjustment file
# output: adjustment factor c
adj_lookup <- function(y, per, adj){
        per <- as.character(per) 
        
        
        if (ctry %in% c("Malawi", "Zimbabwe")) { # CHANGE BECAUSE I HAVE MULTIPLE TIME PERIODS
                labels <- c("84-87", "88-91", "92-95", "96-99", "00-03", "04-07", "08-11", "12-15")
                t0 <- 1984 # ADDED FOR "start <-... " BELOW
                t1 <- 2012
                        
        }
        else if (ctry %in% c("Namibia")) {
                labels <- c("90-93", "94-97", "98-01", "02-05", "06-09", "10-13", "14-17")
                t0 <- 1990
                t1 <- 2014
        }
        else if (ctry %in% c("Kenya", "Rwanda")) {
                labels <- c("91-94", "95-98", "99-02", "03-06", "07-10", "11-14", "15-18")
                t0 <- 1991
                t1 <- 2015
        }
        
        
        
        if(!per %in% labels) stop("----- File contains unhandled per5! ----\n")
        start <- seq(t0, t1, by = 4)[which(labels == per)]
        diff <- as.numeric(y) - start
        # diff the survey-year - start of per5
        # e.g., estimator of 80-84 in a 1998 survey: diff = 18 
        #		estimator of 95-99 in a 1998 survey: diff = 3
        # In the later case, we need HIV adjustment factor for 0-3, 
        #      instead, for now we use just 0-4 for diff < 4
        #      todo: can/should we be more precise for this?
        if(diff > 4){
                period <- paste0(diff - 4, "-", diff)
        }else if(diff >= 0){
                period <- "0-4"
        }else{
                stop("Something wrong with the survey year\n")
        }
        out <- adj[which(adj[, 1] == period), 2]
        if(length(out) == 0) out <- NA
        return(out)
}
# input: data, adjustment matrix, survey year
# output: new data
adjust.meta <- function(d, adj, year){
        for(i in 1:dim(d)[1]){
                if(!is.na(d$u5m[i])){
                        cc <- adj_lookup(year, d[i, "years"], adj)
                        # if adjustment factor is NA, don't do anything
                        if(!is.na(cc)){
                                new <- adjust(d$u5m[i], d$var.est[i], cc)
                                d[i, "u5m"] <- new[1]
                                d[i, "logit.est"] <- new[2]
                                d[i, "var.est"] <- new[3]
                                d[i, "logit.prec"] <- new[4]
                                d[i, "lower"] <- new[2] + qnorm(0.025)*sqrt(new[3])
                                d[i, "upper"] <- new[2] + qnorm(0.975)*sqrt(new[3])				
                        }
                }
        }
        return(d)
}

country.hiv <- c("Malawi", "Rwanda", "Namibia")
region.hiv <- c("Zimbabwe", "Kenya")
if(ctry %in% country.hiv){
        data.nohiv <- data
        hivfiles <- list.files("./data/HIV_Adjustments") # CHANGED FILENAME
        hivfiles <- hivfiles[grep(ctry, hivfiles)]
        if(length(hivfiles) != 0){
                post <- gsub("[^0-9]", "", surveylabels)
                filepost <- gsub("[^0-9]", "", hivfiles)
                ## for every survey year we have
                for(year in post){
                        ## if there's HIV adjustment files
                        if(year %in% filepost){
                                file <- paste0("./data/HIV_Adjustments/", hivfiles[which(filepost == year)])
                                if(grepl(".xlsx", file)){
                                        adj <- read.xlsx(file, 1)
                                }else{
                                        adj <- read.csv(file)	
                                }
                                # get original data of this year
                                which.sub <- which(data$survey == grep(year, surveylabels))
                                sub <- data[which.sub, ]
                                sub.new <- adjust.meta(sub, adj, year)
                                data[which.sub, ] <- sub.new
                        }
                }
        }
}else if(ctry %in% region.hiv){
        ## to-be-updated
        data.nohiv <- data
        hivfiles <- list.files(paste0("./data/HIV_Adjustments/", ctry)) # CHANGED FILENAME
        if(length(hivfiles) > 0){
                filepost <-  gsub(paste0(ctry, "_"), "",  hivfiles)
                yearpost <-  gsub("[^0-9]", "",  filepost)
                regionpost <-  gsub("[0-9]", "",  filepost)
                regionpost <-  tolower(gsub("_.csv", "",  regionpost))
                # actual year and region
                post1 <- gsub("[^0-9]", "", surveylabels)
                post1 <- substring(post1, 1, 4) # remove multi-year labels
                post2 <- tolower(gsub(" ", "", unique(data$region[data$region != "All"])))
                # create a new reformatted region names in data file to be consistent
                regions_tolower <- tolower(data$region)
                regions_tolower <- gsub(" ", "", regions_tolower)
                
                for(year in post1){
                        for(region in post2){
                                select <- intersect(which(yearpost == year), which(regionpost == region))
                                if(length(select) > 0){
                                        file <- paste0("./data/HIV_Adjustments/", ctry, "/", hivfiles[select])
                                        if(grepl(".xlsx", file)){
                                                adj <- read.xlsx(file, 1)
                                        }else{
                                                adj <- read.csv(file)	
                                        }
                                        # todo: finish here
                                        which.sub <- intersect(which(data$survey == grep(year, surveylabels)), which(regions_tolower == region))
                                        sub <- data[which.sub, ]
                                        sub.new <- adjust.meta(sub, adj, year)
                                        data[which.sub, ] <- sub.new
                                        
                                } 
                        }
                }
        }
        
}else{
        data.nohiv <- data
}

# Zimbabwe has both national and regional HIV adjustments
if(ctry %in% c("Zimbabwe")){
        hivfiles <- list.files("./data/HIV_Adjustments") # CHANGED FILENAME
        hivfiles <- hivfiles[grep(ctry, hivfiles)]
        if(length(hivfiles) != 0){
                post <- gsub("[^0-9]", "", surveylabels)
                filepost <- gsub("[^0-9]", "", hivfiles)
                ## for every survey year we have
                for(year in post){
                        ## if there's HIV adjustment files
                        if(year %in% filepost){
                                file <- paste0("./data/HIV_Adjustments/", hivfiles[which(filepost == year)])
                                if(grepl(".xlsx", file)){
                                        adj <- read.xlsx(file, 1)
                                }else{
                                        adj <- read.csv(file)	
                                }
                                # get original data of this year
                                which.sub <- intersect(which(data$survey == grep(year, surveylabels)), which(data$region == "All"))
                                sub <- data[which.sub, ]
                                sub.new <- adjust.meta(sub, adj, year)
                                data[which.sub, ] <- sub.new
                        }
                }
        }
}

data$u5m.nohiv <-  data.nohiv$u5m
data$lower.nohiv <-  data.nohiv$lower
data$upper.nohiv <-  data.nohiv$upper
data$logit.est.nohiv <-  data.nohiv$logit.est
data$var.est.nohiv <-  data.nohiv$var.est
data$logit.prec.nohiv <-  data.nohiv$logit.prec


# add NA rows for incomplete data
regions_in_data <- as.character(unique(data$region))
add.region <- add.year <- add.survey <- NULL
for(year in years.all){
        data.sub <- data[which(data$years == year), ]
        for(survey in 1:length(filenames)){
                data.sub.sub <- data.sub[which(data.sub$survey == survey), ]
                for(region in regions_in_data){
                        if(region %in% data.sub.sub$region == FALSE){
                                add.region <- c(add.region, region)
                                add.year <- c(add.year, year)
                                add.survey <- c(add.survey, survey)
                                cat(paste("Impute NA for missing region:", region, year, "survey", survey, "\n"))
                        }
                }	
        }
}


if(length(add.region) > 0){
        add <- data.frame(region = add.region, 
                          years = add.year,
                          u5m = NA, 
                          lower = NA, 
                          upper = NA,
                          logit.est = NA, 
                          var.est = NA, 
                          region_num = NA, 
                          survey = add.survey,
                          logit.prec = NA,
                          u5m.nohiv = NA, 
                          lower.nohiv = NA, 
                          upper.nohiv = NA, 
                          logit.est.nohiv = NA, 
                          var.est.nohiv = NA, 
                          logit.prec.nohiv = NA)
        data <- rbind(data, add)
}

# remove extra years in data if any
fulldata <- data[which(data$year %in% years.all), ]

##---------------------------------------------------##
# Get aggregated version of estimates
data0 <- fulldata
data0.national <- fulldata
data0 <- data0[data0$region != "All", ]
time_region <- unique(data0[, c(1, 2)])
data <- data.frame(region = time_region$region, years = time_region$years, u5m = NA, lower=NA, upper=NA, logit.est=NA, var.est=NA, region_num = NA, survey = NA, logit.prec = NA,  u5m.nohiv = NA, lower.nohiv = NA, upper.nohiv = NA, logit.est.nohiv = NA, var.est.nohiv = NA,logit.prec.nohiv = NA)
expit<-function(x){
        exp(x)/(1+exp(x))
}
for(i in 1:dim(data)[1]){
        tmp <- intersect(which(data0$region == data$region[i]), 
                         which(data0$years == data$years[i]))
        # Version adjusting for HIV
        data[i, "logit.prec"] <- sum(data0[tmp, "logit.prec"], na.rm = TRUE)
        if(data[i, "logit.prec"] == 0){
                data[i, "var.est"] <- NA
                data[i, "logit.prec"] <- NA
        }else{
                data[i, "var.est"] <- 1 / data[i, "logit.prec"]
                weights <- data0[tmp, "logit.prec"] / data[i, "logit.prec"]
                data[i, "logit.est"] <- sum(weights * data0[tmp, "logit.est"], na.rm = TRUE)
                data[i, "u5m"] <- expit(data[i, "logit.est"])
                
                data[i, "lower"] <- expit(data[i, "logit.est"] + qnorm(0.975)*sqrt(data[i, "var.est"]))
                data[i, "upper"] <- expit(data[i, "logit.est"] + qnorm(0.025)*sqrt(data[i, "var.est"]))
        }
        data[i, "region_num"] <- data0[tmp, "region_num"][1]
        
        
        # Version not adjusting for HIV
        data[i, "logit.prec.nohiv"] <- sum(data0[tmp, "logit.prec.nohiv"], na.rm = TRUE)
        if(data[i, "logit.prec.nohiv"] == 0){
                data[i, "var.est.nohiv"] <- NA
                data[i, "logit.prec.nohiv"] <- NA
        }else{
                data[i, "var.est.nohiv"] <- 1 / data[i, "logit.prec.nohiv"]
                weights <- data0[tmp, "logit.prec.nohiv"] / data[i, "logit.prec.nohiv"]
                data[i, "logit.est.nohiv"] <- sum(weights * data0[tmp, "logit.est.nohiv"], na.rm = TRUE)
                data[i, "u5m.nohiv"] <- expit(data[i, "logit.est.nohiv"])
                
                data[i, "lower.nohiv"] <- expit(data[i, "logit.est.nohiv"] + qnorm(0.975)*sqrt(data[i, "var.est.nohiv"]))
                data[i, "upper.nohiv"] <- expit(data[i, "logit.est.nohiv"] + qnorm(0.025)*sqrt(data[i, "var.est.nohiv"]))
        }
}




data0.national <- data0.national[data0.national$region == "All", ]
time_region <- unique(data0.national[, c(1, 2)])
data.national <- data.frame(region = time_region$region, years = time_region$years, u5m = NA, lower=NA, upper=NA, logit.est=NA, var.est=NA, region_num = NA, survey = NA, logit.prec = NA)

for(i in 1:dim(data.national)[1]){
        tmp <- intersect(which(data0.national$region == fulldata$region[i]), 
                         which(data0.national$years == fulldata$years[i]))
        # version adjusting for HIV
        data.national[i, "logit.prec"] <- sum(data0.national[tmp, "logit.prec"], na.rm = TRUE)
        if(data.national[i, "logit.prec"] == 0){
                data.national[i, "var.est"] <- NA
                data.national[i, "logit.prec"] <- NA
        }else{
                data.national[i, "var.est"] <- 1 / data.national[i, "logit.prec"]
                weights <- data0.national[tmp, "logit.prec"] / data.national[i, "logit.prec"]
                data.national[i, "logit.est"] <- sum(weights * data0.national[tmp, "logit.est"], na.rm = TRUE)
                data.national[i, "u5m"] <- expit(data.national[i, "logit.est"])
                
                data.national[i, "lower"] <- expit(data.national[i, "logit.est"] + qnorm(0.975)*sqrt(data.national[i, "var.est"]))
                data.national[i, "upper"] <- expit(data.national[i, "logit.est"] + qnorm(0.025)*sqrt(data.national[i, "var.est"]))
        }
        data.national[i, "region_num"] <- data0.national[tmp, "region_num"][1]
        
        # version not adjusting for HIV
        data.national[i, "logit.prec.nohiv"] <- sum(data0.national[tmp, "logit.prec.nohiv"], na.rm = TRUE)
        if(data.national[i, "logit.prec.nohiv"] == 0){
                data.national[i, "var.est.nohiv"] <- NA
                data.national[i, "logit.prec.nohiv"] <- NA
        }else{
                data.national[i, "var.est.nohiv"] <- 1 / data.national[i, "logit.prec.nohiv"]
                weights <- data0.national[tmp, "logit.prec.nohiv"] / data.national[i, "logit.prec.nohiv"]
                data.national[i, "logit.est.nohiv"] <- sum(weights * data0.national[tmp, "logit.est.nohiv"], na.rm = TRUE)
                data.national[i, "u5m.nohiv"] <- expit(data.national[i, "logit.est.nohiv"])
                
                data.national[i, "lower.nohiv"] <- expit(data.national[i, "logit.est.nohiv"] + qnorm(0.975)*sqrt(data.national[i, "var.est.nohiv"]))
                data.national[i, "upper.nohiv"] <- expit(data.national[i, "logit.est.nohiv"] + qnorm(0.025)*sqrt(data.national[i, "var.est.nohiv"]))
        }	
}








