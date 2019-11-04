
# NOTES:

# 1) The tcut function categorizes person-years according to the specified variable throughout follow-up 
# 2) Improve code to define calendar year and period since first interview
# 3) Improve code to use the by argument more efficiently. Right now, need to type: by = "+ region + typ_res"


# Function: personyear ---------------------------------------------------------------------------------------

personyear <- function(data, by = NULL) {
        
        surv <- Surv(time = data[, "fu_time"], event = data[, "death"], type = "right") # creates survival object
        
        data$startage <- ifelse( data[, "dob"] < data[, "trunc"], as.numeric(data[, "trunc"] - data[, "dob"]),
                                 ifelse( data[, "dob"] >= data[, "trunc"], 0,
                                         NA))
     
        li <- min(data[, "trunc"]) ; ui <- max(data[, "intv"]) # defines period since first interview
        time.cut <- sort( unique( c( seq(li, ui+12, by = 12), seq((li%/%12 + 1)*12 + 1, (ui%/%12)*12 +1, by = 12 )))) # defines period & calendar years since first interview
        year <- tcut(data[, "dob"], time.cut, labels = as.character(time.cut[1:(length(time.cut)-1)])) # defines time strata used in pyears(.)
        year[data[, "dob"] < data[, "trunc"]] <- tcut(data$trunc[data$dob < data$trunc], time.cut, labels = as.character(time.cut[1:(length(time.cut)-1)]))
        
        age_ref = c(0:59, seq(5, 20, 1)*12, 120*12) # defines age strata used in pyears(.)
        age <- tcut(data[, "startage"], age_ref, labels=as.character(age_ref[1:(length(age_ref)-1)]))
        
        f <- paste("surv ~ cluster + year + age", by) # defines formula in pyears(.)
        child_pp <- pyears(as.formula(f), data, weights = w, scale = 12, data.frame=TRUE)
        
        ppfile <- child_pp$data # store pyear data
        ppfile[, c("age", "cmc")] <- apply(ppfile[, c("age", "year")], 2, function(x) as.numeric(as.character(x)))
        ppfile$intv = max(data[, "intv"])
        ppfile$year = trunc((ppfile$cmc-1)/12) + 1900  # Calendar year
        ppfile$elapsed =  trunc(((ppfile$intv[1] -  ppfile$cmc -1)/12)) # Number of completed years between each period and the survey
        
        # checks:
        stopifnot(nrow(data[is.na(data$startage),]) == 0) # every child has a startage ?
        stopifnot(round(sum(data$fu_time*data$w)/12 - sum(child_pp$data$pyears), 0) < 1) # off table = nber of person-years of exposure in the cohort that was not part of any cell in the pyears array
        stopifnot(ppfile$elapsed >= 0) # every line has a positive elapsed time ?
                
        return(ppfile)
}

       


