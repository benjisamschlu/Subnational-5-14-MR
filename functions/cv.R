

# FUNCTION: Build Coefficient of variation and its associated uncertainty

# Code by: Benjamin-Samuel Schlüter
# Date: 24/7/2020
#--------------------------------------------------------------

cv <- function(data, years, country, age) {
        set.seed(1)
        data$sample <- lapply(data$draws, function(x) sample(x, 2000)) # sample from simulations
        cv <- list()
        # Loop on all years
        for (i in seq_along(years)) {
                sub.df <- data[data$years == years[i], ] # subset a given year
                mat <- do.call(rbind, sub.df$sample) # create a matrix (region*samples)
                mean <- apply(mat, 2, function(x) mean(x)) # compute mean by columns
                SD <- apply(mat, 2, function(x) sd(x)) # compute sd by columns
                CV <- SD/mean # series of cv
                # Create data set
                out <- data.frame(
                        cv = quantile(CV, probs = c(.5))*100,
                        upper = quantile(CV, probs = c(.975))*100, # CI
                        lower = quantile(CV, probs = c(.025))*100, # CI
                        years = years[i],
                        age = age,
                        ctry = country,
                        stringsAsFactors = FALSE
                        
                )
                cv[[years[i]]] <- out
        }
        cv <- do.call("rbind", cv)
        cv$time.id <- match(cv$years, years) # set id for time period to plot later
        return(cv)
}