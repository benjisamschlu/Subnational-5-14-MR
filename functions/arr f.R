

# FUNCTION: Build ARR and its associated uncertainty

# Code by: Benjamin-Samuel Schlüter
# Date: 24/7/2020
#--------------------------------------------------------------

arr <- function(data, years, country, age, region, n) {
        set.seed(1)
        data$sample <- lapply(data$draws, function(x) sample(x, 2000)) # sample from simulations
        data <- midperiod(data) # own defined function to get mid period
        p1 <- unique(data$p.mid[data$years == head(years, 1)]) # first period
        p2 <- unique(data$p.mid[data$years == tail(years, 1)]) # last period
                      
        sub1.df <- data[data$years == head(years, 1), ] # subset a given year
        sub2.df <- data[data$years == tail(years, 1), ]
        mat1 <- do.call(rbind, sub1.df$sample) # create a matrix (region*samples)
        mat2 <- do.call(rbind, sub2.df$sample) # create a matrix (region*samples)
        
        ARR <-  log(mat2/mat1)/(p1-p2) # series of arr
        
        # Create data set
        df.arr <- data.frame(
                arr = unlist(apply(ARR, 1, function(x) quantile(x, probs = c(.5))))*100,
                upper = unlist(apply(ARR, 1, function(x) quantile(x, probs = c(.975))))*100, # CI
                lower = unlist(apply(ARR, 1, function(x) quantile(x, probs = c(.025))))*100, # CI
                age = rep(age, n),
                region = region,
                ctry = rep(country, n)
        )
        return(df.arr)
}
