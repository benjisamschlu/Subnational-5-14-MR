

# FUNCTION: Compute ARR by three periods (<2000, 2000-2010, >=2010)

# Code by: Benjamin-Samuel Schlüter
# Date: 11/3/2020

# Notes:
# 1) 

#-------------------------------------------------------------

arr <- function(data, region) {
        
        df <- data[data$Type == "Space-Time Smoothing"& data$region == region, ]
        df$p.id <- match(df$years, levels(df$years)) # get the periods order
        df <- midperiod(df) # own defined function to get mid period
        df$p.lab <- ifelse(df$p.mid <=2000, "<2000", # defines the label to associate with arr
                           ifelse(df$p.mid > 2000 & df$p.mid <= 2010, "2000-2010",
                                  ">= 2010"))
        
        iteration <- head(as.list(df$p.id), -1) # set the number of iteration -1 (ratio of f(t+1)/f(t))
        ARR <- unlist( lapply(iteration, function(x) { log(df$mean[df$p.id == x+1]/df$mean[df$p.id == x]) /  (df$p.mid[df$p.id == x] - df$p.mid[df$p.id == x+1])} ) ) # compute ARR looping on periods
        label <- unlist (lapply(iteration, function(x) df$p.lab[df$p.id == x+1]) )  # get label associated to ARR (t+1)
        return(data.frame(arr = ARR, period = label))
}
