
# NOTES:

# 1) Create an argument to compute it based on different factors (period, region, ..)
# 2) Argument for period has to be flexible 

# Function: get_nqx ---------------------------------------------------------------------------------------

get_nqx <- function(data, age_i = c(0,5), period = FALSE, back_to = 12, by = 2, lab = c("0-5", "6-12")) {
        
        data <-  data[data[, "age"] >= age_i[1]*12 & data[, "age"] < age_i[2]*12, ] # Keep only children in given age interval
        
        # Raw mortality rates
        # Make the function depends on arguments provided (regions)
        if (period == FALSE) {
                raw_mx = tapply(data[, "event"], list(data[, "age"], data[, "year"]), sum)/tapply(data[, "pyears"], list(data[, "age"], data[, "year"]), sum)
        } else {
                data <- data[data$elapsed < back_to, ]
                data$tips <- cut(data$elapsed, breaks = seq(from = 0, to = 12, by = (12)/by), labels = lab, right = FALSE)
                
                raw_mx = tapply(data[, "event"], list(data[, "age"], data[, "tips"]), sum)/tapply(data[, "pyears"], list(data[, "age"], data[, "tips"]), sum)
                }
        
        n_mx = array(data = 1/12, dim =dim(raw_mx), dimnames = dimnames(raw_mx))
        a_mx = array(data = 0.5/12, dim =dim(raw_mx), dimnames = dimnames(raw_mx))
        # deaths are assumed to be uniformely distributed over the month
        raw_qx = (n_mx*raw_mx)/(1+(n_mx- a_mx)*raw_mx)
        
        if (period == FALSE) {
                raw_5q0b =  as.data.frame(cbind(sort(unique(data$year)), 1-apply(1-raw_qx, c(2), prod))); names(raw_5q0b) = c('Year', "q5b")
                
        } else {
                raw_5q0b =  as.data.frame(cbind(sort(unique(data$tips)), 1-apply(1-raw_qx, c(2), prod))); names(raw_5q0b) = c('tips', "q5b")
                
        }
        
        return(raw_5q0b)
        
}


