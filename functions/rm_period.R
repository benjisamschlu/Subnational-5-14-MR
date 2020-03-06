

# FUNCTION: Remove periods of a DHS where the DHS provides <= specified amout of info (1 year)

# Code by: Benjamin-Samuel Schlüter
# Date: 5/3/2020
#---------------------------------------------------------------------------------------------

# rm_period <- function(data.multi, amount.info = 1){
#         
#         upper_date <- lapply(data, function(x) {1900 + range(x$v008)[2]/12}) # upper date DHS
#         for (j in seq_along(filename1)) {
#                 
#                 time_lapse <- upper_date[[ filename1[j] ]] - head(periodbreaks(data), -1) # time between lower bound period and upper date DHS
#                 
#                 for (i in seq_along(time_lapse)) {
#                         
#                         if(time_lapse[i] >= 0 & time_lapse[i] <= amount.info) { # if <= amount.info, replace estimates by NA 
#                                 
#                                 df <- data.multi
#                                 df[df$surveyYears == filename1[j] & df$years ==years[i], c("mean", "lower", "upper", "logit.est", "var.est", "logit.prec")] <- NA
#                         }
#                 }
#         }
#         df
#         
# }


rm_period <- function(data.multi, amount.info = 1){
        
        upper_date <- lapply(data, function(x) {1900 + range(x$v008)[2]/12}) # upper date DHS
        time_lapse <- lapply(upper_date, function(x) {x - head(periodbreaks(data), -1)})
        dist <- lapply(time_lapse, function(x) {x >= 0 & x <= amount.info})
        drop <- lapply(dist, function(x) {which(x)})
        
        for (i in seq_along(drop)) {
                if(length(drop[[i]]) == 1) {
                        df <- data.multi
                        df[df$surveyYears == names(drop)[i] & df$years ==years[ drop[[i]] ], c("mean", "lower", "upper", "logit.est", "var.est", "logit.prec")] <- NA
                }
        }
        df
        
}



