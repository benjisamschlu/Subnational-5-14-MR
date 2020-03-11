

# FUNCTION: Estimate the number of children in a region 

# Code by: Benjamin-Samuel Schlüter
# Date: 9/3/2020

# Notes:
# 1) NAME OF REGION WILL BE DIFFERENT WITHIN DATA SETS --> function to make first letterS big

#-------------------------------------------------------------

prop.children_by_region <- function(data, breaks, index, survey.id, length.p = 4, u5 = TRUE) { # improve if feasible
        
        # print("Verify that regions are in the same order as REGNAME from geo:")
        # print(levels(data[, "region"]))
       
        if (u5 == TRUE) {
                data %>%
                        filter( ((alive == "yes" | alive == "Yes") & -length.p <= (breaks[index]-dob) & (breaks[index]-dob) < 5) | # alive & born 5 years before survey to upper date of period
                                        ((alive == "no" | alive == "No") & -length.p <= (breaks[index]-dob) & (breaks[index]-dob) < 5 & (dob + age >= breaks[index])) ) %>% # dead & born 5 years before survey to upper date of period & death occurs after lower date of period
                        group_by(region) %>%
                        summarise(prop = sum(weights)) %>%
                        mutate(prop = prop/sum(prop)*100,
                               period = paste(breaks[index], as.numeric(breaks[index+1])-1, sep = "-"), # get the period
                               survey = survey.id, # get the name of the survey used
                               region = factor(region, labels = geo$REGNAME)) # set the region names as Amat of last DHS
                
        }
        else {
                data %>%
                        filter( ((alive == "yes" | alive == "Yes") & (5-length.p) <= (breaks[index]-dob) & (breaks[index]-dob) < 15) | # alive & born 15 years before survey to (5 - length.p) before lower date of period
                                        ((alive == "no" | alive == "No") & (5-length.p) <= (breaks[index]-dob) & (breaks[index]-dob) < 15 & (dob + age >= breaks[index]) & age >= 5) ) %>% # dead & born 15 years before survey to (5 - length.p) before lower date of period & death occurs after lower date of period & age >= 5
                        group_by(region) %>%
                        summarise(prop = sum(weights)) %>%
                        mutate(prop = prop/sum(prop)*100,
                               period = paste(breaks[index], as.numeric(breaks[index+1])-1, sep = "-"), # get the period
                               survey = survey.id, # get the name of the survey used
                               region = factor(region, labels = geo$REGNAME)) # set the region names as Amat of last DHS
                
        }
        
}





# Code before creating the condition to compute eiter <5 or 5-15

# prop.children_by_region <- function(data, breaks, index, survey.id) { # improve if feasible
# 
#         data[data$dob >= breaks[index] & data$dob < breaks[index+1], ] %>%
#                 group_by(region) %>%
#                 summarise(prop = sum(weights)) %>%
#                 mutate(prop = prop/sum(prop)*100,
#                        period = paste(breaks[index], as.numeric(breaks[index+1])-1, sep = "-"),
#                        survey = survey.id)
# 
# }





# Verify that the function gives the same output

# test <- data[[2]][, c("b3", "v005", "v024", "v008")]
# names(test) <- c("dob_cmc", "weights", "region", "intw")
# test$dob <- 1900 + test$dob_cmc/12
# test$weights <- test$weights/1000000
# test2 <- test[test$dob>=2003 & test$dob<2007, ] %>%
#         group_by(region) %>%
#         summarise(prop = sum(weights)) %>%
#         mutate(prop = prop/sum(prop)*100,
#                period = "15-18",
#                survey = "DHS2019")
# test2
