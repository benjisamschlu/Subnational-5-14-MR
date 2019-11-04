
# NOTES:

# 1) Issue with attributes for region: SOLVED --> names()
# 2) Rounding of random number generated for dates ? --> does not really play a role
# 3) Replication for generation of random numbers (set.seed()) ?
# 4) When all region are not present, it creates issues: SOLVED --> with [unique()] 


# Function: structure data frame ----------------------------------------------------------------------------------------------------------------------------

structure_df <- function(data) {
        
        df <- data.frame(death = data[["b5"]] == 0,
                         dob = data[["b3"]] + runif(nrow(data), min = 0, max = 1 ), # add a random date within the month depending on age at death
                         unit_age = as.numeric(substr(data[["b6"]], 1, 1)), # extract unit of age at death
                         d_age = as.numeric(substr(data[["b6"]], 2, 3)), # extract age at death in a given unit (1= day, 2= month, 3= year)
                         intv = min(data[["v008"]]), # define right truncation = first interview date
                         cluster = data[["v001"]],
                         w = data[["v005"]]/1000000,
                         region = factor(data[["v101"]], labels = names(attr(data[["v101"]], "labels")[unique(data$v101)+1])),
                         type_res = factor(data[["v025"]], labels = names(attr(data[["v025"]], "labels"))),
                         random = runif(nrow(data), min = 0, max = 1) # will be used for imputation of age at death (aad)
                         )
        
        df <- df[!(!is.na(df$unit_age) & df$unit_age >3), ]  # drop incorrect unit of age
        
        df$aad <- ifelse(df$death == TRUE & df$unit_age == 1 & df$d_age == 0, df$random, # sometimes 0 --> set a random day in the first month
                         ifelse( df$death == TRUE & df$unit_age == 1 & df$d_age != 0, df$d_age/31,
                                 ifelse( df$death == TRUE & df$unit_age == 2, df$d_age + df$random, # add df$random to set a time within the month
                                         ifelse( df$death == TRUE & df$unit_age == 3, (df$d_age + df$random)*12,
                                                 NA)))) # allow to check that none aad do not end up in one of these conditions
        df$dod <- df$dob + df$aad
        
        df$death[!is.na(df$dod) & df$dod > df$intv] <- FALSE # bring to live children dead after first interview
        df$dod[!is.na(df$dod) & df$dod > df$intv] <- NA
        
        df <- df[df$dob < df$intv, ] # drop children born after first interview date (min(v008))
        
        # checks:
        stopifnot(nrow(df[df$death == TRUE & is.na(df$dod), ]) == 0) # every dead child has a dod ?
        
        return(df)
}







