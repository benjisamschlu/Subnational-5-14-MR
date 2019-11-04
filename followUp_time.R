
# NOTES:

# 1) 


# Function: follow-up time ------------------------------------------------------------------------------------------------------------------------------------------------------------

followUp_time <- function(data, recall_p = 25) {
        
        data$trunc <- data$intv - (recall_p*12)
        data <- data[ !(data$death == TRUE & data$dod < data$trunc), ] # drop death before date of left truncation (children born after first interview date droped in structure_df(.))
        
        # 4 possible scenari for follow-up time:
        data$fu_time <- ifelse( data$death == FALSE & data$dob <= data$trunc, recall_p*12, # alive and birth before date of left trunc.
                                ifelse( data$death == FALSE & data$dob > data$trunc, data$intv - data$dob, # alive and birth after date of left trunc.
                                        ifelse( data$death == TRUE & data$dob <= data$trunc, data$dod - data$trunc, # death and birth before date of left trunc.
                                                ifelse( data$death == TRUE & data$dob > data$trunc, data$dod - data$dob, # death and birth after date of left trunc.
                                                        NA))))
        
        # checks:
        stopifnot(nrow(data[is.na(data$fu_time), ]) == 0) # every child has a follow-up time ?
        
        return(data)
}


