

# FUNCTION: Create data sets from estimates for plotting purpose

# Code by: Benjamin-Samuel Schlüter
# Date: 5/3/2020
#--------------------------------------------------------------

create_df <- function(meta.est, st.est_nat, st.est_subnat) {
        
        df <- data.frame(region = meta.est$region,
                         years = factor(meta.est$years, levels = years),
                         mean = meta.est$mean*1000,
                         lower = meta.est$lower*1000,
                         upper = meta.est$upper*1000,
                         logit.median = NA,
                         logit.lower = NA,
                         logit.upper = NA,
                         Type = "Horvitz-Thompson")
        
        df <- rbind(df, data.frame(region = st.est_nat$region,
                                   years = st.est_nat$years,
                                   mean = st.est_nat$median*1000,
                                   lower = st.est_nat$lower*1000,
                                   upper = st.est_nat$upper*1000,
                                   logit.median = st.est_nat$logit.median,
                                   logit.lower = st.est_nat$logit.lower,
                                   logit.upper = st.est_nat$logit.upper,
                                   Type = "Space-Time Smoothing"))
        
        df <- rbind(df, data.frame(region = st.est_subnat$region,
                                   years = st.est_subnat$years,
                                   mean = st.est_subnat$median*1000,
                                   lower = st.est_subnat$lower*1000,
                                   upper = st.est_subnat$upper*1000,
                                   logit.median = st.est_subnat$logit.median,
                                   logit.lower = st.est_subnat$logit.lower,
                                   logit.upper = st.est_subnat$logit.upper,
                                   Type = "Space-Time Smoothing"))
        df
}
