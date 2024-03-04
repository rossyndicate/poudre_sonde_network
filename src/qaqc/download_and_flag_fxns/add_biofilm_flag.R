add_biofilm_flag <- function(df){

  # We only look for drift due to biofilm growth on turbidity sensors...

  if(unique(df$parameter == "Turbidity")){

    # Define a function to check if a given 3-day window's worth of data has a linear
    # relationship with time
    progressive_drift <- function(x) {
      # Only less than 90% missing data:
      if(length(x[!is.na(x)]) > (length(x) - (length(x)*0.1))){

        # Fit linear model
        model <- lm(x ~ c(1:length(x)), na.action = na.omit)

        # Extract R-squared value
        r_squared <- summary(model)$r.squared

        # Return R-squared value
        return(r_squared)

      } else {

        no_slope <- 0

        return(no_slope)

      }
    }

    drift_tested <- df %>%
      data.table::data.table() %>%
      #bind_rows(flagged_data_dfs[["prospect-Turbidity"]]) %>%
      #filter(as_date(DT_round) >= "2022-10-11" & as_date(DT_round) <= "2022-10-15") %>%
      #mutate(r_squared1 = data.table::frollapply(mean, n = 96, FUN = progressive_drift, align = "left", fill = NA)) %>%
      mutate(r_squared2 = data.table::frollapply(mean, n = 96, FUN = progressive_drift, align = "right", fill = NA)) %>%
      #mutate(r_squared3 = data.table::frollapply(mean, n = 96, FUN = progressive_drift, align = "center", fill = NA)) %>%
      #mutate(r_squared4 = data.table::frollapply(mean, n = 288, FUN = progressive_drift, align = "left", fill = NA)) %>%
      mutate(r_squared5 = data.table::frollapply(mean, n = 288, FUN = progressive_drift, align = "right", fill = NA)) %>%
      #mutate(r_squared6 = data.table::frollapply(mean, n = 288, FUN = progressive_drift, align = "center", fill = NA)) %>%
      # r-squared can only be in the positive direction for biofilm drift:
      add_flag((#r_squared1 >= 0.7 |
                  r_squared2 >= 0.7 |
                  #r_squared3 >= 0.7 |
                #r_squared4 >= 0.8 |
                  r_squared5 >= 0.8 #|
                  #r_squared6 >= 0.8
                )  & !grepl("biofilm", flag), "biofilm") #%>%
      #select(-c(r_squared1, r_squared2, r_squared3)

    return(drift_tested)

  } else { return(df) }
  }



