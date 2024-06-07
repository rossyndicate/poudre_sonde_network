alter_verification_column <- function(verification_choice, df_data) {

  df_name <- paste(unique(df_data$site), unique(df_data$parameter), head(df_data$DT_round, 1))

  # PASS/FAIL statements ----
  # these cases are inverses of each other, both are included
  # in the same if statements to reduce confusion

  # PASS ALL/FAIL NONE
  if (verification_choice %in% c("PASS ALL", "FAIL NONE")) {

    altered_df <- df_data %>%
      mutate(mean_verified = if_else(is.na(flag), mean, NA),
             is_verified = TRUE,
             verification_status = "PASS")

    cat("All points for ", df_name, "have PASSED and are accurate information.\n")

    return(altered_df)

  }

  # PASS VALID/FAIL FLAGGED
  if (verification_choice %in% c("PASS VALID", "FAIL FLAGGED")) {

    altered_df <- df_data %>%
      mutate(mean_verified = mean,
             is_verified = TRUE,
             verification_status = if_else(is.na(flag), "PASS", "FAIL")) # pass valid data, fail flagged data

    cat("All valid points for ", df_name, "have PASSED\n")
    cat("All flagged points for ", df_name, "have FAILED\n")

    return(altered_df)

  }

  # PASS FLAGGED, FAIL VALID
  # if all the flagged data is correct, and none of the valid data is correct,
  # none of it should be shown
  if (verification_choice %in% c("PASS FLAGGED", "FAIL VALID")) {

    altered_df <- df_data %>%
      mutate(mean_verified = NA,
             is_verified = TRUE,
             verification_status = if_else(!is.na(flag), "PASS", "FAIL"))

    cat("All valid points for ", df_name, "have FAILED\n")
    cat("All flagged points for ", df_name, "have PASSED.\n")

    return(altered_df)

  }

  # PASS NONE, FAIL ALL
  # This means that flagged data would actually be accurate data, and valid data would
  # actually be inaccurate data. this is the inverse of pass all
  if (verification_choice %in% c("PASS NONE", "FAIL ALL")) {

    altered_df <- df_data %>%
      mutate(mean_verified = if_else(!is.na(flag), mean, NA),
             is_verified = TRUE,
             verification_status = "FAIL")

    cat("All points for ", df_name, "have FAILED and are inaccurate information.\n")

    return(altered_df)

  }

  # skip statements
  if (verification_choice == "SKIP") {

    altered_df <- df_data %>%
      mutate(mean_verified = if_else(is.na(flag), mean, NA),
             is_verified = TRUE, #false?
             verification_status = "SKIP")

    cat("The accuracy of all points for ", df_name, "cannot be determined as\n")
    cat("accurate or inaccurate information at this time.")

    return(altered_df)

  }

  # inspect statements
  if (verification_choice %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")) {
    stop("You can't inspect here")
  }

  # quit statements ***
  if (verification_choice == "QUIT") {

    cat("Quitting out of verification mode...")
    cat("\n")

    QUIT <<- TRUE

    return(NULL)

  }

}

