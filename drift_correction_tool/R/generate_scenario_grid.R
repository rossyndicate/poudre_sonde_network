#' Generate Corrected Drift Scenarios Grid
#'
#' @description
#' Generates and processes all possible correction strategy and formulation variations
#' for a given set of data and drift windows.
#'
#' @param df Dataframe. The raw sensor data containing \code{DT_round} and \code{mean_analysis} columns
#' @param windows_df Dataframe. Contains user-defined drift window boundaries.
#' @param prepped_data List. Named list containing datasets across alternative site-parameter variations.
#' @param site_order Vector. Defines the geographical/hierarchical sequence used for reference regressions.
#'
#' @details
#' \itemize{
#'   \item Establishes an \code{empty_schema} layout by creating blank placeholder columns (\code{NA_real_}) for all mathematical correction scenarios.
#'   \item Checks for an empty or entirely \code{"non_resolved"} window profile; if found, it early-returns the placeholder layout.
#'   \item Simultaneously builds 8 distinct scenario combinations spanning 4 correction strategies (\emph{linear, exponential, uniform, fitted_linear}) multiplied by 2 formulation variations (\emph{additive, multiplicative}).
#'   \item Invokes backend functions (\code{linear_correction_fxn} and \code{exponential_correction_fxn}) to process each variation, extracting unique calculated outputs via left-joins back onto the master timestamps.
#' }
#'
#' @return A comprehensive wide-format dataframe expanding the original \code{df} with 8 additional numeric columns tracking every calculated correction pathway scenario.
#'
#' @seealso [linear_correction_fxn()], [exponential_correction_fxn()]
generate_scenario_grid <- function(df, windows_df, prepped_data, site_order) {
  # Base condition returns empty schema
  empty_schema <- df %>% mutate(
    linear_add = NA_real_, linear_mult = NA_real_,
    exp_add    = NA_real_, exp_mult    = NA_real_,
    uniform_add = NA_real_, uniform_mult = NA_real_,
    fitted_add = NA_real_, fitted_mult = NA_real_
  )

  if (nrow(windows_df) == 0) return(empty_schema)

  calc_windows <- windows_df %>% filter(arg_drift_type != "non_resolved")
  if (nrow(calc_windows) == 0) return(empty_schema)


  # Force strategy variations AND formulation variations explicitly
  linear_add_dec    <- calc_windows %>% mutate(arg_drift_type = "linear", arg_correction_type = "additive")
  linear_mult_dec   <- calc_windows %>% mutate(arg_drift_type = "linear", arg_correction_type = "multiplicative")

  exp_add_dec       <- calc_windows %>% mutate(arg_drift_type = "exponential", arg_correction_type = "additive")
  exp_mult_dec      <- calc_windows %>% mutate(arg_drift_type = "exponential", arg_correction_type = "multiplicative")

  uniform_add_dec   <- calc_windows %>% mutate(arg_drift_type = "uniform", arg_correction_type = "additive")
  uniform_mult_dec  <- calc_windows %>% mutate(arg_drift_type = "uniform", arg_correction_type = "multiplicative")

  fitted_add_dec    <- calc_windows %>% mutate(arg_drift_type = "fitted_linear", arg_correction_type = "additive")
  fitted_mult_dec   <- calc_windows %>% mutate(arg_drift_type = "fitted_linear", arg_correction_type = "multiplicative")

  df_working <- df

  # Run functions and select columns separately
  linear_add_j <- linear_correction_fxn(decision_df_arg = linear_add_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, linear_add = mean_drift_trans)

  linear_mult_j <- linear_correction_fxn(decision_df_arg = linear_mult_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, linear_mult = mean_drift_trans)

  exp_add_j <- exponential_correction_fxn(decision_df_arg = exp_add_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, exp_add = mean_drift_trans)

  exp_mult_j <- exponential_correction_fxn(decision_df_arg = exp_mult_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, exp_mult = mean_drift_trans)

  uniform_add_j <- linear_correction_fxn(decision_df_arg = uniform_add_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, uniform_add = mean_drift_trans)

  uniform_mult_j <- linear_correction_fxn(decision_df_arg = uniform_mult_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, uniform_mult = mean_drift_trans)

  fitted_add_j <- linear_correction_fxn(decision_df_arg = fitted_add_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, fitted_add = mean_drift_trans)

  fitted_mult_j <- linear_correction_fxn(decision_df_arg = fitted_mult_dec, sensor_df_arg = df_working, data_list =  prepped_data, site_order_template = site_order) %>%
    distinct(DT_round, .keep_all = TRUE) %>% select(DT_round, fitted_mult = mean_drift_trans, pre_post_source)

  # Combined Left Joins
  df_working <- df_working %>%
    left_join(linear_add_j, by = "DT_round") %>% left_join(linear_mult_j, by = "DT_round") %>%
    left_join(exp_add_j, by = "DT_round") %>% left_join(exp_mult_j, by = "DT_round") %>%
    left_join(uniform_add_j, by = "DT_round") %>% left_join(uniform_mult_j, by = "DT_round") %>%
    left_join(fitted_add_j, by = "DT_round") %>% left_join(fitted_mult_j, by = "DT_round")


  return(df_working)
}
