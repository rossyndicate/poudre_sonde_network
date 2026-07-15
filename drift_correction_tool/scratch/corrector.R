# OLD CODE FROM JUAN FOR REFERENCE
# # Convert sensor timestamps from MST to UTC so all data is stored in UTC
# df <- df %>% mutate(DT_round = with_tz(DT_round, "UTC"))
#
# site_val      <- unique(df$site)
# parameter_val <- unique(df$parameter)
#
# # Filter decisions for this site-parameter
# site_decisions <- decision_df %>%
#   filter(site == site_val, parameter == parameter_val)
#
# # 1. Unflag: update drift flags and user_flag (does not change mean_analysis)
# df_working <- unflag_fxn(
#   decision_df_arg = site_decisions %>% filter(decision == "unflag"),
#   sensor_df_arg   = df
# )
#
# # Helper: boolean vector marking rows inside any window in a decision subset
# in_windows <- function(dec_df) {
#   if (nrow(dec_df) == 0) return(rep(FALSE, nrow(df_working)))
#   map2(
#     pull(dec_df, start_dt),
#     pull(dec_df, end_dt),
#     function(s, e) between(df_working$DT_round, s, e)
#   ) %>% reduce(`|`)
# }
#
# # 2. Identify which rows belong to each correction window type
# unflag_decision_df <- site_decisions %>% filter(decision == "unflag")
# linear_decision_df         <- site_decisions %>% filter(arg_drift_type == "linear")
# exp_decision_df            <- site_decisions %>% filter(arg_drift_type == "exponential")
# uniform_decision_df        <- site_decisions %>% filter(arg_drift_type == "uniform")
# fitted_linear_decision_df  <- site_decisions %>% filter(arg_drift_type == "fitted_linear")
#
# in_unflag        <- in_windows(unflag_decision_df)
# in_linear        <- in_windows(linear_decision_df)
# in_exp           <- in_windows(exp_decision_df)
# in_uniform       <- in_windows(uniform_decision_df)
# in_fitted_linear <- in_windows(fitted_linear_decision_df)
#
# # 3. Compute corrections and join results back onto df_working by DT_round.
# #    distinct() guards against row expansion from overlapping decision windows.
# if (any(in_linear)) {
#   linear_join <- linear_correction_fxn(
#     decision_df_arg     = linear_decision_df,
#     sensor_df_arg       = df_working,
#     data_list           = prepped_data,
#     site_order_template = site_order
#   ) %>%
#     distinct(DT_round, .keep_all = TRUE) %>%
#     select(DT_round, linear_trans = mean_drift_trans, linear_source = pre_post_source)
#   df_working <- df_working %>% left_join(linear_join, by = "DT_round")
# } else {
#   df_working <- df_working %>% mutate(linear_trans = NA_real_, linear_source = NA_character_)
# }
#
# if (any(in_exp)) {
#   exp_join <- exponential_correction_fxn(
#     decision_df_arg     = exp_decision_df,
#     sensor_df_arg       = df_working %>% select(-any_of(c("linear_trans", "linear_source"))),
#     data_list           = prepped_data,
#     site_order_template = site_order
#   ) %>%
#     distinct(DT_round, .keep_all = TRUE) %>%
#     select(DT_round, exp_trans = mean_drift_trans, exp_source = pre_post_source)
#   df_working <- df_working %>% left_join(exp_join, by = "DT_round")
# } else {
#   df_working <- df_working %>% mutate(exp_trans = NA_real_, exp_source = NA_character_)
# }
#
# if (any(in_uniform)) {
#   uniform_join <- linear_correction_fxn(
#     decision_df_arg     = uniform_decision_df,
#     sensor_df_arg       = df_working %>% select(-any_of(c("linear_trans", "linear_source", "exp_trans", "exp_source"))),
#     data_list           = prepped_data,
#     site_order_template = site_order
#   ) %>%
#     distinct(DT_round, .keep_all = TRUE) %>%
#     select(DT_round, uniform_trans = mean_drift_trans, uniform_source = pre_post_source)
#   df_working <- df_working %>% left_join(uniform_join, by = "DT_round")
# } else {
#   df_working <- df_working %>% mutate(uniform_trans = NA_real_, uniform_source = NA_character_)
# }
#
# if (any(in_fitted_linear)) {
#   fitted_linear_join <- linear_correction_fxn(
#     decision_df_arg     = fitted_linear_decision_df,
#     sensor_df_arg       = df_working %>% select(-any_of(c("linear_trans", "linear_source", "exp_trans", "exp_source", "uniform_trans", "uniform_source"))),
#     data_list           = prepped_data,
#     site_order_template = site_order
#   ) %>%
#     distinct(DT_round, .keep_all = TRUE) %>%
#     select(DT_round, fitted_linear_trans = mean_drift_trans, fitted_linear_source = pre_post_source)
#   df_working <- df_working %>% left_join(fitted_linear_join, by = "DT_round")
# } else {
#   df_working <- df_working %>% mutate(fitted_linear_trans = NA_real_, fitted_linear_source = NA_character_)
# }
#
# # 4. Stitch: assign corrected values and stamp correction_type for plotting
# df_corrected <- df_working %>%
#   mutate(
#     mean_drift_trans = case_when(
#       in_linear        ~ linear_trans,
#       in_exp           ~ exp_trans,
#       in_uniform       ~ uniform_trans,
#       in_fitted_linear ~ fitted_linear_trans,
#       TRUE             ~ mean_analysis
#     ),
#     correction_type = case_when(
#       in_linear        ~ "linear",
#       in_exp           ~ "exponential",
#       in_uniform       ~ "uniform",
#       in_fitted_linear ~ "fitted_linear",
#       in_unflag        ~ "unflagged",
#       TRUE             ~ "raw"
#     ),
#     pre_post_source = case_when(
#       in_linear        ~ linear_source,
#       in_exp           ~ exp_source,
#       in_uniform       ~ uniform_source,
#       in_fitted_linear ~ fitted_linear_source,
#       TRUE             ~ NA_character_
#     )
#   ) %>%
#   select(-linear_trans, -exp_trans, -uniform_trans, -fitted_linear_trans,
#          -linear_source, -exp_source, -uniform_source, -fitted_linear_source)
#
# # 5. Post-process: smooth (Turbidity only) then interpolate short gaps
# df_corrected %>%
#   apply_interpolation_missing_data(value_col = "mean_drift_trans", new_value_col = "mean_filled")%>%
#   low_pass_filter(correction_col = "mean_filled")
