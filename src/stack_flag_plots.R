# Generate plots with both weekly and daily flagged data.
# This function will generate a list of plots with both weekly and daily flagged data.
# @param site_arg A string of the site name.
# @param parameter_arg A string of the parameter name.
# @param flag_arg A string of the flag name.
# @return A list of plots with both weekly and daily flagged data.
# @examples
# stack_flag_plots(site_arg = "archery", parameter_arg = "Actual Conductivity", flag_arg = "outside of Actual Conductivity sensor specification range")
# stack_flag_plots(site_arg = "boxelder", parameter_arg = "Temperature", flag_arg = "outside of Temperature sensor specification range")

# stack_flag_plots <- function(site_arg, parameter_arg, flag_arg, df_list) {
#   # Call on the weekly and daily functions and fill their args with this
#   # functions args
#   weekly_plot_list <- generate_weekly_flag_plots(site_arg = site_arg, parameter_arg = parameter_arg, flag_arg = flag_arg, df_list = df_list)
#   daily_plot_list <- generate_daily_flag_plots(site_arg = site_arg, parameter_arg = parameter_arg, flag_arg = flag_arg, df_list = df_list)
#
#   # These two functions should always return the same amount of plots, so we can
#   # use map2() to stack them with combine_plots()
#   weekly_daily_plots <- map2(.x = weekly_plot_list, .y = daily_plot_list, ~ggarrange(.x, .y, nrow = 2, ncol = 1, heights = 5, widths = 12))
#   return(weekly_daily_plots)
# }
