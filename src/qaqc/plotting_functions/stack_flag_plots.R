# This function lets you combine the weekly and daily plots into one stacked plot
# to compare the two.
stack_flag_plots <- function(site_arg, parameter_arg, flag_arg) {

  # Call on the weekly and daily functions and fill their args with this
  # functions args
  weekly_plot_list <- generate_weekly_flag_plots(site_arg = site_arg, parameter_arg = parameter_arg, flag_arg = flag_arg)
  daily_plot_list <- generate_daily_flag_plots(site_arg = site_arg, parameter_arg = parameter_arg, flag_arg = flag_arg)

  # These two functions should always return the same amount of plots, so we can
  # use map2() to stack them with combine_plots()
  weekly_daily_plots <- map2(.x = weekly_plot_list, .y = daily_plot_list, ~ggarrange(.x, .y, nrow = 2, ncol = 1))
  return(weekly_daily_plots)
}
