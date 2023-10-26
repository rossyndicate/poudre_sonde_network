# KW's field notes flags
#finding instances in HydroVu and in the field notes where we identified issues with certain site-parameters.

add_malfunction_flag <- function(df){

  df <- df %>%
    add_flag((year == "2022" & DT_round <= "2022-05-25 15:00" & site == "timberline" & parameter == "DO"), "sensor malfunction") %>%
    add_flag((year == "2022" & DT_round >= "2022-05-25 15:00" & DT_round <= "2022-05-26 11:15" & site == "timberline" & parameter == "Specific Conductivity"), "sensor malfunction")

}
