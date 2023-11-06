# KW's field notes flags
#finding instances in HydroVu and in the field notes where we identified issues with certain site-parameters.

add_malfunction_flag <- function(df){

  df <- df %>%

    # LEGACY

    add_flag((year == "2022" & DT_round >= "2022-07-12 10:00:00" & DT_round <= "2022-08-04 09:00:00" & site == "legacy" & parameter %in% c("Turbidity")), "sensor malfunction") %>%
    add_flag((year == "2022" & DT_round >= "2022-07-12 10:00:00" & DT_round <= "2022-07-12 15:00:00" & site == "legacy" & parameter %in% c("Specific Conductivity")), "sensor malfunction") %>%
    # Sensor wasn't submerged starting at this point:
    add_flag((year == "2022" & DT_round >= "2022-09-01 12:35:00" & site == "legacy" & parameter == "Turbidity"), "sensor malfunction") %>%


    # TIMBERLINE
    # DO issue
    add_flag((year == "2022" & DT_round <= "2022-05-25 15:00:00" & site == "timberline" & parameter == "DO"), "sensor malfunction") %>%
    # Conductivity issue
    add_flag((year == "2022" & DT_round >= "2022-05-25 15:00:00" & DT_round <= "2022-05-26 11:15:00" & site == "timberline" & parameter == "Specific Conductivity"), "sensor malfunction") %>%
    # I think this issue can be back-calibrated easily... It is likely that the big turbidity pulse on 7/15-7/16(?) led to issues at this site afterwards. But cal on 7/31 resolved the issue.
    add_flag((year == "2022" & DT_round >= "2022-07-16 22:00:00" & DT_round <= "2022-07-31 13:00:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Severe grime on water
    add_flag((year == "2022" & DT_round >= "2022-08-24 15:00:00" & DT_round <= "2022-08-31 07:15:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Sensor was incredibly dirty and in roots, likely unable to back-calibrate:
    add_flag((year == "2022" & DT_round >= "2022-09-05 11:00:00" & DT_round <= "2022-09-14	13:15:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= "2022-09-13 12:00:00" & DT_round <= "2022-09-14 13:00:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= "2022-09-13 12:00:00" & DT_round <= "2022-09-14 13:00:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-02 16:30:00" & DT_round <= "2022-10-04 16:30:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-11 16:30:00" & DT_round <= "2022-10-14 15:30:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-26 11:30:00" & DT_round <= "2022-10-26 18:30:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Broken Sp Conductivity sensor
    add_flag((year == "2022" & DT_round >= "2022-11-01 08:30:00" & DT_round <= "2022-11-08 09:30:00" & site == "timberline" & parameter == "Specific Conductivity"), "sensor malfunction") %>%
    # Broken Sp Conductivity sensor
    add_flag((year == "2022" & DT_round >= "2022-11-08 09:30:00" & DT_round <= "2022-11-15 14:00:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Broken Sp Conductivity sensor
    add_flag((year == "2023" & DT_round >= "2023-04-24 10:30:00" & DT_round <= "2023-04-27 16:15:00" & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%

    # PROSPECT
    # I think this issue can also be back-calibrated easily...
    add_flag((year == "2022" & DT_round >= "2022-07-24 00:00:00" & DT_round <= "2022-08-01 07:15:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Grime on sensor head... maybe we could do something to fix?
    add_flag((year == "2022" & DT_round >= "2022-08-13 01:00:00" & DT_round <= "2022-08-17 15:30:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Grime on sensor head... maybe we could back calibrate:
    add_flag((year == "2022" & DT_round >= "2022-08-20 09:00:00" & DT_round <= "2022-08-29 13:30:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Severe grime on sensor
    add_flag((year == "2022" & DT_round >= "2022-08-24 20:00:00" & DT_round <= "2022-08-30 18:30:00" & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # major drift due to grime?
    add_flag((year == "2022" & DT_round >= "2022-09-16 23:45:00" & DT_round <= "2022-09-21 13:15:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Major drift. Is there a calibration?
    add_flag((year == "2022" & DT_round >= "2022-09-23 14:15:00" & DT_round <= "2022-09-30 15:30:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-11 16:30:00" & DT_round <= "2022-10-14 14:30:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-26 11:30:00" & DT_round <= "2022-11-04 10:00:00" & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%

    # BOXELDER
    # Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= "2022-09-04 16:00:00" & DT_round <= "2022-09-09 13:30:00" & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Bad drift
    add_flag((year == "2022" & DT_round >= "2022-09-16 15:00:00" & DT_round <= "2022-09-20 16:30:00" & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Seemed to take a while to "stabilize"... ?
    add_flag((year == "2022" & DT_round >= "2022-09-20 16:30:00" & DT_round <= "2022-09-22 15:30:00" & site == "boxelder" & parameter == "ORP"), "sensor malfunction") %>%
    # Weird event but seemed to resolve itself...
    add_flag((year == "2022" & DT_round >= "2022-10-01 14:30:00" & DT_round <= "2022-10-03 22:30:00" & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= "2022-10-26 11:30:00" & DT_round <= "2022-10-28 14:30:00" & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%


    # ARCHERY
    add_flag((year == "2022" & DT_round >= "2022-04-28 10:00:00" & DT_round <= "2022-05-11 00:00:00" & site == "archery" & parameter == "Chl-a Fluorescence"), "sensor malfunction") %>%
    add_flag((year == "2022" & DT_round >= "2022-07-01 22:00:00" & DT_round <= "2022-07-06 11:00:00" & site == "archery" & parameter == "DO"), "sensor malfunction")

  # RIVER BLUFFS
  add_flag((year == "2023" & DT_round >= "2023-04-13 14:00:00" & DT_round <= "2022-04-16 14:20:00" & site == "riverbluffs" & parameter %in% c("ORP", "DO", "Specific Conductivity")), "sensor malfunction")
  add_flag((year == "2023" & DT_round >= "2023-05-01 14:00:00" & DT_round <= "2022-04-16 14:20:00" & site == "riverbluffs" & parameter %in% c("ORP", "DO", "Specific Conductivity")), "sensor malfunction")



}
