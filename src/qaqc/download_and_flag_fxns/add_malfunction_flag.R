# KW's field notes flags
#finding instances in HydroVu and in the field notes where we identified issues with certain site-parameters.

add_malfunction_flag <- function(df){

  df <- df %>%

    # LEGACY
    ## 2022
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-07-12 10:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-04 09:00:00", tz = "MST") & site == "legacy" & parameter %in% c("Turbidity")), "sensor malfunction") %>%
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-07-12 10:00:00", tz = "MST") & DT_round <= ymd_hms("2022-07-12 15:00:00", tz = "MST") & site == "legacy" & parameter %in% c("Specific Conductivity")), "sensor malfunction") %>%
    # Sensor wasn't submerged starting at this point:
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-01 12:35:00", tz = "MST") & site == "legacy" & parameter == "Turbidity"), "sensor malfunction") %>%
    ## 2023
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-13 11:00:00", tz = "MST") & DT_round <= ymd_hms("2023-09-15 16:00:00", tz = "MST") & site == "legacy"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-19 16:00:00", tz = "MST") & DT_round <= ymd_hms("2023-09-22 08:00:00", tz = "MST") & site == "legacy"), "sensor malfunction") %>%

    # LINCOLN
    ## 2023
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-01 10:00:00", tz = "MST") & DT_round <= ymd_hms("2023-09-15 12:00:00", tz = "MST") & site == "lincoln" & parameter == "Turbidity"), "sensor malfunction") %>%

    # TIMBERLINE
    ## 2023
    ### DO issue
    add_flag((year == "2022" & DT_round <= ymd_hms("2022-05-25 15:00:00", tz = "MST") & site == "timberline" & parameter == "DO"), "sensor malfunction") %>%
    ### Conductivity issue
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-05-25 15:00:00", tz = "MST") & DT_round <= ymd_hms("2022-05-26 11:15:00", tz = "MST") & site == "timberline" & parameter == "Specific Conductivity"), "sensor malfunction") %>%
    ### I think this issue can be back-calibrated easily... It is likely that the big turbidity pulse on 7/15-7/16(?) led to issues at this site afterwards. But cal on 7/31 resolved the issue.
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-07-16 22:00:00", tz = "MST") & DT_round <= ymd_hms("2022-07-31 13:00:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Severe grime on water
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-08-24 15:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-31 07:15:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Sensor was incredibly dirty and in roots, likely unable to back-calibrate:
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-05 11:00:00", tz = "MST") & DT_round <= ymd_hms("2022-09-14	13:15:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-13 12:00:00", tz = "MST") & DT_round <= ymd_hms("2022-09-14 13:00:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-13 12:00:00", tz = "MST") & DT_round <= ymd_hms("2022-09-14 13:00:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-02 16:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-04 16:30:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-11 16:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-14 15:30:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-26 11:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-26 18:30:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Broken Sp Conductivity sensor
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-11-01 08:30:00", tz = "MST") & DT_round <= ymd_hms("2022-11-08 09:30:00", tz = "MST") & site == "timberline" & parameter == "Specific Conductivity"), "sensor malfunction") %>%
    ### Broken Sp Conductivity sensor
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-11-08 09:30:00", tz = "MST") & DT_round <= ymd_hms("2022-11-15 14:00:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### Broken Sp Conductivity sensor
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-04-24 10:30:00", tz = "MST") & DT_round <= ymd_hms("2023-04-27 16:15:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ## 2023
    ### timberline biofouling, wiper issue
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-04-24 15:00:00", tz = "MST") & DT_round <= ymd_hms("2023-04-27 17:00:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### wiper issue, affects cond sensor adds extra noise to data
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-05-31 9:00:00", tz = "MST") & DT_round <= ymd_hms("2023-06-26 12:40:00", tz = "MST") & site == "timberline" & parameter %in% c("Actual Conductivity", "Specific Conductivity")), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-07-14 20:00:00", tz = "MST") & DT_round <= ymd_hms("2023-07-16 2:00:00", tz = "MST") & site == "timberline" & parameter %in% c("Actual Conductivity", "Specific Conductivity")), "sensor malfunction") %>%
    ### housing filled with sediment post storm, sensor caked in sediment
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-07-22 11:30:00", tz = "MST") & DT_round <= ymd_hms("2023-07-24 11:30:00", tz = "MST") & site == "timberline" & parameter == "Turbidity"), "sensor malfunction") %>%

    # PROSPECT
    # I think this issue can also be back-calibrated easily...
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-07-24 00:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-01 07:15:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Grime on sensor head... maybe we could do something to fix?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-08-13 01:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-17 15:30:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Grime on sensor head... maybe we could back calibrate:
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-08-20 09:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-29 13:30:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Severe grime on sensor
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-08-24 20:00:00", tz = "MST") & DT_round <= ymd_hms("2022-08-30 18:30:00", tz = "MST") & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # major drift due to grime?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-16 23:45:00", tz = "MST") & DT_round <= ymd_hms("2022-09-21 13:15:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Major drift. Is there a calibration?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-23 14:15:00", tz = "MST") & DT_round <= ymd_hms("2022-09-30 15:30:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-11 16:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-14 14:30:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-26 11:30:00", tz = "MST") & DT_round <= ymd_hms("2022-11-04 10:00:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%
    ## 2023
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-26 12:00:00", tz = "MST") & DT_round <= ymd_hms("2023-09-27 10:00:00", tz = "MST") & site == "prospect" & parameter == "DO"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-10-04 00:00:00", tz = "MST") & DT_round <= ymd_hms("2023-10-10 13:45:00", tz = "MST") & site == "prospect" & parameter == "Turbidity"), "sensor malfunction") %>%

    # BOXELDER
    # Wiper issue, maybe?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-04 16:00:00", tz = "MST") & DT_round <= ymd_hms("2022-09-09 13:30:00", tz = "MST") & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Bad drift
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-16 15:00:00", tz = "MST") & DT_round <= ymd_hms("2022-09-20 16:30:00", tz = "MST") & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Seemed to take a while to "stabilize"... ?
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-09-20 16:30:00", tz = "MST") & DT_round <= ymd_hms("2022-09-22 15:30:00", tz = "MST") & site == "boxelder" & parameter == "ORP"), "sensor malfunction") %>%
    # Weird event but seemed to resolve itself...
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-01 14:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-03 22:30:00", tz = "MST") & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    # Drift/gunk accumulation
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-10-26 11:30:00", tz = "MST") & DT_round <= ymd_hms("2022-10-28 14:30:00", tz = "MST") & site == "boxelder" & parameter == "Turbidity"), "sensor malfunction") %>%
    ## 2023
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-07-31 19:00:00", tz = "MST") & DT_round <= ymd_hms("2023-08-04 13:00:00", tz = "MST") & site == "boxelder"), "sensor malfunction") %>%

    # ARCHERY
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-04-28 10:00:00", tz = "MST") & DT_round <= ymd_hms("2022-05-11 00:00:00", tz = "MST") & site == "archery" & parameter == "Chl-a Fluorescence"), "sensor malfunction") %>%
    add_flag((year == "2022" & DT_round >= ymd_hms("2022-07-01 22:00:00", tz = "MST") & DT_round <= ymd_hms("2022-07-06 11:00:00", tz = "MST") & site == "archery" & parameter == "DO"), "sensor malfunction") %>%
    ## 2023
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-04-11 09:45:00", tz = "MST") & DT_round <= ymd_hms("2023-04-11 10:15:00", tz = "MST") & site == "archery"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-07-17 05:00:00", tz = "MST") & DT_round <= ymd_hms("2023-07-19 14:45:00", tz = "MST") & site == "archery"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-12 06:30:00", tz = "MST") & DT_round <= ymd_hms("2023-09-15 09:30:00", tz = "MST") & site == "archery"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-09-15 09:15:00", tz = "MST") & DT_round <= ymd_hms("2023-10-16 01:30:00", tz = "MST") & site == "archery" & parameter == "Actual Conductivity"), "sensor malfunction") %>%


    # RIVER BLUFFS
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-04-13 14:00:00", tz = "MST") & DT_round <= ymd_hms("2022-04-16 14:20:00", tz = "MST") & site == "riverbluffs" & parameter %in% c("ORP", "DO", "Specific Conductivity")), "sensor malfunction") %>%
    # not specifying a parameter because the data for all of this time is suspect (constantly caked)
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-05-04 12:00:00", tz = "MST") & DT_round <= ymd_hms("2023-07-05 22:00:00", tz = "MST") & site == "riverbluffs"), "sensor malfunction") %>%
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-07-27 01:00:00", tz = "MST") & DT_round <= ymd_hms("2023-08-03 08:00:00", tz = "MST") & site == "riverbluffs"), "sensor malfunction") %>%


    # TAMASAG
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-04-20 09:00:00", tz = "MST") & DT_round <= ymd_hms("2023-05-01 08:00:00", tz = "MST") & site == "tamasag" & parameter %in% c("Specific Conductivity", "pH")), "sensor malfunction") %>%
    ### turb calibration issue
    add_flag((year == "2023" & DT_round >=ymd_hms("2023-05-15 11:20:00", tz = "MST") & DT_round <= ymd_hms("2023-05-15 18:00:00", tz = "MST") & site == "tamasag" & parameter == "Turbidity"), "sensor malfunction") %>%
    ### turb moisture under lens
    add_flag((year == "2023" & DT_round >= ymd_hms("2023-06-13 04:00:00", tz = "MST") & DT_round <= ymd_hms("2023-06-26 12:40:00", tz = "MST") & site == "tamasag" & parameter == "Turbidity"), "sensor malfunction")

}
