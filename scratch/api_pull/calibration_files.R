# Load and munge field calibration files:

cal_files <- list.files("data/calibration_reports", pattern=".html")

cal_tabler <- function(cal_files){

  cal <- read_html(file.path("data/calibration_reports/", cal_files)) %>%
    html_nodes("div") %>%
    html_text() %>%
    as_tibble()

  rdo <- cal %>% filter(grepl("RDO", value)) %>% pull() %>% str_replace_all(., " ", "") %>% tolower()

  ph_orp <- cal %>% filter(grepl("pH/ORP", value)) %>% pull() %>% str_replace_all(., " ", "") %>% tolower()

  conductivity <- cal %>% filter(grepl("Conductivity",value)) %>% pull() %>% str_replace_all(., " ", "") %>% tolower()

  turbidity <- cal %>% filter(grepl("Turbidity",value)) %>% pull() %>% str_replace_all(., " ", "") %>% tolower()

  # Always the fifth sensor when depth is available:
  try(depth <- cal %>% .[5,] %>% pull() %>% str_replace_all(., " ", "") %>% tolower())

  time_mst <- paste0(str_sub(cal_files, -13, -12),":", str_sub(cal_files, -11, -10))

  date <- paste0(str_sub(cal_files, -22, -19),"-", str_sub(cal_files, -18, -17),"-", str_sub(cal_files, -16, -15))

  cal_table <- tibble(site = sub("\\_.*", "", cal_files),

                      DT = ymd_hm(paste(date, time_mst, tz = "MST")),

                      # Depth
                      depth_cal_date = "None",
                      depth_offset = "None",
                      depth_ref_depth = "None",
                      depth_ref_offset = "None",
                      depth_pre_psi = "None",
                      depth_post_psi = "None",

                      # Dissolved Oxygen
                      rdo_slope = str_match(rdo, "slope\\s*(.*?)\\s*offset")[,2],
                      rdo_offset = str_match(rdo, "offset\\s*(.*?)\\s*mg/l")[,2],
                      rdo_100 = str_match(rdo, "premeasurement\\s*(.*?)\\s*%satpost")[,2],
                      rdo_conc = str_match(rdo, "concentration\\s*(.*?)\\s*mg/lpremeasurement")[,2],
                      rdo_temp = str_match(rdo, "temperature\\s*(.*?)\\s*°c")[,2],
                      rdo_pressure = str_match(rdo, "pressure\\s*(.*?)\\s*mbar")[,2],

                      # pH
                      ph_slope_pre = str_match(ph_orp, "offset1slope\\s*(.*?)\\s*mv/ph")[,2],
                      ph_offset_pre = str_match(ph_orp, "mv/phoffset\\s*(.*?)\\s*mvslopeandoffset2")[,2],
                      ph_slope_post = str_match(ph_orp, "offset2slope\\s*(.*?)\\s*mv/ph")[,2],
                      ph_offset_post = str_match(ph_orp, paste0(ph_slope_post,"mv/phoffset\\s*(.*?)\\s*mvorporp"))[,2],
                      # Sometimes, the post value can actually be in the high 6 pH... therefore the post measurement regex matching text is conditional
                      ph_7_nice = str_sub(str_match(ph_orp, "postmeasurementph7\\s*(.*?)\\s*mvcal")[,2], 10, nchar(str_match(ph_orp, "postmeasurementph7\\s*(.*?)\\s*mvcal")[,2])),
                      ph_7_other = str_sub(str_match(ph_orp, "postmeasurementph6\\s*(.*?)\\s*mvcal")[,2], 10, nchar(str_match(ph_orp, "postmeasurementph6\\s*(.*?)\\s*mvcal")[,2])),
                      ph_7 = ifelse(is.na(ph_7_nice), ph_7_other, ph_7_nice),

                      # ORP
                      #Newly encountered thing: sometimes the calibration report calls the ORP standard Zobell's, sometimes it's just called "ORP Standard":
                      orp_offset = ifelse(is.na(str_match(ph_orp, "zobell'soffset\\s*(.*?)\\s*mvtemperature")[,2]),
                                          str_match(ph_orp, "orpstandardoffset\\s*(.*?)\\s*mvtemperature")[,2],
                                          str_match(ph_orp, "zobell'soffset\\s*(.*?)\\s*mvtemperature")[,2]),

                      # Conductivity
                      tds_conversion_ppm = str_sub(str_match(conductivity, "tdsconversionfactor\\s*(.*?)\\s*cellconstant")[,2], 6, nchar(str_match(conductivity, "tdsconversionfactor\\s*(.*?)\\s*cellconstant")[,2])),
                      cond_cell_constant = str_match(conductivity, "cellconstant\\s*(.*?)\\s*referencetemperature")[,2],
                      cond_pre = str_match(conductivity,paste0(str_match(conductivity,
                                                                         "premeasurementactual\\s*(.*?)\\s*specificconductivity")[,2],"specificconductivity\\s*(.*?)\\s*µs/cmpost"))[,2],
                      cond_post = str_match(conductivity,paste0(str_match(conductivity,
                                                                          "postmeasurementactual\\s*(.*?)\\s*specificconductivity")[,2],"specificconductivity\\s*(.*?)\\s*µs/cm"))[,2],

                      # Turbidity
                      ntu_slope = "None",
                      ntu_offset = "None",
                      ntu_10 = "None",
                      ntu_100 = "None") %>%

    select(-c(ph_7_nice, ph_7_other))

  # Not all sondes have depth. So, we "try" to get the values.
  try(cal_table <- cal_table %>%
        mutate(
          # Depth
          depth_cal_date = str_match(depth, "lastcalibrated\\s*(.*?)\\s*calibrationdetails")[,2],
          depth_offset = str_match(depth, "zerooffset\\s*(.*?)\\s*psireferencedepth")[,2],
          depth_ref_depth = str_match(depth, "psireferencedepth\\s*(.*?)\\s*ftreferenceoffset")[,2],
          depth_ref_offset = str_match(depth, "ftreferenceoffset\\s*(.*?)\\s*psipremeasurement")[,2],
          depth_pre_psi = str_match(depth, "psipremeasurement\\s*(.*?)\\s*psipostmeasurement")[,2],
          depth_post_psi = str_match(depth, "psipostmeasurement\\s*(.*?)\\s*psi")[,2]))



  # Not all sondes have turbidity. So, we "try" to get the values.
  try(cal_table <- cal_table %>%
        mutate(
          # Turbidity
          ntu_slope = str_match(turbidity, "slope\\s*(.*?)\\s*offset")[,2],
          ntu_offset = str_match(turbidity, "offset\\s*(.*?)\\s*ntu")[,2],
          ntu_10 = str_match(turbidity, "calibrationpoint1premeasurement\\s*(.*?)\\s*ntupost")[,2],
          ntu_100 = str_match(turbidity, "calibrationpoint2premeasurement\\s*(.*?)\\s*ntupost")[,2]))

  cal_table <- cal_table %>%
    mutate(
      #Factory Defaults
      factory_defaults = paste0(ifelse(is.na(ntu_slope), "Turbidity ", ""),
                                ifelse(is.na(rdo_slope), "RDO ", ""),
                                ifelse(is.na(ph_slope_post), "pH ", ""),
                                ifelse(is.na(orp_offset), "ORP ", ""),
                                ifelse(is.na(cond_post), "Conductivity ", ""),
                                ifelse(is.na(depth_cal_date), "Depth ", "")))
  cal_table

}

cal_table <- map_dfr(cal_files, cal_tabler) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(DT = round_date(DT, "15 minutes")) %>%
  # filter for years that are 2022 and greater
  filter(year(DT) >= 2022)
# this creates a lot of errors, is it okay to ignore them all? Just make a note that these will create a lot of errors.

rm(cal_files, cal_tabler, package_loader)
