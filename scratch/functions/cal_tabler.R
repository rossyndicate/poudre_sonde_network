#' Creating a calibration table from each site visit's calibration reports

cal_tabler <- function(){

  cal_files <- list.files("data/calibration_reports", pattern=".html")

  cal_table <- vector("list", length = length(cal_files))

  for(i in 1:length(cal_table)){

    cal <- rvest::read_html(paste0("data/calibration_reports/", cal_files[i])) %>%
      rvest::html_nodes('div') %>%
      rvest::html_text() %>%
      as_tibble()

    rdo <- cal %>% filter(grepl("RDO", value)) %>% pull() %>% str_replace_all(., " ", "")

    ph_orp <- cal %>% filter(grepl("pH/ORP", value)) %>% pull() %>% str_replace_all(., " ", "")

    conductivity <- cal %>% filter(grepl("Conductivity",value)) %>% pull() %>% str_replace_all(., " ", "")

    turbidity <- cal %>% filter(grepl("Turbidity",value)) %>% pull() %>% str_replace_all(., " ", "")

    time_mst <- paste0(str_sub(cal_files[i], -13, -12),":", str_sub(cal_files[i], -11, -10))

    date <- paste0(str_sub(cal_files[i], -22, -19),"-", str_sub(cal_files[i], -18, -17),"-", str_sub(cal_files[i], -16, -15))

    cal_table[[i]] <- tibble(site = sub("\\_.*", "", cal_files[i]),

                             DT = ymd_hm(paste0(date, " ", time_mst)),

                             #Dissolved Oxygen
                             rdo_slope = str_match(rdo, "Slope\\s*(.*?)\\s*Offset")[,2],
                             rdo_offset = str_match(rdo, "Offset\\s*(.*?)\\s*mg/L")[,2],
                             rdo_100 = str_match(rdo, "PreMeasurement\\s*(.*?)\\s*%SatPost")[,2],
                             rdo_conc = str_match(rdo, "Concentration\\s*(.*?)\\s*mg/LPreMeasurement")[,2],
                             rdo_temp = str_match(rdo, "Temperature\\s*(.*?)\\s*°C")[,2],
                             rdo_pressure = str_match(rdo, "Pressure\\s*(.*?)\\s*mbar")[,2],

                             #pH
                             ph_slope_pre = str_match(ph_orp, "Offset1Slope\\s*(.*?)\\s*mV/pH")[,2],
                             ph_offset_pre = str_match(ph_orp, "mV/pHOffset\\s*(.*?)\\s*mVSlopeandOffset2")[,2],
                             ph_slope_post = str_match(ph_orp, "Offset2Slope\\s*(.*?)\\s*mV/pH")[,2],
                             ph_offset_post = str_match(ph_orp, paste0(ph_slope_post,"mV/pHOffset\\s*(.*?)\\s*mVORPORP"))[,2],
                             ph_7_nice = str_sub(str_match(ph_orp, "PostMeasurementpH7\\s*(.*?)\\s*mVCal")[,2], 10, nchar(str_match(ph_orp, "PostMeasurementpH7\\s*(.*?)\\s*mVCal")[,2])),
                             ph_7_other = str_sub(str_match(ph_orp, "PostMeasurementpH6\\s*(.*?)\\s*mVCal")[,2], 10, nchar(str_match(ph_orp, "PostMeasurementpH6\\s*(.*?)\\s*mVCal")[,2])),
                             ph_7 = ifelse(is.na(ph_7_nice), ph_7_other, ph_7_nice),

                             #ORP
                             orp_offset = ifelse(is.na(str_match(ph_orp, "Zobell'sOffset\\s*(.*?)\\s*mVTemperature")[,2]),
                                                 str_match(ph_orp, "ZoBell'sOffset\\s*(.*?)\\s*mVTemperature")[,2],
                                                 str_match(ph_orp, "Zobell'sOffset\\s*(.*?)\\s*mVTemperature")[,2]),

                             #Conductivity
                             tds_conversion_ppm = str_sub(str_match(conductivity, "TDSConversionFactor\\s*(.*?)\\s*CellConstant")[,2], 6, nchar(str_match(conductivity, "TDSConversionFactor\\s*(.*?)\\s*CellConstant")[,2])),
                             cond_cell_constant = str_match(conductivity, "CellConstant\\s*(.*?)\\s*ReferenceTemperature")[,2],
                             cond_pre = str_match(conductivity,paste0(str_match(conductivity,
                                                                                "PreMeasurementActual\\s*(.*?)\\s*SpecificConductivity")[,2],"SpecificConductivity\\s*(.*?)\\s*µS/cmPost"))[,2],
                             cond_post = str_match(conductivity,paste0(str_match(conductivity,
                                                                                 "PostMeasurementActual\\s*(.*?)\\s*SpecificConductivity")[,2],"SpecificConductivity\\s*(.*?)\\s*µS/cm"))[,2],

                             #Turbidity
                             ntu_slope = str_match(turbidity, "Slope\\s*(.*?)\\s*Offset")[,2],
                             ntu_offset = str_match(turbidity, "Offset\\s*(.*?)\\s*NTU")[,2],
                             ntu_10 = str_match(turbidity, "CalibrationPoint1PreMeasurement\\s*(.*?)\\s*NTUPost")[,2],
                             ntu_100 = str_match(turbidity, "CalibrationPoint2PreMeasurement\\s*(.*?)\\s*NTUPost")[,2],

                             #Factory Defaults
                             factory_defaults = paste0(ifelse(is.na(ntu_slope), "Turbidity ", ""),
                                                       ifelse(is.na(rdo_slope), "RDO ", ""),
                                                       ifelse(is.na(ph_slope_post), "pH ", ""),
                                                       ifelse(is.na(orp_offset), "ORP ", ""),
                                                       ifelse(is.na(cond_post), "Conductivity ", ""))) %>%
      select(-c(ph_7_nice, ph_7_other))
  }

  cal_table <- bind_rows(cal_table) %>%
    distinct(.keep_all = TRUE) %>%
    group_by(site) %>%
    mutate(DT = round_date(DT, "15 minutes")) %>%
    padr::pad(by = 'DT', group = 'site')

  return(cal_table)

}

#test_it <- cal_tabler()
