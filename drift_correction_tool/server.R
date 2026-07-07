server <- function(input, output, session) {

  # Completion Tracking ----
  # Seeded from whatever was already in post_verification before the app launched.
  # Each submit() appends the finished combo so choices update live without restart.
  completed_combos <- reactiveVal(post_ver_site_params)

  # Derive the still-available combos from the tracker
  remaining_combos <- reactive({
    setdiff(all_combos, completed_combos())
  })

  # Keep input$site choices filtered to sites that have ≥1 unfinished parameter.
  # Runs whenever completed_combos changes (i.e. after every submit).
  observe({
    avail_sites <- unique(sapply(strsplit(remaining_combos(), "-"), `[`, 1))
    # Preserve current selection if it is still valid
    cur <- isolate(input$site)
    new_sel <- if (!is.null(cur) && cur %in% avail_sites) cur else avail_sites[1]
    updateSelectInput(session, "site", choices = avail_sites, selected = new_sel)
  })

  # Keep input$parameter filtered to params not yet done for the selected site.
  # Runs when site changes OR when a combo is newly completed.
  observe({
    req(input$site)
    site_combos  <- remaining_combos()[startsWith(remaining_combos(), paste0(input$site, "-"))]
    avail_params <- unique(sapply(strsplit(site_combos, "-"), `[`, 2))
    cur <- isolate(input$parameter)
    new_sel <- if (!is.null(cur) && cur %in% avail_params) cur else avail_params[1]
    updateSelectInput(session, "parameter", choices = avail_params, selected = new_sel)
  })

  # Keep additional_sites choices excluding the current primary site.
  observe({
    req(input$site)
    other_sites <- setdiff(site_choices, input$site)
    # Drop any selected values that are now the primary site
    cur_sel <- setdiff(isolate(input$additional_sites), input$site)
    updateSelectizeInput(session, "additional_sites",
                         choices  = other_sites,
                         selected = cur_sel)
  })

  # Reactive Data Context ----
  current_data <- reactive({
    req(input$site, input$parameter)
    combo <- paste(input$site, input$parameter, sep = "-")
    prepped_data[[combo]]
  })

  # Additional Comparison Sites ----
  # Pulls raw data for up to two extra sites (input$additional_sites), at the
  # same parameter as the primary site, so they can be overlaid on the plot
  # for visual comparison.
  additional_data <- reactive({
    req(input$parameter)

    sites <- input$additional_sites
    if (is.null(sites) || length(sites) == 0) return(NULL)

    # Enforce a hard cap of two additional sites, even if the UI ever allows more
    sites <- head(unique(sites), 2)

    site_list <- lapply(sites, function(s) {
      combo <- paste(s, input$parameter, sep = "-")
      df <- prepped_data[[combo]]
      if (is.null(df)) {
        showNotification(paste("No data found for", combo), type = "warning")
        return(NULL)
      }
      df %>% mutate(site_label = s)
    })

    site_list <- site_list[!vapply(site_list, is.null, logical(1))]
    if (length(site_list) == 0) return(NULL)

    bind_rows(site_list)
  })

  drift_windows <- reactiveVal()

  # Window Initialization ----
  observeEvent(current_data(), {
    df <- current_data()
    # Initialize a flag to see if we successfully loaded data from Excel
    loaded_from_excel <- FALSE

    if (file.exists(excel_path)) {
      decision_df <- readxl::read_excel(excel_path) %>%
        filter(site == input$site & parameter == input$parameter)

      # Only process if the file actually yielded rows for this site/parameter
      if (nrow(decision_df) > 0) {
        windows <- decision_df %>%
          select(start_dt, end_dt, arg_drift_type, arg_correction_type) %>%
          arrange(start_dt) %>%
          rowid_to_column(var = "window_id") %>%
          mutate(
            start_dt = as.POSIXct(start_dt, tz = "MST"), # Ensure timezone is set to MST (they get saved as UTC in Excel)
            end_dt   = as.POSIXct(end_dt, tz = "MST") # Ensure timezone is set to MST (they get saved as UTC in Excel)
          ) %>%
          left_join(
            df %>% select(DT_round, end_data_val = mean_analysis),
            by = join_by(closest(end_dt >= DT_round)) # Changed to == for exact/closest matching
          )

        loaded_from_excel <- TRUE
      }
    }

    # Fallback: Run if file doesn't exist OR if file exists but has 0 matching rows
    if (!loaded_from_excel) {
      windows <- df %>%
        filter(drift & !is.na(mean_analysis)) %>%
        arrange(DT_round) %>%
        mutate(gap = as.numeric(DT_round - lag(DT_round), units = "days")) %>%
        mutate(window_id = cumsum(if_else(is.na(gap) | gap > 1, 1, 0))) %>%
        group_by(window_id) %>%
        summarise(
          start_dt = min(DT_round),
          end_dt = max(DT_round),
          end_data_val = last(mean_analysis),
          .groups = 'drop'
        ) %>%
        mutate(
          arg_drift_type = "None",
          arg_correction_type = "additive"
        )
    }



    drift_windows(windows)
  })

  # Dynamic UI Control Generation ----
  output$dynamic_controls <- renderUI({
    windows <- drift_windows()
    if (is.null(windows) || nrow(windows) == 0) return(p("No drift windows found."))

    ui_elements <- lapply(seq_len(nrow(windows)), function(i) {
      row_data <- windows[i, ]

      div(
        style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 15px; margin: 10px; background-color: #ffffff;",
        h6(paste0("Window Block ", row_data$window_id), style = "margin-top: 0; color: #0d6efd; font-weight: bold;"),
        fluidRow(
          column(width = 3, textInput(paste0("start_dt_", i), "Start Timestamp:", value = format(row_data$start_dt, "%Y-%m-%d %H:%M:%S", tz = "MST"))),
          column(width = 3, textInput(paste0("end_dt_", i), "End Timestamp:", value = format(row_data$end_dt, "%Y-%m-%d %H:%M:%S", tz = "MST"))),
          column(width = 3, selectInput(paste0("decision_", i), "Strategy:",
                                        choices = c("None", "linear", "exponential", "uniform", "fitted_linear", "non_resolved", "unflag"),
                                        selected = row_data$arg_drift_type)),
          column(width = 3, selectInput(paste0("drift_mode_", i), "Formulation Type:", choices = c("additive", "multiplicative"), selected = row_data$arg_correction_type))
        )
      )
    })
    do.call(tagList, ui_elements)
  })

  # Apply Window Parameter Updates ----
  observeEvent(input$apply_windows, {
    windows <- isolate(drift_windows())
    req(windows, nrow(windows) > 0)

    df <- isolate(current_data())
    updated <- FALSE

    for (i in seq_len(nrow(windows))) {
      in_start <- input[[paste0("start_dt_", i)]]
      in_end   <- input[[paste0("end_dt_", i)]]
      in_dec   <- input[[paste0("decision_", i)]]
      in_mode  <- input[[paste0("drift_mode_", i)]]

      if (is.null(in_start) || is.null(in_end) || is.null(in_dec) || is.null(in_mode)) next

      parsed_start <- lubridate::ymd_hms(in_start, tz = "MST", quiet = TRUE)
      parsed_end   <- lubridate::ymd_hms(in_end, tz = "MST", quiet = TRUE)

      if (is.na(parsed_start) || is.na(parsed_end)) {
        showNotification(paste("Invalid date format in Block", i, "- skipping update."), type = "error")
        next
      }

      end_dt_changed   <- windows$end_dt[i]   != parsed_end
      start_dt_changed <- windows$start_dt[i] != parsed_start

      if (start_dt_changed ||
          end_dt_changed ||
          windows$arg_drift_type[i]   != in_dec ||
          windows$arg_correction_type[i] != in_mode) {

        windows$start_dt[i]            <- parsed_start
        windows$end_dt[i]              <- parsed_end
        windows$arg_drift_type[i]      <- in_dec
        windows$arg_correction_type[i] <- in_mode

        # Only re-snap end_data_val when the user actually moved end_dt
        if (end_dt_changed) {
          closest_row <- df %>%
            mutate(time_diff = abs(as.numeric(DT_round - parsed_end, units = "secs"))) %>%
            arrange(time_diff) %>%
            slice(1)

          if (nrow(closest_row) > 0) {
            windows$end_data_val[i] <- closest_row$mean_analysis
          }
        }

        updated <- TRUE
      }
    }


    if (updated) {
      drift_windows(windows)
      #Update the excel file as a way to save the decisions for the current site-parameter combo
      decision_df <- data.frame()
      #Grab the existing decisions for all other site-parameter combos and append the current one to it
      if (file.exists(excel_path)) {
        decision_df <- readxl::read_excel(excel_path) %>%
          filter(!(site == input$site & parameter == input$parameter))
      }

      windows %>%
        as_tibble() %>%
        mutate(site = input$site, parameter = input$parameter, final_decision = FALSE) %>% #Note that these are not "FINAL" Decisions
        select(site, parameter, start_dt, end_dt, arg_drift_type, arg_correction_type, final_decision) %>%
        bind_rows(decision_df) %>%
        writexl::write_xlsx(excel_path)

      showNotification("Window modifications applied and recalculated successfully!", type = "message")

    } else {
      showNotification("No adjustments found to apply.", type = "warning")
    }
  })

  # Scenario Transformation Generation ----
  # Scenario Transformation Generation ----
  corrected_scenarios <- reactive({
    req(current_data(), drift_windows())

    df <- current_data()
    windows_df <- drift_windows()

    if (nrow(windows_df) == 0) {
      return(df %>% mutate(
        linear_add = NA_real_, linear_mult = NA_real_,
        exp_add    = NA_real_, exp_mult    = NA_real_,
        uniform_add = NA_real_, uniform_mult = NA_real_,
        fitted_add = NA_real_, fitted_mult = NA_real_
      ))
    }

    # Filter out non_resolved windows
    calc_windows <- windows_df %>% filter(arg_drift_type != "non_resolved")

    if (nrow(calc_windows) == 0) {
      return(df %>% mutate(
        linear_add = NA_real_, linear_mult = NA_real_,
        exp_add    = NA_real_, exp_mult    = NA_real_,
        uniform_add = NA_real_, uniform_mult = NA_real_,
        fitted_add = NA_real_, fitted_mult = NA_real_
      ))
    }

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
  })

  # Plot Rendering Viewport ----
  output$driftPlot <- renderPlotly({
    req(corrected_scenarios(), drift_windows())

    df <- corrected_scenarios() %>%
      # Round all numeric columns to 3 decimal places for cleaner visualization
      mutate(across(mean_analysis:fitted_mult, ~ round(.x, 3)))

    windows <- drift_windows()

    # Start the plot object
    p <- plot_ly()

    #  BASE LAYER
    p <- p %>% add_trace(
      data = df,
      x = ~DT_round,
      y = ~mean_analysis,
      type = 'scatter',
      mode = 'lines',
      name = 'Raw Data (Full)',
      line = list(color = "black")
    )

    # WINDOWS: Loop through and mask sections or add markers
    if (nrow(windows) > 0) {
      for (i in seq_len(nrow(windows))) {
        t_start <- windows$start_dt[i]
        t_end   <- windows$end_dt[i]
        choice  <- windows$arg_drift_type[i]
        y_val   <- windows$end_data_val[i]

        max_y_val <- max(df$mean_analysis, na.rm = TRUE)

        # Filter data for this specific window chunk
        win_data <- df %>% filter(DT_round >= t_start & DT_round <= t_end)

        # Determine window styling dynamically
        boundary_color      <- "grey"
        window_status_color <- "black"
        is_special_status   <- FALSE

        if (choice == "non_resolved") {
          window_status_color <- "red"
          is_special_status   <- TRUE
        } else if (choice == "unflag") {
          window_status_color <- "cornflowerblue"
          is_special_status   <- TRUE
        }

        # Recolor data if it is "non_resolved" or "unflag" to visually indicate the special status
        if (is_special_status) {
          p <- p %>% add_trace(
            data = win_data,
            x = ~DT_round,
            y = ~mean_analysis,
            type = 'scatter',
            mode = 'lines',
            name = paste( i, ':', choice),
            line = list(color = window_status_color),
            showlegend = TRUE
          )
        }


        # If it is a special status (non_resolved / unflag), skip formula paths
        if (!is_special_status) {
          add_formulation_traces <- function(plotly_obj, data_win, add_col, mult_col, base_name, base_color, strategy_match) {
            obj <- plotly_obj

            # Render Additive Variant
            if (choice == "None" || (choice == strategy_match && windows$arg_correction_type[i] == "additive")) {
              if (all(data_win[[add_col]] >= 0, na.rm = TRUE)) {
                obj <- obj %>% add_trace(data = data_win, x = ~DT_round, y = as.formula(paste0("~", add_col)),
                                         type = 'scatter', mode = 'lines',
                                         name = paste0(i, " ", base_name, ":Add"),
                                         line = list(color = base_color, dash = ifelse(choice == "None", "dot", "solid")))
              }
            }
            # Render Multiplicative Variant
            if (choice == "None" || (choice == strategy_match && windows$arg_correction_type[i] == "multiplicative")) {
              if (all(data_win[[mult_col]] >= 0, na.rm = TRUE)) {
                obj <- obj %>% add_trace(data = data_win, x = ~DT_round, y = as.formula(paste0("~", mult_col)),
                                         type = 'scatter', mode = 'lines',
                                         name = paste0(i," ", base_name, ":Mult"),
                                         line = list(color = base_color, dash = "dash"))
              }
            }
            return(obj)
          }

          p <- add_formulation_traces(p, win_data, "linear_add", "linear_mult", "Linear", "#1f77b4", "linear")
          p <- add_formulation_traces(p, win_data, "exp_add", "exp_mult", "Exp", "#ff7f0e", "exponential")
          p <- add_formulation_traces(p, win_data, "uniform_add", "uniform_mult", "Uniform", "#2ca02c", "uniform")
          p <- add_formulation_traces(p, win_data, "fitted_add", "fitted_mult", "Fitted", "#d62728", "fitted_linear")
        }

        #Boundary Segments & End Marker
        p <- p %>%
          add_segments(x = as.character(t_start), xend = as.character(t_start), y = 0, yend = max_y_val,
                       line = list(color = boundary_color, dash = "dash"), showlegend = FALSE) %>%
          add_segments(x = as.character(t_end), xend = as.character(t_end), y = 0, yend = max_y_val,
                       line = list(color = boundary_color, dash = "solid"), showlegend = FALSE) %>%
          add_markers(x = as.character(t_end), y = y_val,
                      marker = list(size = 7, color = ifelse(is_special_status, window_status_color, "purple")),
                      name = paste("Win", i, ifelse(is_special_status, choice, "End Point")), showlegend = FALSE)
      }

      # 5. Overlay Additional Site Reference Markers
      add_df <- additional_data()
      if (!is.null(add_df) && nrow(add_df) > 0) {
        add_df <- add_df %>%
          filter(!is.na(mean_analysis) & drift == FALSE) %>%
          mutate(mean_analysis = round(mean_analysis, 3))

        site_shapes <- c("diamond", "square")
        site_colors <- c("#8c564b", "#17becf")
        site_names  <- unique(add_df$site_label)

        for (j in seq_along(site_names)) {
          site_df <- add_df %>% filter(site_label == site_names[j])

          p <- p %>% add_trace(
            data = site_df, x = ~DT_round, y = ~mean_analysis,
            type = 'scatter', mode = 'markers',
            marker = list(symbol = site_shapes[j], size = 6, color = site_colors[j]),
            name = paste0(site_names[j], " (Reference)")
          )
        }
      }
    }

    # Final Plotly layout configurations
    p %>% layout(
      xaxis = list(
        type = 'date',
        title = "Date-Time (MST)",
        hoverformat = "%Y-%m-%d %H:%M:%S"
      ),
      yaxis = list(title = "Value")
    )
  })

  # Final Corrections Submission Processing ----
  observeEvent(input$submit, {
    windows_df <- isolate(drift_windows())
    df_scenarios <- isolate(corrected_scenarios())

    req(windows_df, df_scenarios)
    showNotification(
      "Submitting. Please Wait...",
      type = "message", duration = 2
    )

    if (any(windows_df$arg_drift_type == "None")) {
      showNotification("Cannot submit: Please select a valid Correction Strategy for all windows.", type = "error")
      return()
    }

    # Export Meta-Decisions Registry Sheet
    excel_path <- here(post_ver_dir, "drift_corrections.xlsx")
    decision_df <- data.frame()
    if (file.exists(excel_path)) {
      decision_df <- readxl::read_excel(excel_path) %>%
        filter(!(site == input$site & parameter == input$parameter))
    }

    windows_df %>%
      as_tibble() %>%
      mutate(site = input$site, parameter = input$parameter, final_decision = TRUE) %>%
      select(site, parameter, start_dt, end_dt, arg_drift_type, arg_correction_type, final_decision) %>%
      bind_rows(decision_df) %>%
      writexl::write_xlsx(excel_path)

    # Initialize Stitching Array Containers
    df_final <- df_scenarios %>%
      mutate(
        mean_drift_trans = mean_analysis,
        correction_type  = "raw"
      )

    in_any_verified_window <- rep(FALSE, nrow(df_final))

    # Strip Drift Tokens Located Outside Active Window Areas
    remove_drift_token <- function(flag_str) {
      if (is.na(flag_str) || flag_str == "") return(NA_character_)
      tokens <- str_split(flag_str, ";")[[1]] %>% str_replace_all("\\n", " ") %>% str_trim()
      cleaned <- tokens[tokens != "drift" & tokens != ""]
      if (length(cleaned) == 0) return(NA_character_)
      return(paste(cleaned, collapse = "; "))
    }

    # Process Choices Chronologically Per Window
    for (i in seq_len(nrow(windows_df))) {
      s_dt   <- windows_df$start_dt[i]
      e_dt   <- windows_df$end_dt[i]
      d_type <- windows_df$arg_drift_type[i]
      corr_type <- windows_df$arg_correction_type[i]

      in_window <- df_final$DT_round >= s_dt & df_final$DT_round <= e_dt
      in_any_verified_window <- in_any_verified_window | in_window

      if (d_type == "non_resolved") {
        # Mark data as missing and flag status tracks as OMIT
        df_final$mean_analysis[in_window]     <- NA_real_
        df_final$mean_drift_trans[in_window]  <- NA_real_
        df_final$correction_type[in_window]   <- "non_resolved"
        df_final$verification_status[in_window] <- "OMIT"
        df_final$final_status[in_window]        <- "OMIT"
        df_final$pre_post_source[in_window]          <- NA_real_
        } else if( d_type == "unflag" ){
          # user selected "unflag" for a window,
          #remove the drift token from user_flag and set drift to FALSE
          #PASS data if there are no remaining flags
          df_final$mean_drift_trans[in_window] <- df_final$mean_analysis[in_window]
          df_final$correction_type[in_window]  <- "raw"
          df_final$pre_post_source[in_window]          <- NA_real_
          df_final$user_flag[in_window] <- map_chr(df_final$user_flag[in_window], remove_drift_token)
          df_final$drift[in_window] <- FALSE
          df_final$verification_status[in_window] <- if_else(is.na(df_final$user_flag[in_window]) & df_final$verification_status[in_window] != "OMIT", "PASS", df_final$verification_status[in_window])
          df_final$final_status[in_window]        <- if_else(is.na(df_final$user_flag[in_window]) & df_final$final_status[in_window] != "OMIT", "PASS", df_final$final_status[in_window])

        }else {
        #based on user selection, determine which column to use for the final mean_drift_trans
        val_column <- case_when(
          d_type == "linear" & corr_type == "multiplicative"        ~ "linear_mult",
          d_type == "linear" & corr_type == "additive"              ~ "linear_add",
          d_type == "exponential" & corr_type == "multiplicative"  ~ "exp_mult",
          d_type == "exponential" & corr_type == "additive"        ~ "exp_add",
          d_type == "uniform"     & corr_type == "multiplicative"  ~ "uniform_mult",
          d_type == "uniform"     & corr_type == "additive"        ~ "uniform_add",
          d_type == "fitted_linear" & corr_type == "multiplicative" ~ "fitted_mult",
          d_type == "fitted_linear" & corr_type == "additive"       ~ "fitted_add",
          TRUE                      ~ "mean_analysis"
        )

        if (d_type %in% c("linear", "exponential", "uniform", "fitted_linear")) {
          df_final$mean_drift_trans[in_window] <- df_final[[val_column]][in_window]
          df_final$correction_type[in_window]  <- d_type
        }
      }
    }

    outside_unverified_drift <- df_final$drift & !in_any_verified_window
    # if the data is outside a verified window and is flagged as drift, remove the drift token and set the verification_status and final_status to PASS if user_flag is NA

    if (any(outside_unverified_drift)) {
      df_final <- df_final %>%
        mutate(
          user_flag = if_else(outside_unverified_drift, map_chr(user_flag, remove_drift_token), user_flag),
          drift = if_else(outside_unverified_drift, FALSE, drift),
          verification_status = if_else(outside_unverified_drift & is.na(user_flag) & verification_status != "OMIT", "PASS", verification_status),
          final_status        = if_else(outside_unverified_drift & is.na(user_flag) & final_status != "OMIT", "PASS", final_status)
        )
    }
    # Remove Temporary Scenario Columns Before Export
    df_final <- df_final %>%
      select(-any_of(c("linear_add", "linear_mult", "exp_add", "exp_mult",
                       "uniform_add", "uniform_mult", "fitted_add", "fitted_mult")))

    # Export File Storage Sequence
    file_name <- paste0(input$site, "-", input$parameter, ".parquet")
    arrow::write_parquet(df_final, file.path(post_ver_dir, file_name))

    showNotification(paste("Successfully processed and saved:", file_name), type = "message", duration = 5)

    # Register combo as complete and auto-advance ----
    finished_combo <- paste(input$site, input$parameter, sep = "-")
    new_completed  <- unique(c(completed_combos(), finished_combo))
    completed_combos(new_completed)   # triggers the observers above to prune choices

    next_remaining <- setdiff(all_combos, new_completed)

    if (length(next_remaining) == 0) {
      showNotification(
        "All site-parameter combinations have been processed!",
        type = "message", duration = 10
      )
      #quit app
      stopApp()
      message("All site-parameter combinations have been processed! Exiting app.\nProceed to next year's data or publication step")
    } else {
      next_site  <- sapply(strsplit(next_remaining, "-"), `[`, 1)[[1]]
      next_param <- sapply(strsplit(next_remaining, "-"), `[`, 2)[[1]]
      updateSelectInput(session, "site",      selected = next_site)
      updateSelectInput(session, "parameter", selected = next_param)
      showNotification(
        paste("Advancing to:", next_site, "\u2014", next_param),
        type = "message", duration = 4
      )
    }
  })
}
