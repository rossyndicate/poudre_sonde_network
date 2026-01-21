#UI Script
ui <- page_navbar(
  title = "Data Processing Pipeline",
  id = "tabs",
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light") #Toggle light vs dark mode
  ),
  theme = bs_theme(preset = "bootstrap"),



  #### Tab 1: Data Selection ####
  nav_panel(
    title = "Data Selection",
    card(
      card_header("Select Your Data"),
      card_body(
        uiOutput("conditional_data_ui")
      )
    )
  ),


  #### Tab 2: Data Verification ####
  nav_panel(
    title = "Data Verification",
    layout_columns(
      col_widths = c(8,4),

      #### Left column ####
      layout_columns(
        col_widths = 12,
        # Main plot (top left)
         card(
        #   card_header(
        #     div(
        #       actionButton("prev_tab", "← Back to Selection", class = "btn-info"),
        #       keys::useKeys(),
        #       keys::keysInput("q_key", "q"),
        #       actionButton("quit_app", "Quit", class = "btn-danger")
        #     )
        #   ),
          card_body(
            plotOutput("main_plot",
                       brush = brushOpts(
                         id = "plot_brush",
                         resetOnNew = FALSE  # This allows multiple brush selections
                       ))
          ),
          card_footer(
            div(
              class = "d-flex justify-content-between gap-3", # Use space-between instead of evenly
              class = "d-flex gap-3",
              selectizeInput("add_sites", "Additional Sites:",
                             choices = available_sites,
                             multiple = TRUE,
                             options = list(plugins = "remove_button"),
                             width = "300px"),
              div(
                class = "d-flex flex-column gap-2",
                materialSwitch(
                  inputId = "remove_omit",
                  label = "Remove Omit",
                  value = FALSE,
                  width = "200px",
                  status = "success"
                ),
                materialSwitch(
                  inputId = "remove_flag",
                  label = "Remove Flag",
                  value = FALSE,
                  width = "200px",
                  status = "success"
                )
              ),
              div(
                class = "d-flex flex-column gap-2",
                materialSwitch(
                  inputId = "incl_thresholds",
                  label = "Thresholds",
                  value = FALSE,
                  width = "200px",
                  status = "success"
                ),
                materialSwitch(
                  inputId = "plot_log10",
                  label = "Log 10",
                  value = FALSE,
                  width = "200px",
                  status = "success"
                ),
                materialSwitch(
                  inputId = "incl_ex_days",
                  label = "Extra Data",
                  value = TRUE,
                  width = "200px",
                  status = "success"
                )
              ),
              actionButton("prev_week","← Previous Week", class = "btn-secondary", style = "width: 200px;"),
              actionButton("next_week","Next Week →", class = "btn-secondary", style = "width: 200px;"),
              actionButton("reset_week", "Reset Data", class = "btn-danger"),
              keys::useKeys(),
                     keys::keysInput("q_key", "q"),
                     actionButton("quit_app", "Quit", class = "btn-danger")
            )
          )
        ),

        #### Weekly decision card (bottom left, shorter height) ####
        card(
          style = "height: 5px; overflow: hidden;",
          # card_header(
          #   h6("Make weekly decision")
          # ),
          card_body(
            div(
              div(
                class = "d-flex gap-5", # Add flexbox with gap between elements
                div(
                  uiOutput("weekly_decision_radio")
                )
              ),
              uiOutput("submit_decision_ui")
            )
          )
        )

        #### end Weekly decision card ####
      ),

      #### Right column ####
      layout_columns(
        col_widths = 12,
        # Sub plots card (top right)
        card(
          card_header(
            h6("Additional Parameters")
          ),
          card_body(
            # Sub parameter selection
            selectizeInput("sub_parameters", "Select Parameters:",
                           choices = available_parameters,
                           multiple = TRUE,
                           options = list(plugins = "remove_button")),
            selectizeInput("sub_sites", "Select Sites:",
                           choices = available_sites,
                           multiple = TRUE,
                           options = list(plugins = "remove_button")),
            div(
              style = "height: 600px; overflow-y: auto;",  # Make this div scrollable
              plotlyOutput("sub_plots", width = "100%", height = "100%")
            )


          )
        ),
        # Data selection card (bottom right)
        card(
          card_header(
            h6("Data Brush")
          ),
          card_body(
            div(
              class = "d-flex align-items-center gap-5",
              radioButtons("brush_action",
                           "Select Action:",
                           choices = c("Accept" = "A",
                                       "Flag" = "F",
                                       "Omit" = "O"),
                           selected = character(0),
                           inline = TRUE),  # This makes the radio buttons horizontal
              actionButton("clear_brushes", "Clear Brushes")
            ),
            selectizeInput("user_brush_flags", "Select Flags:",
                           choices = available_flags,
                           multiple = TRUE,
                           options = list(plugins = "remove_button")),

            # Conditional submit button
            uiOutput("brush_submit_ui")
          )
        )
      )
    )
  ),

  #### Tab 3: Final Data View ####

  nav_panel(
    title = "Finalize Data",
    layout_columns(
      col_widths = c(10, 2),

      # Main Plot Card
      #To Do: Convert to plotly object for better data vis
      card(
        card_body(
          plotlyOutput("final_plot", height = "100%", width = "100%")
        )
      ),

      # Week Selection and Actions Card
      card(
        card_header("Modify Verification"),
        card_body(
          materialSwitch(
            inputId = "remove_omit_finalplot",
            label = "Remove omitted data from plot",
            value = FALSE,
            width = "200px",
            status = "success"
          ),
          materialSwitch(
            inputId = "log10_finalplot",
            label = "Log Transform",
            value = FALSE,
            width = "200px",
            status = "success"
          ),
          selectInput("final_week_selection", "Select Week:", choices = NULL),
          actionButton("goto_final_week", "Return to Selected Week",
                       class = "btn-primary w-100 mb-3"),
          hr(),
          uiOutput("submit_final_button") # Replaced the direct button with a dynamic UI output
        )
      )
    )
  )

)

