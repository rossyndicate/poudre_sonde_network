ui <- page_navbar(
  title = "Drift Correction Tool",
  id = "tabs",
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light") # Toggle light vs dark mode
  ),
  theme = bs_theme(preset = "bootstrap"),
  nav_panel(
    title = "Drift Correction",
    # Main vertical layout container
    fluidPage(
      fluidRow(
        column(
          width = 12,
          # 1. Top 2/3rds: Plot spans the entire width
          card(
            card_header("Drift Visualization"),
            card_body(
              style = "padding: 0;", # Removes internal padding so the plot fills out nicely
              plotlyOutput("driftPlot", height = "500px") # Gives it a solid, dominating vertical presence
            )
          )
        )
      ),
      # 2. Bottom Row: Split horizontally 10/2
      layout_columns(
        col_widths = c(10, 2),

        # Bottom Left (10): Scrollable window controls
        card(
          card_header("Drift Window Parameter Controls"),
          card_body(
            div(
              style = "max-height: 500px; overflow-y: auto; padding-right: 10px;",
              uiOutput("dynamic_controls")
            )
          )
        ),

        # Bottom Right (2): Navigation dropdowns and action buttons
        card(
          card_header("Configuration & Actions"),
          card_body(
            selectInput("site", "Select Site:",
                        choices = remaining_site_names,
                        selected = remaining_site_names[1]),
            selectInput("parameter", "Select Parameter:", choices = param_choices, selected = param_choices[1]),
            selectizeInput("additional_sites", "Additional Sites:",
                           choices = setdiff(site_choices, remaining_site_names[1]),
                           multiple = TRUE,
                           options = list(maxItems = 2, plugins = "remove_button")),
            hr(),
            actionButton(
              "apply_windows",
              "Apply Window Updates",
              class = "btn-primary",
              style = "width: 100%; font-weight: bold;"
            ),
            actionButton(
              "submit",
              "Submit Final Corrections",
              class = "btn-success",
              style = "width: 100%; font-weight: bold;"
            )
          )
        )
      )
    )
   )
)
