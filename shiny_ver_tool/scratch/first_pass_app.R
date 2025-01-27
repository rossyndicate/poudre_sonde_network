library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)

# Read data
raw_data <- read_csv("data/raw/synthetic_data.csv")

# UI
ui <- page_navbar(
  title = "Time Series Verification Tool",
  id = "nav",

  # Tab 1: Setup
  nav_panel("Setup",
            layout_columns(
              card(
                card_header("Data Selection"),
                selectInput("verifier", "Verifier",
                            choices = c("SJS", "KW", "JDT")),
                selectInput("site", "Site",
                            choices = unique(raw_data$site)),
                selectInput("parameter", "Parameter",
                            choices = unique(raw_data$parameter)),
                actionButton("view_data", "View Available Data", class = "btn-primary"),
                actionButton("verify_btn", "Verify", class = "btn-success"),
                actionButton("quit_setup", "Save & Quit", class = "btn-danger")
              ),
              card(
                card_header("Data Overview"),
                verbatimTextOutput("date_range")
              )
            )
  ),

  # Tab 2: In-depth Check
  nav_panel("Verification",
            layout_columns(
              col_widths = c(8, 4),
              card(
                card_header("Weekly Plot"),
                plotlyOutput("weekly_plot"),
                checkboxInput("log_scale", "Log Scale"),
                checkboxInput("show_others", "Show Other Sites")
              ),
              card(
                card_header("Verification Controls"),
                selectInput("flag_type", "Flag Type",
                            choices = c("sv", "sv_window", "seasonal_range", "drift", "no_flag")),
                textInput("custom_flag", "Custom Flag"),
                textInput("flag_reason", "Reason"),
                actionButton("submit_flags", "Submit Flags", class = "btn-primary"),
                hr(),
                radioButtons("weekly_decision", "Weekly Decision",
                             choices = c("FF" = "FF",
                                         "PA" = "PA",
                                         "PV" = "PV",
                                         "S" = "S")),
                conditionalPanel(
                  condition = "input.weekly_decision == 'S'",
                  textInput("s_reason", "Reason for S")
                ),
                actionButton("submit_weekly", "Submit Weekly Decision",
                             class = "btn-success"),
                hr(),
                actionButton("final_review", "Move to Final Review",
                             class = "btn-info"),
                actionButton("quit_verify", "Save & Quit", class = "btn-danger")
              )
            )
  ),

  # Tab 3: Final Review
  nav_panel("Final Review",
            card(
              card_header("Final Review Plot"),
              plotlyOutput("final_plot"),
              layout_columns(
                actionButton("back_to_verify", "Back to Verification",
                             class = "btn-warning"),
                actionButton("approve_final", "Approve Changes",
                             class = "btn-success"),
                actionButton("quit_final", "Save & Quit", class = "btn-danger")
              )
            )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store working data
  rv <- reactiveValues(
    working_data = NULL,
    current_week = NULL,
    other_sites_data = NULL,
    brushed_points = NULL
  )

  # Function to save working data
  save_working_data <- function() {
    if (!is.null(rv$working_data)) {
      write_csv(rv$working_data,
                file.path("data/working",
                          paste0(input$site, "_",
                                 input$parameter, "_working.csv")))
      showNotification("Progress saved to working directory", type = "message")
    }
  }

  # Handle quit buttons
  observeEvent(input$quit_setup, {
    save_working_data()
    stopApp()
  })

  observeEvent(input$quit_verify, {
    save_working_data()
    stopApp()
  })

  observeEvent(input$quit_final, {
    save_working_data()
    stopApp()
  })

  # Show date range
  output$date_range <- renderText({
    req(input$site, input$parameter)
    data_subset <- raw_data %>%
      filter(site == input$site, parameter == input$parameter)

    sprintf("Date Range: %s to %s",
            min(data_subset$datetime),
            max(data_subset$datetime))
  })

  # Initialize working data when verify button is clicked
  observeEvent(input$verify_btn, {
    rv$working_data <- raw_data %>%
      filter(site == input$site, parameter == input$parameter)
    rv$current_week <- floor_date(min(rv$working_data$datetime), "week")

    # Load other sites data
    rv$other_sites_data <- raw_data %>%
      filter(site != input$site,
             parameter == input$parameter)

    updateNavbarPage(session, "nav", selected = "Verification")
  })

  # Weekly plot
  output$weekly_plot <- renderPlotly({
    req(rv$working_data, rv$current_week)

    plot_data <- rv$working_data %>%
      filter(datetime >= rv$current_week,
             datetime < rv$current_week + weeks(1))

    # Define color palette for flags
    flag_colors <- c(
      "sv" = "#FF4B4B",         # Red
      "sv_window" = "#4B96FF",  # Blue
      "seasonal_range" = "#FFB74B", # Orange
      "drift" = "#9C4BFF",      # Purple
      "no_flag" = "#808080"            # Grey for no flag/NA
    )

    # Create base plot with custom colors
    p <- plot_ly() %>%
      add_trace(data = plot_data,
                x = ~datetime,
                y = ~value,
                type = "scatter",
                mode = "markers",
                color = ~flag,
                colors = flag_colors,
                source = "weekly_plot",
                selectedpoints = rv$brushed_points,
                legendgroup = "flags") %>%
      layout(title = paste("Week of", rv$current_week),
             xaxis = list(title = "Date"),
             yaxis = list(title = input$parameter),
             dragmode = "select")

    if (input$show_others && !is.null(rv$other_sites_data)) {
      other_sites_weekly <- rv$other_sites_data %>%
        filter(datetime >= rv$current_week,
               datetime < rv$current_week + weeks(1))

      for (other_site in unique(other_sites_weekly$site)) {
        site_data <- other_sites_weekly %>% filter(site == !!other_site)
        p <- p %>% add_trace(data = site_data,
                             x = ~datetime,
                             y = ~value,
                             type = "scatter",
                             mode = "lines",
                             name = other_site,
                             legendgroup = "sites",
                             line = list(width = 1))
      }
    }

    if (input$log_scale) {
      p <- p %>% layout(yaxis = list(type = "log"))
    }

    p
  })

  # Observer for plotly selection events
  observeEvent(event_data("plotly_selected", source = "weekly_plot"), {
    selected_data <- event_data("plotly_selected", source = "weekly_plot")
    if (!is.null(selected_data)) {
      rv$brushed_points <- selected_data$pointNumber + 1
    }
  })

  # Final review plot
  output$final_plot <- renderPlotly({
    req(rv$working_data)

    plot_ly() %>%
      add_trace(data = raw_data %>%
                  filter(site == input$site, parameter == input$parameter),
                x = ~datetime, y = ~value, type = "scatter",
                mode = "markers",
                name = "Raw Data") %>%
      add_trace(data = rv$working_data,
                x = ~datetime, y = ~value, type = "scatter",
                mode = "markers",
                name = "Processed Data") %>%
      layout(title = "Final Review",
             xaxis = list(title = "Date"),
             yaxis = list(title = input$parameter))
  })

  # Handle flag submission
  observeEvent(input$submit_flags, {
    req(rv$working_data, rv$brushed_points)

    # Get current week's data indices
    week_data_indices <- which(
      rv$working_data$datetime >= rv$current_week &
        rv$working_data$datetime < rv$current_week + weeks(1)
    )

    # Update flags for selected points
    selected_indices <- week_data_indices[rv$brushed_points]

    # Determine the flag to apply
    flag_to_apply <- if (input$custom_flag != "") {
      input$custom_flag
    } else {
      input$flag_type
    }

    # Update the flags
    rv$working_data$flag[selected_indices] <- flag_to_apply

    # Clear selection
    rv$brushed_points <- NULL

    # Save working data
    save_working_data()

    # Show notification
    showNotification("Flags updated for selected points", type = "message")
  })

  # Handle weekly decision submission
  observeEvent(input$submit_weekly, {
    rv$current_week <- rv$current_week + weeks(1)
    save_working_data()
  })

  # Handle final approval
  observeEvent(input$approve_final, {
    if (!is.null(rv$working_data)) {
      write_csv(rv$working_data,
                file.path("data/final",
                          paste0(input$site, "_",
                                 input$parameter, "_final.csv")))
      showNotification("Data saved to final directory", type = "success")
    }
  })

  # Navigation handlers
  observeEvent(input$final_review, {
    updateNavbarPage(session, "nav", selected = "Final Review")
  })

  observeEvent(input$back_to_verify, {
    updateNavbarPage(session, "nav", selected = "Verification")
  })
}

shinyApp(ui, server)

