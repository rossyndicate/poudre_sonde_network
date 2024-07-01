
library(shiny)
library(tidyverse)
library(plotly)
library(scales)
#Read the data in before running the app to make it faster!
#data <- readRDS("data/virridy_verification/all_data_flagged_complete.RDS")

# Define UI
ui <- fluidPage(
  titlePanel("RAW Data Plotter"),
  sidebarLayout(
    sidebarPanel(
      #radioButtons("data_group", "Network", choices = list("Virridy", "PWQN"), selected = 1),
      selectInput("selected_site", "Select Site(s)",
                  choices = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd", "lincoln", "timberline virridy", "timberline",
                              "prospect virridy", "prospect", "archery virridy", "archery", "boxcreek", "springcreek"),
                  selected = "lincoln", multiple = TRUE),
      selectInput("selected_param", "Select Parameter(s)",
                  choices = c("Depth", "Temperature", "Specific Conductivity", "DO", "Chl-a Fluorescence", "ORP", "Turbidity", "pH", "FDOM Fluorescence"),
                  selected = c("Depth", "Temperature"), multiple = TRUE),
      radioButtons("col_number", "Number of Columns for Facet Wrap:", choices = list("One" = 1, "Two" = 2), selected = 1),
      # selectInput("log_param", "Select Parameter(s) for Log Scale",
      #             choices = NULL, selected = NULL, multiple = TRUE),
      dateRangeInput("date_range", "Select Date Range", start = "2024-03-15", end = "2024-04-15"),
      actionButton("plot_button", "Plot Data")
    ),
    mainPanel(
      fluidRow(
        column(width = 12, plotlyOutput("data_plot", height = "700px"))
      )
    )
  )
)

# Define Server
server <- function(input, output
                  # , session
                   ) {

  # # Update log scale parameter choices based on selected parameters
  # observe({
  #   selected_params <- input$selected_param
  #   updateSelectInput(session, "log_param", choices = selected_params)
  # })

  observeEvent(input$plot_button, {


    req(input$selected_site, input$selected_param, input$date_range)
    # Read in data before running script (results in much faster plotting)
    #data <- readRDS("~/Documents/fork_yeah/poudre_sonde_network/data/virridy_verification/all_data_flagged_complete.RDS")
    # Filter the data based on user input
    site_param_combos <- crossing(site_select = input$selected_site, param = input$selected_param) %>%
      mutate(combo = paste(site_select, param, sep = "-")) %>%
      pull(combo)

    all_select_data <- tibble()
    for (combo in site_param_combos) {
      if (combo %in% names(data)) {
        data_select <- data[[combo]]
        all_select_data <- bind_rows(all_select_data, data_select)
      }
    }

    start_date <- as.POSIXct(input$date_range[1])
    end_date <- as.POSIXct(input$date_range[2])

    trim_select_data <- all_select_data %>%
      filter(DT_round >= start_date & DT_round <= end_date)%>%
      mutate(mean = case_when(parameter == "Depth"~ mean*3.28084,
                              TRUE ~ mean))

    output$data_plot <- renderPlotly({
      # Plot data and apply log scale to selected parameters
      # Base plot
      p <- ggplot(trim_select_data, aes(x = DT_round, y = mean, color = site)) +
        geom_point() +
        geom_line() +
        facet_wrap(~parameter, scales = "free_y", ncol = as.integer(input$col_number)) +
        labs(x = "Date", y = "Value") +
        theme_bw()

      # # Apply log scale transformation if necessary
      # if (!is.null(input$log_param) && length(input$log_param) > 0) {
      #   p <- p + facet_wrap(~parameter, scales = "free_y", ncol = 1) +
      #     geom_line() +
      #     geom_point() +
      #     scale_y_continuous(trans = "log10") +
      #     labs(x = "Date", y = "Value") +
      #     theme_bw()
      # }

      ggplotly(p)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

