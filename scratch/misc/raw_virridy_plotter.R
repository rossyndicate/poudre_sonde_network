# This script is a basic shiny app to help users verify Virridy sonde data by looking at additional parameters and sites

#Read the data in before running the app to make it faster!
#data <- readRDS("data/virridy_verification/all_data_flagged_complete.RDS")

library(shiny)
library(tidyverse)
library(plotly)
library(scales)


# Define UI
ui <- fluidPage(
  titlePanel("RAW Data Plotter"),
  sidebarLayout(
    sidebarPanel(
      # Allows user to select sites
      selectInput("selected_site", "Select Site(s)",
                  choices = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd", "lincoln", "timberline virridy", "timberline",
                              "prospect virridy", "prospect", "archery virridy", "archery", "boxcreek", "springcreek"),
                  selected = "lincoln", multiple = TRUE),
      # User to select parameters
      selectInput("selected_param", "Select Parameter(s)",
                  choices = c("Depth", "Temperature", "Specific Conductivity", "DO", "Chl-a Fluorescence", "ORP", "Turbidity", "pH", "FDOM Fluorescence"),
                  selected = c("Depth", "Temperature"), multiple = TRUE),
      #select the number of columns to facet by
      radioButtons("col_number", "Number of Columns for Facet Wrap:", choices = list("One" = 1, "Two" = 2), selected = 1),
      # selectInput("log_param", "Select Parameter(s) for Log Scale",
      #             choices = NULL, selected = NULL, multiple = TRUE),
      #select dates
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

  # # Trying to get the user to select which parameters to display in log scale. not working yet
  # observe({
  #   selected_params <- input$selected_param
  #   updateSelectInput(session, "log_param", choices = selected_params)
  # })

  observeEvent(input$plot_button, {


    req(input$selected_site, input$selected_param, input$date_range)
    # Read in data before running script (results in much faster plotting)
    #data <- readRDS("~/Documents/fork_yeah/poudre_sonde_network/data/virridy_verification/all_data_flagged_complete.RDS")
    # Filter the data based on user input

    #grab the site and parameter combos the user selected
    site_param_combos <- crossing(site_select = input$selected_site, param = input$selected_param) %>%
      mutate(combo = paste(site_select, param, sep = "-")) %>%
      pull(combo)

#grab the data for the site and parameter combos the user selected
    all_select_data <- site_param_combos %>%
      keep(~ .x %in% names(data)) %>%
      map_dfr(~ data[[.x]])
#grab start and end dates
    start_date <- as.POSIXct(input$date_range[1])
    end_date <- as.POSIXct(input$date_range[2])

    #trim to just the dates selected
    trim_select_data <- all_select_data %>%
      filter(DT_round >= start_date & DT_round <= end_date)%>%
      #convert depth to ft (easier to see changes than meters)
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

      # # trying to Apply log scale to parameters userr selected
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

