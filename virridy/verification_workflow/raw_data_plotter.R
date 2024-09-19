# This script is a basic shiny app to help users verify sonde data by looking at additional parameters and sites

#Run the commented line below to load the data

#data <- readRDS("data/virridy_verification/all_data_flagged_complete.RDS")

# Define UI
library(shiny)
library(plotly)
library(lubridate)
library(tidyverse)
library(shinyjs)  # Library to allow JavaScript functionality

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs

  titlePanel("RAW Data Plotter"),

  # Button to toggle sidebar visibility
  actionButton("toggle_sidebar", "Toggle Sidebar"),

  # Main layout with a conditional sidebar
  fluidRow(
    # Sidebar Panel
    column(
      width = 10,
      div(id = "sidebar",
          sidebarPanel(
            # Allows user to select sites
            selectInput("site_oi", "Select primary Site",
                        choices = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                        selected = "lincoln"),
            selectInput("selected_site", "Select additional Site(s)",
                        choices = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                        selected = c("legacy", "timberline"), multiple = TRUE),
            # User to select parameters
            selectInput("selected_param", "Select Parameter(s)",
                        choices = c("Depth", "Temperature", "Specific Conductivity", "DO", "Chl-a Fluorescence", "ORP", "Turbidity", "pH", "FDOM Fluorescence"),
                        selected = c("Depth", "Temperature"), multiple = TRUE),
            # Select the number of columns to facet by
            radioButtons("col_number", "Number of Columns for Facet Wrap:", choices = list("One" = 1, "Two" = 2), selected = 1),
            # display flags?
            radioButtons("flags_TF", "Display flags?", choices = list("Yes" = "yes", "No" = "no"), selected = "yes"),
            # Select whether you want to transform the data
            radioButtons("transformation", "Transformation of Data:", choices = list("None", "log10()"), selected = "None"),
            # Select dates
            dateRangeInput("date_range", "Select Date Range", start = "2023-03-15", end = ymd(Sys.Date())),
            actionButton("plot_button", "Plot Data")
          )
      )
    ),

    # Main Panel for the plot
    column(
      width = 12,
      div(id = "main_panel",
          plotlyOutput("data_plot", height = "800px")
      )
    )
  )
)



# Define Server
server <- function(input, output) {

  # Keep track of whether the sidebar is visible
  sidebar_visible <- reactiveVal(TRUE)

  # Toggle the sidebar visibility and resize the plot
  observeEvent(input$toggle_sidebar, {
    sidebar_visible(!sidebar_visible())  # Toggle the visibility state

    # Dynamically resize the plot to fill the space
    if (sidebar_visible()) {
      show("sidebar")
      runjs("$('#main_panel').removeClass('col-12').addClass('col-9');")
    } else {
      hide("sidebar")
      runjs("$('#main_panel').removeClass('col-9').addClass('col-12');")
    }
  })



  observeEvent(input$plot_button, {


    req(input$site_oi,  input$selected_param, input$date_range)
    # Read in data before running script (results in much faster plotting)
    #data <- readRDS("~/Documents/fork_yeah/poudre_sonde_network/data/virridy_verification/all_data_flagged_complete.RDS")
    # Filter the data based on user input

    #grab the site and parameter combos the user selected

    sites_selected <- c(input$site_oi, input$selected_site)

    site_param_combos <- crossing(site_select = sites_selected, param = input$selected_param) %>%
      mutate(combo = paste(site_select, param, sep = "-")) %>%
      pull(combo)

    #grab the data for the site and parameter combos the user selected
    all_select_data <- site_param_combos %>%
      keep(~ .x %in% names(data)) %>%
      map_dfr(~ data[[.x]])
    #grab start and end dates
    start_date <- as.POSIXct(input$date_range[1])%>% force_tz("MST")
    end_date <- as.POSIXct(input$date_range[2])%>% force_tz("MST")

    # # Do any transformations...
    # if(input$transformation == "None"){
    #   #trim to just the dates selected
    trim_select_data <- all_select_data %>%
      filter(DT_round >= start_date & DT_round <= end_date)%>%
      #convert depth to ft (easier to see changes than meters)
      mutate(mean = case_when(parameter == "Depth"~ mean*3.28084,
                              TRUE ~ mean))%>%
      #remove rows where "missing data" is included in the flag string but still include rows with NA
      filter(grepl("missing data", flag) == FALSE | is.na(flag))


    #Create the baseplot that will be customized below
    if(length(unique(trim_select_data$site)) == 1 ){
      #when only the primary site is selected or when additional sites are selected but have no data
      base_plot <- ggplot(data = trim_select_data %>% filter(site == input$site_oi),
                          aes(x = DT_round, y = mean)) +
        facet_wrap(~parameter, scales = "free_y", ncol = as.integer(input$col_number)) +
        labs(x = "Date", y = "Value") +
        theme_bw()
   } else {
      #when additional sites are selected and have data
      base_plot <- ggplot() +
        geom_line(data = trim_select_data %>% filter(site != input$site_oi), aes(x = DT_round, y = mean, color = site))+
        facet_wrap(~parameter, scales = "free_y", ncol = as.integer(input$col_number)) +
        labs(x = "Date", y = "Value") +
        theme_bw()
    }

    output$data_plot <- renderPlotly({

    if(input$transformation == "None"){

        # Base plot
        if(input$flags_TF == "yes"){
          p <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, colour = flag)) +
            labs(colour = "Flags on data", color = "Additional Sites")


        } else {
          p <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, colour = site)) +
            labs(colour = "Site OI", color = "Additional Sites")

        }

    }
### Log transformed plot ####
    if(input$transformation == "log10()"){

        # Plot data and apply log scale to selected parameters
        if(input$flags_TF == "yes"){

          # Flags + log 10
          p <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, colour = flag)) +

            labs(colour = "Flags on data", color = "Additional Sites")+
            scale_y_continuous(trans = "log",
                               breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                               # Specify breaks at 1, 10, and 100
                               labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))

        } else {

          #Only log 10

          p <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, colour = site)) +

            scale_y_continuous(trans = "log",
                               breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),  # Specify breaks at 1, 10, and 100
                               labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))+
            labs(colour = "Site OI", color = "Additional Sites")
        }


    }

      ggplotly(p)
    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)

