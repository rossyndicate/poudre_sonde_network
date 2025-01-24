# This script is a shiny app to help users verify sonde data by looking at additional parameters and sites

#Run the commented line below to load the data

#data <- readRDS("data/virridy_verification/all_data_flagged_complete.RDS")

# Define UI
library(shiny)
library(plotly)
library(lubridate)
library(tidyverse)
library(shinyjs)
library(RColorBrewer)
library(purrr)
library(viridisLite)


# Function from here: https://stackoverflow.com/questions/69289623/avoid-legend-duplication-in-plotly-conversion-from-ggplot-with-facet-wrap
# helps remove duplicates in the legend created by facet wrpping into ggplotly
clean_pltly_legend <- function(.pltly_obj, .new_legend = c()) {
  # Cleans up a plotly object legend, particularly when ggplot is facetted

  assign_leg_grp <- function(.legend_group, .leg_nms) {
    # Assigns a legend group from the list of possible entries
    # Used to modify the legend settings for a plotly object

    leg_nms_rem <- .leg_nms

    parse_leg_nms <- function(.leg_options) {
      # Assigns a .leg_name, if possible
      # .leg_options is a 2-element list: 1 = original value; 2 = remaining options

      if (is.na(.leg_options)) {
        .leg_options
      } else if(length(leg_nms_rem) == 0) {
        # No more legend names to assign
        .leg_options
      } else {
        # Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]

        leg_nm_new
      }

    }

    .legend_group %>%
      map(~ parse_leg_nms(.))

  }

  simplify_leg_grps <- function(.legendgroup_vec) {
    # Simplifies legend groups by removing brackets, position numbers and then de-duplicating

    leg_grp_cln <-
      map_chr(.legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))

    modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)

  }

  pltly_obj_data <-
    .pltly_obj$x$data

  pltly_leg_grp <-
    # pltly_leg_grp is a character vector where each element represents a legend group. Element is NA if legend group not required or doesn't exist
    pltly_obj_data%>%
    map(~ pluck(., "legendgroup")) %>%
    map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
    # Elements where showlegend = FALSE have legendgroup = NULL.

    simplify_leg_grps() %>%

    assign_leg_grp(.new_legend)

  pltly_obj_data_new <-
    pltly_obj_data %>%
    map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))
  # i.e. showlegend set to FALSE when is.na(pltly_leg_grp), TRUE when not is.na(pltly_leg_grp)

  .pltly_obj$x$data <- pltly_obj_data_new

  .pltly_obj

}

### Setup colors for plotting flags
# To fix later on: This was an attempt to standardize the colors for all the flag used, but since there are 130 the colors are hard to distinguish
# We could try setting scales for specific colors based on the most important flags (Site Visit, sensor unsubmerged, frozen, etc)

# This should be re run if new flags are added
# sites <- c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
#            "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs")
# params <- c("Depth", "Temperature", "Specific Conductivity", "DO", "Chl-a Fluorescence", "ORP", "Turbidity", "pH", "FDOM Fluorescence")
#
# site_param_combos <- crossing(site_select = sites, param = params) %>%
#   mutate(combo = paste(site_select, param, sep = "-")) %>%
#   pull(combo)
#
# all_data <- site_param_combos %>%
#   keep(~ .x %in% names(data)) %>%
#   map_dfr(~ data[[.x]])%>%
#   filter(grepl("missing data", flag) == FALSE | is.na(flag))
#
# flag_color_combo <- tibble(flag = unique(all_data$flag)) %>%
#   # Get viridis color palette and assign a hex color for each unique flag
#   mutate(flag_fill = viridisLite::viridis(length(flag), option = "plasma", begin = 0, end = 1, direction = -1),
#          flag_fill = if_else(is.na(flag), "grey", flag_fill))
# #write_csv(flag_color_combo, "data/virridy_verification/flag_color_combo.csv")
#
# rm(all_data)



site_color_combo <- tibble(site = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                           color = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",
                                     "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44","#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))





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
                        selected = "legacy"),
            selectInput("selected_site", "Select additional Site(s)",
                        choices = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                        selected = c("tamasag", "timberline"), multiple = TRUE),
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
            # Add daily average line?
            radioButtons("avg_data", "Add averaged data line:", choices = list("Daily" = "day","12 Hour" = "12 hours", "4 Hour" = "4 hours",
                                                                               "Hourly" = "1 hour", "No" = "no"), selected = "no"),

            # Select dates
            dateRangeInput("date_range", "Select Date Range", start = "2023-03-15", end = "2023-05-31"),
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


    req(input$site_oi,  input$selected_param, input$date_range, input$avg_data)

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
    #%>%
      #if flag is NA, change it to "NA"
     # mutate(flag = ifelse(is.na(flag), "No Flag", flag))




    output$data_plot <- renderPlotly({

      ## To do: get color scales to work sites and flags
      ## ggnewscale worked on a static plot by doesn't work with plotly and lines get dropped :(

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
          scale_color_manual(values = setNames(site_color_combo$color, site_color_combo$site)) +
          facet_wrap(~parameter, scales = "free_y", ncol = as.integer(input$col_number))+
          labs(x = "Date", y = "Value") +
          theme_bw()
      }


        if(input$flags_TF == "yes"){
           flagged_plot <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, fill = flag), shape = 21, stroke = 0) +
             #To do: standardize colors for all flags across all sites
             #scale_fill_manual(values = setNames(flag_color_combo$flag_fill, flag_color_combo$flag)) +
            scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9, na.value = "#A0A0A0") +
             labs(fill = "Site of interest w flags (Points)", color = "Additional Sites (Lines)")


        } else {
          flagged_plot <- base_plot +
            geom_point(data = trim_select_data %>% filter(site == input$site_oi),
                       aes(x = DT_round, y = mean, fill = site), shape = 21, stroke = 0) +
            scale_fill_manual(values = "#A0A0A0")+
            labs(fill = "Site of interest (Points)", color = "Additional Sites (Lines)")
        }

  ### add average line to plot ####

      if(input$avg_data != "no"){

        trim_select_avg_data <- trim_select_data %>%
          filter(site == input$site_oi) %>%
          mutate(date_round = round_date(DT_round, unit = input$avg_data)) %>%
          group_by(date_round, parameter, site) %>%
          summarise(median = median(mean, na.rm = TRUE)) %>%
          ungroup()


        avged_plot <- flagged_plot+
          geom_line(data = trim_select_avg_data,
                    aes(x = date_round, y = median), color = "black", linetype = "dashed")

      }else{
        avged_plot <- flagged_plot
      }


### Log transformed plot ####
    if(input$transformation == "log10()"){

          # Flags + log 10
          final_plot <- avged_plot +
            scale_y_continuous(trans = "log10",
                               breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000),
                               # Specify breaks at 1, 10, and 100
                               labels = c("0.001", "0.01", "0.1", "1", "10", "100", "1000"))

        } else {
          #No log transformation
          final_plot <- avged_plot
        }




      gp <- ggplotly(final_plot)%>%
        clean_pltly_legend()


    })

  })
}

# Run the application
shinyApp(ui = ui, server = server)

