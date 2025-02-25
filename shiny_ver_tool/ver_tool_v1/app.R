library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(lubridate)
library(here)
library(ggpubr)
library(gridExtra)
library(plotly)
library(keys)
library(patchwork)
library(digest)
library(fs)
library(shinyFiles)
library(shinyWidgets)
options(shiny.maxRequestSize = 1000 * 1024^2)

#source(here("shiny_ver_tool", "ver_tool_v1", "R", "load_data.R"))
#source(here("shiny_ver_tool", "ver_tool_v1", "R", "selectors.R"))

#load in data



##### Colors + parameters #####

site_color_combo <- tibble(site = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                           color = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",
                                     "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44","#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))

# final_status_colors <- c("PASS" = "#5e9c4d",
#                            "FLAGGED" = "#d88e2f",
#                            "OMIT" = "#ba0012")
#
# final_status_colors <- c("PASS" = "#008744",
#                          "FLAGGED" = "#ffa700",
#                          "OMIT" = "#d62d20")

final_status_colors <- c("PASS" = "#008a18",
                         "OMIT" = "#ff1100",
                         "FLAGGED" = "#ff8200")


#TODO: Create function that actually looks for available sites/parameters in the data
available_parameters <- c("Specific Conductivity", "Temperature", "pH",
                          "Turbidity", "DO", "Depth")
available_sites <- site_color_combo$site

###### End Helper Functions ######


# UI Definition
ui <- page_navbar(
  #useShinyjs(),
#To Do: remove header to save space? it shouldnt need to be used by users
  title = "Data Processing Pipeline",
  id = "tabs",
  theme = bs_theme(version = 5, bootswatch  = "zephyr"),

# #Tab 0: For future version to be shared open source
# nav_panel(
#   title = "Data Setup",
#   card(
#     card_header("Data Upload"),
#     card_body(
#       fileInput("upload", "Upload a file", accept = c(".zip")),
#       tableOutput("uploaded_files")
#     )
#   )
# ),


  #### Tab 1: Data Selection ####
  nav_panel(
    title = "Data Selection",
    card(
      card_header("Select Your Data"),
      card_body(
        # Directory selection
        radioButtons("directory", "Choose Directory:",
                     choices = c("pre", "int"),
                     selected = "pre",
                     inline = TRUE),
        selectInput("user", "Select User:",
                    choices = c("SJS", "JDT", "CLM", "AS"), selected = "SJS"),
        # Site selection
        selectInput("site", "Select Site:",
                    choices = NULL),

        # Main parameter selection (single selection)
        selectInput("parameter", "Select Parameter:",
                    choices = NULL),

        actionButton("load_data", "Load Data", class = "btn-primary")
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
          card_header(
            div(
              actionButton("prev_tab", "← Back to Selection", class = "btn-info"),
              keys::useKeys(),
              keys::keysInput("q_key", "q"),
              actionButton("quit_app", "Quit", class = "btn-danger")
            )
          ),
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
                selectInput("add_sites", "Additional Sites:",
                            choices = available_sites,
                            multiple = TRUE,
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
                materialSwitch(
                  inputId = "incl_thresholds",
                  label = "Thresholds",
                  value = FALSE,
                  width = "200px",
                  status = "success"
                ),
                actionButton("prev_week","← Previous Week", class = "btn-secondary", style = "width: 200px;"),
                actionButton("next_week","Next Week →", class = "btn-secondary", style = "width: 200px;"),
                actionButton("reset_week", "Reset Data", class = "btn-danger")
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
              selectInput("sub_parameters", "Select Parameters:",
                          choices = available_parameters,
                          multiple = TRUE),
              selectInput("sub_sites", "Select Sites:",
                          choices = available_sites,
                          multiple = TRUE),
              div(
                style = "height: 600px; overflow-y: auto;",  # Make this div scrollable
                plotOutput("sub_plots", width = "100%", height = "100%")
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

            selectInput("user_brush_flags", "Select Flags:",
                        choices = c("sv" = "sv",
                                    "suspect data" = "suspect",
                                    "sensor malfunction" = "malfunction",
                                    "drift" = "drift"),
                        multiple = TRUE),

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
    col_widths = c(8, 4),

    # Main Plot Card
#To Do: Convert to plotly object for better data vis
    card(
      card_header("Final Data Overview"),
      card_body(
        plotlyOutput("final_plot", height = "100%", width = "100%")
      )
    ),

    # Week Selection and Actions Card
    card(
      card_header("Modify Verification"),
      card_body(
        selectInput("final_week_selection", "Select Week:", choices = NULL),
        actionButton("goto_final_week", "Return to Selected Week",
                     class = "btn-primary w-100 mb-3"),
        hr(),
        actionButton("submit_final", "Submit Final Changes",
                     class = "btn-success w-100")
      ),
      card_footer(
        div(
          class = "d-flex justify-content-between",
          div(
            checkboxInput("remove_omit_finalplot", "Remove omitted data from plot", value = FALSE),
            style = "margin: auto"
          )
        )
      )
    )
  )
)

)


#### Server ####

# Server Definition
server <- function(input, output, session) {
#### Not used in internal version ####
#
#
#   output$uploaded_files <- renderTable({
#     req(input$upload)
#
#     # Get the uploaded file
#     zip_file <- input$upload$datapath
#     # Create a unique temporary directory for this upload
#     temp_dir <- file.path(tempdir(), paste0("upload_", format(Sys.time(), "%Y%m%d_%H%M%S")))
#     dir.create(temp_dir)
#
#     # Unzip the file to the temporary directory
#     unzip(zip_file, exdir = temp_dir)
#     files <- list.files(
#       path = temp_dir,
#       recursive = TRUE,
#       full.names = TRUE
#     )%>%
#     #remove the temp_dir from the file path
#     gsub(paste0(temp_dir, "/data/"), "", .)
#     # Remove metadata folder entries
#     # Extract folder and site-parameter combinations
#     file_info <- data.frame(
#       Folder = dirname(files),
#       Site_Parameter = basename(files) %>%
#         tools::file_path_sans_ext()  # Remove .csv extension
#     ) %>%
#       # Remove any empty folders or "."
#       filter(Folder != "." & Folder != "" ) %>%
#       filter(!grepl("meta", Folder)) %>%
#       # Sort by folder and site-parameter
#       arrange(Folder, Site_Parameter)%>%
#       # Split Site_Parameter into Site and Parameter
#       tidyr::separate(Site_Parameter,
#                       into = c("Site", "Parameter"),
#                       sep = "-",
#                       remove = TRUE)%>%
#       # Pivot wider to make folders as columns
#       pivot_wider(
#         names_from = Folder,
#         values_from = Parameter,
#         values_fn = function(x) paste(unique(x), collapse = ","),
#         values_fill = ""
#       ) %>%
#       arrange(Site)
#
#
# file_info
#
#     })
#### Not used in internal version ####






#### Reactive values ####
  data <- reactiveVal(NULL)
  current_week <- reactiveVal(NULL) #controlled by next/prev week buttons, and submit weekly decision
  selected_data <- reactiveVal(NULL) # This is essentially site param df
  all_datasets <- reactiveVal(NULL) # List of all datasets and is used in generating sub plots?
  brush_active <- reactiveVal(FALSE) #internal shiny tracker for brush tool


  #### Data Selection functions ####
  # Initialize data directories and load datasets
  observe({
    paths <- load_data_directories()
    datasets <- load_all_datasets(paths)
    all_datasets(datasets)
  })

  # Update site choices when directory changes
  observe({
    req(input$directory, all_datasets())
    sites <- get_sites(all_datasets(), input$directory)
    updateSelectInput(session, "site",
                      choices = sites) #based on directory
  })

  # Update parameter choices when site changes
  observe({
    req(input$directory, input$site, all_datasets())
    parameters <- get_parameters(all_datasets(), input$directory, input$site)
    updateSelectInput(session, "parameter",
                      choices = parameters) #based on site and directory
  })

  # Show/hide and update sub parameters UI based on main parameter selection
  observe({
    req(input$parameter)

    # Get auto-selected parameters for the chosen parameter
    auto_params <- get_auto_parameters(input$parameter)

    # Update sub-parameters selection
    updateSelectInput(session, "sub_parameters",
                      choices = available_parameters,
                      selected = auto_params)
  })
  # Show/hide and update additional sites UI based on site selection
  observe({
    req(input$site)

    # Get auto-selected parameters for the chosen parameter
    auto_sites <- relevant_sonde_selector(input$site)

    # Update sub-parameters selection
    updateSelectInput(session, "sub_sites",
                      choices = available_sites,
                      selected = auto_sites)
    updateSelectInput(session, "add_sites",
                      choices = available_sites,
                      selected = auto_sites)

  })


  # Load data when button is clicked
  observeEvent(input$load_data, {
    req(input$directory, input$site, input$parameter, input$sub_parameters, all_datasets())

    # Get the site-parameter name
    site_param_name <- paste0(input$site, "-", input$parameter)

    # Get the appropriate dataset based on directory selection
    datasets <- all_datasets()

    working_data <- if(input$directory == "pre") {
      datasets$pre_verification_data
    } else {
      datasets$intermediary_data
    }
    # Try to get the specific dataset
    tryCatch({
      site_param_df <- working_data[[site_param_name]]

      if (is.null(site_param_df)) {
        stop(paste("Dataset", site_param_name, "not found"))
      }

      sel_data <- working_data[[site_param_name]]

#TODO: check additional columns (verification status, etc ) to match with old ver system
      processed <- sel_data%>%
        #FOR TESTING PURPOSES ONLY
        mutate(user_flag = flag,
               omit = FALSE,
               user = NA,
               final_status = NA,
               week_decision = NA,
               is_verified = NA)


      # Store the processed data
      selected_data(processed)

      # Set initial week
#TODO: This should update to first week with unverified data if possible
      current_week(min(processed$week))

    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
    })

    #Move to next tab
    updateTabsetPanel(session, inputId = "tabs", selected = "Data Verification")

  })

  #### Data Verification functions ####
# Previous Tab
  observeEvent(input$prev_tab, {
    updateNavbarPage(session, "tabs", selected = "Data Selection")

#Q: Should this update the data files or no?
  })

## Week navigation handlers
  observeEvent(input$prev_week, {
    req(selected_data())
    weeks <- unique(selected_data()$week)
    current <- current_week()
    idx <- which(weeks == current)
    if (idx > 1) {
      current_week(weeks[idx - 1])
    }
  })
  # Go to next week
  observeEvent(input$next_week, {
    req(selected_data())
    weeks <- unique(selected_data()$week)
    current <- current_week()
    idx <- which(weeks == current)
    if (idx < length(weeks)) {
      current_week(weeks[idx + 1])
    }else{
      showNotification(
        "No more weeks to verify. Click Final Verification to see unverified weeks",
        type = "warning"
      )
      current <- current_week()
      idx <- which(weeks == current)
    }
  })

## Main plot
  output$main_plot <- renderPlot({
    req(selected_data(), current_week(), all_datasets())

    all_data <- all_datasets()[["all_data"]]
    pre_verification_data <- all_datasets()[["pre_verification_data"]]
    intermediary_data <- all_datasets()[["intermediary_data"]]
    verified_data <- all_datasets()[["verified_data"]]

    week_data <- selected_data() %>%
      filter(week == current_week())

    year_week <- paste0(as.character(year(min(week_data$DT_round))) ," - ", as.character(min(week(week_data$DT_round))))
    flag_day <- min(week_data$DT_round)


#This isn't being sourced correctly in the shiny app
    #picking the correct name of the data frame
    retrieve_relevant_data_name <- function(df_name_arg, year_week_arg = NULL) {

      if (df_name_arg %in% names(verified_data) & any(year_week_arg %in% verified_data[[df_name_arg]]$y_w)) {
        return("verified_data")
      }
      if (df_name_arg %in% names(intermediary_data) & any(year_week_arg %in% intermediary_data[[df_name_arg]]$y_w)) {
        return("intermediary_data")
      }
      if (df_name_arg %in% names(pre_verification_data) & any(year_week_arg %in% pre_verification_data[[df_name_arg]]$y_w)) {
        return("pre_verification_data")
      }
      if (df_name_arg %in% names(all_data) & any(year_week_arg %in% all_data[[df_name_arg]]$y_w)) {
        return("all_data")
      }

    }

    # site = "timberline"
    # parameter_arg = "Temperature"
    plot_filter <- input$add_sites
    # Get the relevant sonde data
    relevant_sondes <- map(plot_filter, ~ {
      sonde_name <- paste0(.x, "-", input$parameter)
      data_source <- NULL
      sonde_df <- NULL

      # Determine which directory to pull data from
      tryCatch({
        data_source <- retrieve_relevant_data_name(sonde_name, year_week)
        # cat("Data for",sonde_name,"will be pulled from",data_source,"\n")
      }, error = function(err) {
        cat("Data for",sonde_name,"not found.\n")
        return(NULL)  # Return NULL if data source can't be determined
      })

      # Only try to pull in the data if data_source was successfully determined
      if (!is.null(data_source)) {
        tryCatch({
          sonde_df <- get(data_source)[[sonde_name]] %>%
            filter(y_w == year_week)
        }, error = function(err) {
          #cat("Sonde", sonde_name, "not found.\n")
          return(NULL)  # Return NULL if sonde data can't be retrieved
        })
      }

      # Only return a list if both data_source and sonde_df are available
      if (!is.null(data_source) & !is.null(sonde_df)) {
        return(list(sonde_df = sonde_df, data_source = data_source))
      } else {
        return(NULL)  # Return NULL if either data_source or sonde_df is NULL
      }

    })

    # Remove any NULL results from the list
    relevant_sondes <- compact(relevant_sondes)

     # append site_df to relevant sonde list, clean list, and bind dfs
     # to find plot info
     relevant_dfs <- map(relevant_sondes, ~.x[[1]])
     week_plot_data <- append(relevant_dfs, list(week_data)) %>% #
       keep(~ !is.null(.)) %>%
       keep(~ nrow(.)>0) %>%
       bind_rows() %>%
       arrange(day)






    # Check the decision and create appropriate plot
    if (input$weekly_decision != "s") {


      #TODO: Update matrix with final decisions
      week_choice_data <- week_data %>%
        mutate(
          final_decision = case_when(
            #AA:Pass all data
            input$weekly_decision == "aa"  ~ "PASS",
            #ANO: Accept Non Omit
            input$weekly_decision == "ano" & !omit ~ "PASS", # pass data that is not user select omit
            #KF: Keep Flags, data becomes flagged but kept in dataset (mildly sus)
            input$weekly_decision == "kf" & is.na(user_flag) & !omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "kf" & !is.na(user_flag) & !omit ~ "FLAGGED", # tag data that is flagged
            #OF: Omit Flagged
            input$weekly_decision == "of" & is.na(user_flag) & !omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "of" & !is.na(user_flag) & !omit ~ "OMIT", # omit data that is flagged
            #OA: Omit All
            input$weekly_decision == "oa"  ~ "OMIT",
            # Omit any user selected omit data (assuming AA was not the choice)
            input$weekly_decision != "aa" & omit ~ "OMIT"))
      #Remove omitted data (user or from weekly decision)
      if (input$remove_omit) {
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "OMIT")
      }
      if(input$remove_flag){
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "FLAGGED")
      }


      p <- ggplot(week_choice_data, aes(x = DT_round)) +
        map(relevant_sondes, function(sonde_data) {
          add_data <- sonde_data[[1]]
          data_source <- sonde_data[[2]]

          y_column <- ifelse(data_source %in% c("all_data", "pre_verification_data"), "mean", "mean_verified")

          geom_line(data = add_data, aes(x = DT_round, y = .data[[y_column]], color = site), linewidth = 1)
        }) +
        geom_point(aes(y = mean, fill = final_decision),shape = 21, stroke = 0, size = 2)+
        scale_fill_manual(values = final_status_colors)+
        scale_color_manual(values = setNames(site_color_combo$color, site_color_combo$site))+
        labs(
          title = paste0(str_to_title(input$site), " ", input$parameter, " (", format(flag_day, "%B %d, %Y"), ")"),
          x = "Date",
          y = input$parameter,
          fill = "Preview of Final Decision",
          color = "Sites" )+
      theme_bw(base_size = 14)

      if(input$incl_thresholds){
        p <- add_threshold_lines(plot = p,
                                         plot_data = week_plot_data,
                                         site_arg = input$site,
                                         parameter_arg = input$parameter)
      }

      p
    } else {
      #TO DO: Swap with create weekly plot function call, adding in other sites, etc
      if(input$remove_omit){
        week_data <- week_data %>%
          filter(!omit)
      }
      if(input$remove_flag){
        week_data <- week_data %>%
          filter(is.na(user_flag))
      }

      p <-ggplot(week_data, aes(x = DT_round)) +
        map(relevant_sondes, function(sonde_data) {
          add_data <- sonde_data[[1]]
          data_source <- sonde_data[[2]]

          y_column <- ifelse(data_source %in% c("all_data", "pre_verification_data"), "mean", "mean_verified")

          geom_line(data = add_data, aes(x = DT_round, y = .data[[y_column]], color = site), linewidth = 1)
        }) +
        geom_point(aes(y = mean, fill = user_flag),shape = 21, stroke = 0, size = 2)+
        #Add Omitted data in red
        geom_point(data = week_data %>%filter(omit == TRUE),aes(y = mean),shape = 21, stroke = 0, size = 2, fill = "#ff1100")+
      scale_color_manual(
          name = "Sites",
          values = setNames(site_color_combo$color, site_color_combo$site)) +
        # Scale for points (flag-based colors)
        scale_fill_viridis_d(
          name = "Flags",
          option = "plasma",
          begin = 0.1,
          end = 0.9,
          na.value = "grey" )+
        labs(
          title = paste0(str_to_title(input$site), " ", input$parameter, " (", format(flag_day, "%B %d, %Y"), ")"),
          x = "Date",
          y = input$parameter,
          color = "Sites",
          fill = "Flags")+
        theme_bw(base_size = 14)

      # Check if there are any brushed areas
      if(length(brushed_areas()) > 0) {
        #browser()
        # Create a data frame of all brush boundaries
        brush_boxes <- map_dfr(
          brushed_areas()[seq(1, length(brushed_areas()), by = 2)], #for some reason this likes to duplicate values so just grabbing half of them
          ~data.frame(
            xmin_DT = .x$brush_dt_min,
            xmax_DT = .x$brush_dt_max,
            ymin_mean = .x$brush_mean_min,
            ymax_mean = .x$brush_mean_max
          )
        )
        #
        #   # Add rectangles for all brushed areas
        p <- p +
          geom_rect(data = brush_boxes,
                    aes(xmin = xmin_DT,
                        xmax = xmax_DT,
                        ymin = ymin_mean,
                        ymax = ymax_mean),
                    fill = NA,
                    color = "blue",
                    alpha = 0.3, inherit.aes = F)

      }
      #create plot
      if(input$incl_thresholds){
        p <- add_threshold_lines(plot = p,
                                 plot_data = week_plot_data,
                                 site_arg = input$site,
                                 parameter_arg = input$parameter)
      }
      p
    }
  })

## Sub plots output
  output$sub_plots <- renderPlot({
    req(all_datasets(), current_week(), input$site, input$sub_parameters, input$sub_sites)


    all_data <- all_datasets()[["all_data"]]
    pre_verification_data <- all_datasets()[["pre_verification_data"]]
    intermediary_data <- all_datasets()[["intermediary_data"]]
    verified_data <- all_datasets()[["verified_data"]]

    week_data <- selected_data() %>%
      filter(week == current_week())

    year_week <- paste0(as.character(year(min(week_data$DT_round))) ," - ", as.character(min(week(week_data$DT_round))))
    #flag_day <- min(week_data$DT_round)
    all_sub_sites <- c(input$site, input$sub_sites)
    #picking the correct name of the data frame
    retrieve_relevant_data_name <- function(df_name_arg, year_week_arg = NULL) {

      if (df_name_arg %in% names(verified_data) & any(year_week_arg %in% verified_data[[df_name_arg]]$y_w)) {
        return("verified_data")
      }
      if (df_name_arg %in% names(intermediary_data) & any(year_week_arg %in% intermediary_data[[df_name_arg]]$y_w)) {
        return("intermediary_data")
      }
      if (df_name_arg %in% names(pre_verification_data) & any(year_week_arg %in% pre_verification_data[[df_name_arg]]$y_w)) {
        return("pre_verification_data")
      }
      if (df_name_arg %in% names(all_data) & any(year_week_arg %in% all_data[[df_name_arg]]$y_w)) {
        return("all_data")
      }

    }


    # Create individual plots for each sub parameter
    plots <- map(input$sub_parameters, function(param) {

      all_sub_plot_data <- map_dfr(all_sub_sites, function(sub_site) {

        # Get the relevant sonde data
        relevant_sondes <- map(sub_site, ~ {
          sonde_name <- paste0(.x, "-", param)
          data_source <- NULL
          sonde_df <- NULL

          # Determine which directory to pull data from
          tryCatch({
            data_source <- retrieve_relevant_data_name(sonde_name, year_week)
            # cat("Data for",sonde_name,"will be pulled from",data_source,"\n")
          }, error = function(err) {
            #cat("Data for",sonde_name,"not found.\n")
            return(NULL)  # Return NULL if data source can't be determined
          })

          # Only try to pull in the data if data_source was successfully determined
          if (!is.null(data_source)) {
            tryCatch({
              sonde_df <- get(data_source)[[sonde_name]] %>%
                filter(y_w == year_week)
#TODO: correctly filter omit/flag data as needed by data source
            }, error = function(err) {
              #cat("Sonde", sonde_name, "not found.\n")
              return(NULL)  # Return NULL if sonde data can't be retrieved
            })
          }

          # Only return a list if both data_source and sonde_df are available
          if (!is.null(data_source) & !is.null(sonde_df)) {
            return(sonde_df)
          } else {
            return(NULL)  # Return NULL if either data_source or sonde_df is NULL
          }

        })

        # Remove any NULL results from the list
        relevant_sondes <- compact(relevant_sondes)
        if(length(relevant_sondes) == 0) {
          return(NULL)
        }else{
          relevant_sondes
        }

      })


      # Create plot
      p <- ggplot() +
        # Add main site as grey points
        geom_point(data = all_sub_plot_data%>%filter(site == input$site),
                   aes(x = DT_round, y = mean),
                   color = "grey40",
                   size = 2) +
        # Add sub sites as colored lines
        geom_line(data = all_sub_plot_data%>%filter(site %in% input$sub_sites),
                  aes(x = DT_round, y = mean, color = site),
                  linewidth = 1) +
        scale_color_manual(values = setNames(site_color_combo$color, site_color_combo$site)) +
        labs(x = "Date",
             y = param,
             #title = param,
             color = "Sites") +
        theme_minimal() +
        theme(axis.title.x = element_blank())


      return(p)
    }) %>%
      compact()

    if (length(plots) > 0) {
      # Calculate height for each plot: 600px / number of plots
     # plot_height <- 800 / length(plots)
      plots[[1]] <- plots[[1]] +
        theme(axis.text.x = element_text())
      # Add x-axis label to last plot only
      plots[[length(plots)]] <- plots[[length(plots)]] +
        theme(axis.title.x = element_text(),
              axis.text.x = element_text())

      # Combine plots with specific heights
      wrap_plots(plots, ncol = 1)+
        #heights = rep(plot_height/800, length(plots))) +
        plot_layout(guides = "collect") &
        theme(legend.position='top')
    }


  })

#### Brush Tools ####

  # Create a reactive value to store multiple brush selections
  brushed_areas <- reactiveVal(list())

  # Add an observer to collect brush selections
  observeEvent(input$plot_brush, {
    req(input$plot_brush)

    # Get current week's data
    week_data <- selected_data() %>%
      filter(week == current_week())

    # Get brushed points
    brushed <- brushedPoints(week_data, input$plot_brush,
                             xvar = "DT_round", yvar = "mean")

    if (nrow(brushed) > 0) {
      # Store brush coordinates
      current_brush <- list(
        brush_dt_max = max(brushed$DT_round, na.rm = TRUE),
        brush_dt_min = min(brushed$DT_round, na.rm = TRUE),
        brush_mean_max = max(brushed$mean, na.rm = TRUE),
        brush_mean_min = min(brushed$mean, na.rm = TRUE)
      )
      existing_brushes <- brushed_areas()
      brushed_areas(c(existing_brushes, list(current_brush)))
        }

  })


  # Observer to clear brushed areas
  observeEvent(input$clear_brushes, {
    brushed_areas(list())
    session$resetBrush("plot_brush")

  })


 # Brush submit button UI
  output$brush_submit_ui <- renderUI({
    can_submit <- FALSE

    if (!is.null(input$plot_brush) & !is.null(input$brush_action)) {

      if(input$brush_action != "F"){
        can_submit = TRUE
      }else{
        if(input$brush_action == "F"){
          can_submit = FALSE
          if(!is.null(input$user_brush_flags)){
            can_submit = TRUE
          }
        }
      }
    }


    actionButton(
      "submit_brush",
      "Submit Selection",
      class = ifelse(can_submit, "btn-success", "btn-secondary"),
      disabled = !can_submit
    )

  })


  # Modified submit observer
  observeEvent(input$submit_brush, {
    req(brushed_areas(), input$brush_action, selected_data())

    user_brush_select <- input$brush_action

    # flag_choices <- if(input$brush_action == "F") {
    #   input$user_brush_flags
    # } else {
    #   NA
    # }

    brush_boxes <- map_dfr(
      brushed_areas()[seq(1, length(brushed_areas()), by = 2)],
      ~data.frame(
        xmin_DT = .x$brush_dt_min,
        xmax_DT = .x$brush_dt_max,
        ymin_mean = .x$brush_mean_min,
        ymax_mean = .x$brush_mean_max
      )
    )

    # Initialize updated data with current data
    updated_data <- selected_data()

    # Process each brushed area
    for(i in 1:nrow(brush_boxes)) {
      # Get points within current brush area
      brush <- brush_boxes %>% slice(i)

      xmin_DT <- pull(brush, xmin_DT)
      xmax_DT <- pull(brush, xmax_DT)
      ymin_mean <- pull(brush, ymin_mean)
      ymax_mean <- pull(brush, ymax_mean)

      # Update data for current brush area
      updated_data <- updated_data %>%
        mutate(
          user_flag = case_when(
            #Accept
            between(DT_round, xmin_DT, xmax_DT) &
              between(mean, ymin_mean, ymax_mean) &
              user_brush_select == "A" ~ as.character(NA),
            #Flag
            between(DT_round, xmin_DT, xmax_DT) &
              between(mean, ymin_mean, ymax_mean) &
              user_brush_select == "F" ~ paste(input$user_brush_flags, collapse = ";\n"),
            #omit
            between(DT_round, xmin_DT, xmax_DT) &
              between(mean, ymin_mean, ymax_mean) &
              user_brush_select == "O" ~ user_flag,
            TRUE ~ user_flag),

          omit = case_when(
            between(DT_round, xmin_DT, xmax_DT) &
              between(mean, ymin_mean, ymax_mean) &
              user_brush_select == "O" ~ TRUE,
            #Accept
            between(DT_round, xmin_DT, xmax_DT) &
              between(mean, ymin_mean, ymax_mean) &
              user_brush_select %in% c("A","F")  ~ FALSE,
            TRUE ~ omit),

          user = ifelse(between(DT_round, xmin_DT, xmax_DT) &
                          between(mean, ymin_mean, ymax_mean),
                        input$user,
                        user)
        )
    }

    # Update the data
    selected_data(updated_data)

    # Clear brushed areas after submission
    brushed_areas(list())
    session$resetBrush("plot_brush")
    #reset input$user_brush_flags to nothing
    updateRadioButtons(session, "user_brush_flags", selected = "")


    showNotification("Brush Changes saved.", type = "message")
  })


# Resets data to remove all brush inputs and weekly decisions
  observeEvent(input$reset_week, {

    req(selected_data(), current_week())

    updated_data <- selected_data() %>%
      filter(week == current_week())%>%
      mutate(user_flag = flag,
             omit = FALSE,
             user = NA,
             final_status = NA,
             week_decision = NA,
             is_verified = NA)

    other_data <- selected_data() %>%
      filter(week != current_week())

    selected_data(rbind(updated_data, other_data))

  })

#### Weekly Decision ####

  # UI for weekly decision radio buttons
  output$weekly_decision_radio <- renderUI({

   week_data <-  selected_data()%>%
      filter(week == current_week())

    radioButtons(
      "weekly_decision",
      label = "Make Weekly Decision:",
      choices = c("Accept ALL" = "aa",
                  "Accept Non Omit" = "ano",
                  "Keep Flags" = "kf",
                  "Omit Flagged" = "of",
                  "Omit ALL" = "oa",
                  "Skip" = "s"),

      selected = ifelse(all(is.na(week_data$week_decision)), "s", unique(week_data$week_decision)[1]), # If a week has an existing decision made, it will show up first here
      inline = TRUE
    )
  })


  # Submit decision button UI
  # Toggles on (green) if user has correctly selected a decision
  output$submit_decision_ui <- renderUI({
    req(input$weekly_decision)
    can_submit <- FALSE

    if (input$weekly_decision != "s") {
      can_submit <- T
    }

    actionButton(
      "submit_decision",
      "Submit Weekly Decision",
      class = ifelse(can_submit, "btn-success", "btn-secondary"),
      disabled = !can_submit
    )
  })

# Update selected_data() on backend with submitted decision
  observeEvent(input$submit_decision, {
    req(input$weekly_decision != "s", selected_data())
    #update backend data

      #weekly_decision <- input$weekly_decision
    updated_week_data <- selected_data() %>%
        filter(week == current_week())%>%
        mutate(
          omit = case_when(
            #removing all flags if user selects accept all
            input$weekly_decision == "aa" ~ FALSE,
            input$weekly_decision == "oa" ~ TRUE,
            input$weekly_decision == "of" & !is.na(user_flag) ~ TRUE,
            TRUE ~ omit),
          final_status = case_when(
            #AA:Pass all data
            input$weekly_decision  == "aa"  ~ "PASS",
            #ANO: Accept Non Omit
            input$weekly_decision == "ano" & !omit ~ "PASS", # pass data that is not user select omit
            #KF: Keep FLagged, retain flag into final data (sus but on the edge)
            input$weekly_decision == "kf" & is.na(user_flag) & !omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "kf" & !is.na(user_flag) & !omit ~ "FLAGGED", # tag data that is flagged
            #OF: Omit Flagged
            input$weekly_decision == "of" & is.na(user_flag) & !omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "of" & !is.na(user_flag) & !omit ~ "OMIT", # omit data that is flagged
            #OA: Omit All
            input$weekly_decision == "oa"  ~ "OMIT",
            # Omit any user selected omit data (assuming AA was not the choice)
            input$weekly_decision != "aa" & omit ~ "OMIT"),
          user_flag = case_when(
            #removing all flags if user selects accept all
            input$weekly_decision == "aa" ~ NA,
            TRUE ~ user_flag),
          user = input$user,
          week_decision = input$weekly_decision,
          is_verified = TRUE)

      other_data <- selected_data() %>%
        filter(week != current_week())

      selected_data(bind_rows(other_data, updated_week_data)%>%arrange(DT_round))
#TO DO: save to int directory/general save data function

    # Get all weeks and current week
    weeks <- unique(selected_data()$week)
    current <- current_week()
    idx <- which(weeks == current)

    # Move to next week if available
    if(all(!is.na(selected_data()$final_status))){

      showNotification("All weeks have been reviewed.", type = "message")
      updateTabsetPanel(session, inputId = "tabs", selected = "Finalize Data")
      }else{
      if(idx == length(weeks)){
        # Find min week where is_verified is NA
        week_min <- selected_data()%>%filter(is.na(is_verified))%>%pull(week)%>%min()
        idx <- which(weeks == week_min)
        current_week(weeks[idx])

        showNotification(
          "No more weeks to verify. Moving to earliest unverified week",
          type = "warning")

      }else{
        current_week(weeks[idx + 1])
      }

    }
#To Do: If next week has been reviewed, move to closest week without verified data

    # Show notification of submission
    showNotification(
      paste("Decision", toupper(input$weekly_decision), "submitted"),
      type = "message"
    )
    # Reset weekly decision back to "s"
    updateRadioButtons(session, "weekly_decision", selected = "s")
    updateCheckboxInput(session, "remove_omit", value = FALSE)
  })


##### Final Verification Tab ####
  observe({
    req(selected_data())
    weeks <- selected_data() %>%
      pull(week) %>%
      unique() %>%
      sort()

    updateSelectInput(session, "final_week_selection",
                      choices = weeks)
  })

  # Handle week selection in final tab
  observeEvent(input$goto_final_week, {
    req(input$final_week_selection)
    selected_week <- as.numeric(input$final_week_selection)
    current_week(selected_week)
    updateNavbarPage(session, inputId = "tabs", selected = "Data Verification")
  })

  # Add plotly plot for final overview
  output$final_plot <- renderPlotly({
    req(selected_data())

    final_plot_data <- selected_data()

    if (input$remove_omit_finalplot) {

  #To Do: Update if omit is T/F not T vs NA
      final_plot_data <- final_plot_data %>%
        filter(!omit)
    }



#start & end of period (still not tested on multiyear datasets)
min_year <- min(selected_data()$year)
max_year <- max(selected_data()$year)
#Find start of week (even if it is before the start of the week)
start_week <- min(selected_data()$week)
start_date <- parse_date_time(paste(min_year, start_week, 0, sep="/"),'Y/W/w')
#Find end of last week (even if it is after the end of the data)
end_week <- max(selected_data()$week)
end_date <- parse_date_time(paste(max_year, end_week, 6, sep="/"),'Y/W/w')
#create vertical lines to seperate each week
vline_dates <- seq(start_date, end_date, by = "week")
#add 3 days to each vertical line to center the week
week_dates <- vline_dates + days(3)

final_status_colors <- c("PASS" = "#008a18",
                         "OMIT" = "#ff1100",
                         "FLAGGED" = "#ff8200",
                         "NA" = "grey")

# final_status_colors <- c(
#   "PASS" = "green",
#   "FLAGGED" = "yellow",
#   "OMIT" = "red",
#   "NA" = "gray"  # Assign a color for NA values
# )
#browser()
# Replace NA values in final_status (this will not affect the actual saved data, just for plotting)
final_plot_data$final_status <- ifelse(is.na(final_plot_data$final_status), "NA", final_plot_data$final_status)
# This seems to be important for the plotly to work
final_plot_data$final_status <- as.factor(final_plot_data$final_status)


# Create plotly object
 p_plotly <- plot_ly(
  data = final_plot_data,
  x = ~DT_round,
  y = ~mean,
  type = 'scatter',
  mode = 'markers',
  color = ~final_status,
  colors = final_status_colors,
  text = ~paste0("Week ", week, "\nStatus: ", final_status),
  hoverinfo = "text"
) %>%
    layout(
      title = list(
        text = paste0("Complete Dataset Overview: ", input$site, "-", input$parameter),
        x = 0.5 # Centers the title
      ),
      xaxis = list(
        title = "Date",
        tickformat = "%b %d",
        tickmode = "array",
        tickvals = week_dates,
        ticktext = paste0("Week ", week(week_dates), "\n", format(week_dates, "%b %d")),
        showgrid = TRUE,
        domain = c(0, 1)  # Ensure it spans the full width
      ),
      yaxis = list(
        title = input$parameter
      ),
      xaxis2 = list(
        title = "Week #",
        tickmode = "array",
        tickvals = week_dates,
        ticktext = as.character(week(week_dates)),
        overlaying = "x",
        side = "top",
        showgrid = FALSE,
        zeroline = FALSE,
        ticks = "outside",
        ticklen = 5
      ),
      shapes = lapply(vline_dates, function(date) {
        list(
          type = "line",
          x0 = date, x1 = date, y0 = 0, y1 = 1,
          xref = "x", yref = "paper",
          line = list(color = "black", width = 1)
        )
      })
    )
  if (input$remove_omit_finalplot) {
    p_plotly <- p_plotly %>%
      layout(
        annotations = list(
          list(
            text = "Omitted data removed",
            x = 0.5,
            y = 1.02,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 12, color = "gray50")
          )
        )
      )
  }

  p_plotly

  })

  # Handle final submission
  observeEvent(input$submit_final, {
    showNotification("Final changes submitted successfully!", type = "message")
    updateNavbarPage(session, inputId = "tabs", selected = "Data Selection")

#TODO: sync_files()/reset available data (load data)

  })

  #### Extras ####
  # Handle quit button
  observeEvent(input$quit_app, {
    stopApp()
  })

  # Add this to handle the keyboard shortcut
  observeEvent(input$q_key, {
    if (input$q_key == "q") {
      stopApp()
    }
  })

}

shinyApp(ui, server)
