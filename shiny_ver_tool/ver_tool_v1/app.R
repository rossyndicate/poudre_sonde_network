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
library(glue)
library(anytime)
options(shiny.maxRequestSize = 10000 * 1024^2)

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

# final_status_colors <- c("PASS" = "#008a18",
#                          "OMIT" = "#ff1100",
#                          "FLAGGED" = "#ff8200")
final_status_colors <- c("PASS" = "green",
                         "OMIT" = "red",
                         "FLAGGED" = "orange")


#TODO: Create function that actually looks for available parameters in the data
available_parameters <- c("Specific Conductivity", "Temperature", "pH",
                          "Turbidity", "DO", "Depth")
#TODO: Create function that actually looks for available sites in the data
available_sites <- site_color_combo$site

###### End Helper Functions ######


# UI Definition
ui <- page_navbar(
  title = "Data Processing Pipeline",
  id = "tabs",
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light") #Toggle light vs dark mode
  ),
  #theme = bs_theme(version = 5, bootswatch  = "zephyr")
  theme = bs_theme(preset = "bootstrap"),

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
                  inputId = "incl_ex_days",
                  label = "Extra Data",
                  value = TRUE,
                  width = "200px",
                  status = "success"
                )
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
#TODO: Update this so that it comes from a list of user defined flags
            selectizeInput("user_brush_flags", "Select Flags:",
                        choices = c("sv" = "sv",
                                    "suspect data" = "suspect",
                                    "sensor malfunction" = "malfunction",
                                    "drift" = "drift"),
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
        actionButton("submit_final", "Submit Finalized Dataset",
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
  #all_filepaths <- reactiveVal(NULL) # List of all filepaths in data

auto_refresh <- reactiveTimer(30000) #refresh every 30 sec

  # Check if data folder exists and if data/all_data subfolder has files, if files are available, show table of available files and allow user selection
  output$conditional_data_ui <- renderUI({
    # Check if data folder exists and if data/all_data subfolder has files
    data_folder_exists <- dir.exists(here("shiny_ver_tool", "ver_tool_v1", "data"))
    all_data_path <- here("shiny_ver_tool", "ver_tool_v1", "data", "all_data_directory")
    all_data_subfolder_empty <- FALSE

    if(data_folder_exists) {
      all_data_subfolder_exists <- dir.exists(all_data_path)
      if(!all_data_subfolder_exists | length(list.files(all_data_path)) == 0) {
        all_data_subfolder_empty <- TRUE
      }
    }

    if(!data_folder_exists|all_data_subfolder_empty){
      # Show file upload and timezone input if conditions are met
      tagList(
        textInput("timezone", "Enter your timezone:", value = "MST"),
        fileInput("data_upload", "Upload your data files:", multiple = FALSE,
                  accept = c(".csv", ".xlsx", ".zip", ".feather", ".rds"))
      )
    } else {
#Show regular UI if files are present
      tagList(

        # Display files in data folder as a table
        h4("Files in data folder:"),
        DT::dataTableOutput("data_files_table"),
        # Directory selection
        selectInput("directory", "Choose Directory:",
                     choices = c("pre_verification", "intermediary"),
                     selected = "pre_verification"),
        selectInput("user", "Select User:",
                    choices = c("SJS", "JDT", "CLM", "AS"), selected = "SJS"),
        # Site selection
        selectInput("site", "Select Site:",
                    choices = NULL),

        # Main parameter selection (single selection)
        selectInput("parameter", "Select Parameter:",
                    choices = NULL),

        actionButton("load_data", "Load Data", class = "btn-primary"),

      )
    }
  })

  #constantly updating file paths for shiny app
  all_filepaths <- reactive({

  #auto_refresh() #refresh every 30 sec

  get_filenames()%>%mutate(
    site = map_chr(filename, ~ split_filename(.x)$site),
    parameter = map_chr(filename, ~ split_filename(.x)$parameter))

})


#data table for available parameters and which folder they belong to
  output$data_files_table <- renderDataTable({
    files <- all_filepaths() %>%
      filter(directory %in% c("pre_verification", "intermediary", "verified")) %>%
      mutate(
        site = map_chr(filename, ~ split_filename(.x)$site),
        parameter = map_chr(filename, ~ split_filename(.x)$parameter)
      ) %>%
      select(-filename) %>%
      distinct() %>%
      pivot_wider(
        names_from = parameter,
        values_from = directory,
        values_fill = NA
      ) %>%
      arrange(site)

    DT::datatable(files, options = list(pageLength = 25)) %>%
      DT::formatStyle(
        columns = names(files)[-1],  # All columns except the first (site)
        valueColumns = names(files)[-1],
        backgroundColor = DT::styleEqual(
          c("pre_verification", "intermediary", "verified"),
          c("#D3D3D3", "#FFA500", "#90EE90")
        )
      )
  })

  # Observer to handle data uploads and timezone input
  observeEvent(input$data_upload, {
    req(input$data_upload, input$timezone)

    # Check if both data_upload has files and timezone has a value
    if (!is.null(input$data_upload) & !is.null(input$timezone) & input$timezone != "") {

      upload_path <- input$data_upload$datapath
      # Call the setup function with the uploaded files and timezone
      # and capture its return value
      result_message <- setup_directories_from_upload(
        uploaded_file_path = upload_path,
        timezone = input$timezone
      )

      # Force auto-refresh to update the UI
      auto_refresh()

      # Show notification with the returned message
      showNotification(
        result_message,
        type = "message",
        duration = 5
      )
    }
  })

  #### Data Selection functions ####


  # Update site choices when directory changes
  observe({
    req(input$directory, all_filepaths())
    sites <- all_filepaths() %>%
      filter(directory == input$directory) %>%
      pull(site) %>%
      unique()

    updateSelectInput(session, "site",
                      choices = sites) #based on directory
  })

  # Update parameter choices when site changes
  observe({
    req(input$directory, input$site, all_filepaths())

    parameters <- all_filepaths() %>% filter(directory == input$directory & site == input$site)%>%
      pull(parameter) %>%
      unique()

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
  #Show/hide and update additional sites UI based on site selection
  observe({
    req(input$site)

    # Get auto-selected parameters for the chosen parameter
    auto_sites <- relevant_sonde_selector(site_arg = input$site)

    # Update sub-parameters selection
    updateSelectInput(session, "sub_sites",
                      choices = available_sites,
                      selected = auto_sites)
    updateSelectInput(session, "add_sites",
                      choices = available_sites,
                      selected = auto_sites)

  })


  # Load data when button is clicked
#TODO: This will need to be updated to match new site/param names on backend, as long as the data is loaded to selected_data(), everything should work downstream
  observeEvent(input$load_data, {
    req(input$directory, input$site, input$parameter, input$sub_parameters)
    # Initialize data directories and load datasets

#browser()
#TODO: This loads all data and is probably inefficient, should be updated to only load the data needed
      datasets <- load_all_datasets()
      # Get the site-parameter name using split_filename on the name of each list
      datasets <- map(datasets, function(data_list) {
        #if data_list is empty, return data_list immideatly
        if(is_empty(data_list)){
          return(data_list)
        }

        file_names <- map(names(data_list), split_filename) %>%
          bind_rows() %>%
          mutate(site_param = paste(site, parameter, sep = "-"))

        # Assign new names to the list
        names(data_list) <- file_names$site_param
        return(data_list)
      })

      all_datasets(datasets)

    # Get the site-parameter name
    site_param_name <- paste0(input$site, "-", input$parameter)


    # Try to get the specific dataset
    tryCatch({

      if(input$directory == "pre_verification") {
        site_param_df <- datasets$pre_verification_data[[site_param_name]]
      } else {
        site_param_df <- datasets$intermediary_data[[site_param_name]]
      }

      if (is.null(site_param_df)) {
        stop(paste("Dataset", site_param_name, "not found"))
      }

#
# #TODO: check additional columns (verification status, etc ) to match with old ver system
#       processed <- sel_data%>%
#         mutate(user_flag = flag, # Keep Auto flags in flag column, user added flags/alterations live in user flag
#                brush_omit = FALSE, # User Brush Omit instances, default to FALSE
#                user = NA, #User initials
#                final_status = NA, # Final status of the data point after weekly decision
#                week_decision = NA, #store weekly decision
#                is_verified = NA) # Store if the data point is verified

      # Store the processed data
      selected_data(site_param_df)

      # Set initial week
#TODO: This should update to first week with unverified data if possible
      current_week(min(site_param_df$week))

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
    req(selected_data(), current_week(), all_datasets(), input$weekly_decision)

#TODO:See previous note, this should be loaded only once rather than each time the plot updates
    pre_verification_data <- all_datasets()[["pre_verification_data"]]
    intermediary_data <- all_datasets()[["intermediary_data"]]
    verified_data <- all_datasets()[["verified_data"]]

    week_data <- selected_data() %>%
      filter(week == current_week())

    week_min_day = min(week_data$DT_round, na.rm = T)
    week_max_day = max(week_data$DT_round, na.rm = T)
    #grab two days on either end of the week
    week_plus_data <- selected_data() %>%
      filter(DT_round >= week_min_day - days(2) & DT_round <= week_max_day + days(2))
      #remove data from the week


    year_week <- paste0(as.character(year(min(week_data$DT_round))) ," - ", as.character(min(week(week_data$DT_round))))
    flag_day <- min(week_data$DT_round)


#This isn't being sourced correctly in the shiny app
#TODO: Source this correctly
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
            filter(DT_round >= week_min_day - days(2) & DT_round <= week_max_day + days(2))
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

     # append week_data to relevant sonde list, clean list, and bind dfs
     relevant_dfs <- map(relevant_sondes, ~.x[[1]])
     week_plot_data <- append(relevant_dfs, list(week_data)) %>%
       append(., list(week_plus_data)) %>%
       keep(~ !is.null(.)) %>%
       keep(~ nrow(.)>0) %>%
       bind_rows() %>%
       arrange(day)


    # Check the decision and create appropriate plot
    if (input$weekly_decision != "s") {
# Show final decision to user to preview weekly decision
      week_choice_data <- week_data %>%
        mutate(
          final_decision = case_when(
            #AA:Pass all data
            input$weekly_decision == "aa"  ~ "PASS",
            #ANO: Accept Non Omit
            input$weekly_decision == "ano" & !brush_omit ~ "PASS", # pass data that is not user select omit
            #KF: Keep Flags, data becomes flagged but kept in dataset (mildly sus)
            input$weekly_decision == "kf" & is.na(user_flag) & !brush_omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "kf" & !is.na(user_flag) & !brush_omit ~ "FLAGGED", # tag data that is flagged
            #OF: Omit Flagged
            input$weekly_decision == "of" & is.na(user_flag) & !brush_omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "of" & !is.na(user_flag) & !brush_omit ~ "OMIT", # omit data that is flagged
            #OA: Omit All
            input$weekly_decision == "oa"  ~ "OMIT",
            # Omit any user selected omit data (assuming AA was not the choice)
            input$weekly_decision != "aa" & brush_omit ~ "OMIT"))

#Remove omitted data (user or from weekly decision)
      if (input$remove_omit) {
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "OMIT")

        week_plus_data <- week_plus_data %>%
          filter(final_status != "OMIT"| is.na(final_status))
      }
#Remove flagged data if user desired
      if(input$remove_flag){
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "FLAGGED")

        week_plus_data <- week_plus_data %>%
          filter(final_status != "FLAGGED"| is.na(final_status))
      }
      week_min_check <- week_plus_data %>%
        filter(week < current_week())

      week_max_check <- week_plus_data %>%
        filter(week > current_week())



# Create plot for preview of weekly decision
      p <- ggplot(week_choice_data, aes(x = DT_round))

      if (nrow(week_min_check) > 0) {

        week_plus_min <- week_min_check %>%
          summarise(xmin = min(DT_round), xmax = max(DT_round))

        p <- p + annotate(geom = "rect", xmin = week_plus_min$xmin, xmax = week_plus_min$xmax, ymin = -Inf, ymax = Inf, color = "transparent", fill = "grey", alpha = 0.2)
      }
      if (nrow(week_max_check) > 0) {
        week_plus_max <- week_max_check %>%
          summarise(xmin = min(DT_round), xmax = max(DT_round))

        p <- p + annotate(geom = "rect", xmin = week_plus_max$xmin, xmax = week_plus_max$xmax, ymin = -Inf, ymax = Inf, color = "transparent", fill = "grey", alpha = 0.2)
      }
        p <- p +
          map(relevant_sondes, function(sonde_data) {
          add_data <- sonde_data[[1]]
          data_source <- sonde_data[[2]]

          y_column <- ifelse(data_source %in% c("all_data", "pre_verification_data"), "mean", "mean_verified")

          geom_line(data = add_data, aes(x = DT_round, y = .data[[y_column]], color = site), linewidth = 1)
        }) + #plot other sites
        geom_point(aes(y = mean, fill = final_decision),shape = 21, stroke = 0, size = 2)+ #plot main site with colors matching final decision
        geom_point(data = week_plus_data%>%filter(week != current_week()), aes(y = mean, fill = final_status), shape = 21, stroke = 0, size = 1.5, alpha = 0.5)+ #add two extra days on the side
        #add a grey box from the end of week_data to the end of week plus data on both sides of week data
        scale_fill_manual(values = final_status_colors, na.value = "grey")+ #set fill colors to weekly status colors
        scale_color_manual(values = setNames(site_color_combo$color, site_color_combo$site))+ #set other colors for add sites
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
        if(input$incl_ex_days){
          p <- p +
            scale_x_datetime(
              limits = c(min(week_plus_data$DT_round, na.rm = TRUE),
                         max(week_plus_data$DT_round, na.rm = TRUE)),
              expand = c(0, 0), # Remove extra white space
              date_labels = "%b %d", # Formats as "Jan 01", "Feb 15", etc.
              date_breaks = "1 day" # Remove extra white space
            )
        }else{
          p <- p +
            scale_x_datetime(
              limits = c(min(week_choice_data$DT_round, na.rm = TRUE),
                         max(week_choice_data$DT_round, na.rm = TRUE)),
              expand = c(0, 0), # Remove extra white space
              date_labels = "%b %d", # Formats as "Jan 01", "Feb 15", etc.
              date_breaks = "1 day" # Remove extra white space
            )
        }

      p
    } else {
      #TO DO: Swap with create weekly plot function call, adding in other sites, etc
      if(input$remove_omit){
        week_data <- week_data %>%
          filter(!brush_omit)

        week_plus_data <- week_plus_data %>%
          filter(final_status != "OMIT"| is.na(final_status))
      }
      if(input$remove_flag){
        week_data <- week_data %>%
          filter(is.na(user_flag))

        week_plus_data <- week_plus_data %>%
          filter(final_status != "FLAGGED"| is.na(final_status))
      }

      week_min_check <- week_plus_data %>%
        filter(week < current_week())

      week_max_check <- week_plus_data %>%
        filter(week > current_week())

      p <-ggplot(week_data, aes(x = DT_round))

      if (nrow(week_min_check) > 0) {

        week_plus_min <- week_min_check %>%
          summarise(xmin = min(DT_round), xmax = max(DT_round))

        p <- p + annotate(geom = "rect", xmin = week_plus_min$xmin, xmax = week_plus_min$xmax, ymin = -Inf, ymax = Inf, color = "transparent", fill = "grey", alpha = 0.2)
      }
      if (nrow(week_max_check) > 0) {
        week_plus_max <- week_max_check %>%
          summarise(xmin = min(DT_round), xmax = max(DT_round))

        p <- p + annotate(geom = "rect", xmin = week_plus_max$xmin, xmax = week_plus_max$xmax, ymin = -Inf, ymax = Inf, color = "transparent", fill = "grey", alpha = 0.2)
      }
      p <- p +
        map(relevant_sondes, function(sonde_data) {
          add_data <- sonde_data[[1]]
          data_source <- sonde_data[[2]]

          y_column <- ifelse(data_source %in% c("all_data", "pre_verification_data"), "mean", "mean_verified")

          geom_line(data = add_data, aes(x = DT_round, y = .data[[y_column]], color = site), linewidth = 1)
        }) + # add other sites
        geom_point(data = week_plus_data%>%filter(week != current_week()), aes(y = mean),fill = "black",shape = 21, stroke = 0, size = 1.5, alpha = 0.5)+ #add two extra days on the side
        geom_point(aes(y = mean, fill = user_flag),shape = 21, stroke = 0, size = 2)+ #plot main site with colors matching  user flag column
        #Add Omitted data in red
        geom_point(data = week_data %>%filter(brush_omit == TRUE),aes(y = mean),shape = 21, stroke = 0, size = 2, fill = "#ff1100")+
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

        # Create a data frame of all brush boundaries
        brush_boxes <- map_dfr(
          brushed_areas()[seq(1, length(brushed_areas()), by = 2)], #for some reason this likes to duplicate values so just grabbing half of them
          ~data.frame(
            xmin_DT = .x$brush_dt_min,
            xmax_DT = .x$brush_dt_max,
            ymin_mean = .x$brush_mean_min,
            ymax_mean = .x$brush_mean_max)
        )
        #   # Add rectangles to plot for all brushed areas
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
      if(input$incl_ex_days){
        p <- p +
          scale_x_datetime(
            limits = c(min(week_plus_data$DT_round, na.rm = TRUE),
                       max(week_plus_data$DT_round, na.rm = TRUE)),
            expand = c(0, 0), # Remove extra white space
            date_labels = "%b %d", # Formats as "Jan 01", "Feb 15", etc.
            date_breaks = "1 day" # Remove extra white space
          )
      }else{
        p <- p +
          scale_x_datetime(
            limits = c(min(week_data$DT_round, na.rm = TRUE),
                       max(week_data$DT_round, na.rm = TRUE)),
            date_labels = "%b %d", # Formats as "Jan 01", "Feb 15", etc.
            date_breaks = "1 day" # Remove extra white space
          )
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
#TODO: This creates duplicates for some reason but does work...
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

#Can only click button if there is a brush selection and a decision has been made
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
      "Submit Brush Decision",
      class = ifelse(can_submit, "btn-success", "btn-secondary"),
      disabled = !can_submit
    )

  })


  # Modified submit observer
  observeEvent(input$submit_brush, {
    req(brushed_areas(), input$brush_action, selected_data())

    user_brush_select <- input$brush_action
    # Initialize updated data with current data
    updated_data <- selected_data()


    #deal with empty brush areas
    if(is_empty(brushed_areas())){
      # Update the data
      selected_data(updated_data)

      # Clear brushed areas after submission
      brushed_areas(list())
      session$resetBrush("plot_brush")
      #reset input$user_brush_flags to nothing
      updateRadioButtons(session, "user_brush_flags", selected = "")


      showNotification("No Points Brushed, no changes applied", type = "message")

    }else{

    #go through brushed areas and apply user flags, brush omit or user id as needed
    updated_data <- reduce(
      brushed_areas()[seq(1, length(brushed_areas()), by = 2)], #skip everyother to reduce duplicates
      function(data, brush) {
        data %>%
          mutate(
            user_flag = case_when(
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max) &
                user_brush_select == "A" ~ as.character(NA),
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max) &
                user_brush_select == "F" ~ paste(input$user_brush_flags, collapse = ";\n"),
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max) &
                user_brush_select == "O" ~ user_flag,
              TRUE ~ user_flag
            ),
            brush_omit = case_when(
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max) &
                user_brush_select == "O" ~ TRUE,
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max) &
                user_brush_select %in% c("A", "F") ~ FALSE,
              TRUE ~ brush_omit
            ),
            user = ifelse(
              between(DT_round, brush$brush_dt_min, brush$brush_dt_max) &
                between(mean, brush$brush_mean_min, brush$brush_mean_max),
              input$user,
              user
            )
          )
      },
      .init = updated_data
    )

    # Update the data
    selected_data(updated_data)

    # Clear brushed areas after submission
    brushed_areas(list())
    session$resetBrush("plot_brush")
    #reset input$user_brush_flags to nothing
    updateRadioButtons(session, "user_brush_flags", selected = "")


    showNotification("Brush Changes saved.", type = "message")
    }
  })


# Resets data to remove all brush inputs and weekly decisions
  observeEvent(input$reset_week, {

    req(selected_data(), current_week())

    updated_data <- selected_data() %>%
      filter(week == current_week())%>%
      mutate(user_flag = flag,
             brush_omit = FALSE,
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
          # No longer saving omit decision in brush_omit only in final status so that it can be easily overwritten
          # brush_omit = case_when(
          #   #removing all flags if user selects accept all
          #   input$weekly_decision == "aa" ~ FALSE,
          #   input$weekly_decision == "oa" ~ TRUE,
          #   input$weekly_decision == "of" & !is.na(user_flag) ~ TRUE,
          #   TRUE ~ omit),
          final_status = case_when(
            #AA:Pass all data
            input$weekly_decision  == "aa"  ~ "PASS",
            #ANO: Accept Non Omit
            input$weekly_decision == "ano" & !brush_omit ~ "PASS", # pass data that is not user select omit
            #KF: Keep FLagged, retain flag into final data (sus but on the edge)
            input$weekly_decision == "kf" & is.na(user_flag) & !brush_omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "kf" & !is.na(user_flag) & !brush_omit ~ "FLAGGED", # tag data that is flagged
            #OF: Omit Flagged
            input$weekly_decision == "of" & is.na(user_flag) & !brush_omit ~ "PASS", # pass data that is not user select omit
            input$weekly_decision == "of" & !is.na(user_flag) & !brush_omit ~ "OMIT", # omit data that is flagged
            #OA: Omit All
            input$weekly_decision == "oa"  ~ "OMIT",
            # Omit any user selected omit data (assuming AA was not the choice)
            input$weekly_decision != "aa" & brush_omit ~ "OMIT"),
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
#TODO: save to int directory/general save data function
#Syncfolder/update_int function

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

      }else{ #move to next week, if this is verified, user should probably just move to final data tab to see what remains
        current_week(weeks[idx + 1])
      }

    }

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

      final_plot_data <- final_plot_data %>%
        filter(final_status != "OMIT"|is.na(final_status))
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
