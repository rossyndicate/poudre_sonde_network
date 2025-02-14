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

##### Helper functions for data loading #####

load_data_directories <- function() {
#To Do: Convert to {here} paths (for all file paths)
  list(
    all_path = "data/all_data_directory",
    pre_verification_path = "data/pre_verification_directory",
    intermediary_path = "data/intermediary_directory",
    verified_path = "data/verified_directory")
}

load_all_datasets <- function(paths) {
  list(
    all_data = set_names(
      map(list.files(paths$all_path, full.names = TRUE), read_rds),
      list.files(paths$all_path)
    ),
    pre_verification_data = set_names(
      map(list.files(paths$pre_verification_path, full.names = TRUE), read_rds),
      list.files(paths$pre_verification_path)
    ),
    intermediary_data = set_names(
      map(list.files(paths$intermediary_path, full.names = TRUE), read_rds),
      list.files(paths$intermediary_path)
    ),
    verified_data = set_names(
      map(list.files(paths$verified_path, full.names = TRUE), read_rds),
      list.files(paths$verified_path)
    )
  )
}

# Helper functions for data processing
get_sites <- function(datasets, directory) {
  if (directory == "pre") {
    names(datasets$pre_verification_data)%>%
      str_split("-") %>%
      map_chr(1) %>%
      unique()
  } else {
    names(datasets$intermediary_data)%>%
      str_split("-") %>%
      map_chr(1) %>%
      unique()
  }
}

get_parameters <- function(datasets, directory, site) {
  if (directory == "pre") {
    data_list <- datasets$pre_verification_data
  } else {
    data_list <- datasets$intermediary_data
  }

  names(data_list) %>%
    keep(str_detect(., paste0("^", site, "-"))) %>%
    str_remove(paste0(site, "-"))
}

get_auto_parameters <- function(parameter) {
  tryCatch({
    read_csv("data/meta/parameter_autoselections.csv", show_col_types = F) %>%
      filter(main_parameter == parameter) %>%
      pull(sub_parameters) %>%
      first() %>%
      str_split(",", simplify = TRUE) %>%
      as.character() %>%
      str_trim() %>%
      .[. != ""] # Remove any empty strings
  }, error = function(e) character(0))
}

relevant_sonde_selector <- function(site_arg) {
  if (site_arg == "joei") {
    plot_filter <- c("cbri")
  }
  if (site_arg == "cbri") {
    plot_filter <- c("joei", "chd")
  }
  if (site_arg == "chd") {
    plot_filter <- c("cbri", "pfal")
  }
  if (site_arg == "pfal") {
    plot_filter <- c("chd", "sfm")
  }
  if (site_arg == "sfm") {
    plot_filter <- c("pfal", "pbd")
  }
  if (site_arg == "penn") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "lbea") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "pbd") {
    plot_filter <- c("sfm", "tamasag")
  }
  if (site_arg == "tamasag") {
    plot_filter <- c("pbd", "legacy")
  }
  if (site_arg == "legacy") {
    plot_filter <- c("tamasag", "lincoln")
  }
  if (site_arg == "lincoln") {
    plot_filter <- c("legacy", "timberline", "timberline virridy")
  }
  if (site_arg == "timberline") {
    plot_filter <- c("lincoln", "timberline virridy", "prospect")
  }
  if (site_arg == "timberline virridy") {
    plot_filter <- c("lincoln", "timberline", "prospect", "prospect virridy")
  }
  if (site_arg == "springcreek") {
    plot_filter <- c("prospect virridy", "prospect")
  }
  if (site_arg == "prospect") {
    plot_filter <- c("timberline", "prospect virridy", "boxelder")
  }
  if (site_arg == "prospect virridy") {
    plot_filter <- c("timberline virridy", "prospect", "boxelder")
  }
  if (site_arg == "boxelder") {
    plot_filter <- c("prospect",
                     "prospect virridy",
                     "archery",
                     "archery virridy")
  }
  if (site_arg == "boxcreek") {
    plot_filter <- c("archery", "archery virridy")
  }
  if (site_arg == "archery") {
    plot_filter <- c("boxelder", "archery virridy", "river bluffs")
  }
  if (site_arg == "archery virridy") {
    plot_filter <- c("boxelder", "archery", "river bluffs")
  }
  if (site_arg == "river bluffs") {
    plot_filter <- c("archery", "archery virridy")
  }
  return(plot_filter)
}

site_color_combo <- tibble(site = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","tamasag","legacy", "lincoln", "timberline virridy", "timberline",
                                    "prospect virridy", "prospect","boxelder",  "archery virridy", "archery", "boxcreek", "springcreek", "river bluffs"),
                           color = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",
                                     "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44","#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))

final_status_colors <- c("PASS" = "green",
                           "TAG" = "yellow",
                           "OMIT" = "red")
# All available parameters for sub-parameter selection
available_parameters <- c("Specific Conductivity", "Temperature", "pH",
                          "Turbidity", "DO", "Depth")
available_sites <- c("legacy", "lincoln", "timberline", "tamasag")
#TO DO: This only has the primary datasets for testing purposes need to add (FDOM, CHLA, ORP)
###### End Helper Functions ######


# UI Definition
ui <- page_navbar(
  #useShinyjs(),
#To Do: remove header to save space? it shouldnt need to be used by users
  title = "Data Processing Pipeline",
  id = "tabs",
  theme = bs_theme(version = 5),

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
          card_body(
            plotOutput("main_plot", brush = "plot_brush")
          ),
          card_footer(
            div(
              class = "d-flex justify-content-evenly align-items-center",
              actionButton("prev_tab", "← Back to Selection", class = "btn-info"),
              actionButton("prev_week", "← Previous Week", class = "btn-secondary"),
              actionButton("next_week", "Next Week →", class = "btn-secondary"),
              actionButton("reset_week", "Reset Data", class = "btn-danger"),
              checkboxInput("remove_omit", "Remove OMIT data", value = FALSE),
              keys::useKeys(),
              keys::keysInput("q_key", "q"),
              actionButton("quit_app", "Quit", class = "btn-danger")
            )
          )
        ),

       #### Weekly decision card (bottom left, shorter height) ####
        card(
          style = "height: 5px; overflow: hidden;",
          card_header(
            "Make weekly decision"
          ),
          card_body(
           # style = "padding: 4px 12px;",
            div(
              #class = "d-flex align-items-center gap-3",
              div(
               # style = "margin: -10px 0;", # Negative margin to reduce radio button spacing
                radioButtons(
                  "weekly_decision",
                  label = NULL,
                  choices = c("AA" = "aa",
                              "ANO" = "ano",
                              "TF" = "tf",
                              "OF" = "of",
                              "OA" = "oa",
                              "S" = "s"),
                  selected = "s",
                  inline = TRUE
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
          card_header("Sub Parameter Plots"),
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
            h4("Data Selection Tools", class = "m-0")
          ),
          card_body(
            div(
              class = "d-flex align-items-center gap-3",
              radioButtons("brush_action",
                           "Select Action:",
                           choices = c("Accept" = "A",
                                       "Flag" = "F",
                                       "Omit" = "O"),
                           selected = character(0),
                           inline = TRUE)  # This makes the radio buttons horizontal
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
        plotOutput("final_plot", height = "500px")
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
#### Reactive values ####
  data <- reactiveVal(NULL)
  current_week <- reactiveVal(NULL) #controlled by next/prev week buttons, and submit weekly decision
  selected_data <- reactiveVal(NULL) # This is essentially site param df
  all_datasets <- reactiveVal(NULL) # List of all datasets and is used in generating sub plots?
  brush_active <- reactiveVal(FALSE) #internal shiny tracker for brush tool

  # observeEvent(input$remove_omit, {
  #   # Toggle the button's appearance
  #   shinyjs::toggleClass("remove_omit", "btn-danger")
  #   shinyjs::toggleClass("remove_omit", "btn-success")
  # })

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
  observe({
    req(input$site)

    # Get auto-selected parameters for the chosen parameter
    auto_sites <- relevant_sonde_selector(input$site)

    # Update sub-parameters selection
    updateSelectInput(session, "sub_sites",
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

      #To do: Add additional columns (verification status, etc ) to match with old ver system
      processed <- sel_data%>%
        #FOR TESTING PURPOSES ONLY
        mutate(omit = NA,
               user = NA,
               final_status = NA)


      # Store the processed data
      selected_data(processed)

      # Set initial week
      #To Do: This should update to first week with unverified data if possible
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
    }
  })

## Main plot
  output$main_plot <- renderPlot({
    req(selected_data(), current_week())

    week_data <- selected_data() %>%
      filter(week == current_week())
#browser()
    # Check the decision and create appropriate plot
    if (input$weekly_decision != "s") {
#Q: not sure if this is necessary?
        weekly_decision <- input$weekly_decision

  #TO DO: Update matrix with final decisions

      week_choice_data <- week_data %>%
        mutate(
          final_decision = case_when(
          #AA:Pass all data
          weekly_decision == "aa"  ~ "PASS",
          #ANO: Accept Non Omit
          weekly_decision == "ano" & is.na(omit) ~ "PASS", # pass data that is not user select omit
          #TF: Tag Flagged
          weekly_decision == "tf" & is.na(flag) & is.na(omit) ~ "PASS", # pass data that is not user select omit
          weekly_decision == "tf" & !is.na(flag) & is.na(omit) ~ "TAG", # tag data that is flagged
          #OF: Omit Flagged
          weekly_decision == "of" & is.na(flag) & is.na(omit) ~ "PASS", # pass data that is not user select omit
          weekly_decision == "of" & !is.na(flag) & is.na(omit) ~ "OMIT", # omit data that is flagged
          #OA: Omit All
          weekly_decision == "oa"  ~ "OMIT",
          # Omit any user selected omit data (assuming AA was not the choice)
          weekly_decision != "aa" & !is.na(omit) ~ "OMIT"))
#Remove omitted data (user or from weekly decision)
      if (input$remove_omit) {
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "OMIT")
      }
#To Do: Add in other sites + other information
      p <- ggplot(week_choice_data, aes(x = DT_round)) +
        geom_point(aes(y = mean, color = final_decision))+
        labs(
          title = paste0("Weekly Data for:", input$site, "-", input$parameter),
          x = "Date",
          y = input$parameter )+
        scale_color_manual(values = final_status_colors)

      plot(p)
    } else {
  #TO DO: Swap with create weekly plot function call, adding in other sites, etc
      if(input$remove_omit){
        week_data <- week_data %>%
          filter(is.na(omit))
      }

   p <- ggplot(week_data, aes(x = DT_round)) +
      geom_point(aes(y = mean, color = flag))+
     #Add Omitted data in red
      geom_point(data = week_data %>%filter(omit == TRUE),aes(y = mean), color = "red")+
      labs(
        title = paste0("Weekly Data for:", input$site, "-", input$parameter),
        x = "Date",
        y = input$parameter )

   # Add brush rectangle if brush is active and brush exists

#Q This might need to be reactive later on ?
   if(!is.null(input$plot_brush)) {
     req(input$plot_brush)

     # Get brushed points
     brushed_data <- brushedPoints(week_data, input$plot_brush,
                                   xvar = "DT_round", yvar = "mean")
#If data exists, add a rectangle to it
#Note: rectangle doesnt show up for single point but data does update correctly
     if(nrow(brushed_data) > 0) {
       # Add rectangle around brushed points

       p <- p +
         geom_rect(aes(xmin = min(brushed_data$DT_round, na.rm = T),
                                         xmax = max(brushed_data$DT_round, na.rm = T),
                                         ymin = min(brushed_data$mean, na.rm = T),
                                         ymax = max(brushed_data$mean, na.rm = T)),
         fill = NA, color = "blue", alpha = 0.3)
     }
   }
 #create plot
   p
}
  })



## Sub plots output
  output$sub_plots <- renderPlot({
    req(all_datasets(), current_week(), input$site, input$sub_parameters, input$sub_sites)
#TODO: correctly grab the appropriate file (ver prefered, int (remove omitted), or pre)
    datasets <- all_datasets()
    working_data <- if(input$directory == "pre") {
      datasets$pre_verification_data
    } else {
      datasets$intermediary_data
    }

    # Create individual plots for each sub parameter
    plots <- map(input$sub_parameters, function(param) {
      # Get main site data
      main_site_param_name <- paste0(input$site, "-", param)
      main_site_data <- working_data[[main_site_param_name]]
      main_week_data <- filter(main_site_data, week == current_week())

      # Get sub sites data
      sub_sites_data <- map_dfr(input$sub_sites, function(sub_site) {
        site_param_name <- paste0(sub_site, "-", param)
        site_data <- working_data[[site_param_name]]
        week_data <- filter(site_data, week == current_week())
        week_data$site <- sub_site  # Add site identifier
        return(week_data)
      })
     # browser()
      # Create plot
      p <- ggplot() +
        # Add main site as grey points
        geom_point(data = main_week_data,
                   aes(x = DT_round, y = mean),
                   color = "grey40",
                   size = 2) +
        # Add sub sites as colored lines
        geom_line(data = sub_sites_data,
                  aes(x = DT_round, y = mean, color = site),
                  linewidth = 1) +
        scale_color_manual(values = setNames(site_color_combo$color, site_color_combo$site)) +
        labs(x = "Date",
             y = param,
             #title = param,
             color = "Sites") +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
          #plot.title = element_text(hjust = 0.5),
          #legend.position = "top"
          )

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


  # Brush submit button UI
  # Brush submit button UI
  output$brush_submit_ui <- renderUI({
    can_submit <- FALSE

  if (!is.null(input$brush_action) & !is.null(input$plot_brush) & !is.null(input$brush_action)) {

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



  # Handle brush submission
  observeEvent(input$submit_brush, {
    req(input$plot_brush, input$brush_action, selected_data())

    # Get current week's data
    week_data <- selected_data() %>%
      filter(week == current_week())

    # Get brushed points
    brushed <- brushedPoints(week_data, input$plot_brush,
                                  xvar = "DT_round", yvar = "mean")
    brush_dt_max <- max(brushed$DT_round, na.rm = T)
    brush_dt_min <- min(brushed$DT_round, na.rm = T)
    brush_mean_max <- max(brushed$mean, na.rm = T)
    brush_mean_min <- min(brushed$mean, na.rm = T)


    user_brush_select <- input$brush_action

    if(input$brush_action == "F") {
      flag_choices <- input$user_brush_flags

    }else{
      flag_choices <- NA
    }

      updated_data <- selected_data() %>%
        mutate(
          flag = case_when(
            #Accept
            between(DT_round, brush_dt_min, brush_dt_max) &
            between(mean, brush_mean_min, brush_mean_max) & user_brush_select == "A" ~ as.character(NA),
            #Flag
            between(DT_round, brush_dt_min, brush_dt_max) &

  #TO DO: Turn into function add flag
            between(mean, brush_mean_min, brush_mean_max) &  user_brush_select == "F" ~ as.character(flag_choices),
            #Omit
#TO DO: If a user selects Omit, do they need to give the data a flag?
            #Keep existing flags
            between(DT_round, brush_dt_min, brush_dt_max) &
            between(mean, brush_mean_min, brush_mean_max) &   user_brush_select == "O" ~ flag,
            TRUE ~ flag),

          #if a user brushes points as omit, then change omit to TRUE
        omit = case_when(
            between(DT_round, brush_dt_min, brush_dt_max) &
            between(mean, brush_mean_min, brush_mean_max) &  user_brush_select == "O" ~ TRUE,
            #Accept
            between(DT_round, brush_dt_min, brush_dt_max) &
              between(mean, brush_mean_min, brush_mean_max) & user_brush_select %in% c("A","F")  ~ NA,
            TRUE ~ omit),
          #if a user brushes points, add their initials to the user column
          user = ifelse(between(DT_round, brush_dt_min, brush_dt_max) &
                          between(mean, brush_mean_min, brush_mean_max), input$user, NA)
        )
#TO DO: Redundant?
      selected_data(updated_data)
      showNotification("Brush Changes saved.", type = "message")

    # Reset brush action and flags
    updateRadioButtons(session, "brush_action", selected = character(0))
    if(!is.null(input$user_brush_flags)) {
      updateSelectInput(session, "user_brush_flags", selected = character(0))
    }
    # Reset the brush by clearing it
    session$resetBrush("plot_brush")

  })
# To Do: Add week reset button to undo brush submissions

## Weekly Decision
# To Do: If a user moves to a week with a decision already made, show this somehow
  # Submit decision button UI
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

  # Update data on backend with submitted decision
  observeEvent(input$submit_decision, {
    req(input$weekly_decision != "s", selected_data())
    #update backend data

      weekly_decision <- input$weekly_decision
#TO DO: Update matrix with final decisions
    updated_week_data <- selected_data() %>%
        filter(week == current_week())%>%
        mutate(
          omit = case_when(
            #removing all flags if user selects accept all
            weekly_decision == "aa" ~ NA,
            weekly_decision == "oa" ~ TRUE,
            weekly_decision == "of" & !is.na(flag) ~ TRUE,
            TRUE ~ omit),
          final_status = case_when(
            #AA:Pass all data
            weekly_decision == "aa"  ~ "PASS",
            #ANO: Accept Non Omit
            weekly_decision == "ano" & is.na(omit) ~ "PASS", # pass data that is not user select omit
            #TF: Tag Flagged
            weekly_decision == "tf" & is.na(flag) & is.na(omit) ~ "PASS", # pass data that is not user select omit
            weekly_decision == "tf" & !is.na(flag) & is.na(omit) ~ "TAG", # tag data that is flagged
            #OF: Omit Flagged
            weekly_decision == "of" & is.na(flag) & is.na(omit) ~ "PASS", # pass data that is not user select omit
            weekly_decision == "of" & !is.na(flag) & is.na(omit) ~ "OMIT", # omit data that is flagged
            #OA: Omit All
            weekly_decision == "oa"  ~ "OMIT",
            # Omit any user selected omit data (assuming AA was not the choice)
            weekly_decision != "aa" & !is.na(omit) ~ "OMIT"),
          flag = case_when(
            #removing all flags if user selects accept all
            weekly_decision == "aa" ~ NA,
            TRUE ~ flag),
          user = input$user)

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
      } else{
      current_week(weeks[idx + 1])
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
  output$final_plot <- renderPlot({
    req(selected_data())

    final_plot_data <- selected_data()

    if (input$remove_omit_finalplot) {

  #To Do: Update if omit is T/F not T vs NA
      final_plot_data <- final_plot_data %>%
        filter(is.na(omit))
    }

start_date <-round_date(min(final_plot_data$DT_round, na.rm = T), unit = "day")
end_date <- round_date(max(final_plot_data$DT_round, na.rm = T), unit = "day")

vline_dates <- seq(start_date, end_date, by = "week")
week_dates <- vline_dates + days(3)
week_num = week(vline_dates)


  p <- ggplot(final_plot_data, aes(x = DT_round)) +
          geom_point(aes(y = mean, color = final_status)) +
      scale_color_manual(values = final_status_colors) +
      geom_vline(xintercept = as.numeric(vline_dates), color = "black") +
      labs(
        title = paste0("Complete Dataset Overview: ", input$site, "-", input$parameter),
        subtitle = ifelse(input$remove_omit_finalplot, "Omitted data removed",  ""),
        x = "Date",
        y = input$parameter,
        color = "Final Status") +
      theme_bw()+
      scale_x_datetime(date_breaks = "1 week",
                       date_labels = "%b %d",
                       minor_breaks = week_dates,
                       sec.axis = sec_axis(~., breaks = week_dates, labels = unique(week_num)))

  p
#To Do: geom vline not playing nice in ggplotly
#     ggplotly(p) %>%
#       layout(dragmode = "select") %>%
#       config(modeBarButtons = list(list("select2d", "lasso2d", "zoom2d", "pan2d",
#                                         "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d")))
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
