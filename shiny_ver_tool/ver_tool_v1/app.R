library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(lubridate)
library(here)
library(ggpubr)
library(gridExtra)

##### Helper functions for data loading #####
load_data_directories <- function() {
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

final_decision_colors <- c("PASS" = "green",
                           "TAG" = "yellow",
                           "OMIT" = "red")
# All available parameters for sub-parameter selection
available_parameters <- c("Specific Conductivity", "Temperature", "pH",
                          "Turbidity", "DO", "Depth")
#TO DO: This only has the primary datasets for testing purposes need to add (FDOM, CHLA, ORP)
###### End Helper Functions ######


# UI Definition
ui <- page_navbar(
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

        # Sub parameter selection
        tags$div(
          id = "sub_parameters_ui",
          selectInput("sub_parameters", "Select Sub Parameters:",
                      choices = available_parameters,
                      multiple = TRUE)
        ),

        actionButton("load_data", "Load Data", class = "btn-primary")
      )
    )
  ),

  #### Tab 2: Data Verification ####
  nav_panel(
    title = "Data Verification",
    layout_columns(
      col_widths = c(8, 4),

      # Main Plot Card
#TO DO: Make plot wider but shorter
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            h4("Main Plot", class = "m-0"),
            div(
              class = "btn-group",
              actionButton("prev_week", "← Previous Week", class = "btn-secondary"),
              actionButton("next_week", "Next Week →", class = "btn-secondary")
            )
          )
        ),
        card_body(
          plotOutput("main_plot", height = "400px",
                     brush = "plot_brush")
        ),
        card_footer(
          div(
            class = "d-flex justify-content-between",
            actionButton("prev_tab", "← Back to Selection", class = "btn-info"),
            div(
              checkboxInput("show_subplots", "Show Sub Parameters", value = FALSE),
              style = "margin: auto"
            ),
            actionButton("quit_app", "Quit", class = "btn-danger")
          )
        )
      ),
      # Decision and Sub Plots Cards Column

#To DO: move weekly decision to bottom and give sub plots their own card
      layout_columns(
        col_widths = 12,
        # Decision Card
        card(
          card_header("Make weekly decision"),
          card_body(
  #To Do: Update with final terminology/decision matrix
            radioButtons("weekly_decision", "Select decision:",
                         choices = c("AA" = "aa",
                                     "ANO" = "ano",
                                     "TF" = "tf",
                                     "OF" = "of",
                                     "OA" = "oa",
                                     "S" = "s"),
                         selected = "s"),
            div(
              checkboxInput("remove_fail", "Remove OMIT data", value = FALSE),
              style = "margin: auto"
            ),
            uiOutput("submit_decision_ui")
          )
        ),

      # Sub Plots Card (conditionally shown)
      uiOutput("subplot_card"),
      uiOutput("brush_card")
      )
    )
  ),

#### Tab 3: Final Data View ####
## To do: This doesnt work at all yet, just a placeholder
  nav_panel(
    title = "Final Data",
    card(
      card_header("Processed Dataset"),
      card_body(
        downloadButton("download_data", "Download Data"),
        br(), br(),
        DTOutput("final_table")
      )
    )
  )
)


#### Server ####

# Server Definition
server <- function(input, output, session) {
#### Reactive values ####
  data <- reactiveVal(NULL)
  current_week <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL) # site param df
  all_datasets <- reactiveVal(NULL)
  brush_active <- reactiveVal(FALSE)


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
                      choices = sites)
  })

  # Update parameter choices when site changes
  observe({
    req(input$directory, input$site, all_datasets())
    parameters <- get_parameters(all_datasets(), input$directory, input$site)
    updateSelectInput(session, "parameter",
                      choices = parameters)
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


    updateTabsetPanel(session, inputId = "tabs", selected = "Data Verification")
    # Try to get the specific dataset
    tryCatch({
      site_param_df <- working_data[[site_param_name]]

      if (is.null(site_param_df)) {
        stop(paste("Dataset", site_param_name, "not found"))
      }

      sel_data <- working_data[[site_param_name]]
      processed <- sel_data%>%
        #FOR TESTING PURPOSES ONLY
        mutate(omit = NA,
               user = NA,
               final_status = NA)


      # Store the processed data
      selected_data(processed)

      # Set initial week
      current_week(min(processed$week))

      # Navigate to verification tab
      updateNavbarPage(session, "navbar", selected = "Data Verification")

    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
    })
  })

  #### Data Verification functions ####
# Previous Tab
  observeEvent(input$prev_tab, {
    updateNavbarPage(session, "tabs", selected = "Data Selection")
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
      if (input$remove_fail) {
        week_choice_data <- week_choice_data %>%
          filter(final_decision != "OMIT")
      }

      p <- ggplot(week_choice_data, aes(x = DT_round)) +
        geom_point(aes(y = mean, color = final_decision))+
        labs(
          title = paste0("Weekly Data for:", input$site, "-", input$parameter),
          x = "Date",
          y = input$parameter )+
        scale_color_manual(values = final_decision_colors)

      plot(p)
    } else {
  #TO DO: Swap with create weekly plot function call
   p <- ggplot(week_data, aes(x = DT_round)) +
      geom_point(aes(y = mean, color = flag))+
     #Add Omitted data in red
      geom_point(data = week_data %>%filter(omit == TRUE),aes(y = mean), color = "red")+
      labs(
        title = paste0("Weekly Data for:", input$site, "-", input$parameter),
        x = "Date",
        y = input$parameter )

   # Add brush highlight if brush is active and brush exists

# This might need to be reactive later on ?
   if(input$toggle_brush && !is.null(input$plot_brush)) {
     req(input$plot_brush)

     # Get brushed points
     brushed_data <- brushedPoints(week_data, input$plot_brush,
                                   xvar = "DT_round", yvar = "mean")

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

## Subplot card UI
  output$subplot_card <- renderUI({
    req(input$show_subplots)

    card(
      card_header("Sub Parameter Plots"),
      card_body(
        plotOutput("sub_plots", height = "600px")
      )
    )
  })

## Sub plots output
  output$sub_plots <- renderPlot({
    req(all_datasets(), current_week(), input$show_subplots, input$sub_parameters)

    #To do: this should probably be moved to make it faster?
    datasets <- all_datasets()
    working_data <- if(input$directory == "pre") {
      datasets$pre_verification_data
    } else {
      datasets$intermediary_data
    }


    # Create individual plots for each sub parameter
    plots <- map(input$sub_parameters, function(param) {

      site_param_name <- paste0(input$site, "-", param)
      site_param_df <- working_data[[site_param_name]]
      week_data <- filter(site_param_df, week == current_week())

      ggplot(week_data, aes(x = DT_round)) +
        geom_point(aes(y = mean))+
        labs(x = "Date",
             y = param) +
        theme_minimal()
    })%>%
      compact()

    if (length(plots) > 0) {
      gridExtra::grid.arrange(grobs = plots, ncol = 1)
    }
  })

## Brush card
  # UI
  output$brush_card <- renderUI({
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          h4("Data Selection Tools", class = "m-0"),
          checkboxInput("toggle_brush", "Enable Brush Tool", value = FALSE)
        )
      ),
      card_body(
        conditionalPanel(
          condition = "input.toggle_brush == true",
          radioButtons("brush_action", "Select Action:",
                       choices = c("Accept" = "A",
                                   "Flag" = "F",
                                   "Omit" = "O"),
                       selected = character(0)),

          # Show flag options if Flag is selected
          conditionalPanel(
            condition = "input.brush_action == 'F'",
            selectInput("user_brush_flags", "Select Flags:",
                        choices = c("sv" = "sv",
                                    "suspect data" = "suspect",
                                    "sensor malfunction" = "malfunction",
                                    "drift" = "drift"),
                        multiple = TRUE)
          ),

          # Conditional submit button
          uiOutput("brush_submit_ui")
        )
      )
    )
  })

  # Brush submit button UI
  output$brush_submit_ui <- renderUI({
    req(input$plot_brush, input$brush_action)
    # For Flag action, also require flag options
    if(input$brush_action %in% c("F")) {
      req(input$user_brush_flags)
    }
    actionButton("submit_brush", "Submit Selection", class = "btn-success")
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

## Weekly Decision
  # Submit decision button UI
  output$submit_decision_ui <- renderUI({
    req(input$weekly_decision)
    if (input$weekly_decision != "s") {
      actionButton("submit_decision", "Submit Weekly Decision",
                   class = "btn-success")
    }
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
    if (idx < length(weeks)) {
      current_week(weeks[idx + 1])
    }else{
#TO DO: This should actually check if all data has been reviewed

      showNotification("All weeks have been reviewed.", type = "message")
      updateTabsetPanel(session, inputId = "tabs", selected = "Data Verification")
    }

    # Show notification of submission
    showNotification(
      paste("Decision", toupper(input$weekly_decision), "submitted"),
      type = "message"
    )
    # Reset weekly decision back to "s"
    updateRadioButtons(session, "weekly_decision", selected = "s")
  })


##### Final Verification Tab ####
  # Final table output
  output$final_table <- renderDT({
    req(selected_data())
    datatable(selected_data(),
              options = list(pageLength = 10),
              style = "bootstrap5")
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$site, "_", input$directory, "_processed.csv")
    },
    content = function(file) {
      selected_data() %>% write_csv(file)
    }
  )

  #### Extras ####
  # Handle quit button
  observeEvent(input$quit_app, {
    stopApp()

  #TO DO: Update backend and save file
  })

}

shinyApp(ui, server)
