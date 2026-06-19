library(shiny)
library(ggplot2)
library(dplyr)

# Generate three different time series datasets
set.seed(123)

generate_time_series <- function(n_days = 2) {
  timestamps <- seq.POSIXt(from = Sys.time() - 60 * 60 * 24 * n_days, 
                           to = Sys.time(), by = "15 min")
  values <- rnorm(length(timestamps), mean = 10, sd = 2)
  data.frame(
    timestamp = timestamps,
    value = values,
    flag = rep(NA, length(timestamps)),
    comments = rep(NA, length(timestamps)), 
    user_commented = rep(NA, length(timestamps))
  )
}

# Three sample time series datasets
time_series_data_pre <- list(
  "legacy" = list(
    "Temperature" = generate_time_series(2),
    "Depth" = generate_time_series(3),
    "Turbidity" = generate_time_series(4)
  ),
  "lincoln" = list(
    "Temperature" = generate_time_series(2),
    "Depth" = generate_time_series(3),
    "Turbidity" = generate_time_series(4)
  ),
  "timberline" = list(
    "Temperature" = generate_time_series(2),
    "Depth" = generate_time_series(3),
    "Turbidity" = generate_time_series(4)
  )
)
time_series_data_int <- time_series_data_pre # Duplicate for simplicity

# Define UI
ui <- fluidPage(
  titlePanel("Time Series Viewer"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # Tab 1: Selection panel
    tabPanel(
      "Select Data",
      sidebarLayout(
        sidebarPanel(
          selectInput("directory", "Select Directory", choices = c("pre", "int")),
          selectInput("site_code", "Select Site Code", choices = c("legacy", "lincoln", "timberline")),
          selectInput("parameter", "Select Parameter", choices = c("Temperature", "Depth", "Turbidity")),
          actionButton("submit", "Submit")
        ),
        mainPanel(
          h4("Please select the directory, site code, and parameter, then press Submit.")
        )
      )
    ),
    
    # Tab 2: Plot panel
    tabPanel(
      "Time Series Plot",
      sidebarLayout(
        sidebarPanel(
          textInput("flag_input", "Alter flag value", value = ""),
          textInput("comment_input", "Alter comment value", value = ""),
          selectInput("user", "Select User", choices = c("SJS", "JDT", "CLM", "AS"), selected = "SJS"),
          actionButton("submit_changes", "Submit Changes"),
          tableOutput("brushed_points_table")
        ),
        mainPanel(
          plotOutput("selected_plot", brush = "plot_brush")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive to store selected data
  selected_data <- reactiveVal(NULL)
  
  # Observe the submit button to update selected data
  observeEvent(input$submit, {
    req(input$directory, input$site_code, input$parameter)
    
    data <- if (input$directory == "pre") {
      time_series_data_pre[[input$site_code]][[input$parameter]]
    } else {
      time_series_data_int[[input$site_code]][[input$parameter]]
    }
    
    selected_data(data)
    
    updateTabsetPanel(session, "main_tabs", selected = "Time Series Plot")
  })
  
  # Render the plot with dynamic coloring based on the 'flag' column
  output$selected_plot <- renderPlot({
    req(selected_data())
    plot_data <- selected_data()
    
    ggplot(plot_data, aes(x = timestamp, y = value, color = flag)) +
      geom_point(size = 3) +
      labs(color = "Flag", 
           y = input$parameter,
           title = paste0(input$parameter, " data from ",  input$site_code)) +
      theme_minimal() 
  })
  
  # Show the brushed points in a table
  output$brushed_points_table <- renderTable({
    req(selected_data())
    brushedPoints(selected_data(), input$plot_brush)
  })
  
  # Update data based on brushed points and user inputs
  observeEvent(input$submit_changes, {
    req(selected_data())
    brushed <- brushedPoints(selected_data(), input$plot_brush)
    
    if (nrow(brushed) > 0) {
      updated_data <- selected_data() %>%
        mutate(
          flag = ifelse(row.names(selected_data()) %in% row.names(brushed), input$flag_input, flag),
          comments = ifelse(row.names(selected_data()) %in% row.names(brushed), input$comment_input, comments), 
          user_commented = ifelse(row.names(selected_data()) %in% row.names(brushed), input$user, user_commented)
        )
      selected_data(updated_data)
      showNotification("Changes saved.", type = "message")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

