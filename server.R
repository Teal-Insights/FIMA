
# start: ------------------------------------------------------------------
server <- function(input, output, session) {
  # -------------------------------------------------------------------------
  # data --------------------------------------------------------------------
  # data preparation
  # -------------------------------------------------------------------------


  # -------------------------------------------------------------------------
  # interventions
  # -------------------------------------------------------------------------
  # get baseline data for 2024-2033
  get_baseline_data <- function() {
    year_start <- 2024
    year_end <- year_start + 9
    years <- year_start:year_end
    # return dataframe
    data.frame(
      Year = years,
      NGDP_Growth = runif(length(years), 1.5, 3.5),
      Interest_Rate = runif(length(years), 1.0, 4.0),
      Primary_Balance = runif(length(years), -2.0, 2.0)
    )
  }
  
  # Generate baseline data
  baseline <- get_baseline_data()
  
  # Get active interventions based on KPI selection
  active_interventions <- reactive({
    if (input$id_kpi == "Protection Gap") {
      return(input$id_intervention_pg)
    } else if (input$id_kpi == "Regenerative Agriculture") {
      return(input$id_intervention_ra)
    } else {
      return(character(0))
    }
  })
  
  # Create reactive values to store user inputs
  values <- reactiveValues(
    ngdp = list(),
    interest = list(),
    pb = list()
  )
  
  # Initialize values when interventions change
  observe({
    interventions <- active_interventions()
    year_start <- 2024
    year_end <- year_start + 9
    years <- year_start:year_end
    
    for (intervention in interventions) {
      # Initialize NGDP values if not already set
      if (is.null(values$ngdp[[intervention]])) {
        values$ngdp[[intervention]] <- setNames(rep(0, length(years)), as.character(years))
      }
      
      # Initialize interest rate values if not already set
      if (is.null(values$interest[[intervention]])) {
        values$interest[[intervention]] <- setNames(rep(0, length(years)), as.character(years))
      }
      
      # Initialize primary balance values if not already set
      if (is.null(values$pb[[intervention]])) {
        values$pb[[intervention]] <- setNames(rep(0, length(years)), as.character(years))
      }
    }
  })
  
  # Create constant value controls for NGDP
  output$ngdp_constant_controls <- renderUI({
    interventions <- active_interventions()
    
    if (length(interventions) == 0) {
      return(NULL)
    }
    
    tagList(
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-2",
          selectInput(
            inputId = "ngdp_constant_intervention",
            label = NULL,
            choices = c("Select Intervention" = "", interventions),
            width = "200px"
          )
        ),
        div(
          class = "me-2",
          numericInput(
            inputId = "ngdp_constant_value",
            label = NULL,
            value = 0,
            step = 0.1,
            width = "100px"
          )
        ),
        actionButton(
          inputId = "apply_ngdp_constant",
          label = "Apply",
          class = "btn-sm btn-light"
        )
      )
    )
  })
  
  # Create constant value controls for Interest Rate
  output$interest_constant_controls <- renderUI({
    interventions <- active_interventions()
    
    if (length(interventions) == 0) {
      return(NULL)
    }
    
    tagList(
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-2",
          selectInput(
            inputId = "interest_constant_intervention",
            label = NULL,
            choices = c("Select Intervention" = "", interventions),
            width = "200px"
          )
        ),
        div(
          class = "me-2",
          numericInput(
            inputId = "interest_constant_value",
            label = NULL,
            value = 0,
            step = 0.1,
            width = "100px"
          )
        ),
        actionButton(
          inputId = "apply_interest_constant",
          label = "Apply",
          class = "btn-sm btn-light"
        )
      )
    )
  })
  
  # Create constant value controls for Primary Balance
  output$pb_constant_controls <- renderUI({
    interventions <- active_interventions()
    
    if (length(interventions) == 0) {
      return(NULL)
    }
    
    tagList(
      div(
        class = "d-flex align-items-center",
        div(
          class = "me-2",
          selectInput(
            inputId = "pb_constant_intervention",
            label = NULL,
            choices = c("Select Intervention" = "", interventions),
            width = "200px"
          )
        ),
        div(
          class = "me-2",
          numericInput(
            inputId = "pb_constant_value",
            label = NULL,
            value = 0,
            step = 0.1,
            width = "100px"
          )
        ),
        actionButton(
          inputId = "apply_pb_constant",
          label = "Apply",
          class = "btn-sm btn-light"
        )
      )
    )
  })
  
  # Apply constant value for NGDP
  observeEvent(input$apply_ngdp_constant, {
    intervention <- input$ngdp_constant_intervention
    value <- input$ngdp_constant_value
    
    req(intervention, value)
    if (intervention != "") {
      years <- as.character(baseline$Year)
      values$ngdp[[intervention]] <- setNames(rep(value, length(years)), years)
      
      # Update the UI inputs to reflect the new constant values
      for (year in years) {
        updateNumericInput(
          session,
          inputId = paste0("ngdp_", intervention, "_", year),
          value = value
        )
      }
    }
  })
  
  # Apply constant value for Interest Rate
  observeEvent(input$apply_interest_constant, {
    intervention <- input$interest_constant_intervention
    value <- input$interest_constant_value
    
    req(intervention, value)
    if (intervention != "") {
      years <- as.character(baseline$Year)
      values$interest[[intervention]] <- setNames(rep(value, length(years)), years)
      
      # Update the UI inputs to reflect the new constant values
      for (year in years) {
        updateNumericInput(
          session,
          inputId = paste0("interest_", intervention, "_", year),
          value = value
        )
      }
    }
  })
  
  # Apply constant value for Primary Balance
  observeEvent(input$apply_pb_constant, {
    intervention <- input$pb_constant_intervention
    value <- input$pb_constant_value
    
    req(intervention, value)
    if (intervention != "") {
      years <- as.character(baseline$Year)
      values$pb[[intervention]] <- setNames(rep(value, length(years)), years)
      
      # Update the UI inputs to reflect the new constant values
      for (year in years) {
        updateNumericInput(
          session,
          inputId = paste0("pb_", intervention, "_", year),
          value = value
        )
      }
    }
  })
  
  # Create NGDP table UI
  output$ngdp_inputs <- renderUI({
    interventions <- active_interventions()
    years <- baseline$Year
    
    # Create data for the table
    data <- data.frame(Year = years, Baseline = round(baseline$NGDP_Growth, 2))
    
    # Create a list to store the intervention columns
    intervention_columns <- list()
    
    # Add intervention columns
    for (intervention in interventions) {
      # Get the current values
      intervention_values <- sapply(years, function(year) {
        input_id <- paste0("ngdp_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          return(input[[input_id]])
        } else if (!is.null(values$ngdp[[intervention]][[as.character(year)]])) {
          return(values$ngdp[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      
      data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate policy shock
    if (length(interventions) > 0) {
      policy_shock <- data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + data[[paste0(intervention)]]
      }
      data$`Policy Shock` <- round(policy_shock, 2)
    } else {
      data$`Policy Shock` <- round(data$Baseline, 2)
    }
    
    # Create table UI
    table_ui <- fluidRow(
      column(12,
             tags$table(
               class = "table table-bordered",
               tags$thead(
                 tags$tr(
                   tags$th("Year"),
                   tags$th("Baseline"),
                   lapply(interventions, function(intervention) {
                     tags$th(paste0(intervention))
                   }),
                   tags$th("Policy Shock")
                 )
               ),
               tags$tbody(
                 lapply(1:nrow(data), function(i) {
                   year <- data$Year[i]
                   baseline_val <- data$Baseline[i]
                   policy_shock <- data$`Policy Shock`[i]
                   
                   tags$tr(
                     tags$td(year),
                     tags$td(baseline_val),
                     lapply(interventions, function(intervention) {
                       input_id <- paste0("ngdp_", intervention, "_", year)
                       tags$td(
                         numericInput(
                           inputId = input_id,
                           label = NULL,
                           value = if (!is.null(values$ngdp[[intervention]][[as.character(year)]])) {
                             values$ngdp[[intervention]][[as.character(year)]]
                           } else {
                             0
                           },
                           step = 0.1,
                           width = "100%"
                         )
                       )
                     }),
                     tags$td(policy_shock)
                   )
                 })
               )
             )
      )
    )
    
    table_ui
  })
  
  # Create Interest Rate table UI
  output$interest_inputs <- renderUI({
    interventions <- active_interventions()
    years <- baseline$Year
    
    # Create data for the table
    data <- data.frame(Year = years, Baseline = round(baseline$Interest_Rate, 2))
    
    # Create a list to store the intervention columns
    intervention_columns <- list()
    
    # Add intervention columns
    for (intervention in interventions) {
      # Get the current values
      intervention_values <- sapply(years, function(year) {
        input_id <- paste0("interest_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          return(input[[input_id]])
        } else if (!is.null(values$interest[[intervention]][[as.character(year)]])) {
          return(values$interest[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      
      data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate policy shock
    if (length(interventions) > 0) {
      policy_shock <- data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + data[[paste0(intervention)]]
      }
      data$`Policy Shock` <- round(policy_shock, 2)
    } else {
      data$`Policy Shock` <- round(data$Baseline, 2)
    }
    
    # Create table UI
    table_ui <- fluidRow(
      column(12,
             tags$table(
               class = "table table-bordered",
               tags$thead(
                 tags$tr(
                   tags$th("Year"),
                   tags$th("Baseline"),
                   lapply(interventions, function(intervention) {
                     tags$th(paste0(intervention))
                   }),
                   tags$th("Policy Shock")
                 )
               ),
               tags$tbody(
                 lapply(1:nrow(data), function(i) {
                   year <- data$Year[i]
                   baseline_val <- data$Baseline[i]
                   policy_shock <- data$`Policy Shock`[i]
                   
                   tags$tr(
                     tags$td(year),
                     tags$td(baseline_val),
                     lapply(interventions, function(intervention) {
                       input_id <- paste0("interest_", intervention, "_", year)
                       tags$td(
                         numericInput(
                           inputId = input_id,
                           label = NULL,
                           value = if (!is.null(values$interest[[intervention]][[as.character(year)]])) {
                             values$interest[[intervention]][[as.character(year)]]
                           } else {
                             0
                           },
                           step = 0.1,
                           width = "100%"
                         )
                       )
                     }),
                     tags$td(policy_shock)
                   )
                 })
               )
             )
      )
    )
    
    table_ui
  })
  
  # Create Primary Balance table UI
  output$pb_inputs <- renderUI({
    interventions <- active_interventions()
    years <- baseline$Year
    
    # Create data for the table
    data <- data.frame(Year = years, Baseline = round(baseline$Primary_Balance, 2))
    
    # Create a list to store the intervention columns
    intervention_columns <- list()
    
    # Add intervention columns
    for (intervention in interventions) {
      # Get the current values
      intervention_values <- sapply(years, function(year) {
        input_id <- paste0("pb_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          return(input[[input_id]])
        } else if (!is.null(values$pb[[intervention]][[as.character(year)]])) {
          return(values$pb[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      
      data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate policy shock
    if (length(interventions) > 0) {
      policy_shock <- data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + data[[paste0(intervention)]]
      }
      data$`Policy Shock` <- round(policy_shock, 2)
    } else {
      data$`Policy Shock` <- round(data$Baseline, 2)
    }
    
    # Create table UI
    table_ui <- fluidRow(
      column(12,
             tags$table(
               class = "table table-bordered",
               tags$thead(
                 tags$tr(
                   tags$th("Year"),
                   tags$th("Baseline"),
                   lapply(interventions, function(intervention) {
                     tags$th(paste0(intervention))
                   }),
                   tags$th("Policy Shock")
                 )
               ),
               tags$tbody(
                 lapply(1:nrow(data), function(i) {
                   year <- data$Year[i]
                   baseline_val <- data$Baseline[i]
                   policy_shock <- data$`Policy Shock`[i]
                   
                   tags$tr(
                     tags$td(year),
                     tags$td(baseline_val),
                     lapply(interventions, function(intervention) {
                       input_id <- paste0("pb_", intervention, "_", year)
                       tags$td(
                         numericInput(
                           inputId = input_id,
                           label = NULL,
                           value = if (!is.null(values$pb[[intervention]][[as.character(year)]])) {
                             values$pb[[intervention]][[as.character(year)]]
                           } else {
                             0
                           },
                           step = 0.1,
                           width = "100%"
                         )
                       )
                     }),
                     tags$td(policy_shock)
                   )
                 })
               )
             )
      )
    )
    
    table_ui
  })
  
  # Observe NGDP inputs and update reactive values
  observe({
    interventions <- active_interventions()
    years <- baseline$Year
    
    for (intervention in interventions) {
      for (year in years) {
        input_id <- paste0("ngdp_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          values$ngdp[[intervention]][[as.character(year)]] <- input[[input_id]]
        }
      }
    }
  })
  
  # Observe Interest Rate inputs and update reactive values
  observe({
    interventions <- active_interventions()
    years <- baseline$Year
    
    for (intervention in interventions) {
      for (year in years) {
        input_id <- paste0("interest_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          values$interest[[intervention]][[as.character(year)]] <- input[[input_id]]
        }
      }
    }
  })
  
  # Observe Primary Balance inputs and update reactive values
  observe({
    interventions <- active_interventions()
    years <- baseline$Year
    
    for (intervention in interventions) {
      for (year in years) {
        input_id <- paste0("pb_", intervention, "_", year)
        if (!is.null(input[[input_id]])) {
          values$pb[[intervention]][[as.character(year)]] <- input[[input_id]]
        }
      }
    }
  })
  
  # Function to prepare all data for export or further computation
  prepare_all_data <- reactive({
    # Get current interventions
    interventions <- active_interventions()
    years <- baseline$Year
    
    # Prepare NGDP data
    ngdp_data <- data.frame(Year = years, Baseline = round(baseline$NGDP_Growth, 2))
    for (intervention in interventions) {
      intervention_values <- sapply(years, function(year) {
        if (!is.null(values$ngdp[[intervention]][[as.character(year)]])) {
          return(values$ngdp[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      ngdp_data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate NGDP policy shock
    if (length(interventions) > 0) {
      policy_shock <- ngdp_data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + ngdp_data[[paste0(intervention)]]
      }
      ngdp_data$Policy_Shock <- round(policy_shock, 2)
    } else {
      ngdp_data$Policy_Shock <- round(ngdp_data$Baseline, 2)
    }
    
    # Prepare Interest Rate data
    interest_data <- data.frame(Year = years, Baseline = round(baseline$Interest_Rate, 2))
    for (intervention in interventions) {
      intervention_values <- sapply(years, function(year) {
        if (!is.null(values$interest[[intervention]][[as.character(year)]])) {
          return(values$interest[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      interest_data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate Interest Rate policy shock
    if (length(interventions) > 0) {
      policy_shock <- interest_data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + interest_data[[paste0(intervention)]]
      }
      interest_data$Policy_Shock <- round(policy_shock, 2)
    } else {
      interest_data$Policy_Shock <- round(interest_data$Baseline, 2)
    }
    
    # Prepare Primary Balance data
    pb_data <- data.frame(Year = years, Baseline = round(baseline$Primary_Balance, 2))
    for (intervention in interventions) {
      intervention_values <- sapply(years, function(year) {
        if (!is.null(values$pb[[intervention]][[as.character(year)]])) {
          return(values$pb[[intervention]][[as.character(year)]])
        } else {
          return(0)
        }
      })
      pb_data[[paste0(intervention)]] <- intervention_values
    }
    
    # Calculate Primary Balance policy shock
    if (length(interventions) > 0) {
      policy_shock <- pb_data$Baseline
      for (intervention in interventions) {
        policy_shock <- policy_shock + pb_data[[paste0(intervention)]]
      }
      pb_data$Policy_Shock <- round(policy_shock, 2)
    } else {
      pb_data$Policy_Shock <- round(pb_data$Baseline, 2)
    }
    
    # Return a list with all data
    return(list(
      kpi = input$id_kpi,
      interventions = interventions,
      ngdp = ngdp_data,
      interest = interest_data,
      pb = pb_data
    ))
  })
  # For NGDP data
  output$data_table <- renderDataTable({
    data <- prepare_all_data()
    datatable(
      data = data$ngdp,
      options = list(pageLength = 11, searching = FALSE, scrollX = TRUE),
      caption = "NGDP Growth Data",
      rownames = FALSE
    )
  })
}


# end: --------------------------------------------------------------------


