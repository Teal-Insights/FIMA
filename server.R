
# start: ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(reactable)
  library(openxlsx)
})
# turn off warnings
options(warn = -1)
options(scipen = 999)

# load necessary scripts
source(file = "R/server/fima_baseline_scenario.R")
source(file = "R/server/fima_server_interventions.R")
source(file = "R/server/fima_alternative_scenario.R")

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# server
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  # -------------------------------------------------------------------------
  # interventions
  # -------------------------------------------------------------------------
  
  # Create a reactive value to track all selected interventions
  selected_interventions <- reactive({
    # Combine all selected interventions from different KPIs
    c(
      if ("protection_gap" %in% input$kpi_selection) input$protection_gap_interventions else c(),
      if ("land_use" %in% input$kpi_selection) input$land_use_interventions else c()
    )
  })
  # -------------------------------------------------------------------------
  # data --------------------------------------------------------------------
  # data preparation
  # -------------------------------------------------------------------------
  server_data_baseline <- fima_baseline_scenario()
  
  get_interventions <- reactive({
    req(selected_interventions())
    fima_data_interventions(chosen_interventions = selected_interventions())
  })
  
  server_data_interventions <- interventions_results <- reactive({
    req(server_data_baseline, get_interventions())
    # Apply function with current selections
    fima_server_interventions(
      data_baseline = server_data_baseline,
      data_interventions = get_interventions(),
      chosen_interventions = selected_interventions()
    )
  })
  
  server_data_alternative <- reactive({
    req(server_data_interventions())
    fima_alternative_scenario(
      data_baseline = server_data_baseline,
      data_interventions = server_data_interventions()
    )
  })
  
  server_data_alternative_viz <- reactive({
    # Try to generate the alternative visualization data
    result <- tryCatch(
      {
        data <- fima_alternative_viz(
          data_baseline = server_data_baseline,
          data_alternative = server_data_alternative()
        )
        
        # Check if the returned data exists (is not NULL or NA)
        if (is.null(data) || all(is.na(data))) {
          # Return baseline data if result is NULL or all NA
          server_data_baseline %>% mutate(group = "Baseline Scenario")  
        } else {
          data  # Return the alternative viz data if it exists
        }
      },
      error = function(e) {
        # Return the baseline data in case of error
        server_data_baseline %>% mutate(group = "Baseline Scenario")
      },
      warning = function(w) {
        # Continue with the result
        NULL
      }
    )
    
    # If result is NULL after warning handler, return baseline
    if (is.null(result)) {
      server_data_baseline %>% mutate(group = "Baseline Scenario")
    } else {
      result
    }
  })
  
  # observe({
  #   data <- server_data_alternative_viz()
  #   print("Class of server_data_alternative_viz:")
  #   print(class(data))
  #   # If you want to see structure as well
  #   str(data)
  # })
  # -------------------------------------------------------------------------
  # analysis tab - value boxes
  # -------------------------------------------------------------------------
  # Credit Rating
  output$vb_cra <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2025,2033)) %>% 
      select(year, credit_rating, Scenario = group) %>% 
      mutate(Scenario = str_remove_all(string = Scenario, pattern = " Scenario")) %>% 
      pivot_wider(names_from = year, values_from = credit_rating)
    # Check if there are multiple scenarios to compare
    total_rows <- nrow(data) > 1
    
    # Default colors if condition not met
    bgc_theme <- "#524f4e"
    bgc_header <- "#3e3b3a"
    
    # Only try to determine color change if we have multiple scenarios
    if (total_rows) {
      # Find baseline and alternative rows
      data_cra <- data %>% 
        mutate(value = fima_credit_rating_to_number(rating = `2033`)) %>% 
        select(Scenario,`2033` = "value") 
      
      baseline_row <- data_cra %>% filter(Scenario == "Baseline")
      alternative_row <- data_cra %>% filter(Scenario == "Alternative")
      
      # Check if both scenarios exist before comparing
      if (nrow(baseline_row) > 0 && nrow(alternative_row) > 0) {
        # Compare 2033 values - if baseline is higher than alternative, use green
        if (baseline_row$`2033` < alternative_row$`2033`) {
          bgc_theme <- "green"
          bgc_header <- "darkgreen"
        }
      }
    }
    
    # Create and return the reactable
    reactable::reactable(
      data = data,
      theme = reactable::reactableTheme(
        backgroundColor = bgc_theme,
        color = "white",
        borderColor = "rgba(255, 255, 255, 0.3)",
        headerStyle = list(
          backgroundColor = bgc_header,
          color = "white",
          padding = "2px 4px"  # Reduce header padding (vertical, horizontal)
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduce cell padding to match
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(
          width = 110,  # Slightly reduced
          headerStyle = list(paddingLeft = "4px")  # Adjust left padding specifically
        ),
        `2025` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        ),
        `2033` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        )
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Debt to Nominal GDP ratio,%
  output$vb_debt <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2025, 2033)) %>% 
      select(year, value = gross_debt_pct_gdp, Scenario = group) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = year, values_from = value)
    
    # Check if there are multiple scenarios to compare
    total_rows <- nrow(data) > 1
    
    # Default colors if condition not met
    bgc_theme <- "#524f4e"
    bgc_header <- "#3e3b3a"
    
    # Only try to determine color change if we have multiple scenarios
    if (total_rows) {
      # Find baseline and alternative rows
      baseline_row <- data %>% filter(Scenario == "Baseline")
      alternative_row <- data %>% filter(Scenario == "Alternative")
      
      # Check if both scenarios exist before comparing
      if (nrow(baseline_row) > 0 && nrow(alternative_row) > 0) {
        # Compare 2033 values - if baseline is higher than alternative, use green
        if (baseline_row$`2033` > alternative_row$`2033`) {
          bgc_theme <- "green"
          bgc_header <- "darkgreen"
        }
      }
    }
    
    # Create and return the reactable
    reactable::reactable(
      data = data,
      theme = reactable::reactableTheme(
        backgroundColor = bgc_theme,
        color = "white",
        borderColor = "rgba(255, 255, 255, 0.3)",
        headerStyle = list(
          backgroundColor = bgc_header,
          color = "white",
          padding = "2px 4px"  # Reduce header padding (vertical, horizontal)
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduce cell padding to match
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(
          width = 110,  # Slightly reduced
          headerStyle = list(paddingLeft = "4px")  # Adjust left padding specifically
        ),
        `2025` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        ),
        `2033` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        )
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Nominal GDP growth (%)
  output$vb_ngdp_growth <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2025,2033)) %>% 
      select(year, value = gdp_growth_pct, Scenario = group) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = year, values_from = value)
    
    # Check if there are multiple scenarios to compare
    total_rows <- nrow(data) > 1
    
    # Default colors if condition not met
    bgc_theme <- "#524f4e"
    bgc_header <- "#3e3b3a"
    
    # Only try to determine color change if we have multiple scenarios
    if (total_rows) {
      # Find baseline and alternative rows
      baseline_row <- data %>% filter(Scenario == "Baseline")
      alternative_row <- data %>% filter(Scenario == "Alternative")
      
      # Check if both scenarios exist before comparing
      if (nrow(baseline_row) > 0 && nrow(alternative_row) > 0) {
        # Compare 2033 values - if baseline is higher than alternative, use green
        if (baseline_row$`2033` < alternative_row$`2033`) {
          bgc_theme <- "green"
          bgc_header <- "darkgreen"
        }
      }
    }
    
    # Create and return the reactable
    reactable::reactable(
      data = data,
      theme = reactable::reactableTheme(
        backgroundColor = bgc_theme,
        color = "white",
        borderColor = "rgba(255, 255, 255, 0.3)",
        headerStyle = list(
          backgroundColor = bgc_header,
          color = "white",
          padding = "2px 4px"  # Reduce header padding (vertical, horizontal)
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduce cell padding to match
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(
          width = 110,  # Slightly reduced
          headerStyle = list(paddingLeft = "4px")  # Adjust left padding specifically
        ),
        `2025` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        ),
        `2033` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        )
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  # Interest Payments (% Revenue)
  output$vb_interest <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2025,2033)) %>% 
      select(year, value = interest_payments_pct_revenue, Scenario = group) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(
        names_from = year, 
        values_from = value
      )
    
    # Check if there are multiple scenarios to compare
    total_rows <- nrow(data) > 1
    
    # Default colors if condition not met
    bgc_theme <- "#524f4e"
    bgc_header <- "#3e3b3a"
    
    # Only try to determine color change if we have multiple scenarios
    if (total_rows) {
      # Find baseline and alternative rows
      baseline_row <- data %>% filter(Scenario == "Baseline")
      alternative_row <- data %>% filter(Scenario == "Alternative")
      
      # Check if both scenarios exist before comparing
      if (nrow(baseline_row) > 0 && nrow(alternative_row) > 0) {
        # Compare 2033 values - if baseline is higher than alternative, use green
        if (baseline_row$`2033` > alternative_row$`2033`) {
          bgc_theme <- "green"
          bgc_header <- "darkgreen"
        }
      }
    }
    
    # Create and return the reactable
    reactable::reactable(
      data = data,
      theme = reactable::reactableTheme(
        backgroundColor = bgc_theme,
        color = "white",
        borderColor = "rgba(255, 255, 255, 0.3)",
        headerStyle = list(
          backgroundColor = bgc_header,
          color = "white",
          padding = "2px 4px"  # Reduce header padding (vertical, horizontal)
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduce cell padding to match
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(
          width = 110,  # Slightly reduced
          headerStyle = list(paddingLeft = "4px")  # Adjust left padding specifically
        ),
        `2025` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        ),
        `2033` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        )
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Primary Balance, % of Nominal GDP
  output$vb_pb <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2025,2033)) %>% 
      select(year, value = primary_net_lending_pct_gdp, Scenario = group) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = year, values_from = value)
    
    # Check if there are multiple scenarios to compare
    total_rows <- nrow(data) > 1
    
    # Default colors if condition not met
    bgc_theme <- "#524f4e"
    bgc_header <- "#3e3b3a"
    
    # Only try to determine color change if we have multiple scenarios
    if (total_rows) {
      # Find baseline and alternative rows
      baseline_row <- data %>% filter(Scenario == "Baseline")
      alternative_row <- data %>% filter(Scenario == "Alternative")
      
      # Check if both scenarios exist before comparing
      if (nrow(baseline_row) > 0 && nrow(alternative_row) > 0) {
        # Compare 2033 values - if baseline is higher than alternative, use green
        if (baseline_row$`2033` < alternative_row$`2033`) {
          bgc_theme <- "green"
          bgc_header <- "darkgreen"
        }
      }
    }
    
    # Create and return the reactable
    reactable::reactable(
      data = data,
      theme = reactable::reactableTheme(
        backgroundColor = bgc_theme,
        color = "white",
        borderColor = "rgba(255, 255, 255, 0.3)",
        headerStyle = list(
          backgroundColor = bgc_header,
          color = "white",
          padding = "2px 4px"  # Reduce header padding (vertical, horizontal)
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduce cell padding to match
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(
          width = 110,  # Slightly reduced
          headerStyle = list(paddingLeft = "4px")  # Adjust left padding specifically
        ),
        `2025` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        ),
        `2033` = colDef(
          width = 75,  # Reduced width
          align = "center"  # Center alignment helps with space perception
        )
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  # -------------------------------------------------------------------------
  # render data - data tab - Baseline Scenario
  # -------------------------------------------------------------------------
  output$data_table_baseline <- renderReactable({
    processed_data <- server_data_baseline %>% 
      select(
        # year
        year, 
        # gross debt 
        gross_debt_pct_gdp,
        # primary balance
        primary_net_lending_pct_gdp,
        # interest payment
        interest_payments_pct_revenue,
        # nominal GDP
        gdp_growth_pct,
        # credit rating
        credit_rating
      ) %>% 
      # Round all numeric values except 'year'
      mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
      # Convert all variables to character
      mutate(across(everything(), as.character)) %>%
      # Pivot longer
      pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
      # Pivot wider
      pivot_wider(names_from = year, values_from = values) %>% 
      # Clean up indicators names
      mutate(
        indicators = str_replace(indicators, "_", " "),
        indicators = str_replace(indicators, "pct", "% of"),
        indicators = str_to_title(str_replace_all(indicators, "_", " ")),
        indicators = str_replace(indicators, "Gdp", "GDP"),
        indicators = str_replace(indicators, "Of", "of"),
        indicators = str_replace(indicators, "GDP Growth % of", "GDP Growth %"),
        indicators = str_replace(indicators, "Dspb", "Debt-stabilising primary balance"),
        indicators = str_replace(indicators, " Ngdp| GDP", " NGDP")
      )
    
    # Return reactable with options
    reactable(
      processed_data,
      pagination = FALSE,  # Disable pagination
      showPagination = FALSE,  # Hide pagination controls
      defaultPageSize = 500,  # Show a large number of rows by default
      showPageInfo = FALSE,  # Hide page info (e.g., "1-12 of 12 rows")
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(
          minWidth = 200,  # Set minimum width for indicators column
          width = 350,     # Set default width for indicators column
          sticky = "left"  # Freeze the indicators column on the left when scrolling
        )
      )
    )
  })
  # -------------------------------------------------------------------------
  # render data - data tab - Alternative Scenario
  # -------------------------------------------------------------------------
  server_data_tab <- reactive({
    result <- tryCatch(
      {
        data <- fima_alternative_table(
          data_baseline = server_data_baseline,
          data_alternative = server_data_alternative()
        )
        # Check if the returned data exists (is not NULL or NA)
        if (is.null(data) || all(is.na(data))) {
          # Return baseline data if result is NULL or all NA
          server_data_baseline %>%
            select(
              year,gross_debt_billions, gross_debt_pct_gdp,nominal_interest_rate,
              primary_net_lending_billions,primary_net_lending_pct_gdp,
              dspb_pct_ngdp,interest_payments_billions,interest_payments_pct_revenue,
              gdp_current_prices_billions,gdp_growth_pct,revenue_billions,credit_rating
            ) %>% 
            filter(year < 2024)  
        } else {
          data
        }
      },
      error = function(e) {
        # Return the baseline data in case of error
        server_data_baseline %>%
          select(
            year,gross_debt_billions, gross_debt_pct_gdp,nominal_interest_rate,
            primary_net_lending_billions,primary_net_lending_pct_gdp,
            dspb_pct_ngdp,interest_payments_billions,interest_payments_pct_revenue,
            gdp_current_prices_billions,gdp_growth_pct,revenue_billions,credit_rating
          ) %>% 
          filter(year < 2024)  
      },
      warning = function(w) {
        # Continue with the result
        NULL
      }
    )
    # If result is NULL after warning handler, return baseline
    if (is.null(result)) {
      server_data_baseline %>%
        select(
          year,gross_debt_billions, gross_debt_pct_gdp,nominal_interest_rate,
          primary_net_lending_billions,primary_net_lending_pct_gdp,
          dspb_pct_ngdp,interest_payments_billions,interest_payments_pct_revenue,
          gdp_current_prices_billions,gdp_growth_pct,revenue_billions,credit_rating
        ) %>% 
        filter(year < 2024)  
    } else {
      result
    }
  })
  output$data_table_alternative <- renderReactable({
    processed_data <- server_data_tab() %>% 
      select(
        # year
        year, 
        # gross debt 
        gross_debt_pct_gdp,
        # primary balance
        primary_net_lending_pct_gdp,
        # interest payment
        interest_payments_pct_revenue,
        # nominal GDP
        gdp_growth_pct,
        # credit rating
        credit_rating
      ) %>% 
      # Round all numeric values except 'year'
      mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
      # Convert all variables to character
      mutate(across(everything(), as.character)) %>%
      # Pivot longer
      pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
      # Pivot wider
      pivot_wider(names_from = year, values_from = values) %>% 
      # Clean up indicators names
      mutate(
        indicators = str_replace(indicators, "_", " "),
        indicators = str_replace(indicators, "pct", "% of"),
        indicators = str_to_title(str_replace_all(indicators, "_", " ")),
        indicators = str_replace(indicators, "Gdp", "GDP"),
        indicators = str_replace(indicators, "Of", "of"),
        indicators = str_replace(indicators, "GDP Growth % of", "GDP Growth %"),
        indicators = str_replace(indicators, "Dspb", "Debt-stabilising primary balance"),
        indicators = str_replace(indicators, " Ngdp| GDP", " NGDP")
      )
    
    # Return reactable with options
    reactable(
      processed_data,
      pagination = FALSE,  # Disable pagination
      showPagination = FALSE,  # Hide pagination controls
      defaultPageSize = 500,  # Show a large number of rows by default
      showPageInfo = FALSE,  # Hide page info (e.g., "1-12 of 12 rows")
      filterable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(
          minWidth = 200,  # Set minimum width for indicators column
          width = 350,     # Set default width for indicators column
          sticky = "left"  # Freeze the indicators column on the left when scrolling
        )
      )
    )
  })
  # -------------------------------------------------------------------------
  # download data - data tab
  # -------------------------------------------------------------------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste("FIMA_Explorer_Data_Ruritania",".xlsx", sep = "")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      baseline_data <- server_data_baseline %>% 
        select(
          # year
          year, 
          # gross debt 
          gross_debt_pct_gdp,
          # primary balance
          primary_net_lending_pct_gdp,
          # interest payment
          interest_payments_pct_revenue,
          # nominal GDP
          gdp_growth_pct,
          # credit rating
          credit_rating
        ) %>% 
        # Round all numeric values except 'year'
        mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
        # Convert all variables to character
        mutate(across(everything(), as.character)) %>%
        # Pivot longer
        pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
        # Pivot wider
        pivot_wider(names_from = year, values_from = values) %>% 
        # Clean up indicators names
        mutate(
          indicators = str_replace(indicators, "_", " "),
          indicators = str_replace(indicators, "pct", "% of"),
          indicators = str_to_title(str_replace_all(indicators, "_", " ")),
          indicators = str_replace(indicators, "Gdp", "GDP"),
          indicators = str_replace(indicators, "Of", "of"),
          indicators = str_replace(indicators, "GDP Growth % of", "GDP Growth %"),
          indicators = str_replace(indicators, "Dspb", "Debt-stabilising primary balance"),
          indicators = str_replace(indicators, " Ngdp| GDP", " NGDP")
        )
      
      # Add the first sheet with baseline data (non-reactive)
      addWorksheet(wb, "Baseline Scenario")
      writeData(wb, "Baseline Scenario", baseline_data, startRow = 1, startCol = 1)
      
      # Style the headers
      headerStyle <- createStyle(
        fontSize = 12, fontColour = "#FFFFFF", halign = "center",
        fgFill = "#4F81BD", border = "TopBottom", borderColour = "#4F81BD"
      )
      addStyle(wb, "Baseline Scenario", headerStyle, rows = 1, cols = 1:ncol(server_data_baseline))
      
      # Auto-size columns
      setColWidths(wb, "Baseline Scenario", cols = 1:ncol(server_data_baseline), widths = "auto")
      
      # Add the second sheet with alternative scenario data
      alternative_data <- server_data_tab() %>% 
        select(
          # year
          year, 
          # gross debt 
          gross_debt_pct_gdp,
          # primary balance
          primary_net_lending_pct_gdp,
          # interest payment
          interest_payments_pct_revenue,
          # nominal GDP
          gdp_growth_pct,
          # credit rating
          credit_rating
        ) %>% 
        # Round all numeric values except 'year'
        mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
        # Convert all variables to character
        mutate(across(everything(), as.character)) %>%
        # Pivot longer
        pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
        # Pivot wider
        pivot_wider(names_from = year, values_from = values) %>% 
        # Clean up indicators names
        mutate(
          indicators = str_replace(indicators, "_", " "),
          indicators = str_replace(indicators, "pct", "% of"),
          indicators = str_to_title(str_replace_all(indicators, "_", " ")),
          indicators = str_replace(indicators, "Gdp", "GDP"),
          indicators = str_replace(indicators, "Of", "of"),
          indicators = str_replace(indicators, "GDP Growth % of", "GDP Growth %"),
          indicators = str_replace(indicators, "Dspb", "Debt-stabilising primary balance"),
          indicators = str_replace(indicators, " Ngdp| GDP", " NGDP")
        )
      
      addWorksheet(wb, "Alternative Scenario")
      writeData(wb, "Alternative Scenario", alternative_data, startRow = 1, startCol = 1)
      
      # Apply the same styling to the second sheet
      addStyle(wb, "Alternative Scenario", headerStyle, rows = 1, cols = 1:ncol(alternative_data))
      setColWidths(wb, "Alternative Scenario", cols = 1:ncol(alternative_data), widths = "auto")
      
      # Save the workbook to the file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # -------------------------------------------------------------------------
  # visualizations - home tab
  # -------------------------------------------------------------------------
  # Credit rating 
  output$home_credit_rating <- echarts4r::renderEcharts4r({
    # Generate labels for all possible ratings
    rating_labels <- fima_cra_y_axis(y_values = 1:22)
    
    # Convert the mapping to JSON for all ratings
    rating_json <- jsonlite::toJSON(setNames(rating_labels, 1:22))
    
    server_data_alternative_viz() %>% 
      select(c(year, credit_rating_number, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      dplyr::group_by(group) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(credit_rating_number) %>%
      echarts4r::e_x_axis(
        name = "",
        type = "category"
      ) %>%
      echarts4r::e_y_axis(
        scale = FALSE,
        # Let's use JavaScript formatter for dynamic min/max
        min = "dataMin",  # Use data minimum
        max = "dataMax",  # Use data maximum
        minInterval = 1,  # Ensure integer steps
        axisLabel = list(
          formatter = htmlwidgets::JS(paste0("
        function(value) {
          var labels = ", rating_json, ";
          return labels[Math.round(value)] || value;
        }
      "))
        )
      ) %>%
      echarts4r::e_legend(
        bottom = "0%",
        orient = "horizontal",
        x = "center",
        padding = c(5, 10, 5, 10)
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS(paste0("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          var labels = ", rating_json, ";
          
          params.forEach(function(param) {
            var numValue = Number(param.value[1]);
            var formattedValue = labels[Math.round(numValue)] || numValue.toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + formattedValue;
          });
          return result;
        }
      ")),
        axisPointer = list(
          type = "cross"
        )
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        top = "5%",  
        bottom = "7%",
        left = "5%",
        right = "5%"
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  # General government gross debt (% GDP)
  output$home_debt_ngdp <- echarts4r::renderEcharts4r({
    server_data_alternative_viz() %>% 
      select(c(year, gross_debt_pct_gdp, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      dplyr::group_by(group) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(gross_debt_pct_gdp) %>%
      echarts4r::e_x_axis(
        name = "",
        type = "category"
      ) %>%
      echarts4r::e_y_axis(
        scale = TRUE
      ) %>%
      echarts4r::e_legend(
        bottom = "0%",
        orient = "horizontal",
        x = "center",
        padding = c(5, 10, 5, 10)
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          params.forEach(function(param) {
            var value = Number(param.value[1]).toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + value;
          });
          return result;
        }
      "),
        axisPointer = list(
          type = "cross"
        )
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        top = "5%",  
        bottom = "7%",
        left = "5%",
        right = "5%"
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  # Nominal GDP growth (%)
  output$home_ngdp_growth <- echarts4r::renderEcharts4r({
    server_data_alternative_viz() %>% 
      select(c(year, gdp_growth_pct, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      dplyr::group_by(group) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(gdp_growth_pct) %>%
      echarts4r::e_x_axis(
        name = "",
        type = "category"
      ) %>%
      echarts4r::e_y_axis(
        scale = TRUE
      ) %>%
      echarts4r::e_legend(
        bottom = "0%",
        orient = "horizontal",
        x = "center",
        padding = c(5, 10, 5, 10)
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          params.forEach(function(param) {
            var value = Number(param.value[1]).toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + value;
          });
          return result;
        }
      "),
        axisPointer = list(
          type = "cross"
        )
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        top = "5%",  
        bottom = "7%",
        left = "5%",
        right = "5%"
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  # General government interest payments (% Revenue)
  output$home_ir_revenue <- echarts4r::renderEcharts4r({
    server_data_alternative_viz() %>% 
      select(c(year, interest_payments_pct_revenue, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      dplyr::group_by(group) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(interest_payments_pct_revenue) %>%
      echarts4r::e_x_axis(
        name = "",
        type = "category"
      ) %>%
      echarts4r::e_y_axis(
        scale = TRUE
      ) %>%
      echarts4r::e_legend(
        bottom = "0%",
        orient = "horizontal",
        x = "center",
        padding = c(5, 10, 5, 10)
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          params.forEach(function(param) {
            var value = Number(param.value[1]).toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + value;
          });
          return result;
        }
      "),
        axisPointer = list(
          type = "cross"
        )
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        top = "5%",  
        bottom = "7%",
        left = "5%",
        right = "5%"
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
  # General government primary balance (% of Nominal GDP)
  output$home_pb <- echarts4r::renderEcharts4r({
    server_data_alternative_viz() %>% 
      select(c(year, primary_net_lending_pct_gdp, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      dplyr::group_by(group) %>%
      echarts4r::e_charts(year) %>%
      echarts4r::e_line(primary_net_lending_pct_gdp) %>%
      echarts4r::e_x_axis(
        name = "",
        type = "category"
      ) %>%
      echarts4r::e_y_axis(
        scale = TRUE
      ) %>%
      echarts4r::e_legend(
        bottom = "0%",
        orient = "horizontal",
        x = "center",
        padding = c(5, 10, 5, 10)
      ) %>%
      echarts4r::e_tooltip(
        trigger = "axis",
        formatter = htmlwidgets::JS("
        function(params) {
          var year = params[0].axisValue;
          var result = year;
          params.forEach(function(param) {
            var value = Number(param.value[1]).toFixed(3);
            result += '<br/>' + param.marker + param.seriesName + ': ' + value;
          });
          return result;
        }
      "),
        axisPointer = list(
          type = "cross"
        )
      ) %>%
      echarts4r::e_grid(
        containLabel = TRUE,
        top = "5%",  
        bottom = "7%",
        left = "5%",
        right = "5%"
      ) %>%
      e_toolbox_feature(feature = c("saveAsImage"))
  })
  
}


# end: --------------------------------------------------------------------


