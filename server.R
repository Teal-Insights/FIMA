
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
source(file = "R/server/fima_echarts_main.R")
source(file = "R/server/fima_echarts_ratings.R")

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# server
# -------------------------------------------------------------------------

server <- function(input, output, session) {

  # -------------------------------------------------------------------------
  # Hide tabs initially
  # -------------------------------------------------------------------------
  shiny::hideTab(inputId = "main_navbar", target = "analysis")
  shiny::hideTab(inputId = "main_navbar", target = "data")
  shiny::hideTab(inputId = "main_navbar", target = "docs")
  
  observeEvent(input$id_country, {
    if (!is.null(input$id_country) && input$id_country != "") {
      shiny::showTab(inputId = "main_navbar", target = "analysis")
      shiny::showTab(inputId = "main_navbar", target = "data")
      shiny::showTab(inputId = "main_navbar", target = "docs")
      # updateNavbarPage(session, "main_navbar", selected = "analysis")
    }
  })
  # -------------------------------------------------------------------------
  # selected interventions and instruments
  # -------------------------------------------------------------------------
  # Create reactive values to track all selected interventions and instruments
  selected_interventions <- reactive({
    # Combine all selected interventions from different KPIs
    c(
      if ("protection_gap" %in% input$kpi_selection) input$protection_gap_interventions else c(),
      if ("land_use" %in% input$kpi_selection) input$land_use_interventions else c()
    )
  })
  
  # Create reactive value to track all selected instruments
  selected_instruments <- reactive({
    # Check if either protection_gap or land_use KPIs are selected
    if (any(c("protection_gap", "land_use") %in% input$kpi_selection)) {
      # Return the selected instruments from the id_instruments checkboxGroupInput
      input$id_instruments
    } else {
      # Return empty vector if neither KPI is selected
      c()
    }
  })
  
  # -------------------------------------------------------------------------
  # data preparation
  # -------------------------------------------------------------------------
  # selected country
  server_selected_country <- reactive({input$id_country})
  # One off adjustment to stock of debt (LCU billions)
  server_data_adjustment <- reactive({
    fima_adjustment(by_country = server_selected_country())
  })
  # selected KPIs
  server_data_kpi <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Interventions") %>% 
      filter(country == input$id_country) %>% 
      pull(type) %>% 
      unique() %>% 
      sort()
  })
  # selected land use interventions
  server_data_lu_interventions <- reactive({
    # Add debugging print statements
    print(paste("Current country selected:", input$id_country))
    
    interventions <- readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Interventions") %>% 
      filter(country == input$id_country) %>% 
      filter(type == "Land Use") %>% 
      pull(intervention) %>% 
      unique() %>% 
      sort()
    
    print("Filtered interventions:")
    print(interventions)
    
    return(interventions)
  })
  # selected protection gap interventions
  server_data_pg_interventions <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Interventions") %>% 
      filter(country == input$id_country) %>% 
      filter(type == "Protection Gap") %>% 
      pull(intervention) %>% 
      unique() %>% 
      sort()
  })
  # selected instruments
  server_data_instruments <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Instruments") %>% 
      filter(country == input$id_country) %>% 
      pull(instrument) %>% 
      unique() %>% 
      sort()
  })
  # selected vulnerabilities
  server_data_vulnerabilities <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Vulnerabilities") %>% 
      filter(country == input$id_country) %>% 
      pull(vulnerability) %>% 
      unique() %>% 
      sort()
  })
  # baseline data
  server_data_baseline <- reactive({
    fima_baseline_scenario(
      by_country = server_selected_country()
    )
  })
  
  # interventions and instruments data
  get_interventions_and_instruments <- reactive({
    req(selected_interventions())
    fima_data_interventions(
      chosen_interventions = selected_interventions(), 
      chosen_instruments = selected_instruments()
    )
  })
  
  # Apply interventions and instruments to calculate policy shocks
  server_data_interventions <- interventions_results <- reactive({
    req(server_data_baseline(), get_interventions_and_instruments())
    # Apply function with current selections
    fima_server_interventions(
      data_baseline = server_data_baseline(),
      data_interventions = get_interventions_and_instruments(),
      chosen_interventions = selected_interventions(),
      chosen_instruments = selected_instruments()
    )
  })
  # alternative scenario data
  server_data_alternative <- reactive({
    req(server_data_interventions())
    fima_alternative_scenario(
      data_baseline = server_data_baseline(),
      data_interventions = server_data_interventions(),
      data_adjustment = server_data_adjustment()
    )
  })
  
  server_data_alternative_viz <- reactive({
    # Try to generate the alternative visualization data
    result <- tryCatch(
      {
        data <- fima_alternative_viz(
          data_baseline = server_data_baseline(),
          data_alternative = server_data_alternative()
        )
        
        # Check if the returned data exists (is not NULL or NA)
        if (is.null(data) || all(is.na(data))) {
          # Return baseline data if result is NULL or all NA
          server_data_baseline() %>% mutate(group = "Baseline Scenario")  
        } else {
          data  # Return the alternative viz data if it exists
        }
      },
      error = function(e) {
        # Return the baseline data in case of error
        server_data_baseline() %>% mutate(group = "Baseline Scenario")
      },
      warning = function(w) {
        # Continue with the result
        NULL
      }
    )
    
    # If result is NULL after warning handler, return baseline
    if (is.null(result)) {
      server_data_baseline() %>% mutate(group = "Baseline Scenario")
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
    processed_data <- server_data_baseline() %>% 
      select(
        # year
        year, 
        # nominal GDP
        "Nominal GDP growth (yoy%)" = gdp_growth_pct,
        # gross debt 
        "General government gross debt (% NGDP)" = gross_debt_pct_gdp,
        # primary balance
        "General government primary balance (% NGDP)" = primary_net_lending_pct_gdp,
        # interest payment
        "General government interest payments (% Revenue)" = interest_payments_pct_revenue,
        # credit rating
        "Credit Rating" = credit_rating
      ) %>% 
      # Round all numeric values except 'year'
      mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
      # Convert all variables to character
      mutate(across(everything(), as.character)) %>%
      # Pivot longer
      pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
      # Pivot wider
      pivot_wider(names_from = year, values_from = values)
    
    # Return reactable with options
    reactable(
      processed_data,
      pagination = FALSE,  # Disable pagination
      showPagination = FALSE,  # Hide pagination controls
      showPageInfo = FALSE,  # Hide page info (e.g., "1-12 of 12 rows")
      filterable = FALSE,
      searchable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(
          minWidth = 200,  # Set minimum width for indicators column
          width = 400,     # Set default width for indicators column
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
          data_baseline = server_data_baseline(),
          data_alternative = server_data_alternative()
        )
        # Check if the returned data exists (is not NULL or NA)
        if (is.null(data) || all(is.na(data))) {
          # Return baseline data if result is NULL or all NA
          server_data_baseline() %>%
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
        server_data_baseline() %>%
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
      server_data_baseline() %>%
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
        # nominal GDP
        "Nominal GDP growth (yoy%)" = gdp_growth_pct,
        # gross debt 
        "General government gross debt (% NGDP)" = gross_debt_pct_gdp,
        # primary balance
        "General government primary balance (% NGDP)" = primary_net_lending_pct_gdp,
        # interest payment
        "General government interest payments (% Revenue)" = interest_payments_pct_revenue,
        # credit rating
        "Credit Rating" = credit_rating
      ) %>% 
      # Round all numeric values except 'year'
      mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
      # Convert all variables to character
      mutate(across(everything(), as.character)) %>%
      # Pivot longer
      pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
      # Pivot wider
      pivot_wider(names_from = year, values_from = values)
    
    # Return reactable with options
    reactable(
      processed_data,
      pagination = FALSE,  # Disable pagination
      showPagination = FALSE,  # Hide pagination controls
      showPageInfo = FALSE,  # Hide page info (e.g., "1-12 of 12 rows")
      filterable = FALSE,
      searchable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE,
      resizable = TRUE,
      columns = list(
        indicators = colDef(
          minWidth = 200,  # Set minimum width for indicators column
          width = 400,     # Set default width for indicators column
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
      paste("FIMA_Explorer_Data_",input$id_country,".xlsx", sep = "")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      baseline_data <- server_data_baseline() %>% 
        select(
          # year
          year, 
          # nominal GDP
          "Nominal GDP growth (yoy%)" = gdp_growth_pct,
          # gross debt 
          "General government gross debt (% NGDP)" = gross_debt_pct_gdp,
          # primary balance
          "General government primary balance (% NGDP)" = primary_net_lending_pct_gdp,
          # interest payment
          "General government interest payments (% Revenue)" = interest_payments_pct_revenue,
          # credit rating
          "Credit Rating" = credit_rating
        ) %>% 
        # Round all numeric values except 'year'
        mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
        # Convert all variables to character
        mutate(across(everything(), as.character)) %>%
        # Pivot longer
        pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
        # Pivot wider
        pivot_wider(names_from = year, values_from = values)
      
      # Add the first sheet with baseline data (non-reactive)
      addWorksheet(wb, "Baseline Scenario")
      writeData(wb, "Baseline Scenario", baseline_data, startRow = 1, startCol = 1)
      
      # Style the headers
      headerStyle <- createStyle(
        fontSize = 12, fontColour = "#FFFFFF", halign = "center",
        fgFill = "#4F81BD", border = "TopBottom", borderColour = "#4F81BD"
      )
      addStyle(wb, "Baseline Scenario", headerStyle, rows = 1, cols = 1:ncol(baseline_data))
      
      # Auto-size columns
      setColWidths(wb, "Baseline Scenario", cols = 1:ncol(baseline_data), widths = "auto")
      
      # Add the second sheet with alternative scenario data
      alternative_data <- server_data_tab() %>% 
        select(
          # year
          year, 
          # nominal GDP
          "Nominal GDP growth (yoy%)" = gdp_growth_pct,
          # gross debt 
          "General government gross debt (% NGDP)" = gross_debt_pct_gdp,
          # primary balance
          "General government primary balance (% NGDP)" = primary_net_lending_pct_gdp,
          # interest payment
          "General government interest payments (% Revenue)" = interest_payments_pct_revenue,
          # credit rating
          "Credit Rating" = credit_rating
        ) %>% 
        # Round all numeric values except 'year'
        mutate(across(where(is.numeric) & !year, ~round(., digits = 1))) %>%
        # Convert all variables to character
        mutate(across(everything(), as.character)) %>%
        # Pivot longer
        pivot_longer(cols = -year,names_to = "indicators",values_to = "values") %>%
        # Pivot wider
        pivot_wider(names_from = year, values_from = values)
      
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
    fima_echarts_ratings(
      data = server_data_alternative_viz(),
      x_col = "year",
      y_col = "credit_rating_number",
      group_col = "group",
      group_levels = c("Baseline Scenario", "Alternative Scenario")
    )
  })
  # General government gross debt (% GDP)
  output$home_debt_ngdp <- echarts4r::renderEcharts4r({
    fima_echarts_main(
      data = server_data_alternative_viz(),
      x_col = "year",
      y_col = "gross_debt_pct_gdp",
      group_col = "group",  # Make sure this column exists
      group_levels = c("Baseline Scenario", "Alternative Scenario")
    ) 
  })
  
  # Nominal GDP growth (%)
  output$home_ngdp_growth <- echarts4r::renderEcharts4r({
    fima_echarts_main(
      data = server_data_alternative_viz(),
      x_col = "year",
      y_col = "gdp_growth_pct",
      group_col = "group",  # Make sure this column exists
      group_levels = c("Baseline Scenario", "Alternative Scenario")
    )
  })
  
  # General government interest payments (% Revenue)
  output$home_ir_revenue <- echarts4r::renderEcharts4r({
    fima_echarts_main(
      data = server_data_alternative_viz(),
      x_col = "year",
      y_col = "interest_payments_pct_revenue",
      group_col = "group",  # Make sure this column exists
      group_levels = c("Baseline Scenario", "Alternative Scenario")
    )
  })
  
  # General government primary balance (% of Nominal GDP)
  output$home_pb <- echarts4r::renderEcharts4r({
    fima_echarts_main(
      data = server_data_alternative_viz(),
      x_col = "year",
      y_col = "primary_net_lending_pct_gdp",
      group_col = "group",  # Make sure this column exists
      group_levels = c("Baseline Scenario", "Alternative Scenario")
    )
  })
  # -------------------------------------------------------------------------
  # text output
  # -------------------------------------------------------------------------
  # KPI's
  output$analysis_kpi <- renderText({
    paste0("KPIs - ", input$id_country)
  })
  
  # baseline scenario
  output$data_text_baseline <- renderText({
    paste0(input$id_country, " - Baseline Scenario data")
  })
  
  # alternative scenario
  output$data_text_alternative <- renderText({
    paste0(input$id_country, " - Alternative Scenario data")
  })
  # -------------------------------------------------------------------------
  # server components
  # -------------------------------------------------------------------------
  # Vulnerabilities
  output$vulnerability_list <- renderUI({
    vulnerabilities <- server_data_vulnerabilities()
    # Create the UI elements for the vulnerabilities list
    tags$div(
      class = "kpi-list",
      lapply(vulnerabilities, function(item) {
        tags$div(
          class = "kpi-item",
          tags$span(
            class = "bullet-point",
            HTML("&#8226;") # Bullet point character
          ),
          tags$span(
            class = "kpi-label",
            style = "margin-left: 8px;",
            item
          )
        )
      })
    )
  })
  
  #  KPIs
  output$dynamic_kpi_checkboxes <- renderUI({
    # Get the KPI options from your reactive component
    kpi_options <- server_data_kpi()
    
    # Create internal values by converting each display name
    internal_values <- sapply(kpi_options, function(kpi) {
      tolower(gsub(" ", "_", kpi))
    })
    
    # Create named vector where names are display labels and values are internal codes
    kpi_choices <- setNames(internal_values, kpi_options)
    
    # Return the checkbox group with dynamic choices
    checkboxGroupInput(
      inputId = "kpi_selection",
      label = NULL,
      choices = kpi_choices,
      selected = NULL
    )
  })
  
  # Add an observer to print selected values to console
  observeEvent(input$kpi_selection, {
    print(input$kpi_selection)
  })
  
  # instruments
  output$dynamic_instruments_checkboxes <- renderUI({
    # Get the instruments options from your reactive component
    instruments_options <- server_data_instruments()
    
    # Print the original instruments options to console
    print("Available instruments options:")
    print(instruments_options)
    
    # Create internal values by converting each display name
    internal_values <- sapply(instruments_options, function(instrument) {
      tolower(gsub(" |-", "_", instrument))
    })
    
    # Create named vector where names are display labels and values are internal codes
    # NOTE: The order is flipped here compared to the KPI implementation
    instruments_choices <- setNames(internal_values, instruments_options)
    
    # Return the checkbox group with dynamic choices
    checkboxGroupInput(
      inputId = "id_instruments",
      label = NULL,
      choices = instruments_choices,
      selected = NULL
    )
  })
  
  # Add an observer to print selected values to console
  observeEvent(input$id_instruments, {
    print("Selected instruments:")
    print(input$id_instruments)
  })
  
  # land use
  output$dynamic_land_use_interventions_checkboxes <- renderUI({
    # Get the land use interventions from your reactive component
    lu_interventions_options <- server_data_lu_interventions()
    
    # Print the original options to console
    print("Available land use interventions:")
    print(lu_interventions_options)
    
    # Create internal values by converting each display name
    internal_values <- sapply(lu_interventions_options, function(intervention) {
      tolower(gsub(" |-", "_", intervention))
    })
    
    # Create named vector where display names are the original values and internal values have underscores
    lu_interventions_choices <- setNames(internal_values, lu_interventions_options)
    
    # Return the checkbox group with dynamic choices
    checkboxGroupInput(
      inputId = "land_use_interventions",
      label = NULL,
      choices = lu_interventions_choices,
      selected = NULL
    )
  })
  
  # Add an observer to print selected values to console
  observeEvent(input$land_use_interventions, {
    print("Selected land use interventions:")
    print(input$land_use_interventions)
  })
  # protection gap
  output$dynamic_protection_gap_interventions_checkboxes <- renderUI({
    # Get the protection gap interventions from your reactive component
    pg_interventions_options <- server_data_pg_interventions()
    
    # Print the original options to console
    print("Available protection gap interventions:")
    print(pg_interventions_options)
    
    # Create internal values by converting each display name
    internal_values <- sapply(pg_interventions_options, function(intervention) {
      tolower(gsub(" |-", "_", intervention))
    })
    
    # Create named vector where display names are the original values and internal values have underscores
    pg_interventions_choices <- setNames(internal_values, pg_interventions_options)
    
    # Return the checkbox group with dynamic choices
    checkboxGroupInput(
      inputId = "protection_gap_interventions",
      label = NULL,
      choices = pg_interventions_choices,
      selected = NULL
    )
  })
  
  # Add an observer to print selected values to console
  observeEvent(input$protection_gap_interventions, {
    print("Selected protection gap interventions:")
    print(input$protection_gap_interventions)
  })
  
  observeEvent(input$id_country, {
    print("Current server_data_lu_interventions values after country change:")
    print(server_data_lu_interventions())
  })
}

# end: --------------------------------------------------------------------


