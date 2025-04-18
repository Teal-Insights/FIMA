
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
    interventions <- readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Interventions") %>% 
      filter(country == input$id_country) %>% 
      filter(type == "Land Use") %>% 
      pull(intervention) %>% 
      unique()
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
      unique()
  })
  # selected instruments
  server_data_instruments <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "Instruments") %>% 
      filter(country == input$id_country) %>% 
      pull(instrument) %>% 
      unique()
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
  
  # selected color code
  server_data_color_code <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "ColorCode") %>% 
      filter(country == input$id_country) %>% 
      mutate(
        picked_color = case_when(
          color_type == "Dark green" ~ "#006400",
          color_type == "Medium green" ~ "#0a830a",
          color_type == "Light green" ~ "#e8f5e8",
          .default = "#006400"
        )
      )
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
      filter(year %in% c(2033)) %>% 
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
          padding = "2px 4px"  # Reduced header padding
        ),
        cellStyle = list(
          padding = "2px 4px"  # Reduced cell padding
        )
      ),
      compact = TRUE,
      columns = list(
        Scenario = colDef(width = 110, headerStyle = list(paddingLeft = "4px")),
        `2033` = colDef(align = "right")
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Debt to Nominal GDP ratio,%
  output$vb_debt <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
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
        Scenario = colDef(width = 110, headerStyle = list(paddingLeft = "4px")),
        `2033` = colDef(align = "right")
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Nominal GDP growth (%)
  output$vb_ngdp_growth <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
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
        Scenario = colDef(width = 110, headerStyle = list(paddingLeft = "4px")),
        `2033` = colDef(align = "right")
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  # Interest Payments (% Revenue)
  output$vb_interest <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
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
        Scenario = colDef(width = 110, headerStyle = list(paddingLeft = "4px")),
        `2033` = colDef(align = "right")
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  
  # Primary Balance, % of Nominal GDP
  output$vb_pb <- reactable::renderReactable({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
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
        Scenario = colDef(width = 110, headerStyle = list(paddingLeft = "4px")),
        `2033` = colDef(align = "right")
      ),
      width = "100%",
      bordered = FALSE
    )
  })
  # -------------------------------------------------------------------------
  # render text on value boxes - analysis tab
  # -------------------------------------------------------------------------
  # Credit Rating
  output$vb_cra_text <- renderText({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
      select(Scenario = group, year, value = credit_rating_number) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = Scenario, values_from = value) 
    
    # Baseline and Alternative must exist
    col_names_exist <- sum(names(data) %in% c("Baseline","Alternative")) == 2
    
    if (col_names_exist) {
      data <- data %>% 
        mutate(
          net_baseline = round(x = (Alternative - Baseline),digits = 1)
        )
      # Store the result
      result <- data %>% pull(net_baseline)
      # Return the value to display in the UI
      if (result > 0) {
        paste0(abs(result), " Notch Upgrade in Credit Rating")
      }else if (result < 0) {
        paste0(abs(result), " Notch Downgrade in Credit Rating")
      }
    }
    
  })
  # Debt, % of NGDP
  output$vb_debt_text <- renderText({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
      select(Scenario = group, year, value = gross_debt_pct_gdp) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = Scenario, values_from = value) 
    
    # Baseline and Alternative must exist
    col_names_exist <- sum(names(data) %in% c("Baseline","Alternative")) == 2
    
    if (col_names_exist) {
      data <- data %>% 
        mutate(
          net_baseline = round(x = (Alternative - Baseline),digits = 1)
        )
      # Store the result
      result <- data %>% pull(net_baseline)
      # Return the value to display in the UI
      if (result < 0) {
        paste0(abs(result), "% Decrease in Debt to GDP")
      }else if (result > 0) {
        paste0(abs(result), "% Increase in Debt to GDP")
      }
    }
    
  })
  # NGDP Growth (%)
  output$vb_ngdp_growth_text <- renderText({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
      select(Scenario = group, year, value = gdp_growth_pct) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = Scenario, values_from = value) 
    
    # Baseline and Alternative must exist
    col_names_exist <- sum(names(data) %in% c("Baseline","Alternative")) == 2
    
    if (col_names_exist) {
      data <- data %>% mutate(net_baseline = round(x = (Alternative - Baseline),digits = 1))
      # Store the result
      result <- data %>% pull(net_baseline)
      # Return the value to display in the UI
      if (result > 0) {
        paste0(glue::glue("{abs(result)} % Increase in Nominal GDP Growth"))
      }else if (result > 0) {
        paste0(glue::glue("{abs(result)} % Decrease in Nominal GDP Growth"))
      }
    }
    
  })
  # Interest % of Revenue
  output$vb_interest_text <- renderText({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
      select(Scenario = group, year, value = interest_payments_pct_revenue) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = Scenario, values_from = value) 
    
    # Baseline and Alternative must exist
    col_names_exist <- sum(names(data) %in% c("Baseline","Alternative")) == 2
    
    if (col_names_exist) {
      data <- data %>% 
        mutate(
          net_baseline = round(x = (Alternative - Baseline),digits = 1)
        )
      # Store the result
      result <- data %>% pull(net_baseline)
      # Return the value to display in the UI
      if (result < 0) {
        paste0(abs(result), "% Decrease in Interest Payments to Revenue")
      }else if (result > 0) {
        paste0(abs(result), "% Increase in Interest Payments to Revenue")
      }
    }
    
  })
  # Primary Balance, % of NGDP
  output$vb_pb_text <- renderText({
    data <- server_data_alternative_viz() %>% 
      filter(year %in% c(2033)) %>% 
      select(Scenario = group, year, value = primary_net_lending_pct_gdp) %>% 
      mutate(
        Scenario = str_remove_all(string = Scenario, pattern = " Scenario"),
        value = round(x = value, digits = 1)
      ) %>% 
      pivot_wider(names_from = Scenario, values_from = value) 
    
    # Baseline and Alternative must exist
    col_names_exist <- sum(names(data) %in% c("Baseline","Alternative")) == 2
    
    if (col_names_exist) {
      data <- data %>% 
        mutate(
          net_baseline = round(x = (Alternative - Baseline),digits = 1)
        )
      # Store the result
      result <- data %>% pull(net_baseline)
      # Return the value to display in the UI
      if (result < 0) {
        paste0(abs(result), "% Decrease in Primary Balance to GDP")
      }else if (result > 0) {
        paste0(abs(result), "% Increase in Primary Balance to GDP")
      }
    }
    
  })
  # -------------------------------------------------------------------------
  # home tab - about countries
  # -------------------------------------------------------------------------
  # data
  server_data_about_country <- reactive({
    readxl::read_excel(
      path = "data-raw/FIMA_APP.xlsx",
      sheet = "About") %>% 
      filter(country == input$id_country)
  })
  # header
  output$about_country_header <- renderText({
    glue::glue("About {input$id_country}")
  })
  
  # content
  output$about_country_content <- renderUI({
    req(input$id_country)
    
    # Get data from your reactive component
    country_data <- server_data_about_country()
    
    # Check if data exists for the selected country
    if(is.null(country_data) || nrow(country_data) == 0) {
      return(tags$p("No information available for this country."))
    }
    
    # Create the two paragraphs
    tagList(
      # first paragraph
      tags$div(
        class = "country-about",
        tags$p(country_data$about)
      ),
      # second paragraph
      tags$div(
        class = "country-vulnerabilities",
        tags$p(country_data$prone_to)
      )
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
  output$analysis_check_country <- renderText({
    paste0("Country: ", input$id_country)
  })
  
  # data tab country
  output$data_check_country <- renderText({
    paste0("Country: ", input$id_country)
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
    
    # Generate the HTML for the custom-styled checkbox group
    items <- lapply(seq_along(kpi_choices), function(i) {
      name <- names(kpi_choices)[i]
      value <- kpi_choices[i]
      
      # Create a row with light green background
      tags$div(
        class = "kpi-row",
        style = "display: flex; width: 100%; border-bottom: 1px solid #e0e0e0; background-color: #e8f5e8;",
        
        # Checkbox container
        tags$div(
          class = "checkbox-column",
          style = "width: 50px; min-width: 50px; display: flex; justify-content: center; align-items: center;",
          tags$input(
            type = "checkbox",
            name = "kpi_selection", 
            id = paste0("kpi_selection_", i),
            value = value,
            style = "width: 20px; height: 20px; margin: 0;"
          )
        ),
        
        # Label container
        tags$div(
          class = "label-column",
          style = "flex-grow: 1; padding: 8px 10px; min-height: 36px; display: flex; align-items: center; color: black;",
          tags$label(
            `for` = paste0("kpi_selection_", i),
            style = "display: block; margin-bottom: 0; font-weight: normal; word-wrap: break-word;",
            name
          )
        )
      )
    })
    
    # Create a div with the correct inputId to allow Shiny to bind to it
    div(
      id = "kpi_selection",
      class = "form-group shiny-input-checkboxgroup shiny-input-container",
      style = "width: 100%; margin: 0;",
      items
    )
  })
  
  # instruments
  output$dynamic_instruments_checkboxes <- renderUI({
    # Get the instruments options from your reactive component
    instruments_options <- server_data_instruments()
    
    # Create internal values by converting each display name
    internal_values <- sapply(instruments_options, function(instrument) {
      tolower(gsub(" |-", "_", instrument))
    })
    
    # Create named vector where names are display labels and values are internal codes
    instruments_choices <- setNames(internal_values, instruments_options)
    
    # Define instrument-to-color mapping with a named list using HEX colors
    instrument_colors <- list(
      # Dark green (100%)
      "Sustainability-linked bonds" = "#006400",
      "Sustainability-linked loans" = "#006400",
      
      # Medium green (60%)
      "Debt-for-nature swaps" = "#0a830a",
      "Carbon credits" = "#0a830a",
      
      # Light green (10%)
      "Biodiversity credits" = "#e8f5e8",
      "Credit enhancement" = "#e8f5e8"
    )
    
    # Default color for any instrument not in the mapping
    default_color <- "#006400"
    
    # Generate the HTML for the two-column layout checkbox group
    items <- lapply(seq_along(instruments_choices), function(i) {
      name <- names(instruments_choices)[i]
      value <- instruments_choices[i]
      
      # Get background color from the mapping, or use default
      bg_color <- if(name %in% names(instrument_colors)) {
        instrument_colors[[name]]
      } else {
        default_color
      }
      
      # Determine text color (white for dark/medium green, black for light green)
      text_color <- if(bg_color %in% c("#006400", "#0a830a")) {
        "white"
      } else {
        "black"
      }
      
      # Create a two-column row with white background for checkbox
      tags$div(
        class = "instrument-row",
        style = "display: flex; width: 100%; border-bottom: 1px solid #e0e0e0;",
        # Column 1: White background for checkbox
        tags$div(
          class = "checkbox-column",
          style = "background-color: white; width: 50px; min-width: 50px; display: flex; justify-content: center; align-items: center;",
          tags$input(
            type = "checkbox",
            name = "id_instruments", 
            id = paste0("id_instruments_", i),
            value = value,
            style = "width: 20px; height: 20px; margin: 0;"
          )
        ),
        # Column 2: Colored background for label
        tags$div(
          class = "label-column",
          style = paste0("background-color: ", bg_color, "; flex-grow: 1; padding: 8px 10px; min-height: 36px; display: flex; align-items: center; color: ", text_color, ";"),
          tags$label(
            `for` = paste0("id_instruments_", i),
            style = "display: block; margin-bottom: 0; font-weight: normal; word-wrap: break-word;",
            name
          )
        )
      )
    })
    
    # Create a div with the correct inputId to allow Shiny to bind to it
    div(
      id = "id_instruments",
      class = "form-group shiny-input-checkboxgroup shiny-input-container",
      style = "width: 100%; margin: 0;",
      items
    )
  })
  
  # land use
  output$dynamic_land_use_interventions_checkboxes <- renderUI({
    # getting data
    data_colors_interventions <- server_data_color_code() %>% 
      filter(kpi == "Land Use")
    # Get the land use interventions from your reactive component
    lu_interventions_options <- data_colors_interventions %>% 
      pull(interventions)
    
    # Create internal values by converting each display name
    internal_values <- sapply(lu_interventions_options, function(intervention) {
      tolower(gsub(" |-", "_", intervention))
    })
    
    # Create named vector where display names are the original values and internal values have underscores
    lu_interventions_choices <- setNames(internal_values, lu_interventions_options)
    
    # Define intervention-to-color 
    intervention_names <- data_colors_interventions %>% pull(interventions)
    intervention_picked_colors <- data_colors_interventions %>% pull(picked_color)
    intervention_colors <- setNames(as.list(intervention_picked_colors), intervention_names)
    
    
    # Default color for any intervention not in the mapping
    default_color <- "#006400"
    
    # Generate the HTML for the two-column layout checkbox group
    items <- lapply(seq_along(lu_interventions_choices), function(i) {
      name <- names(lu_interventions_choices)[i]
      value <- lu_interventions_choices[i]
      
      # Get background color from the mapping, or use default
      bg_color <- if(name %in% names(intervention_colors)) {
        intervention_colors[[name]]
      } else {
        default_color
      }
      
      # Determine text color (white for dark/medium green, black for light green)
      text_color <- if(bg_color %in% c("#006400", "#0a830a")) {
        "white"
      } else {
        "black"
      }
      
      # Create a two-column row with white background for checkbox
      tags$div(
        class = "intervention-row",
        style = "display: flex; width: 100%; border-bottom: 1px solid #e0e0e0;",
        # Column 1: White background for checkbox
        tags$div(
          class = "checkbox-column",
          style = "background-color: white; width: 50px; min-width: 50px; display: flex; justify-content: center; align-items: center;",
          tags$input(
            type = "checkbox",
            name = "land_use_interventions", 
            id = paste0("land_use_interventions_", i),
            value = value,
            style = "width: 20px; height: 20px; margin: 0;"
          )
        ),
        # Column 2: Colored background for label
        tags$div(
          class = "label-column",
          style = paste0("background-color: ", bg_color, "; flex-grow: 1; padding: 8px 10px; min-height: 36px; display: flex; align-items: center; color: ", text_color, ";"),
          tags$label(
            `for` = paste0("land_use_interventions_", i),
            style = "display: block; margin-bottom: 0; font-weight: normal; word-wrap: break-word;",
            name
          )
        )
      )
    })
    
    # Create a div with the correct inputId to allow Shiny to bind to it
    div(
      id = "land_use_interventions",
      class = "form-group shiny-input-checkboxgroup shiny-input-container",
      style = "width: 100%; margin: 0;",
      items
    )
  })
  
  # protection gap
  output$dynamic_protection_gap_interventions_checkboxes <- renderUI({
    # getting data
    data_colors_interventions <- server_data_color_code() %>% 
      filter(kpi == "Protection Gap")
    
    # Get the protection gap interventions from your reactive component
    pg_interventions_options <- data_colors_interventions %>% 
      pull(interventions)
    
    # Create internal values by converting each display name
    internal_values <- sapply(pg_interventions_options, function(intervention) {
      tolower(gsub(" |-", "_", intervention))
    })
    
    # Create named vector where display names are the original values and internal values have underscores
    pg_interventions_choices <- setNames(internal_values, pg_interventions_options)
    
    # Define intervention-to-color 
    intervention_names <- data_colors_interventions %>% pull(interventions)
    intervention_picked_colors <- data_colors_interventions %>% pull(picked_color)
    intervention_colors <- setNames(as.list(intervention_picked_colors), intervention_names)
    
    # Default color for any intervention not in the mapping
    default_color <- "#006400"
    
    # Generate the HTML for the two-column layout checkbox group
    items <- lapply(seq_along(pg_interventions_choices), function(i) {
      name <- names(pg_interventions_choices)[i]
      value <- pg_interventions_choices[i]
      
      # Get background color from the mapping, or use default
      bg_color <- if(name %in% names(intervention_colors)) {
        intervention_colors[[name]]
      } else {
        default_color
      }
      
      # Determine text color (white for dark/medium green, black for light green)
      text_color <- if(bg_color %in% c("#006400", "#0a830a")) {
        "white"
      } else {
        "black"
      }
      
      # Create a two-column row with white background for checkbox
      tags$div(
        class = "intervention-row",
        style = "display: flex; width: 100%; border-bottom: 1px solid #e0e0e0;",
        # Column 1: White background for checkbox
        tags$div(
          class = "checkbox-column",
          style = "background-color: white; width: 50px; min-width: 50px; display: flex; justify-content: center; align-items: center;",
          tags$input(
            type = "checkbox",
            name = "protection_gap_interventions", 
            id = paste0("protection_gap_interventions_", i),
            value = value,
            style = "width: 20px; height: 20px; margin: 0;"
          )
        ),
        # Column 2: Colored background for label
        tags$div(
          class = "label-column",
          style = paste0("background-color: ", bg_color, "; flex-grow: 1; padding: 8px 10px; min-height: 36px; display: flex; align-items: center; color: ", text_color, ";"),
          tags$label(
            `for` = paste0("protection_gap_interventions_", i),
            style = "display: block; margin-bottom: 0; font-weight: normal; word-wrap: break-word;",
            name
          )
        )
      )
    })
    
    # Create a div with the correct inputId to allow Shiny to bind to it
    div(
      id = "protection_gap_interventions",
      class = "form-group shiny-input-checkboxgroup shiny-input-container",
      style = "width: 100%; margin: 0;",
      items
    )
  })
  
}

# end: --------------------------------------------------------------------


