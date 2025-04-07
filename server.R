
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
  output$home_credit_rating <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, credit_rating_number, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = credit_rating_number, color = group, linetype = group, group = group)) +
      geom_line(linewidth = 1.25) +
      scale_color_manual(values = c("Baseline Scenario" = "blue", "Alternative Scenario" = "red")) +
      scale_linetype_manual(values = c("Baseline Scenario" = "solid", "Alternative Scenario" = "dashed")) +
      labs(
        title = "Credit Rating",
        # subtitle = "Comparison over time",
        x = "",
        y = "")  +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0,0)
      ) +
      scale_y_continuous(
        breaks = 1:22,
        labels = fima_cra_y_axis(y_values = 1:22)
      ) +
      theme_minimal() +
      theme(
        # Legend settings for one line
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        # Increase axis text size
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        # Add solid line at the bottom
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        # Remove default x-axis line to replace with our custom one
        panel.border = element_blank(),
        # Ensure ticks are visible
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        panel.grid.major.x = element_blank()
      )+
      guides(color = guide_legend(nrow = 1)) +
      geom_rug(sides = "b", aes(color = NULL), alpha = 0.5)
  })
  # General government gross debt (% GDP)
  output$home_debt_ngdp <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, gross_debt_pct_gdp, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = gross_debt_pct_gdp, color = group, linetype = group, group = group)) +
      geom_line(linewidth = 1.25) +
      scale_color_manual(values = c("Baseline Scenario" = "blue", "Alternative Scenario" = "red")) +
      scale_linetype_manual(values = c("Baseline Scenario" = "solid", "Alternative Scenario" = "dashed")) +
      labs(
        title = "General government gross debt (% of Nominal GDP)",
        # subtitle = "Comparison over time",
        x = "",
        y = "")  +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0,0)
      )+
      theme_minimal() +
      theme(
        # Legend settings for one line
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        # Increase axis text size
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        # Add solid line at the bottom
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        # Remove default x-axis line to replace with our custom one
        panel.border = element_blank(),
        # Ensure ticks are visible
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        panel.grid.major.x = element_blank()
      )+
      guides(color = guide_legend(nrow = 1)) +
      geom_rug(sides = "b", aes(color = NULL), alpha = 0.5)
  })
  
  # Nominal GDP growth (%)
  output$home_ngdp_growth <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, gdp_growth_pct, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = gdp_growth_pct, color = group, linetype = group, group = group)) +
      geom_line(linewidth = 1.25) +
      scale_color_manual(values = c("Baseline Scenario" = "blue", "Alternative Scenario" = "red")) +
      scale_linetype_manual(values = c("Baseline Scenario" = "solid", "Alternative Scenario" = "dashed")) +
      labs(
        title = "Nominal GDP growth (%)",
        # subtitle = "Comparison over time",
        x = "",
        y = "")  +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0,0)
      )+
      theme_minimal() +
      theme(
        # Legend settings for one line
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        # Increase axis text size
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        # Add solid line at the bottom
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        # Remove default x-axis line to replace with our custom one
        panel.border = element_blank(),
        # Ensure ticks are visible
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        panel.grid.major.x = element_blank()
      )+
      guides(color = guide_legend(nrow = 1)) +
      geom_rug(sides = "b", aes(color = NULL), alpha = 0.5)
  })
  
  # General government interest payments (% Revenue)
  output$home_ir_revenue <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, interest_payments_pct_revenue, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = interest_payments_pct_revenue, color = group, linetype = group, group = group)) +
      geom_line(linewidth = 1.25) +
      scale_color_manual(values = c("Baseline Scenario" = "blue", "Alternative Scenario" = "red")) +
      scale_linetype_manual(values = c("Baseline Scenario" = "solid", "Alternative Scenario" = "dashed")) +
      labs(
        title = "General government interest payments (% Revenue)",
        # subtitle = "Comparison over time",
        x = "",
        y = "")  +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0,0)
      )+
      theme_minimal() +
      theme(
        # Legend settings for one line
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        # Increase axis text size
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        # Add solid line at the bottom
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        # Remove default x-axis line to replace with our custom one
        panel.border = element_blank(),
        # Ensure ticks are visible
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        panel.grid.major.x = element_blank()
      )+
      guides(color = guide_legend(nrow = 1)) +
      geom_rug(sides = "b", aes(color = NULL), alpha = 0.5)
  })
  
  # General government primary balance (% of Nominal GDP)
  output$home_pb <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, primary_net_lending_pct_gdp, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = primary_net_lending_pct_gdp, color = group, linetype = group, group = group)) +
      geom_line(linewidth = 1.25) +
      scale_color_manual(values = c("Baseline Scenario" = "blue", "Alternative Scenario" = "red")) +
      scale_linetype_manual(values = c("Baseline Scenario" = "solid", "Alternative Scenario" = "dashed")) +
      labs(
        title = "General government primary balance (% of Nominal GDP)",
        # subtitle = "Comparison over time",
        x = "",
        y = "")  +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        expand = c(0,0)
      )+
      theme_minimal() +
      theme(
        # Legend settings for one line
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.margin = margin(t = -15, r = 0, b = 0, l = 0),
        legend.spacing.x = unit(0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        # Increase axis text size
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold", size = 12),
        # Add solid line at the bottom
        axis.line.x = element_line(color = "black", linewidth = 0.5),
        # Remove default x-axis line to replace with our custom one
        panel.border = element_blank(),
        # Ensure ticks are visible
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(0.25, "cm"),
        panel.grid.major.x = element_blank()
      )+
      guides(color = guide_legend(nrow = 1)) +
      geom_rug(sides = "b", aes(color = NULL), alpha = 0.5)
  })
  
}


# end: --------------------------------------------------------------------


