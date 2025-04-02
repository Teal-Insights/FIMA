
# start: ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(reactable)
})
# turn off warnings
options(warn = -1)
options(scipen = 999)

# load necessary scripts
source(file = "R/server/fima_baseline_scenario.R")
source(file = "R/server/fima_server_interventions.R")
source(file = "R/server/fima_alternative_scenario.R")


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
    req(server_data_alternative())
    fima_alternative_viz(
      data_baseline = server_data_baseline,
      data_alternative = server_data_alternative()
    )
  })
  
  output$data_table <- renderReactable({
    # Process the data directly within renderReactable
    processed_data <- server_data_alternative() %>% 
      pivot_longer(
        cols = -year,  
        names_to = "indicators",
        values_to = "values"
      ) %>% 
      mutate(values = round(x = values, digits = 1)) %>% 
      pivot_wider(names_from = year, values_from = values) %>% 
      # Clean up indicators names
      mutate(
        indicators = str_replace(indicators, "_", " "),
        indicators = str_replace(indicators, "pct", "% of"),
        indicators = str_to_title(str_replace_all(indicators, "_", " ")),
        indicators = str_replace(indicators, "Gdp", "GDP"),
        indicators = str_replace(indicators, "Of", "of"),
        indicators = str_replace(indicators, "GDP Growth % of", "GDP Growth %")
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
          width = 300,     # Set default width for indicators column
          sticky = "left"  # Freeze the indicators column on the left when scrolling
        )
      )
    )
  })
  # -------------------------------------------------------------------------
  # visualizations - Home
  # -------------------------------------------------------------------------
  # General government gross debt (% GDP)
  output$home_debt_ngdp <- renderPlot({
    server_data_alternative_viz() %>% 
      select(c(year, gross_debt_pct_gdp, group)) %>% 
      mutate(
        group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
      ) %>% 
      ggplot(aes(x = year, y = gross_debt_pct_gdp, color = group, group = group)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "General government gross debt (% of Nominal GDP)",
        subtitle = "Comparison over time",
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
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
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
      ggplot(aes(x = year, y = gdp_growth_pct, color = group, group = group)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "Nominal GDP growth (%)",
        subtitle = "Comparison over time",
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
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
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
      ggplot(aes(x = year, y = interest_payments_pct_revenue, color = group, group = group)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "General government interest payments (% Revenue)",
        subtitle = "Comparison over time",
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
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
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
      ggplot(aes(x = year, y = primary_net_lending_pct_gdp, color = group, group = group)) +
      geom_line(linewidth = 1) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "General government primary balance (% of Nominal GDP)",
        subtitle = "Comparison over time",
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
        # Make the legend key wider to avoid wrapping
        legend.key.width = unit(1, "cm"),
        # Other theme elements
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
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


