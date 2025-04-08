
# start: ------------------------------------------------------------------
# load necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(echarts4r)
})
# turn off warnins
options(warn = -1)
options(scipen = 999)

# load necessary scripts
source(file = "R/server/fima_baseline_scenario.R")
source(file = "R/server/fima_interventions.R")
source(file = "R/server/fima_alternative_scenario.R")

# data: -------------------------------------------------------------------
# baseline
bot_baseline <- fima_baseline_scenario()
bot_interventions <- fima_interventions(data_baseline = bot_baseline)
bot_alternative <- fima_alternative_scenario(data_baseline = bot_baseline,data_interventions = bot_interventions)
bot_alternative_table <- fima_alternative_table(data_baseline = bot_baseline,data_alternative = bot_alternative)
bot_alternative_viz <- fima_alternative_viz(data_baseline = bot_baseline,data_alternative = bot_alternative)
# visualizations ----------------------------------------------------------
# credit rating
bot_alternative_viz %>% 
  select(c(year, credit_rating_number, group)) %>% 
  mutate(
    group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
  ) %>% 
  ggplot(aes(x = year, y = credit_rating_number, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
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

# Debt-GDP ratio, %
bot_alternative_viz %>% 
  select(c(year, gross_debt_pct_gdp, group)) %>% 
  mutate(
    group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
  ) %>% 
  ggplot(aes(x = year, y = gross_debt_pct_gdp, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Debt as % of NGDP",
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

# Nominal GDP growth (%)
bot_alternative_viz %>% 
  select(c(year, gdp_growth_pct, group)) %>% 
  mutate(
    group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
  ) %>% 
  ggplot(aes(x = year, y = gdp_growth_pct, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
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

# Interest % Revenue
bot_alternative_viz %>% 
  select(c(year, interest_payments_pct_revenue, group)) %>% 
  mutate(
    group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
  ) %>% 
  ggplot(aes(x = year, y = interest_payments_pct_revenue, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Interest % Revenue",
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

# Primary Balance, % of Nominal GDP
bot_alternative_viz %>% 
  select(c(year, primary_net_lending_pct_gdp, group)) %>% 
  mutate(
    group = factor(x = group, levels = c("Baseline Scenario","Alternative Scenario"))
  ) %>% 
  ggplot(aes(x = year, y = primary_net_lending_pct_gdp, color = group, group = group)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Primary Balance, % of Nominal GDP",
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

# -------------------------------------------------------------------------
# echarts
# -------------------------------------------------------------------------
# credit rating
# Generate labels for all possible ratings
rating_labels <- fima_cra_y_axis(y_values = 1:22)

# Convert the mapping to JSON for all ratings
rating_json <- jsonlite::toJSON(setNames(rating_labels, 1:22))

# Create the chart with the focused y-axis range
bot_alternative_viz %>% 
  # filter(year > 2013) %>% 
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

# Debt-GDP ratio, %
bot_alternative_viz %>% 
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

# Nominal GDP growth (%)
bot_alternative_viz %>% 
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

# Interest % Revenue
bot_alternative_viz %>% 
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

# Primary Balance, % of Nominal GDP
bot_alternative_viz %>% 
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

# end: --------------------------------------------------------------------


