
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
source(file = "R/server/fima_echarts_main.R")
source(file = "R/server/fima_echarts_ratings.R")
source(file = "R/server/fima_adjustment.R")

# data: -------------------------------------------------------------------
# country
bot_country <- "Xenon"
# baseline
bot_baseline <- fima_baseline_scenario(by_country = bot_country)
bot_adjustment <- fima_adjustment(by_country = bot_country)
bot_interventions <- fima_interventions(data_baseline = bot_baseline)
bot_alternative <- fima_alternative_scenario(
  data_baseline = bot_baseline,
  data_interventions = bot_interventions,
  data_adjustment = bot_adjustment
)
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
fima_echarts_ratings(
  data = bot_alternative_viz,
  x_col = "year",
  y_col = "credit_rating_number",
  group_col = "group",
  group_levels = c("Baseline Scenario", "Alternative Scenario")
)

# Debt-GDP ratio, %
fima_echarts_main(
  data = bot_alternative_viz,
  x_col = "year",
  y_col = "gross_debt_pct_gdp",
  group_col = "group",  # Make sure this column exists
  group_levels = c("Baseline Scenario", "Alternative Scenario")
)  

# Nominal GDP growth (%)
fima_echarts_main(
  data = bot_alternative_viz,
  x_col = "year",
  y_col = "gdp_growth_pct",
  group_col = "group",  # Make sure this column exists
  group_levels = c("Baseline Scenario", "Alternative Scenario")
)

# Interest % Revenue
fima_echarts_main(
  data = bot_alternative_viz,
  x_col = "year",
  y_col = "interest_payments_pct_revenue",
  group_col = "group",  # Make sure this column exists
  group_levels = c("Baseline Scenario", "Alternative Scenario")
)

# Primary Balance, % of Nominal GDP
fima_echarts_main(
  data = bot_alternative_viz,
  x_col = "year",
  y_col = "primary_net_lending_pct_gdp",
  group_col = "group",  # Make sure this column exists
  group_levels = c("Baseline Scenario", "Alternative Scenario")
)

# end: --------------------------------------------------------------------
