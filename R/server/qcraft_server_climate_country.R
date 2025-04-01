
# start: ------------------------------------------------------------------
qcraft_server_climate_country <- function(by_iso3c){
  # loading climate data
  df <- readr::read_rds(file = "data/climate.rds")
  # computation
  df_country <- df %>% 
    filter(iso3c == by_iso3c) %>% 
    mutate(
      climate_scenario = sub(
        pattern = " \\(.*\\)", 
        replacement = "", 
        x = climate_scenario)) %>%
    mutate(climate_scenario = paste0(climate_scenario,"_pct_gdp_loss")) %>% 
    pivot_wider(names_from = climate_scenario, values_from = values) %>% 
    janitor::clean_names() %>% 
    mutate(
      paris_gdp_index = 100 + paris_pct_gdp_loss,
      moderate_gdp_index = 100 + moderate_pct_gdp_loss,
      high_gdp_index = 100 + high_pct_gdp_loss,
      hot_gdp_index = 100 + hot_pct_gdp_loss,
      hot_adapted_gdp_index = 100 + hot_adapted_pct_gdp_loss,
      hot_unadapted_gdp_index = 100 + hot_unadapted_pct_gdp_loss
    ) %>% 
    mutate(
      paris_variation = (paris_gdp_index/lag(paris_gdp_index)*100 - 100),
      moderate_variation = (moderate_gdp_index/lag(moderate_gdp_index)*100 - 100),
      high_variation = (high_gdp_index/lag(high_gdp_index)*100 - 100),
      hot_variation = (hot_gdp_index/lag(hot_gdp_index)*100 - 100),
      hot_adapted_variation = (hot_adapted_gdp_index/lag(hot_adapted_gdp_index)*100 - 100),
      hot_unadapted_variation = (hot_unadapted_gdp_index/lag(hot_unadapted_gdp_index)*100 - 100)
    )
  # return dataframe
  return(df_country)
}

# end: --------------------------------------------------------------------

