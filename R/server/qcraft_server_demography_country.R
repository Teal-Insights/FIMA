
# start: ------------------------------------------------------------------
qcraft_server_demography_country <- function(by_iso3c, by_level){
  # reading demography data
  df <- readr::read_rds(file = "data/demography.rds")
  # computations
  df_country <- df %>% 
    filter(iso3c == by_iso3c) %>% 
    mutate(status_age_group = paste0(status,"_",age_group)) %>% 
    select(-c(status, age_group)) %>% 
    pivot_wider(names_from = status_age_group,values_from = values) %>% 
    janitor::clean_names() %>% 
    mutate(
      medium_below_15 = (medium_total - medium_15_64 - medium_over_65),
      high_below_15 = (high_total - high_15_64 - high_over_65),
      low_below_15 = (low_total - low_15_64 - low_over_65)
    ) %>% 
    # population share
    mutate(
      # medium
      medium_pop_share_below_15 = (medium_below_15/medium_total*100),
      medium_pop_share_15_64 = (medium_15_64/medium_total*100),
      medium_pop_share_over_65 = (medium_over_65/medium_total*100),
      # high
      high_pop_share_below_15 = (high_below_15/high_total*100),
      high_pop_share_15_64 = (high_15_64/high_total*100),
      high_pop_share_over_65 = (high_over_65/high_total*100),
      # low
      low_pop_share_below_15 = (low_below_15/low_total*100),
      low_pop_share_15_64 = (low_15_64/low_total*100),
      low_pop_share_over_65 = (low_over_65/low_total*100)
    ) %>% 
    # dependency ratio
    mutate(
      medium_dependency_ratio = (medium_over_65 + medium_below_15)/medium_15_64,
      high_dependency_ratio = (high_over_65 + high_below_15)/high_15_64,
      low_dependency_ratio = (low_over_65 + low_below_15)/low_15_64
    ) %>% 
    # demography level
    mutate(
      demography_level_15_64 = case_when(
        by_level == "Medium" ~ medium_15_64,
        by_level == "High" ~ high_15_64,
        by_level == "Low" ~ low_15_64,
        .default = NA
      ),
      demography_level_total = case_when(
        by_level == "Medium" ~ medium_total,
        by_level == "High" ~ high_total,
        by_level == "Low" ~ low_total,
        .default = NA
      ),
      demography_level_over_65 = case_when(
        by_level == "Medium" ~ medium_over_65,
        by_level == "High" ~ high_over_65,
        by_level == "Low" ~ low_over_65,
        .default = NA
      ),
      demography_level_below_15 = case_when(
        by_level == "Medium" ~ medium_below_15,
        by_level == "High" ~ high_below_15,
        by_level == "Low" ~ low_below_15,
        .default = NA
      )
    ) %>% 
    # demography growth
    mutate(
      # scenario
      demography_growth_15_64 = (demography_level_15_64/lag(demography_level_15_64)*100) - 100,
      demography_growth_total = (demography_level_total/lag(demography_level_total)*100) - 100,
      demography_growth_over_65 = (demography_level_over_65/lag(demography_level_over_65)*100) - 100,
      demography_growth_below_15 = (demography_level_below_15/lag(demography_level_below_15)*100) - 100,
      
      # age group 15-64 years
      demography_growth_medium_15_64 = (medium_15_64/lag(medium_15_64)*100) - 100,
      demography_growth_high_15_64 = (high_15_64/lag(high_15_64)*100) - 100,
      demography_growth_low_15_64 = (low_15_64/lag(low_15_64)*100) - 100,
      
      # age group over 65 years
      demography_growth_medium_over_65 = (medium_over_65/lag(medium_over_65)*100) - 100,
      demography_growth_high_over_65 = (high_over_65/lag(high_over_65)*100) - 100,
      demography_growth_low_over_65 = (low_over_65/lag(low_over_65)*100) - 100,
      
      # age group below 15 years
      demography_growth_medium_below_15 = (medium_below_15/lag(medium_below_15)*100) - 100,
      demography_growth_high_below_15 = (high_below_15/lag(high_below_15)*100) - 100,
      demography_growth_low_below_15 = (low_below_15/lag(low_below_15)*100) - 100
    )
}

# end ---------------------------------------------------------------------
