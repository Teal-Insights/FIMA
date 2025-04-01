
# start: ------------------------------------------------------------------
qcraft_server_inflation_country <- function(start, end, rate, turning_point, by_iso3c){
  df <- readr::read_rds(file = "data/macrofiscal.rds") 
  
  df_country <- df %>% 
    filter(iso3c == by_iso3c) %>% 
    select(iso3c, country, years, gdp_deflator_growth_percent)
  
  weo_end_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% 
    max()
  
  conve_years <- seq(tail(weo_end_year+1, 1),2100)
  counter <- seq_along(conve_years)
  growth <- start + (end - start) * 
    ((1/(1 + exp(-rate * (counter - turning_point))))^rate)
  
  df_assumptions <- data.frame(
    years = conve_years, gdp_deflator_growth_percent = growth
  )
  
  df_final <- bind_rows(df_country, df_assumptions) %>% 
    fill(c(iso3c, country)) %>% 
    rename("inflation" = "gdp_deflator_growth_percent") %>% 
    filter(years > 2001)
  # return dataframe
  return(df_final)
}

# end: --------------------------------------------------------------------


