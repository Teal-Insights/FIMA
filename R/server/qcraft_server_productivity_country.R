
# start: ------------------------------------------------------------------
qcraft_server_productivity_country <- function(){
  qcraft_get_future <- function(data,start, end, rate){
    # Create new data frame with years from 2023 to 2100
    future_years <- seq(tail(start + 1,n = 1), end)
    future_oecd <- tibble(
      years = future_years
    )
    
    # Project values based on the last known value
    last_value <- data$values[data$years == start]
    
    # Apply the growth formula for each future year
    future_oecd$values <- last_value * (1 + rate/100)^(future_years - start)
    # return tibble
    return(future_oecd)
  }
  
  df <- readr::read_rds(file = "data/productivity.rds")
  weo_end_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% 
    max()
  
  oecd_members <- df %>% 
    select(years, country, values) %>% 
    filter(country == "OECD members")
  
  # computing growth
  oecd_start <- oecd_members %>% pull(values) %>% head(n = 1)
  oecd_end <- oecd_members %>% pull(values) %>% tail(n = 1)
  oecd_length <- oecd_members %>% nrow() - 1
  oecd_growth <- ((oecd_end/oecd_start)^(1/oecd_length) - 1)*100
  oecd_end_year <- oecd_members %>% pull(years) %>% tail(n = 1)
  
  
  # Combine historical and projected oecd data
  future_oecd <- qcraft_get_future(
    data = oecd_members, 
    start = 2022, 
    end = 2100,
    rate = oecd_growth)
  complete_oecd <- bind_rows(oecd_members, future_oecd) %>% 
    fill(country, .direction = "down")
  
  # Combine historical and projected country specific data
  df_country <- df %>% 
    filter(iso3c == "KEN") %>% 
    select(years, country, values)
  
  future_country <- qcraft_get_future(
    data = df_country, 
    start = 2022, 
    end = 2029,
    rate = 5)
  
  complete_country <- bind_rows(df_country, future_country) %>% 
    fill(country, .direction = "down")
  
  # productivity convergence
  conv_start = 5
  conv_end = 1.2
  conv_rate = 0.5
  conv_turning_point = 15 #J21
  country_last_value = complete_country %>% 
    filter(years == weo_end_year) %>% 
    pull(values)
  
  conve_years <- seq(tail(complete_country$years+1, 1),2100)
  counter <- seq_along(conve_years)
  productivity_growth_convergence <- conv_start + (conv_end - conv_start) * 
    ((1/(1 + exp(-conv_rate * (counter - conv_turning_point))))^conv_rate)
  
  df_convergence <- data.frame(
    counter, years = conve_years, rate = productivity_growth_convergence
  ) %>% 
    mutate(growth_factors = cumprod(1 + rate/100)) %>% 
    mutate(values = country_last_value * growth_factors)
  
  df_country_convergence <- bind_rows(complete_country, df_convergence) %>% 
    fill(country, .direction = "down") %>% 
    select(years, country, values)
  
  df_oecd_country <- bind_rows(complete_oecd, df_country_convergence) %>% 
    pivot_wider(names_from = country, values_from = values) %>% 
    mutate(productivity_level_percent_oecd = (.[[3]] / `OECD members`)*100) %>% 
    mutate(productivity_growth_rate_perecent = (.[[3]]/lag(.[[3]])*100) - 100)
  # return a dataframe
  return(df_oecd_country)
}

# end: --------------------------------------------------------------------
