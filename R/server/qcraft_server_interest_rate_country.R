
# start: ------------------------------------------------------------------
qcraft_server_interest_rate_country <- function(df_baseline_v1,
                                                by_iso3c, 
                                                select_rate = "Nominal interest rate",
                                                long_run_interest_rate = 1) {
  
  # Read macrofiscal data
  df_int_macrofiscal <- readr::read_rds(file = "data/macrofiscal.rds")
  weo_max_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% max()
  # inputs
  df_inputs <- df_int_macrofiscal %>% 
    filter(iso3c == by_iso3c, years == weo_max_year) %>% 
    select(interest_rate_percent, interest_growth_differential_percentage_point)
  # interest rate
  base_nominal_interest_rate = df_inputs %>% 
    pull(interest_rate_percent) %>% 
    head(1)
  # interest growth differential percentage point
  base_interest_growth_differential = df_inputs %>% 
    pull(interest_growth_differential_percentage_point) %>% 
    head(1)
  # Filter and prepare macrofiscal data
  df_indicators_macrofiscal <- df_int_macrofiscal %>% 
    filter(iso3c == by_iso3c) %>% 
    select(
      iso3c, 
      country,
      years,
      nominal_interest_rate = interest_rate_percent,
      nominal_gdp_growth_percent,
      inflation = gdp_deflator_growth_percent,
      interest_growth_differential = interest_growth_differential_percentage_point
    ) %>% 
    mutate(across(
      c(nominal_interest_rate, nominal_gdp_growth_percent, inflation), 
      ~replace(., 1, NA))) %>% 
    mutate(across(
      c(interest_growth_differential), 
      ~replace(., 1:2, NA))) %>% 
    bind_rows(df_baseline_v1 %>% 
                select(
                  iso3c, country, years,
                  nominal_gdp_growth_percent,
                  inflation = gdp_deflator_growth_percent) %>% 
                filter(years > weo_max_year)) %>% 
    mutate(
      real_interest_rate = (nominal_interest_rate/100 - inflation/100)/(1+inflation/100)*100
    )
  
  # Constant dashboard assumptions
  df_int_rate_constant <- df_baseline_v1 %>% 
    filter(years >= weo_max_year) %>% 
    select(years, nominal_gdp_growth_percent,
           inflation = gdp_deflator_growth_percent) %>% 
    mutate(
      base_nominal_interest_rate = base_nominal_interest_rate,
      base_interest_growth_differential = base_interest_growth_differential,
      long_run_interest_rate = long_run_interest_rate
    ) %>% 
    mutate(
      interest_growth_differential = (
        (1+nominal_gdp_growth_percent/100)*(1+base_interest_growth_differential/100)*100-100
      ),
      real_interest_rate = (1+long_run_interest_rate/100)*(1+inflation/100)*100-100
    ) %>% 
    mutate(
      years = years + 1
    ) %>% 
    filter(years <= 2100)
  
  # Select rate based on input parameter
  if (select_rate == "Nominal interest rate") {
    df_int_rate_final <- df_int_rate_constant %>% 
      mutate(
        nominal_interest_rate = base_nominal_interest_rate
      ) %>% 
      select(years, nominal_interest_rate)
  } else if (select_rate == "Interest-growth differential") {
    df_int_rate_final <- df_int_rate_constant %>% 
      mutate(
        nominal_interest_rate = interest_growth_differential
      ) %>% 
      select(years, nominal_interest_rate)
  } else {
    df_int_rate_final <- df_int_rate_constant %>% 
      mutate(
        nominal_interest_rate = real_interest_rate
      ) %>% 
      select(years, nominal_interest_rate)
  }
  
  # Combine historical and projected rates
  old_nominal_rate = df_indicators_macrofiscal %>% 
    filter(years <= weo_max_year) %>% pull(nominal_interest_rate)
  
  df_final_interest_rate <- df_indicators_macrofiscal %>% 
    mutate(
      nominal_interest_rate = c(old_nominal_rate,
                                df_int_rate_final$nominal_interest_rate)
    ) %>% 
    mutate(
      real_interest_rate = (nominal_interest_rate/100 - inflation/100)/(1+inflation/100)*100
    ) %>% 
    mutate(
      interest_growth_differential = case_when(
        years > weo_max_year ~ (nominal_interest_rate/100 - nominal_gdp_growth_percent/100)/
          (1+nominal_gdp_growth_percent/100)*100,
        .default = interest_growth_differential
      )
    )
  # Return the final dataframe
  return(df_final_interest_rate)
}

# end: --------------------------------------------------------------------
