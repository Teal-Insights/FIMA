
# start: ------------------------------------------------------------------
fima_alternative_scenario <- function(data_baseline, data_interventions){
  data_alternative <- data_baseline %>% 
    filter(year < 2024) %>% 
    select(
      year, gross_debt_billions, gross_debt_pct_gdp,nominal_interest_rate,
      gdp_growth_pct, primary_net_lending_pct_gdp,
      interest_payments_billions,interest_payments_pct_revenue, 
      primary_net_lending_billions, gdp_current_prices_billions) %>% 
    full_join(y = data_interventions, by = "year") %>% 
    left_join(y = data_baseline %>% select(year,revenue_billions),by = "year") %>% 
    # One off adjustment to stock of debt (LCU billions)
    mutate(
      one_off_adjustment_to_debt_stock = case_when(
        year == 2024 ~ 1000, 
        year > 2024 ~ 0,
        .default = NA
      )
    )
  
  # Recursive
  data_alternative_combined <- data_alternative
  
  # Identify future years (years > 2029)
  future_years <- which(data_alternative_combined$year > 2023)
  
  # Only proceed if there are future years to project
  if (length(future_years) > 0) {
    for (i in future_years) {
      # Calculate GDP for future years
      data_alternative_combined$gdp_current_prices_billions[i] <- 
        data_alternative_combined$gdp_current_prices_billions[i-1] * 
        (1 + data_alternative_combined$policy_shock_ngdp_growth[i]/100)
      
      # General government primary balance (LCU billions)
      data_alternative_combined$primary_net_lending_billions[i] <- 
        data_alternative_combined$policy_shock_primary_balance[i] * 
        data_alternative_combined$gdp_current_prices_billions[i]/100
      
      # General government gross debt in (LCU billions)
      if (i==future_years[1]) {
        data_alternative_combined$gross_debt_billions[i] <- 
          data_alternative_combined$gross_debt_billions[i-1] * 
          (1 + data_alternative_combined$nominal_interest_rate[i-1]/100) - 
          data_alternative_combined$primary_net_lending_billions[i] +
          data_alternative_combined$one_off_adjustment_to_debt_stock[i]
      }else if (i > future_years[1]) {
        data_alternative_combined$gross_debt_billions[i] <- 
          data_alternative_combined$gross_debt_billions[i-1] * 
          (1 + data_alternative_combined$policy_shock_nominal_interest[i-1]/100) - 
          data_alternative_combined$primary_net_lending_billions[i] +
          data_alternative_combined$one_off_adjustment_to_debt_stock[i]
      }
      
      # General government gross debt (% GDP)
      data_alternative_combined$gross_debt_pct_gdp[i] <- 
        data_alternative_combined$gross_debt_billions[i] / 
        data_alternative_combined$gdp_current_prices_billions[i] * 100
      
      # General government interest payments (LCU billions)
      data_alternative_combined$interest_payments_billions[i] <- 
        data_alternative_combined$gross_debt_billions[i] * 
        data_alternative_combined$policy_shock_nominal_interest[i] / 100
      
      # General government interest payments (% Revenue)
      data_alternative_combined$interest_payments_pct_revenue[i] <- 
        data_alternative_combined$interest_payments_billions[i] / 
        data_alternative_combined$revenue_billions[i] * 100
      
      # Nominal GDP growth (%)
      data_alternative_combined$gdp_growth_pct[i] <- 
        data_alternative_combined$policy_shock_ngdp_growth[i]
      
      # General government primary balance (% GDP)
      data_alternative_combined$primary_net_lending_pct_gdp[i] <- 
        data_alternative_combined$policy_shock_primary_balance[i]
    }
  }
  # return data
  return(data_alternative_combined)
}


# -------------------------------------------------------------------------
# viz data
# -------------------------------------------------------------------------
fima_alternative_viz <- function(data_baseline, data_alternative){
  final_baseline <- data_baseline %>% 
    select(
      year, gross_debt_pct_gdp,gdp_growth_pct,interest_payments_pct_revenue,
      primary_net_lending_pct_gdp
    ) %>% 
    mutate(group = "Baseline Scenario")
  
  final_alternative <- data_alternative %>% 
    select(year, gross_debt_pct_gdp,gdp_growth_pct,interest_payments_pct_revenue,
           primary_net_lending_pct_gdp) %>% 
    mutate(group = "Alternative Scenario")
  
  # final data
  data_alternative_viz <- 
    bind_rows(
      # baseline data
      final_baseline,
      # alternative scenario data
      final_alternative
    )
  # return dataframe
  return(data_alternative_viz)
}



# end: --------------------------------------------------------------------

