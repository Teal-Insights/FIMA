
# start: ------------------------------------------------------------------
fima_baseline_scenario <- function(){
  # -------------------------------------------------------------------------
  # data
  # -------------------------------------------------------------------------
  # Create the dataframe with all economic indicators
  data_baseline <- data.frame(
    # years
    year = 2013:2029,
    
    # GDP data 
    gdp_current_prices_billions = c(21350, 24136, 27086, 28687, 30492, 32506, 35379, 36278, 
                                    40367, 43771, 47843, 52368, 57057, 61922, 67898, 73757, 79897),
    
    # Government debt data 
    gross_debt_billions = c(5257, 6447, 7902, 8927, 9939, 11480, 13167, 16802, 
                            20250, 24789, 27783, 31042, 31896, 33526, 35575, 38099, 40860),
    
    # Government net lending
    net_lending_billions = c(-346, -378, -553, -854, -998, -944, -785, -1966, 
                             -1961, -2982, -2508, -2121, -1737, -1885, -2013, -2193, -2403),
    
    # Government primary net lending
    primary_net_lending_billions = c(-131, -165, -256, -494, -619, -510, -264, -1302, 
                                     -1177, -2012, -1269, -854, -265, -422, -504, -620, -721),
    
    # Government revenue
    revenue_billions = c(3040, 3293, 3917, 4189, 4523, 4764, 5299, 5424, 
                         6295, 6684, 7771, 8776, 10047, 11068, 12298, 13475, 14894)
  ) %>%
    mutate(
      # Debt and fiscal metrics as percent of GDP
      gross_debt_pct_gdp = (gross_debt_billions / gdp_current_prices_billions) * 100,
      net_lending_pct_gdp = (net_lending_billions / gdp_current_prices_billions) * 100,
      primary_net_lending_pct_gdp = (primary_net_lending_billions / gdp_current_prices_billions) * 100,
      revenue_pct_gdp = (revenue_billions / gdp_current_prices_billions) * 100,
      
      # Interest payments calculations
      interest_payments_billions = primary_net_lending_billions - net_lending_billions,
      interest_payments_pct_revenue = (interest_payments_billions / revenue_billions) * 100,
      
      # Year-over-year GDP growth
      gdp_growth_pct = c(NA, diff(gdp_current_prices_billions) / head(gdp_current_prices_billions, -1) * 100),
      # nominal interest rate
      nominal_interest_rate = (interest_payments_billions/gross_debt_billions) * 100
    ) %>% 
    select(
      year,
      gdp_current_prices_billions,             # "Gross domestic product, current prices (LCU billions)"
      gdp_growth_pct,                          # "Nominal GDP growth (yoy%)"
      gross_debt_billions,                     # "General government gross debt (LCU billions)"
      gross_debt_pct_gdp,                      # "General government gross debt (%GDP)"
      net_lending_billions,                    # "General government net lending/borrowing (LCU billions)"
      primary_net_lending_billions,            # "General government primary net lending/borrowing (LCU billions)"
      primary_net_lending_pct_gdp,             # "General government primary net lending/borrowing (%GDP)"
      interest_payments_billions,              # "General government interest payments (LCU billions)"
      interest_payments_pct_revenue,           # "General government interest payments (% Revenue)"
      nominal_interest_rate,                   # Nominal interest rate
      revenue_billions,                        # "General government revenue (LCU billions)"
      revenue_pct_gdp                          # "General government revenue (% GDP)"
    ) %>% 
    # extend years to 2033
    bind_rows(data.frame(year = 2030:2033)) %>% 
    # use final values to extend to 2033 for
    fill(
      c(gdp_growth_pct,primary_net_lending_pct_gdp,nominal_interest_rate,
        revenue_pct_gdp),
      .direction = "down"
    )
  # -------------------------------------------------------------------------
  # Recursive
  # -------------------------------------------------------------------------
  # Create a copy of the baseline data
  data_baseline_final <- data_baseline
  
  # Identify future years (years > 2029)
  future_years <- which(data_baseline_final$year > 2029)
  
  # Only proceed if there are future years to project
  if (length(future_years) > 0) {
    for (i in future_years) {
      # Calculate GDP for future years
      data_baseline_final$gdp_current_prices_billions[i] <- 
        data_baseline_final$gdp_current_prices_billions[i-1] * 
        (1 + data_baseline_final$gdp_growth_pct[i]/100)
      
      # Calculate revenue based on GDP and revenue percentage
      data_baseline_final$revenue_billions[i] <- 
        data_baseline_final$revenue_pct_gdp[i] * 
        data_baseline_final$gdp_current_prices_billions[i] / 100
      
      # Calculate debt-to-GDP ratio using debt dynamics equation
      data_baseline_final$gross_debt_pct_gdp[i] <- 
        data_baseline_final$gross_debt_pct_gdp[i-1] * 
        (1 + data_baseline_final$nominal_interest_rate[i-1]/100) / 
        (1 + data_baseline_final$gdp_growth_pct[i-1]/100) - 
        data_baseline_final$primary_net_lending_pct_gdp[i]
      
      # General government gross debt (LCU billions)
      data_baseline_final$gross_debt_billions[i] <- 
        data_baseline_final$gross_debt_pct_gdp[i] * 
        data_baseline_final$gdp_current_prices_billions[i]/100
      
      # General government primary net lending/borrowing (LCU billions)
      data_baseline_final$primary_net_lending_billions[i] <- 
        data_baseline_final$primary_net_lending_pct_gdp[i] * 
        data_baseline_final$gdp_current_prices_billions[i]/100
      
      # General government interest payments (LCU billions)
      data_baseline_final$interest_payments_billions[i] <- 
        data_baseline_final$gross_debt_billions[i] * 
        data_baseline_final$nominal_interest_rate[i]/100
      
      # General government interest payments (% Revenue)
      data_baseline_final$interest_payments_pct_revenue[i] <- 
        data_baseline_final$interest_payments_billions[i] / 
        data_baseline_final$revenue_billions[i] * 100
    }
  }
  # return dataframe
  return(data_baseline_final)
}

# end: --------------------------------------------------------------------


