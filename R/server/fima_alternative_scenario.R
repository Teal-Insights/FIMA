
# start: ------------------------------------------------------------------
fima_alternative_scenario <- function(
    data_baseline, 
    data_interventions,
    data_adjustment
    ){
  data_alternative <- data_baseline %>% 
    filter(year < 2024) %>% 
    select(
      year, gross_debt_billions, gross_debt_pct_gdp,nominal_interest_rate,
      gdp_growth_pct, primary_net_lending_pct_gdp,
      interest_payments_billions,interest_payments_pct_revenue, 
      primary_net_lending_billions, gdp_current_prices_billions,
      dspb_pct_ngdp) %>% 
    mutate(
      credit_rating = NA,
      credit_rating_number = NA
    ) %>% 
    full_join(y = data_interventions, by = "year") %>% 
    left_join(y = data_baseline %>% select(year,revenue_billions),by = "year") %>% 
    # One off adjustment to stock of debt (LCU billions)
    left_join(y = data_adjustment %>% select(-country), by = "year") %>% 
    mutate(
      one_off_adjustment_to_debt_stock = case_when(
        is.na(one_off_adjustment_to_debt_stock) ~ 0 , 
        .default = one_off_adjustment_to_debt_stock
      )
    )
  
  # Recursive
  data_alternative_combined <- data_alternative
  
  # Identify future years
  future_years <- which(data_alternative_combined$year > 2023)
  selected_country <- data_adjustment$country %>% unique()
  
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
      
      # Debt-stabilising primary balance	%age of NGDP
      data_alternative_combined$dspb_pct_ngdp[i] <- 
        (
          (
            data_alternative_combined$policy_shock_nominal_interest[i] -
              data_alternative_combined$policy_shock_ngdp_growth[i])/100
        ) /
        (1 + data_alternative_combined$policy_shock_ngdp_growth[i]) * 
        data_alternative_combined$gross_debt_pct_gdp[i]
      
      # credit rating
      # credit rating
      data_alternative_combined$credit_rating_number[i] <- 
        if (selected_country == "Ruritania") {
          ifelse(data_alternative_combined$gross_debt_pct_gdp[i] > 43, 10, 11)
        } else if (selected_country == "Xenon") {
          ifelse(data_alternative_combined$gross_debt_pct_gdp[i] > 36, 9, 10)
        } else if (selected_country == "Aurelia") {
          ifelse(data_alternative_combined$gross_debt_pct_gdp[i] > 25, 14, 15)
        }
      
      # Then convert the number to credit rating as you're already doing
      data_alternative_combined$credit_rating[i] <- 
        fima_number_to_credit_rating(
          number = data_alternative_combined$credit_rating_number[i]
        )
    }
  }
  # return data
  return(data_alternative_combined)
}

# -------------------------------------------------------------------------
# credit rating number to credit rating
# -------------------------------------------------------------------------
fima_number_to_credit_rating <- function(number) {
  # Create the mapping from numbers to credit ratings
  number_map <- c(
    "22" = "AAA",
    "21" = "AA+",
    "20" = "AA",
    "19" = "AA-",
    "18" = "A+",
    "17" = "A",
    "16" = "A-",
    "15" = "BBB+",
    "14" = "BBB",
    "13" = "BBB-",
    "12" = "BB+",
    "11" = "BB",
    "10" = "BB-",
    "9" = "B+",
    "8" = "B",
    "7" = "B-",
    "6" = "CCC+",
    "5" = "CCC",
    "4" = "CCC-",
    "3" = "CC",
    "2" = "C",
    "1" = "RD"
  )
  
  # Check if input is a vector
  if (length(number) > 1) {
    # Apply the function to each element in the vector
    return(sapply(number, function(n) {
      if (is.na(n)) {
        return(NA)
      } else {
        # Convert number to character for lookup
        n_char <- as.character(n)
        # Return the rating or NA if number is not found
        return(number_map[n_char])
      }
    }))
  } else {
    # Handle single input
    if (is.na(number)) {
      return(NA)
    } else {
      # Convert number to character for lookup
      n_char <- as.character(number)
      return(number_map[n_char])
    }
  }
}
# -------------------------------------------------------------------------
# renaming y-axis values on credit rating plot
# -------------------------------------------------------------------------
fima_cra_y_axis <- function(y_values) {
  # Define the mapping from numbers to credit ratings
  number_map <- c(
    "22" = "AAA",
    "21" = "AA+",
    "20" = "AA",
    "19" = "AA-",
    "18" = "A+",
    "17" = "A",
    "16" = "A-",
    "15" = "BBB+",
    "14" = "BBB",
    "13" = "BBB-",
    "12" = "BB+",
    "11" = "BB",
    "10" = "BB-",
    "9" = "B+",
    "8" = "B",
    "7" = "B-",
    "6" = "CCC+",
    "5" = "CCC",
    "4" = "CCC-",
    "3" = "CC",
    "2" = "C",
    "1" = "RD"
  )
  
  # Convert numeric values to character ratings
  sapply(y_values, function(y) {
    if(is.na(y)) {
      return(NA)
    } else {
      y_char <- as.character(y)
      return(number_map[y_char])
    }
  })
}
# -------------------------------------------------------------------------
# viz data (home tab)
# -------------------------------------------------------------------------
fima_alternative_viz <- function(data_baseline, data_alternative){
  final_baseline <- data_baseline %>% 
    select(
      year, gross_debt_pct_gdp,gdp_growth_pct,interest_payments_pct_revenue,
      primary_net_lending_pct_gdp,credit_rating, credit_rating_number
    ) %>% 
    mutate(group = "Baseline Scenario")
  
  final_alternative <- data_alternative %>% 
    select(
      year, gross_debt_pct_gdp, gdp_growth_pct, interest_payments_pct_revenue,
      primary_net_lending_pct_gdp,credit_rating, credit_rating_number
      ) %>% 
    mutate(group = "Alternative Scenario") %>% 
    filter(year >= 2023)
  
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

# -------------------------------------------------------------------------
# table data (data tab)
# -------------------------------------------------------------------------
fima_alternative_table <- function(data_baseline, data_alternative){
  final_baseline <- data_baseline %>% 
    select(
      # year
      year, 
      # gross debt
      gross_debt_billions, 
      gross_debt_pct_gdp,
      # nominal interest rate
      nominal_interest_rate,
      # primary balance
      primary_net_lending_billions,
      primary_net_lending_pct_gdp,
      dspb_pct_ngdp,
      # interest payment
      interest_payments_billions,
      interest_payments_pct_revenue,
      # nominal GDP
      gdp_current_prices_billions,
      gdp_growth_pct,
      # revenue
      revenue_billions,
      # credit rating
      credit_rating
    ) %>% 
    # mutate(group = "Baseline Scenario") %>% 
    filter(year < 2024)
  
  final_alternative <- data_alternative %>% 
    select(
      # year
      year, 
      # gross debt
      gross_debt_billions, 
      gross_debt_pct_gdp,
      # nominal interest rate
      nominal_interest_rate = policy_shock_nominal_interest,
      # primary balance
      primary_net_lending_billions,
      primary_net_lending_pct_gdp,
      dspb_pct_ngdp,
      # interest payment
      interest_payments_billions,
      interest_payments_pct_revenue,
      # nominal GDP
      gdp_current_prices_billions,
      gdp_growth_pct,
      # revenue
      revenue_billions,
      # credit rating
      credit_rating
    ) %>% 
    # mutate(group = "Alternative Scenario") %>% 
    filter(year >= 2024)
  
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
