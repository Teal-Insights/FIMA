
# start: ------------------------------------------------------------------
fima_baseline_scenario <- function(by_country){
  # -------------------------------------------------------------------------
  # data
  # -------------------------------------------------------------------------
  # read the macrofiscal data
  df_macrofiscal_raw <- readxl::read_excel(
    path = "data-raw/FIMA_APP.xlsx",
    sheet = "Macrofiscal") %>% 
    select(-c(indicator))
  # clean the data
  df_macrofiscal_clean <- df_macrofiscal_raw %>% 
    filter(country == by_country) %>% 
    mutate(across(matches("^\\d{4}$"), as.character)) %>%
    pivot_longer(
      cols = -c(country, col_name),
      names_to = "year",
      values_to = "values"
    ) %>% 
    pivot_wider(
      names_from = col_name,
      values_from = values
    ) %>%
    mutate(
      year = as.integer(year),
      across(-c(country, year, credit_rating), as.numeric),
      credit_rating = case_when(
        credit_rating == "NA" ~ NA, .default = credit_rating
      )
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
      nominal_interest_rate = (interest_payments_billions/gross_debt_billions) * 100,
      # Debt-stabilising primary balance	%age of NGDP
      dspb_pct_ngdp = ((nominal_interest_rate - gdp_growth_pct)/100) / (1 + gdp_growth_pct) * gross_debt_pct_gdp,
      # credit rating number
      credit_rating_number = fima_credit_rating_to_number(rating = credit_rating)
    ) %>% 
    select(
      # year
      year,
      # Gross domestic product, current prices (LCU billions)
      gdp_current_prices_billions,             
      # Nominal GDP growth (yoy%)
      gdp_growth_pct,                          
      # General government gross debt (LCU billions)
      gross_debt_billions,                     
      # General government gross debt (%GDP)
      gross_debt_pct_gdp,                      
      # General government net lending/borrowing (LCU billions)
      net_lending_billions,                    
      # General government primary net lending/borrowing (LCU billions)
      primary_net_lending_billions,            
      # General government primary net lending/borrowing (%GDP)
      primary_net_lending_pct_gdp,             
      # General government interest payments (LCU billions)
      interest_payments_billions,              
      # General government interest payments (% Revenue)
      interest_payments_pct_revenue,           
      # Nominal interest rate
      nominal_interest_rate,                   
      # General government revenue (LCU billions)
      revenue_billions,                        
      # General government revenue (% GDP)
      revenue_pct_gdp,                         
      # Debt-stabilising primary balance (%age of NGDP)
      dspb_pct_ngdp, 
      # credit rating
      credit_rating,
      credit_rating_number
    ) %>% 
    # extend years to 2033
    bind_rows(data.frame(year = 2030:2033)) %>% 
    # use final values to extend to 2033 for
    fill(
      c(gdp_growth_pct,primary_net_lending_pct_gdp,nominal_interest_rate,
        revenue_pct_gdp,credit_rating),
      .direction = "down"
    )
  # -------------------------------------------------------------------------
  # Recursive
  # -------------------------------------------------------------------------
  # Create a copy of the baseline data
  data_baseline_final <- df_macrofiscal_clean
  
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
      
      # Debt-stabilising primary balance	%age of NGDP
      data_baseline_final$dspb_pct_ngdp[i] <- 
        (
          (
            data_baseline_final$nominal_interest_rate[i] -
            data_baseline_final$gdp_growth_pct[i])/100
          ) /
        (1 + data_baseline_final$gdp_growth_pct[i]) * 
        data_baseline_final$gross_debt_pct_gdp[i]
      
      # credit rating number
      data_baseline_final$credit_rating_number = fima_credit_rating_to_number(
        rating = data_baseline_final$credit_rating)
    }
  }
  # return dataframe
  return(data_baseline_final)
}

# -------------------------------------------------------------------------
# credit rating to number
# -------------------------------------------------------------------------
fima_credit_rating_to_number <- function(rating) {
  # Create the mapping from credit ratings to numbers
  rating_map <- c(
    "AAA" = 22,
    "AA+" = 21,
    "AA" = 20,
    "AA-" = 19,
    "A+" = 18,
    "A" = 17,
    "A-" = 16,
    "BBB+" = 15,
    "BBB" = 14,
    "BBB-" = 13,
    "BB+" = 12,
    "BB" = 11,
    "BB-" = 10,
    "B+" = 9,
    "B" = 8,
    "B-" = 7,
    "CCC+" = 6,
    "CCC" = 5,
    "CCC-" = 4,
    "CC" = 3,
    "C" = 2,
    "RD" = 1
  )
  
  # Check if input is a vector
  if (length(rating) > 1) {
    # Apply the function to each element in the vector
    return(sapply(rating, function(r) {
      if (is.na(r)) {
        return(NA)
      } else {
        # Return the number or NA if rating is not found
        return(rating_map[r])
      }
    }))
  } else {
    # Handle single input
    if (is.na(rating)) {
      return(NA)
    } else {
      return(rating_map[rating])
    }
  }
}

# end: --------------------------------------------------------------------


