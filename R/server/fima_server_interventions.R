
# -------------------------------------------------------------------------
# fima_data_interventions
# -------------------------------------------------------------------------

# start: ------------------------------------------------------------------

fima_data_interventions <- function(chosen_interventions, chosen_instruments = NULL) {
  # Define the intervention/instrument values
  intervention_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  intervention_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  instrument_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  instrument_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  
  # If chosen_instruments is NULL, set to empty vector
  if (is.null(chosen_instruments)) {
    chosen_instruments <- c()
  }
  
  # Define all interventions and instruments
  all_interventions <- c(
    "silvopasture", "reduced_till_farming", "dams_and_seawalls", 
    "restoring_degraded_forest", "precision_agriculture", "agroforestry", 
    "large_and_medium_scale_irrigation", "climate_resilient_seeds", 
    "catastrophe_bonds", "insurance_premium_subsidies", "microinsurance", 
    "cross_border_reinsurance", "compulsory_insurance_coverage", 
    "insurance_bundling", "risk_based_solvency_capital_requirements"
  )
  
  all_instruments <- c(
    "sustainability_linked_bonds", "sustainability_linked_loans", 
    "debt_for_nature_swaps", "carbon_credits", "biodiversity_credits", 
    "credit_enhancement"
  )
  
  # Define which interventions/instruments affect each indicator
  gdp_interventions <- c(
    "silvopasture", "reduced_till_farming", "dams_and_seawalls", 
    "restoring_degraded_forest", "precision_agriculture", "agroforestry", 
    "large_and_medium_scale_irrigation", "climate_resilient_seeds", 
    "catastrophe_bonds", "microinsurance"
  )
  
  primary_balance_interventions <- c(
    "silvopasture", "precision_agriculture", "large_and_medium_scale_irrigation"
  )
  
  primary_balance_instruments <- c(
    "carbon_credits", "biodiversity_credits"
  )
  
  interest_rate_interventions <- c(
    "catastrophe_bonds", "insurance_premium_subsidies", "microinsurance", 
    "cross_border_reinsurance", "compulsory_insurance_coverage", 
    "insurance_bundling", "risk_based_solvency_capital_requirements"
  )
  
  interest_rate_instruments <- c(
    "sustainability_linked_bonds", "sustainability_linked_loans", 
    "debt_for_nature_swaps", "credit_enhancement"
  )
  
  # Create a dataframe with years
  data_interventions <- data.frame(year = 2024:2033)
  
  # Add all interventions with zero values initially
  for (intervention in all_interventions) {
    data_interventions[[intervention]] <- rep(0, 10)
  }
  
  # Add all instruments with zero values initially
  for (instrument in all_instruments) {
    data_interventions[[instrument]] <- rep(0, 10)
  }
  
  # Update selected interventions with actual values
  for (intervention in chosen_interventions) {
    # For interest rate interventions, use negative values
    if (intervention %in% interest_rate_interventions) {
      data_interventions[[intervention]] <- intervention_values_interest
    } else {
      # For all other interventions, use positive values
      data_interventions[[intervention]] <- intervention_values
    }
  }
  
  # Update selected instruments with actual values
  for (instrument in chosen_instruments) {
    # For interest rate instruments, use negative values
    if (instrument %in% interest_rate_instruments) {
      data_interventions[[instrument]] <- instrument_values_interest
    } else {
      # For all other instruments, use positive values
      data_interventions[[instrument]] <- instrument_values
    }
  }
  
  # Create additional indicator-specific columns for clearer tracking
  
  # GDP impact columns (always use positive intervention values)
  data_interventions$gdp_interventions_impact <- 0
  gdp_selected <- intersect(chosen_interventions, gdp_interventions)
  
  if (length(gdp_selected) > 0) {
    for (intervention in gdp_selected) {
      # Always use positive intervention values for GDP impact
      data_interventions$gdp_interventions_impact <- 
        data_interventions$gdp_interventions_impact + intervention_values
    }
  }
  
  # Primary balance impact columns (interventions) - always use positive values
  data_interventions$pb_interventions_impact <- 0
  pb_interventions_selected <- intersect(chosen_interventions, primary_balance_interventions)
  
  if (length(pb_interventions_selected) > 0) {
    for (intervention in pb_interventions_selected) {
      # Always use positive intervention values for primary balance
      data_interventions$pb_interventions_impact <- 
        data_interventions$pb_interventions_impact + intervention_values
    }
  }
  
  # Primary balance impact columns (instruments) - always use positive values
  data_interventions$pb_instruments_impact <- 0
  pb_instruments_selected <- intersect(chosen_instruments, primary_balance_instruments)
  
  if (length(pb_instruments_selected) > 0) {
    for (instrument in pb_instruments_selected) {
      # Always use positive instrument values for primary balance
      data_interventions$pb_instruments_impact <- 
        data_interventions$pb_instruments_impact + instrument_values
    }
  }
  
  # Interest rate impact columns (interventions) - use negative values as stored
  data_interventions$ir_interventions_impact <- 0
  ir_interventions_selected <- intersect(chosen_interventions, interest_rate_interventions)
  
  if (length(ir_interventions_selected) > 0) {
    for (intervention in ir_interventions_selected) {
      # Use the values as stored in the data frame (which are negative)
      data_interventions$ir_interventions_impact <- 
        data_interventions$ir_interventions_impact + data_interventions[[intervention]]
    }
  }
  
  # Interest rate impact columns (instruments) - use negative values as stored
  data_interventions$ir_instruments_impact <- 0
  ir_instruments_selected <- intersect(chosen_instruments, interest_rate_instruments)
  
  if (length(ir_instruments_selected) > 0) {
    for (instrument in ir_instruments_selected) {
      # Use the values as stored in the data frame (which are negative)
      data_interventions$ir_instruments_impact <- 
        data_interventions$ir_instruments_impact + data_interventions[[instrument]]
    }
  }
  
  # return dataframe
  return(data_interventions)
}

# end: --------------------------------------------------------------------

# -------------------------------------------------------------------------
# fima_server_interventions
# -------------------------------------------------------------------------

# start: ------------------------------------------------------------------
fima_server_interventions <- function(
    data_baseline, 
    data_interventions, 
    chosen_interventions, 
    chosen_instruments) {
  # Define which interventions/instruments affect each variable
  gdp_interventions <- c(
    "silvopasture", "reduced_till_farming", "dams_and_seawalls", 
    "restoring_degraded_forest", "precision_agriculture", "agroforestry", 
    "large_and_medium_scale_irrigation", "climate_resilient_seeds", 
    "catastrophe_bonds", "microinsurance"
  )
  
  primary_balance_interventions <- c(
    "silvopasture", "precision_agriculture", "large_and_medium_scale_irrigation"
  )
  
  primary_balance_instruments <- c(
    "carbon_credits", "biodiversity_credits"
  )
  
  interest_rate_interventions <- c(
    "catastrophe_bonds", "insurance_premium_subsidies", "microinsurance", 
    "cross_border_reinsurance", "compulsory_insurance_coverage", 
    "insurance_bundling", "risk_based_solvency_capital_requirements"
  )
  
  interest_rate_instruments <- c(
    "sustainability_linked_bonds", "sustainability_linked_loans", 
    "debt_for_nature_swaps", "credit_enhancement"
  )
  
  # Create the final data frame with baseline and policy shock values
  data_final <- data_baseline %>%
    select(year, gdp_growth_pct, primary_net_lending_pct_gdp, nominal_interest_rate) %>%
    left_join(
      data_interventions %>% 
        select(year, gdp_interventions_impact, pb_interventions_impact, 
               pb_instruments_impact, ir_interventions_impact, ir_instruments_impact),
      by = "year"
    )
  
  # Calculate policy shock values by adding impacts to baseline
  data_final <- data_final %>%
    mutate(
      # GDP growth: Baseline + GDP interventions impact
      policy_shock_ngdp_growth = gdp_growth_pct + gdp_interventions_impact,
      
      # Primary balance: Baseline + PB interventions impact + PB instruments impact
      policy_shock_primary_balance = primary_net_lending_pct_gdp + pb_interventions_impact + pb_instruments_impact,
      
      # Interest rate: Baseline + IR interventions impact + IR instruments impact
      policy_shock_nominal_interest = nominal_interest_rate + ir_interventions_impact + ir_instruments_impact
    ) %>%
    select(year, policy_shock_ngdp_growth, policy_shock_primary_balance, policy_shock_nominal_interest)
  
  # return dataframe
  return(data_final)
}

# end: --------------------------------------------------------------------
