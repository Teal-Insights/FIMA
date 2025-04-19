# -------------------------------------------------------------------------
# fima_data_interventions - With Strict Instrument Impact Categories
# -------------------------------------------------------------------------

fima_data_interventions <- function(chosen_interventions, chosen_instruments = NULL) {
  # Define the intervention/instrument values
  intervention_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  intervention_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  instrument_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  instrument_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  
  # Convert nulls to empty vectors to prevent errors
  if (is.null(chosen_interventions)) chosen_interventions <- c()
  if (is.null(chosen_instruments)) chosen_instruments <- c()
  
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
  
  # Define GDP instruments (currently none)
  gdp_instruments <- c()
  
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
  
  # Initialize impact columns with zeros
  data_interventions$gdp_interventions_impact <- rep(0, 10)
  data_interventions$gdp_instruments_impact <- rep(0, 10)
  data_interventions$pb_interventions_impact <- rep(0, 10)
  data_interventions$pb_instruments_impact <- rep(0, 10)
  data_interventions$ir_interventions_impact <- rep(0, 10)
  data_interventions$ir_instruments_impact <- rep(0, 10)
  
  # Add all interventions with zero values initially
  for (intervention in all_interventions) {
    data_interventions[[intervention]] <- rep(0, 10)
  }
  
  # Add all instruments with zero values initially
  for (instrument in all_instruments) {
    data_interventions[[instrument]] <- rep(0, 10)
  }
  
  # Process interventions
  valid_interventions <- intersect(chosen_interventions, all_interventions)
  for (intervention in valid_interventions) {
    # Set values in the individual intervention columns
    if (intervention %in% interest_rate_interventions) {
      data_interventions[[intervention]] <- intervention_values_interest
    } else {
      data_interventions[[intervention]] <- intervention_values
    }
    
    # Update impact columns based on intervention type
    if (intervention %in% gdp_interventions) {
      data_interventions$gdp_interventions_impact <- data_interventions$gdp_interventions_impact + intervention_values
    }
    
    if (intervention %in% primary_balance_interventions) {
      data_interventions$pb_interventions_impact <- data_interventions$pb_interventions_impact + intervention_values
    }
    
    if (intervention %in% interest_rate_interventions) {
      data_interventions$ir_interventions_impact <- data_interventions$ir_interventions_impact + intervention_values_interest
    }
  }
  
  # Process instruments - ONLY affect their specific impact category
  valid_instruments <- intersect(chosen_instruments, all_instruments)
  for (instrument in valid_instruments) {
    # For proper record-keeping, still update the individual instrument columns
    if (instrument %in% interest_rate_instruments) {
      data_interventions[[instrument]] <- instrument_values_interest
    } else {
      data_interventions[[instrument]] <- instrument_values
    }
    
    # CRITICAL CHANGE: Only update the specific impact category
    # Primary balance instruments
    if (instrument %in% primary_balance_instruments) {
      data_interventions$pb_instruments_impact <- data_interventions$pb_instruments_impact + instrument_values
    }
    # Interest rate instruments 
    else if (instrument %in% interest_rate_instruments) {
      data_interventions$ir_instruments_impact <- data_interventions$ir_instruments_impact + instrument_values_interest
    }
    # GDP instruments (if added in the future)
    else if (instrument %in% gdp_instruments) {
      data_interventions$gdp_instruments_impact <- data_interventions$gdp_instruments_impact + instrument_values
    }
  }
  
  return(data_interventions)
}

# -------------------------------------------------------------------------
# fima_server_interventions - Strict application of impacts
# -------------------------------------------------------------------------

fima_server_interventions <- function(
    data_baseline, 
    data_interventions, 
    chosen_interventions, 
    chosen_instruments) {
  
  # Create the final data frame with baseline and policy shock values
  data_final <- data_baseline %>%
    select(year, gdp_growth_pct, primary_net_lending_pct_gdp, nominal_interest_rate) %>%
    left_join(
      data_interventions %>% 
        select(
          year, gdp_interventions_impact, gdp_instruments_impact,
          pb_interventions_impact, pb_instruments_impact, 
          ir_interventions_impact, ir_instruments_impact
        ),
      by = "year"
    )
  
  # Calculate policy shock values by adding impacts to baseline
  data_final <- data_final %>%
    mutate(
      # GDP growth: Baseline + GDP interventions impact + GDP instruments impact
      policy_shock_ngdp_growth = gdp_growth_pct + gdp_interventions_impact + gdp_instruments_impact,
      
      # Primary balance: Baseline + PB interventions impact + PB instruments impact
      policy_shock_primary_balance = primary_net_lending_pct_gdp + pb_interventions_impact + pb_instruments_impact,
      
      # Interest rate: Baseline + IR interventions impact + IR instruments impact
      policy_shock_nominal_interest = nominal_interest_rate + ir_interventions_impact + ir_instruments_impact
    ) %>%
    select(year, policy_shock_ngdp_growth, policy_shock_primary_balance, policy_shock_nominal_interest)
  
  # Return dataframe
  return(data_final)
}