
# starts: -----------------------------------------------------------------
# Implement fima_server_interventions function
fima_server_interventions <- function(data_baseline, data_interventions, chosen_interventions) {
  # Join datasets
  data_interventions_combined <- data_baseline %>% 
    select(year, gdp_growth_pct, primary_net_lending_pct_gdp, nominal_interest_rate) %>% 
    left_join(data_interventions, by = "year")
  
  # Define which interventions affect each variable
  gdp_interventions <- c(
    "silvopasture", "reduced_till_farming", "dams_and_seawalls", 
    "restoring_degraded_forest", "precision_agriculture", "agroforestry", 
    "large_and_medium_scale_irrigation", "climate_resilient_seeds", 
    "catastrophe_bonds", "microinsurance"
  )
  
  primary_balance_interventions <- c(
    "silvopasture", "precision_agriculture", "large_and_medium_scale_irrigation"
  )
  
  interest_rate_interventions <- c(
    "catastrophe_bonds", "insurance_premium_subsidies", "microinsurance", 
    "cross_border_reinsurance", "compulsory_insurance_coverage", 
    "insurance_bundling", "risk_based_solvency_capital_requirements"
  )
  
  # Filter to only include selected interventions
  selected_gdp_interventions <- intersect(chosen_interventions, gdp_interventions)
  selected_primary_balance_interventions <- intersect(chosen_interventions, primary_balance_interventions)
  selected_interest_rate_interventions <- intersect(chosen_interventions, interest_rate_interventions)
  
  data_interventions_final <- data_interventions_combined %>% 
    # apply interventions on variables
    mutate(
      # Start with baseline values
      policy_shock_ngdp_growth = gdp_growth_pct,
      policy_shock_primary_balance = primary_net_lending_pct_gdp,
      policy_shock_nominal_interest = nominal_interest_rate
    )
  
  # Add GDP growth interventions if selected
  if (length(selected_gdp_interventions) > 0) {
    for (intervention in selected_gdp_interventions) {
      data_interventions_final$policy_shock_ngdp_growth <- 
        data_interventions_final$policy_shock_ngdp_growth + data_interventions_combined[[intervention]]
    }
  }
  
  # Add primary balance interventions if selected
  if (length(selected_primary_balance_interventions) > 0) {
    for (intervention in selected_primary_balance_interventions) {
      data_interventions_final$policy_shock_primary_balance <- 
        data_interventions_final$policy_shock_primary_balance + data_interventions_combined[[intervention]]
    }
  }
  
  # Add interest rate interventions if selected (with negative multiplier)
  if (length(selected_interest_rate_interventions) > 0) {
    for (intervention in selected_interest_rate_interventions) {
      data_interventions_final$policy_shock_nominal_interest <- 
        data_interventions_final$policy_shock_nominal_interest - data_interventions_combined[[intervention]]
    }
  }
  
  # Select only the relevant columns
  data_interventions_final <- data_interventions_final %>%
    select(year, contains("policy_shock_"))
  
  # return dataframe
  return(data_interventions_final)
}

# -------------------------------------------------------------------------
# data interventions
# -------------------------------------------------------------------------
fima_data_interventions <- function(chosen_interventions){
  # Define the intervention values
  intervention_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  
  # Create the initial dataframe with all interventions set to 0
  all_interventions <- c(
    "silvopasture", "reduced_till_farming", "dams_and_seawalls", 
    "restoring_degraded_forest", "precision_agriculture", "agroforestry", 
    "large_and_medium_scale_irrigation", "climate_resilient_seeds", 
    "catastrophe_bonds", "insurance_premium_subsidies", "microinsurance", 
    "cross_border_reinsurance", "compulsory_insurance_coverage", 
    "insurance_bundling", "risk_based_solvency_capital_requirements"
  )
  
  # Create a baseline intervention dataframe with zeros
  data_interventions <- data.frame(year = 2024:2033)
  
  # Add all interventions with zero values initially
  for (intervention in all_interventions) {
    data_interventions[[intervention]] <- rep(0, 10)
  }
  
  # Update selected interventions with actual values
  selected <- chosen_interventions
  for (intervention in selected) {
    data_interventions[[intervention]] <- intervention_values
  }
  # return dataframe
  return(data_interventions)
}



# ends: -------------------------------------------------------------------


