# Modified fima_server_interventions function with instruments added
fima_server_interventions <- function(data_baseline, data_interventions, chosen_interventions, chosen_instruments) {
  # Define the intervention/instrument values
  intervention_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  instrument_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  
  # Join datasets
  data_interventions_combined <- data_baseline %>% 
    select(year, gdp_growth_pct, primary_net_lending_pct_gdp, nominal_interest_rate) %>% 
    left_join(data_interventions, by = "year")
  
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
  
  # Filter to only include selected interventions and instruments
  selected_gdp_interventions <- intersect(chosen_interventions, gdp_interventions)
  selected_primary_balance_interventions <- intersect(chosen_interventions, primary_balance_interventions)
  selected_primary_balance_instruments <- intersect(chosen_instruments, primary_balance_instruments)
  selected_interest_rate_interventions <- intersect(chosen_interventions, interest_rate_interventions)
  selected_interest_rate_instruments <- intersect(chosen_instruments, interest_rate_instruments)
  
  # Create a separate data frame for interest rate effects (interventions + instruments)
  data_interest <- data.frame(year = 2024:2033)
  
  # Add selected interest rate interventions
  for (intervention in selected_interest_rate_interventions) {
    data_interest[[intervention]] <- intervention_values_interest
  }
  
  # Add selected interest rate instruments
  for (instrument in selected_interest_rate_instruments) {
    data_interest[[instrument]] <- instrument_values_interest
  }
  
  # Join with data_baseline to get the base interest rate
  data_interest <- data_interest %>%
    left_join(data_baseline %>% select(year, nominal_interest_rate), by = "year") %>%
    # Calculate the policy shock nominal interest rate (starting with baseline)
    mutate(
      policy_shock_nominal_interest = nominal_interest_rate
    )
  
  # Add effect of selected interest rate interventions and instruments
  if (length(selected_interest_rate_interventions) > 0 || length(selected_interest_rate_instruments) > 0) {
    # Update the formula to add all selected interventions and instruments
    interest_items <- c(selected_interest_rate_interventions, selected_interest_rate_instruments)
    interest_formula <- paste0("nominal_interest_rate + (", 
                               paste(interest_items, collapse = " + "), 
                               ")")
    data_interest <- data_interest %>%
      mutate(
        policy_shock_nominal_interest = eval(parse(text = interest_formula))
      )
  }
  
  # Create the data frame for GDP and primary balance effects
  data_ngdp_pb <- data_interventions_combined %>%
    mutate(
      # Start with baseline values
      policy_shock_ngdp_growth = gdp_growth_pct,
      policy_shock_primary_balance = primary_net_lending_pct_gdp
    )
  
  # Add GDP growth interventions if selected
  if (length(selected_gdp_interventions) > 0) {
    for (intervention in selected_gdp_interventions) {
      data_ngdp_pb$policy_shock_ngdp_growth <- 
        data_ngdp_pb$policy_shock_ngdp_growth + data_interventions_combined[[intervention]]
    }
  }
  
  # Add primary balance interventions if selected
  if (length(selected_primary_balance_interventions) > 0) {
    for (intervention in selected_primary_balance_interventions) {
      data_ngdp_pb$policy_shock_primary_balance <- 
        data_ngdp_pb$policy_shock_primary_balance + data_interventions_combined[[intervention]]
    }
  }
  
  # Add primary balance instruments if selected
  if (length(selected_primary_balance_instruments) > 0) {
    for (instrument in selected_primary_balance_instruments) {
      # Assuming instruments have the same values as interventions in data_interventions
      data_ngdp_pb$policy_shock_primary_balance <- 
        data_ngdp_pb$policy_shock_primary_balance + data_interventions_combined[[instrument]]
    }
  }
  
  # Combine the two data frames
  data_interventions_final <- data_ngdp_pb %>%
    select(year, policy_shock_ngdp_growth, policy_shock_primary_balance) %>%
    left_join(data_interest %>% select(year, policy_shock_nominal_interest), by = "year")
  
  # return dataframe
  return(data_interventions_final)
}

# Modified fima_data_interventions function with instruments added
fima_data_interventions <- function(chosen_interventions, chosen_instruments = NULL){
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
  
  # Define interest rate interventions and instruments
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
    if (intervention %in% interest_rate_interventions) {
      data_interventions[[intervention]] <- intervention_values_interest
    } else {
      data_interventions[[intervention]] <- intervention_values
    }
  }
  
  # Update selected instruments with actual values
  for (instrument in chosen_instruments) {
    if (instrument %in% interest_rate_instruments) {
      data_interventions[[instrument]] <- instrument_values_interest
    } else {
      data_interventions[[instrument]] <- instrument_values
    }
  }
  
  # return dataframe
  return(data_interventions)
}