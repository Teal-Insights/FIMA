
# start: ------------------------------------------------------------------
fima_interventions <- function(data_baseline){
  # data
  intervention_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  
  # Create the dataframe
  data_interventions <- data.frame(
    year = 2024:2033,
    silvopasture = intervention_values,
    reduced_till_farming = intervention_values,
    dams_and_seawalls = intervention_values,
    restoring_degraded_forest = intervention_values,
    precision_agriculture = intervention_values,
    agroforestry = intervention_values,
    large_and_medium_scale_irrigation = intervention_values,
    climate_resilient_seeds = intervention_values,
    catastrophe_bonds = intervention_values,
    insurance_premium_subsidies = intervention_values,
    microinsurance = intervention_values,
    cross_border_reinsurance = intervention_values,
    compulsory_insurance_coverage = intervention_values,
    insurance_bundling = intervention_values,
    risk_based_solvency_capital_requirements = intervention_values
  )
  
  # data from baseline
  data_interventions_combined <- data_baseline %>% 
    select(year,gdp_growth_pct, primary_net_lending_pct_gdp, nominal_interest_rate) %>% 
    left_join(data_interventions, by = "year") 
  
  data_interventions_final <- data_interventions_combined%>% 
    # apply interventions on variables
    mutate(
      # nominal gdp growth
      policy_shock_ngdp_growth = (gdp_growth_pct) + (
        silvopasture + 
          reduced_till_farming + 
          dams_and_seawalls + 
          restoring_degraded_forest + 
          precision_agriculture + 
          agroforestry + 
          large_and_medium_scale_irrigation + 
          climate_resilient_seeds + 
          catastrophe_bonds + 
          microinsurance
      ),
      # primary balance
      policy_shock_primary_balance = (primary_net_lending_pct_gdp) + (
        silvopasture + precision_agriculture + large_and_medium_scale_irrigation
      ),
      # nominal interest rate
      policy_shock_nominal_interest = (nominal_interest_rate) + (-1) * (
        catastrophe_bonds + insurance_premium_subsidies + 
          microinsurance + cross_border_reinsurance + 
          compulsory_insurance_coverage + insurance_bundling + 
          risk_based_solvency_capital_requirements
      )
    ) %>% 
    select(year, contains("policy_shock_"))
  # return dataframe
  return(data_interventions_final)
}

# ends: -------------------------------------------------------------------

