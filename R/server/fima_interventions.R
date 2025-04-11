
# start: ------------------------------------------------------------------
fima_interventions <- function(data_baseline){
  # intervention data values
  intervention_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  intervention_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  # instrument data values
  instrument_values <- c(0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.15, 0.15)
  instrument_values_interest <- c(-0.01, -0.01, -0.02, -0.04, -0.06, -0.08, -0.1, -0.12, -0.12, -0.12)
  
  # Create the interevntions dataframe
  data_ngdp_pb <- data.frame(
    year = 2024:2033,
    # interventions
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
    risk_based_solvency_capital_requirements = intervention_values,
    # instrument
    sustainability_linked_bonds = instrument_values, 
    sustainability_linked_loans = instrument_values, 
    debt_for_nature_swaps = instrument_values, 
    carbon_credits = instrument_values, 
    biodiversity_credits = instrument_values, 
    credit_enhancement = instrument_values
  ) %>% 
    # left join
    left_join(
      y = data_baseline %>% select(year,gdp_growth_pct, primary_net_lending_pct_gdp),
      by = "year"
    ) %>% 
    # mutate
    mutate(
      # nominal gdp growth
      policy_shock_ngdp_growth = (gdp_growth_pct) + (
        # add interventions
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
        # add inerventions
        silvopasture + precision_agriculture + 
          large_and_medium_scale_irrigation + 
          # add instruments
          carbon_credits + biodiversity_credits
      )
    )
  
  # Create the interevntions dataframe
  data_interest <- data.frame(
    year = 2024:2033,
    # interventions
    catastrophe_bonds = intervention_values_interest,
    insurance_premium_subsidies = intervention_values_interest,
    microinsurance = intervention_values_interest,
    cross_border_reinsurance = intervention_values_interest,
    compulsory_insurance_coverage = intervention_values_interest,
    insurance_bundling  = intervention_values_interest,
    risk_based_solvency_capital_requirements = intervention_values_interest,
    # instruments
    sustainability_linked_bonds = instrument_values_interest, 
    sustainability_linked_loans = instrument_values_interest, 
    debt_for_nature_swaps = instrument_values_interest,
    credit_enhancement = instrument_values_interest
  ) %>% 
    # left join
    left_join(
      y = data_baseline %>% select(year,nominal_interest_rate),
      by = "year"
    ) %>% 
    # mutate
    mutate(
      policy_shock_nominal_interest = (nominal_interest_rate) + (
        # add interventions
        catastrophe_bonds + insurance_premium_subsidies + 
          microinsurance + cross_border_reinsurance + 
          compulsory_insurance_coverage + insurance_bundling + 
          risk_based_solvency_capital_requirements + 
          # add instruments
          sustainability_linked_bonds + 
          sustainability_linked_loans + debt_for_nature_swaps + credit_enhancement
      )
    )
  # combined data
  data_final <- left_join(x = data_ngdp_pb, y = data_interest, by = "year") %>% 
    select(year, contains("policy_shock_"))
  # return dataframe
  return(data_final)
}

# ends: -------------------------------------------------------------------

