
# start: ------------------------------------------------------------------
qcraft_server_hot_country <- function(data_baseline,iso3c,expenditure_rigidity){
  weo_max_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% max()
  # climate data
  df_climate <- qcraft_server_climate_country(by_iso3c = iso3c) %>% 
    select(years, hot_variation) %>% 
    filter(years > weo_max_year)
  
  df_hot_discrete_risks <- data.frame(
    years = seq(weo_max_year+1, 2100),
    discrete_risk_revenue = 0,
    discrete_risk_primary_expenditure = 0
  )
  # part 1
  df_hot_v1 <- data_baseline %>% 
    select(
      iso3c, country,years, working_age_population, 
      employment_growth, 
      labour_productivity_growth = productivity_growth,
      gdp_deflator_growth_percent = inflation,
      # real gdp
      real_gdp,
      real_gdp_growth_percent,
      # nominal GDP
      nominal_gdp,
      nominal_gdp_growth_percent,
      # revenue
      revenue,
      revenue_percent_ngdp,
      # primary expenditure
      primary_expenditure,
      primary_expenditure_percent_gdp,
      # primary balance
      primary_balance,
      primary_balance_percent_gdp,
      # overall balance
      overall_balance,
      overall_balance_percent_gdp,
      # interest expenditure
      interest_expenditure,
      interest_expenditure_percent_gdp,
      # total expenditure
      total_expenditure,
      total_expenditure_percent_gdp,
      # weighted interest rate
      weighted_interest_rate = interest_rate_percent,
      # Gross debt % of NGDP
      debt_to_gdp_ratio_percent,
      # Gross debt Level
      debt,
      # Debt-stabilising primary balance % of NGDP
      dspb_percent_ngdp,
      # Interest expenditure % of Revenue
      interest_expenditure_percent_revenue
    ) %>% 
    left_join(y = df_hot_discrete_risks, by = "years") %>% 
    left_join(y = df_climate, by = "years") %>% 
    mutate(
      # labour productivity growth
      labour_productivity_growth = coalesce(labour_productivity_growth, 0) + 
        coalesce(hot_variation, 0)
    )
  
  for (i in seq_len(nrow(df_hot_v1))) {
    if (df_hot_v1$years[i] > weo_max_year) {
      # real gdp growth
      df_hot_v1$real_gdp_growth_percent[i] = (1 + df_hot_v1$employment_growth[i] / 100) * 
        (1 + df_hot_v1$labour_productivity_growth[i] / 100) * 100 - 100
      # nominal gdp growth
      df_hot_v1$nominal_gdp_growth_percent[i] = (1 + df_hot_v1$real_gdp_growth_percent[i] / 100) * 
        (1 + df_hot_v1$gdp_deflator_growth_percent[i] / 100) * 100 - 100
      # nominal GDP
      df_hot_v1$nominal_gdp[i] <- df_hot_v1$nominal_gdp[i - 1] * 
        (1 + round(df_hot_v1$nominal_gdp_growth_percent[i],5) / 100)
      # real GDP
      df_hot_v1$real_gdp[i] <- df_hot_v1$real_gdp[i - 1] * 
        (1 + df_hot_v1$real_gdp_growth_percent[i] / 100)
    }
  }
  
  # part 2
  df_hot_v2 <- data_baseline %>% 
    select(
      years,
      baseline_interest_expenditure_percent_gdp = interest_expenditure_percent_gdp,
      baseline_primary_expenditure_percent_gdp = primary_expenditure_percent_gdp,
      baseline_primary_balance_percent_gdp = primary_balance_percent_gdp,
      baseline_overall_balance_percent_gdp = overall_balance_percent_gdp,
      baseline_debt_to_gdp_ratio_percent = debt_to_gdp_ratio_percent,
      baseline_primary_expenditure = primary_expenditure
    ) %>% 
    left_join(y = df_hot_v1 %>% select(years, nominal_gdp),by = "years") %>% 
    mutate(
      primary_expenditure_with_baseline_percent_ngdp = (
        baseline_primary_expenditure_percent_gdp / 100 * nominal_gdp
      ),
      primary_expenditure_recalibration = (
        baseline_primary_expenditure - primary_expenditure_with_baseline_percent_ngdp
      ) %>% round(x = .,3)
    ) %>% 
    select(-c(nominal_gdp)) %>% 
    left_join(x = df_hot_v1,y = ., by = "years")
  
  # part 3
  df_hot_v3 <- df_hot_v2
  
  for (i in seq_len(nrow(df_hot_v3))) {
    if (df_hot_v3$years[i] > weo_max_year) {
      # revenue % of NGDP
      df_hot_v3$revenue_percent_ngdp[i] = df_hot_v3$revenue_percent_ngdp[i] + 
        df_hot_v3$discrete_risk_revenue[i]
      # revenue value
      df_hot_v3$revenue[i] = df_hot_v3$nominal_gdp[i] * 
        df_hot_v3$revenue_percent_ngdp[i] / 100
      # primary expenditure value
      df_hot_v3$primary_expenditure[i] = df_hot_v3$primary_expenditure[i] - 
        ((1 - expenditure_rigidity/100) * df_hot_v3$primary_expenditure_recalibration[i]) + 
        (df_hot_v3$nominal_gdp[i] * df_hot_v3$discrete_risk_primary_expenditure[i] / 100)
      # primary expenditure % of NGDP
      df_hot_v3$primary_expenditure_percent_gdp[i] = (
        df_hot_v3$primary_expenditure[i] / df_hot_v3$nominal_gdp[i] * 100
      )
      # primary balance value
      df_hot_v3$primary_balance[i] = df_hot_v3$revenue[i] - 
        df_hot_v3$primary_expenditure[i]
      # primary balance % of NGDP
      df_hot_v3$primary_balance_percent_gdp[i] = (
        df_hot_v3$primary_balance[i] / df_hot_v3$nominal_gdp[i] * 100
      )
      # Gross debt % of NGDP
      df_hot_v3$debt_to_gdp_ratio_percent[i] = (
        (df_hot_v3$debt_to_gdp_ratio_percent[i - 1] * 
           (1 + df_hot_v3$weighted_interest_rate[i]/100) / 
           (1 + df_hot_v3$nominal_gdp_growth_percent[i]/100)) - 
          (df_hot_v3$primary_balance_percent_gdp[i])
      )
      # Gross debt Level
      df_hot_v3$debt[i] = df_hot_v3$nominal_gdp[i] * 
        (df_hot_v3$debt_to_gdp_ratio_percent[i]/100)
      # Debt-stabilising primary balance % of NGDP
      df_hot_v3$dspb_percent_ngdp[i] = df_hot_v3$debt_to_gdp_ratio_percent[i - 1] * 
        ((df_hot_v3$weighted_interest_rate[i] - df_hot_v3$nominal_gdp_growth_percent[i])/100) / 
        (1 + df_hot_v3$nominal_gdp_growth_percent[i]/100)
      # interest expenditure value
      df_hot_v3$interest_expenditure[i] = df_hot_v3$debt[i - 1] * 
        (df_hot_v3$weighted_interest_rate[i]/100)
      # interest expenditure % of NGDP
      df_hot_v3$interest_expenditure_percent_gdp[i] = (
        (df_hot_v3$interest_expenditure[i]) / df_hot_v3$nominal_gdp[i] * 100
      )
      # Interest expenditure % of Revenue
      df_hot_v3$interest_expenditure_percent_revenue[i] = (
        (df_hot_v3$interest_expenditure[i] / df_hot_v3$revenue[i]) * 100
      )
      # total expenditure value
      df_hot_v3$total_expenditure[i] = (
        df_hot_v3$primary_expenditure[i] + df_hot_v3$interest_expenditure[i]
      )
      # total expenditure % of NGDP
      df_hot_v3$total_expenditure_percent_gdp[i] = (
        (df_hot_v3$total_expenditure[i]) / df_hot_v3$nominal_gdp[i] * 100
      )
      # overall balance value
      df_hot_v3$overall_balance[i] = (
        df_hot_v3$revenue[i] - df_hot_v3$total_expenditure[i]
      )
      # overall balance % of NGDP
      df_hot_v3$overall_balance_percent_gdp[i] = (
        (df_hot_v3$overall_balance[i]) / df_hot_v3$nominal_gdp[i] * 100
      )
    }
  }
  # return dataframe
  return(df_hot_v3)
}

# end: --------------------------------------------------------------------


