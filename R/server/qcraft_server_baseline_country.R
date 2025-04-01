
# start: ------------------------------------------------------------------
baseline_v1 <- function(data_inflation,data_demography,by_iso3c,by_level){
  weo_max_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% max()
  
  productivity_max_year <- readr::read_rds(file = "data/productivity.rds") %>% 
    pull(years) %>% max()
  # inflation
  df_inflation <- data_inflation %>% 
    filter(years>=2009)
  # demography
  df_demography <- readr::read_rds(file = "data/demography.rds") %>% 
    filter(
      iso3c == by_iso3c,
      status == by_level,
      years >= 2009,
      age_group %in% c("15-64","Total")) %>% 
    mutate(
      age_group = case_when(
        age_group == "15-64" ~ "working_age_population", 
        age_group == "Total" ~ "total_population",
        .default = age_group)) %>% 
    pivot_wider(names_from = age_group, values_from = values) %>% 
    select(-c(status))
  
  df_population_growth <- data_demography %>% 
    select(iso3c, country, years, population_growth = demography_growth_total) %>% 
    filter(years>=2009)
  
  df_productivity <- qcraft_server_productivity_country() %>% 
    filter(years >= 2009) %>% 
    select(years, labour_productivity_growth = productivity_growth_rate_perecent)
  
  df_complete_demography <- left_join(
    x = df_demography, 
    y = df_population_growth,
    by = c("iso3c","country","years"))
  # macrofiscal
  df_macrofiscal <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    filter(iso3c == by_iso3c, years >= 2009) 
  
  df_growth_percent <- df_macrofiscal %>% 
    select(iso3c, country, years,real_gdp,nominal_gdp,gdp_deflator,
           contains("_growth_percent"))
  
  df_growth_productivity <- left_join(
    df_productivity,df_growth_percent,by = "years") %>% 
    relocate(c(iso3c, country), .before = years) %>% 
    fill(c(iso3c,country), .direction = "down") %>% 
    mutate(
      employment_growth = case_when(
        years < productivity_max_year ~ ((real_gdp_growth_percent/100) - (labour_productivity_growth/100))/
          (1+labour_productivity_growth/100)*100,
        .default = NA
      )
    ) %>% 
    left_join(
      x = .,
      y = df_complete_demography,
      by = c("iso3c","country","years")
    ) %>% 
    mutate(
      employment_growth = case_when(
        is.na(employment_growth) ~ (working_age_population/lag(working_age_population))*100 - 100,
        .default = employment_growth
      )
    ) %>% 
    mutate(
      labour_productivity_growth = case_when(
        between(x = years, left = weo_max_year - 6, right = weo_max_year) ~ 
          ((real_gdp_growth_percent/100) - (employment_growth/100))/
          (1+employment_growth/100)*100,
        .default = labour_productivity_growth
      )
    )
  
  # Compute real_gdp recursively
  for (i in seq_len(nrow(df_growth_productivity))) {
    if (df_growth_productivity$years[i] > weo_max_year) {
      df_growth_productivity$real_gdp[i] <- df_growth_productivity$real_gdp[i - 1] * 
        (1 + df_growth_productivity$employment_growth[i] / 100) * 
        (1 + df_growth_productivity$labour_productivity_growth[i] / 100)
    }
  }
  # compute real gdp growth beyond weo projections
  df_baseline_v1 <- df_growth_productivity %>% 
    mutate(
      real_gdp_growth_percent = case_when(
        is.na(real_gdp_growth_percent) ~ (real_gdp/lag(real_gdp))*100 - 100,
        .default = real_gdp_growth_percent
      )
    ) %>% 
    mutate(gdp_deflator_growth_percent = df_inflation$inflation)
  # Compute nominal_gdp recursively
  for (i in seq_len(nrow(df_baseline_v1))) {
    if (df_baseline_v1$years[i] > weo_max_year) {
      # nominal GDP value
      df_baseline_v1$nominal_gdp[i] <- df_baseline_v1$nominal_gdp[i - 1] * 
        (1 + df_baseline_v1$real_gdp_growth_percent[i] / 100) * 
        (1 + df_baseline_v1$gdp_deflator_growth_percent[i] / 100)
      # nominal GDP growth
      df_baseline_v1$nominal_gdp_growth_percent[i] = (
        df_baseline_v1$nominal_gdp[i] / df_baseline_v1$nominal_gdp[i-1]*100 - 100
      )
      # gdp deflator
      df_baseline_v1$gdp_deflator[i] <- df_baseline_v1$gdp_deflator[i - 1] * 
        (1 + df_baseline_v1$gdp_deflator_growth_percent[i+1] / 100)
    }
  }
  # return dataframe
  return(df_baseline_v1)
}

# qcraft_server_baseline_country ------------------------------------------

qcraft_server_baseline_country <- function(
    data_baseline,data_interest,data_macrofiscal,debt_target,fiscal_rule,by_iso3c){
  # weighted interest rate
  df_baseline_v1 <- data_baseline
  df_final_interest_rate <- data_interest
  df_macrofiscal <- data_macrofiscal %>% filter(iso3c == by_iso3c)
  weo_max_year <- readr::read_rds(file = "data/macrofiscal.rds") %>% 
    pull(years) %>% max()
  
  df_baseline_v2 <- left_join(
    x = df_baseline_v1, 
    y = df_final_interest_rate %>% 
      select(years, weighted_interest_rate = nominal_interest_rate) %>% 
      filter(years >= 2009), 
    by="years")
  # macroeconomics
  df_me_baseline_v1 <- df_macrofiscal %>% 
    select(
      years,
      debt,
      debt_to_gdp_ratio_percent,
      interest_expenditure_percent_revenue,
      revenue,
      revenue_percent_gdp,
      total_expenditure = expenditure,
      interest_expenditure,
      primary_expenditure,
      primary_expenditure_percent_gdp,
      primary_balance,
      primary_balance_percent_gdp,
      overall_balance,
      overall_balance_percent_gdp
    )
  
  df_baseline_v3 <- left_join(
    x = df_baseline_v2,y = df_me_baseline_v1, by = "years"
  )
  # compute revenue and revenue % of NGDP
  for (i in seq_len(nrow(df_baseline_v3))) {
    if (df_baseline_v3$years[i] > weo_max_year) {
      # revenue value
      df_baseline_v3$revenue[i] <- df_baseline_v3$revenue[i - 1] * 
        (1 + df_baseline_v3$nominal_gdp_growth_percent[i] / 100)
      # revenue % of nominal GDP
      df_baseline_v3$revenue_percent_gdp[i] = (
        df_baseline_v3$revenue[i] / df_baseline_v3$nominal_gdp[i] * 100
      )
    }
  }
  
  # compute debt stabilising primary balance % of NGDP
  df_baseline_v3$dspb_percent_ngdp <- NA
  
  for (i in 2:nrow(df_baseline_v3)) {
    if (df_baseline_v3$years[i] <= weo_max_year) {
      # debt stabilising primary balance % of NGDP
      df_baseline_v3$dspb_percent_ngdp[i] <- df_baseline_v3$debt_to_gdp_ratio_percent[i - 1] * 
        ((
          df_baseline_v3$weighted_interest_rate[i] -
            df_baseline_v3$nominal_gdp_growth_percent[i]
        ) / 100
        ) / 
        (1 + df_baseline_v3$nominal_gdp_growth_percent[i]/100)
    }
  }
  
  df_baseline_v4 <- df_baseline_v3 %>% 
    mutate(
      fiscal_gap = ((primary_balance_percent_gdp - dspb_percent_ngdp)/100) * nominal_gdp,
      total_expenditure_percent_gdp = (total_expenditure / nominal_gdp) * 100,
      interest_expenditure_percent_gdp = (interest_expenditure / nominal_gdp) * 100,
      debt_target = debt_target
    ) %>% 
    mutate(
      debt_trajectory_value_one = case_when(
        debt_target == 0 ~ 0,
        debt_to_gdp_ratio_percent <= debt_target ~ 0,
        .default = fiscal_gap
      ),
      debt_trajectory_value_two = case_when(
        debt_target == 0 ~ 0,
        debt_to_gdp_ratio_percent >= debt_target ~ 0,
        .default = fiscal_gap
      ),
      debt_trajectory_class = case_when(
        debt_to_gdp_ratio_percent > lag(debt_to_gdp_ratio_percent) ~ 1,
        debt_to_gdp_ratio_percent < lag(debt_to_gdp_ratio_percent) ~ 2,
        .default = NA
      ),
      fiscal_rule_value = case_when(
        fiscal_rule == "No" ~ 0,
        debt_trajectory_class == 1 ~ debt_trajectory_value_one,
        debt_trajectory_class == 2 ~ debt_trajectory_value_two,
        .default = NA
      )
    )
  # recursive baseline computation
  for (i in seq_len(nrow(df_baseline_v4))) {
    if (df_baseline_v4$years[i] > weo_max_year) {
      # compute primary expenditure
      df_baseline_v4$primary_expenditure[i] <- df_baseline_v4$primary_expenditure[i - 1] * 
        (1 + df_baseline_v4$labour_productivity_growth[i]/100) * 
        (1 + df_baseline_v4$gdp_deflator_growth_percent[i]/100) * 
        (1 + df_baseline_v4$population_growth[i]/100) + 
        df_baseline_v4$fiscal_rule_value[i - 1]
      # compute primary expenditure % of NGDP
      df_baseline_v4$primary_expenditure_percent_gdp[i] <- df_baseline_v4$primary_expenditure[i] / 
        df_baseline_v4$nominal_gdp[i] * 100
      # compute primary balance value
      df_baseline_v4$primary_balance[i] <- df_baseline_v4$revenue[i] - 
        df_baseline_v4$primary_expenditure[i]
      # compute primary balance % of NGDP
      df_baseline_v4$primary_balance_percent_gdp[i] <- df_baseline_v4$primary_balance[i] / 
        df_baseline_v4$nominal_gdp[i] * 100
      # compute Gross debt % of NGDP
      df_baseline_v4$debt_to_gdp_ratio_percent[i] <- if_else(
        condition = (
          ((df_baseline_v4$debt_to_gdp_ratio_percent[i - 1]) * 
             (1 + df_baseline_v4$weighted_interest_rate[i]) / 
             (1 + df_baseline_v4$nominal_gdp_growth_percent[i]) - 
             (df_baseline_v4$primary_balance_percent_gdp[i])) < 0
        ),
        true = 0,
        false = (df_baseline_v4$debt_to_gdp_ratio_percent[i - 1]) * 
          (1 + df_baseline_v4$weighted_interest_rate[i]/100) / 
          (1 + df_baseline_v4$nominal_gdp_growth_percent[i]/100) - 
          (df_baseline_v4$primary_balance_percent_gdp[i])
      )
      # compute Gross debt value
      df_baseline_v4$debt[i] <- df_baseline_v4$debt_to_gdp_ratio_percent[i]/100 * 
        df_baseline_v4$nominal_gdp[i]
      # compute interest expenditure
      df_baseline_v4$interest_expenditure[i] <- df_baseline_v4$debt[i - 1] * 
        (df_baseline_v4$weighted_interest_rate[i]/100)
      # compute interest expenditure % of revenue
      df_baseline_v4$interest_expenditure_percent_revenue[i] <- df_baseline_v4$interest_expenditure[i] / 
        df_baseline_v4$revenue[i]*100
      # compute interest expenditure % of NGDP
      df_baseline_v4$interest_expenditure_percent_gdp[i] <- df_baseline_v4$interest_expenditure[i] / 
        df_baseline_v4$nominal_gdp[i]*100
      # compute total expenditure
      df_baseline_v4$total_expenditure[i] <- df_baseline_v4$interest_expenditure[i] + 
        df_baseline_v4$primary_expenditure[i]
      # compute total expenditure % of NGDP
      df_baseline_v4$total_expenditure_percent_gdp[i] <- df_baseline_v4$total_expenditure[i] / 
        df_baseline_v4$nominal_gdp[i] * 100
      # compute overall balance
      df_baseline_v4$overall_balance[i] <- df_baseline_v4$revenue[i] - 
        df_baseline_v4$total_expenditure[i]
      # compute overall balance
      df_baseline_v4$overall_balance_percent_gdp[i] <- df_baseline_v4$overall_balance[i] / 
        df_baseline_v4$nominal_gdp[i] * 100
      # compute Debt-stabilising primary balance % of NGDP
      df_baseline_v4$dspb_percent_ngdp[i] <- df_baseline_v4$debt_to_gdp_ratio_percent[i - 1] * 
        ((df_baseline_v4$weighted_interest_rate[i] - df_baseline_v4$nominal_gdp_growth_percent[i])/100) / 
        (1 + df_baseline_v4$nominal_gdp_growth_percent[i]/100)
      # compute Fiscal gap
      df_baseline_v4$fiscal_gap[i] <- (
        df_baseline_v4$primary_balance_percent_gdp[i] - df_baseline_v4$dspb_percent_ngdp[i])/100 * 
        df_baseline_v4$nominal_gdp[i]
      # compute debt trajectories
      df_baseline_v4$debt_trajectory_value_one[i] = case_when(
        df_baseline_v4$debt_target[i] == 0 ~ 0,
        df_baseline_v4$debt_to_gdp_ratio_percent[i] <= df_baseline_v4$debt_target[i] ~ 0,
        .default = df_baseline_v4$fiscal_gap[i]
      )
      
      df_baseline_v4$debt_trajectory_value_two[i] = case_when(
        df_baseline_v4$debt_target[i] == 0 ~ 0,
        df_baseline_v4$debt_to_gdp_ratio_percent[i] >= df_baseline_v4$debt_target[i] ~ 0,
        .default = df_baseline_v4$fiscal_gap[i]
      )
      
      df_baseline_v4$debt_trajectory_class[i] = case_when(
        df_baseline_v4$debt_to_gdp_ratio_percent[i] > df_baseline_v4$debt_to_gdp_ratio_percent[i-1] ~ 1,
        df_baseline_v4$debt_to_gdp_ratio_percent[i] < df_baseline_v4$debt_to_gdp_ratio_percent[i-1] ~ 2,
        .default = NA
      )
      # compute fiscal rule
      df_baseline_v4$fiscal_rule_value[i] = case_when(
        fiscal_rule[i] == "No" ~ 0,
        df_baseline_v4$debt_trajectory_class[i] == 1 ~ df_baseline_v4$debt_trajectory_value_one[i],
        df_baseline_v4$debt_trajectory_class[i] == 2 ~ df_baseline_v4$debt_trajectory_value_two[i],
        .default = NA
      )
    }
  }
  
  # final data
  df_baseline_final <- df_baseline_v4 %>% 
    rename(
      "productivity_growth" = "labour_productivity_growth", 
      "inflation" = "gdp_deflator_growth_percent",
      "revenue_percent_ngdp" = "revenue_percent_gdp",
      "interest_rate_percent" = "weighted_interest_rate")
  # return dataframe
  return(df_baseline_final)
}

# ends --------------------------------------------------------------------
