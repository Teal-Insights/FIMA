
# start: ------------------------------------------------------------------
qcraft_data_macrofiscal <- function(){
  # data raw
  df_raw <- readxl::read_excel(
    path = "data-raw/qcraft-toolv10.xlsx",
    sheet = "Macrofiscal", skip = 65)
  
  # data clean
  df_clean <- df_raw %>% 
    fill(c(`Subject Descriptor`, Scale), .direction = "down") %>% 
    filter(Country != "Country") %>% 
    select(-c(`Estimates Start After`, `Country/Series-specific Notes`)) %>% 
    pivot_longer(
      cols = -c("Country","Subject Descriptor","Scale" ),
      names_to = "years",
      values_to = "values") %>% 
    mutate(values = case_when(values == "n/a" ~ NA, .default = values)) %>% 
    mutate(
      values = as.numeric(values),
      years = as.integer(years)) %>% 
    mutate(
      iso3c = countrycode::countrycode(
        sourcevar = Country,
        origin = "country.name",
        destination = "iso3c")) %>% 
    janitor::clean_names() %>% 
    relocate(iso3c, .before = country) %>% 
    filter(years > 2000,iso3c == "CIV")
  
  # data final
  df_final <- df_clean %>% 
    mutate(
      subject_descriptor = case_when(
        subject_descriptor == "Gross domestic product, constant prices" ~ "real_gdp",
        subject_descriptor == "Gross domestic product, current prices" ~ "nominal_gdp",
        subject_descriptor == "Gross domestic product, deflator" ~ "gdp_deflator",
        subject_descriptor == "General government revenue" ~ "revenue",
        subject_descriptor == "General government total expenditure" ~ "expenditure",       
        subject_descriptor == "General government net lending/borrowing" ~ "overall_balance",
        subject_descriptor == "General government primary net lending/borrowing" ~ "primary_balance",
        subject_descriptor == "General government gross debt" ~ "debt",
        .default = subject_descriptor
      )
    ) %>% 
    select(-c(scale)) %>% 
    spread(key = subject_descriptor, value = values) %>% 
    mutate(
      interest_expenditure = (primary_balance - overall_balance),
      primary_expenditure = (expenditure - interest_expenditure),
      real_gdp_growth_percent = (real_gdp/lag(real_gdp)*100-100),
      nominal_gdp_growth_percent = (nominal_gdp/lag(nominal_gdp)*100-100),
      gdp_deflator_growth_percent = (gdp_deflator/lag(gdp_deflator)*100-100),
      revenue_percent_gdp = (revenue/nominal_gdp*100),
      primary_expenditure_percent_gdp = (primary_expenditure/nominal_gdp*100),
      interest_rate_percent = (interest_expenditure/debt*100),
      debt_to_gdp_ratio_percent = (debt/nominal_gdp*100),
      overall_balance_percent_gdp = (overall_balance/nominal_gdp*100),
      primary_balance_percent_gdp = (primary_balance/nominal_gdp*100),
      interest_expenditure_percent_revenue = (interest_expenditure/revenue*100),
      interest_growth_differential_percentage_point = (interest_rate_percent/100-nominal_gdp_growth_percent/100)/(1+nominal_gdp_growth_percent/100)*100
    )
  
  # data export
  readr::write_rds(x = df_final, file = "data/macrofiscal.rds", compress = "xz")
  # return dataframe
  return(df_final)
}

# end: --------------------------------------------------------------------

