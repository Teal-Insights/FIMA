
# start: ------------------------------------------------------------------
qcraft_data_climate <- function(){
  # data raw
  df_raw <- readxl::read_excel(
    path = "data-raw/qcraft-toolv10.xlsx",
    sheet = "Climate Database", skip = 24)
  
  init_scenario <- names(df_raw)[1]
  # data clean
  df_clean <- df_raw %>% 
    fill(all_of(names(.)[1]), .direction = "down") %>% 
    rename(
      "climate_scenario" = names(.)[1],
      "Country" = names(.)[2]
    ) %>% 
    mutate(
      climate_scenario = case_when(is.na(climate_scenario) ~ init_scenario,
                                        .default = climate_scenario)) %>% 
    pivot_longer(
      cols = -c("Country","climate_scenario"),
      names_to = "years",
      values_to = "values",
      values_transform = list(values = as.character)) %>% 
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
    relocate(climate_scenario, .after = country) %>% 
    filter(years > 2014)
  
  # data final
  df_final <- df_clean
  
  # data export
  readr::write_rds(x = df_final, file = "data/climate.rds", compress = "xz")
  # return dataframe
  return(df_final)
}

# end: --------------------------------------------------------------------

