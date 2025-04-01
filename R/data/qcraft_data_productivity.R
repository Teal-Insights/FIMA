
# start: ------------------------------------------------------------------
qcraft_data_productivity <- function(){
  # data raw
  df_raw <- readxl::read_excel(
    path = "data-raw/qcraft-toolv10.xlsx",
    sheet = "Productivity", skip = 61)
  
  year_pattern <- "^\\d{4}\\s*-\\s*"
  keep_cols <- names(df_raw)[!grepl(year_pattern, names(df_raw))]
  
  # data clean
  df_clean <- df_raw %>% 
    select(all_of(keep_cols)) %>% 
    rename("Country" = "Country Name") %>% 
    pivot_longer(
      cols = -c("Country"),
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
    relocate(iso3c, .before = country)
  
  # data final
  df_final <- df_clean
  
  # data export
  readr::write_rds(x = df_final, file = "data/productivity.rds", compress = "xz")
  # return dataframe
  return(df_final)
}

# end: --------------------------------------------------------------------

