
# start: ------------------------------------------------------------------
qcraft_data_demography <- function(){
  # data raw
  df_raw <- readxl::read_excel(
    path = "data-raw/qcraft-toolv10.xlsx",
    sheet = "Demography", skip = 115)
  
  df <- df_raw %>% 
    rename("status" = `MATERIAL BELOW SHOULD BE HIDDEN`) %>% 
    mutate(age_group = case_when(
      !is.na(status) ~ .data[[names(.)[2]]], # This gets the second column
      .default = NA
    )) %>% 
    relocate(age_group, .after = status) %>% 
    fill(c(status, age_group), .direction = "down") %>% 
    filter(.data[[names(.)[2]]] != .data[[names(.)[3]]]) %>% 
    {
      # Capture the dataframe at this point
      temp_df <- .
      
      # Get original column names and first row values
      original_names <- names(temp_df)
      first_row_values <- as.character(temp_df[1, ])
      
      # Create new column names
      new_names <- c(
        original_names[1:2], 
        first_row_values[3:length(first_row_values)]
      )
      
      # Rename columns
      colnames(temp_df) <- new_names
      
      # Return modified dataframe
      temp_df
    } %>%
    # Remove the first row
    slice(-1) %>%
    # Reset row indices
    {
      rownames(.) <- NULL
      .
    } %>% 
    filter(Country != "Country")
  
  # data clean
  df_clean <- df %>% 
    pivot_longer(
      cols = -c("status", "age_group", "Country"),
      names_to = "years",
      values_to = "values",
      values_transform = list(values = as.character)) %>% 
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
    relocate(c(status,age_group), .after = country) %>% 
    filter(!is.na(years))
  
  # data final
  df_final <- df_clean
  
  # data export
  readr::write_rds(x = df_final, file = "data/demography.rds", compress = "xz")
  # return dataframe
  return(df_final)
}

# end: --------------------------------------------------------------------

