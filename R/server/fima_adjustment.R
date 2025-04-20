
# start: ------------------------------------------------------------------
fima_adjustment <- function(by_country){
  readxl::read_excel(
    path = "data-raw/FIMA_APP.xlsx",
    sheet = "Adjustment"
  ) %>% 
    filter(country == by_country)
}

# end: --------------------------------------------------------------------
