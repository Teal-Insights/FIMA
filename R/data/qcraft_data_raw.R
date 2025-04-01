
# start: ------------------------------------------------------------------
qcraft_data_raw <- function(){
  
}

# end: --------------------------------------------------------------------

url <- "https://www.imf.org/-/media/Files/Topics/Fiscal/Fiscal-Risks/Tool/qcraft-toolv10.ashx"
destfile <- "qcraft_toolv10.xlsx"
curl::curl_download(url, destfile)
qcraft_toolv10 <- readxl::read_excel(destfile)
