
# start: ------------------------------------------------------------------
# load necessary libraries
suppressPackageStartupMessages({
  library(tidyverse)
})
# turn off warnins
options(warn = -1)

# load necessary scripts
source(file = "R/data/qcraft_data_macrofiscal.R")

# data: -------------------------------------------------------------------
# macrofiscal
bot_macrofiscal <- qcraft_data_macrofiscal()

# end: --------------------------------------------------------------------


