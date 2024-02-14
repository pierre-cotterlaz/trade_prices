# * Load Libraries -------------------------------------------------------

#library(renv)
library(here)
library(fst)
library(tidyverse)

source(here("R", "00a_define_functions.R"))

# * Define paths ----------------------------------------------------------

wd <- getwd()
if (grepl("D:", wd) == T) {
  location <- "pc"
}
if (grepl("dell5820108", wd) == TRUE) {
  location <- "baci8"
}
paths <- define_paths(location)
rm(wd)

# * Parameters ------------------------------------------------------------

versions <- list(
  HS = 1,
  baci_V = "202401",
  wtfc_V = "202104",
  trade_price_V = "202402")

first_year <- define_first_year(revision = versions$HS)
rm(define_first_year)

# * Run programs ----------------------------------------------------------

library(callr)
library(here)

source(here("R", "01_prepare_data.R"))
source(here("R", "02a_functions_compute_price_indices.R"))
