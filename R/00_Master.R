# * Load Libraries -------------------------------------------------------

library(here)
library(fst)
library(tidyverse)

# * Define paths ----------------------------------------------------------

define_paths <- function(location) {
  if (location == "pc") {
    PC_p <- file.path("D:", "cotterlaz")
    nomenclatures_p <- file.path(PC_p, "BACI", "Nomenclatures")
    pc_baci_p <- file.path(PC_p, "BACI", "BACI")
    baci_p <- pc_baci_p
    wtfc_p <- file.path(PC_p, "BACI", "WTFC")
  }
  if (location == "baci8") {
    PC_p <- file.path("\\\\dell5820108", "cotterlaz$")
    nomenclatures_p <- file.path(PC_p, "BACI", "Nomenclatures")
    pc_baci_p <- file.path(PC_p, "BACI", "BACI")
    baci_p <- file.path("D:", "BACI", "BACI")
    wtfc_p <- file.path("D:", "BACI", "WTFC")
  }
    return(list(
      PC_p = PC_p,
      nomenclatures_p = nomenclatures_p,
      pc_baci_p = pc_baci_p,
      baci_p = baci_p,
      wtfc_p = wtfc_p
    ))
}

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
  baci_V = "202301",
  wtfc_V = "202005b",
  trade_price_V = "202306")

define_first_year <- function(revision){
  if (revision == 1){
    first_year <- 2000
  }
  return(first_year)
}
first_year <- define_first_year(revision = versions$HS)
rm(define_first_year)

# * Run programs ----------------------------------------------------------

library(callr)
library(here)

source(here("R", "01_prepare_data.R"))
source(here("R", "02a_functions_compute_price_indices.R"))
