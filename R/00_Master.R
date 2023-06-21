# * Load Libraries -------------------------------------------------------

library(fst)
library(tidyverse)

# * Define paths ----------------------------------------------------------


wd <- getwd()
if (grepl("D:", wd) == T) {
  location <- "pc"
}

define_paths <- function(location) {
  if (location == "pc") {
    PC_p <- file.path("D:", "cotterlaz")
    nomenclatures_p <- file.path(PC_p, "BACI", "Nomenclatures")
    pc_baci_p <- file.path(PC_p, "BACI", "BACI")
  }
    return(list(
      PC_p = PC_p,
      nomenclatures_p = nomenclatures_p,
      pc_baci_p = pc_baci_p
    ))
}

paths <- define_paths(location)

# * Parameters ------------------------------------------------------------

versions <- list(
  HS = 5,
  baci_V = "202301",
  trade_price_V = "202306")


