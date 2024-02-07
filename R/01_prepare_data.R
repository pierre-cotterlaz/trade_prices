
library(tidyverse)

# Concordance tables ------------------------------------------------------

k_varname <- sym(paste0("HS", versions$HS))
k_varname_str <- paste0("HS", versions$HS)

# HS - stade

hs_stade_df <- 
  here("data", "nomenclatures", 
       paste0("HS", versions$HS, "-stade--share.csv")) |>
  read_csv(show_col_types = FALSE) |>
  as_tibble()

# HS - ISIC
hs_isic_for_prices <- 
  here("data", "nomenclatures", 
       paste0("HS", versions$HS, "-our_ISIC--share.csv")) |>
  read_csv(show_col_types = FALSE) |>
  as_tibble()

filen <- paste0("nace_2d--nace_2d_name.csv")
file <- file.path(paths$nomenclatures_p, "NACE", filen)
isic_2d_dict <- 
  read_csv(file, show_col_types = FALSE) |>
  as_tibble() |>
  mutate(code = as.character(code)) |>
  mutate(manuf = as.numeric(str_detect(description, "Manufacture of "))) |>
  mutate(description = str_remove(description, "Manufacture of ")) |>
  mutate(description = case_when(
    code == "16" ~ "Wood",
    code == "18" ~ "Recorded media",
    code == "21" ~ "Pharmacy",
    code == "25" ~ "Fabricated metal products",
    code == "29" ~ "Motor vehicles & trailers",
    code == "62" ~ "Computer programming, consultancy",
    T ~ description
  )) |>
  add_row( 
    code = "XX", description =  "Other manuf") |>
  add_row( 
    code = "NEC", description =  "NEC") |>
  rename(isic_2d = code, 
         isic_2d_name = description)
  
rm(nb_obs_per_isic_2d, distribution_nb_isic_per_hs)
