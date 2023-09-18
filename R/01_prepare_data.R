
library(tidyverse)

# Read BACI ---------------------------------------------------------------

filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$baci_p, "Data", versions$baci_V, filen)
message(file.info(file)$mtime) 
baci_df <- 
  read_fst(file) |>
  mutate(uv = v / q)

list_k_in_baci <- 
  baci_df |>
  distinct(k)

# Concordance tables ------------------------------------------------------

k_varname <- sym(paste0("HS", versions$HS))
k_varname_str <- paste0("HS", versions$HS)

# HS - stade
filen <- paste0("HS", versions$HS, "-stade--share.csv")
file <- file.path(paths$nomenclatures_p, "conversions", filen)
hs_stade_df <- 
  read_csv(file) |> 
  mutate(k = as.numeric(!!k_varname)) |>
  select(k, stade, share) |>
  full_join(list_k_in_baci, by = "k") |> 
  mutate(stade = case_when(
    .default = stade, 
    is.na(stade) ~ "6_NEC"
  )) |>
  replace_na(list(share = 1)) |>
  arrange(k)
rm(list_k_in_baci)

filen <- paste0("HS", versions$HS, "-our_ISIC--share.csv")
file <- here("data", "nomenclatures", filen)
hs_isic_for_prices <- 
  read_csv(file) |>
  as_tibble()

filen <- paste0("nace_2d--nace_2d_name.csv")
file <- file.path(paths$nomenclatures_p, "NACE", filen)
isic_2d_dict <- 
  read_csv(file) |>
  as_tibble() |>
  mutate(code = as.character(code)) %>%
  mutate(manuf = as.numeric(str_detect(description, "Manufacture of "))) %>%
  mutate(description = str_remove(description, "Manufacture of ")) %>%
  mutate(description = case_when(
    code == "16" ~ "Wood",
    code == "18" ~ "Recorded media",
    code == "21" ~ "Pharmarcy",
    code == "25" ~ "Fabricated metal products",
    code == "29" ~ "Motor vehicles & trailers",
    code == "62" ~ "Computer programming, consultancy",
    T ~ description
  )) %>%
  add_row( 
    code = "XX", description =  "Other manuf") |>
  add_row( 
    code = "NED", description =  "NED") |>
  rename(isic_2d = code, 
         isic_2d_name = description)
  
rm(nb_obs_per_isic_2d, distribution_nb_isic_per_hs)
