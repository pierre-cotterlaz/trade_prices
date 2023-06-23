filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
message(file.info(file)$mtime) 
baci_df <- 
  read_fst(file) |>
  mutate(uv = v/q)

# HS - ISIC_2d
filen <- paste0("HS", versions$HS, "-CPA2_1--share.csv")
file <- file.path(paths$nomenclatures_p, "conversions", filen)
hs_isic_df <- 
  read_csv(file) |>
  as_tibble() |>
  mutate(isic_2d = substr(CPA2_1, 0, 2)) |>
  distinct(HS5, isic_2d) |>
  group_by(HS5) |>
  mutate(share = 1 / n()) |>
  ungroup() |>
  mutate(k = as.numeric(HS5)) |>
  select(k, isic_2d, share)
  
distribution_nb_isic_per_hs <- 
  hs_isic_df |> 
  mutate(nb_isic = 1 / share) |>
  summarize(.by = nb_isic, 
            nb_hs = n_distinct(k))

# HS - stade
filen <- paste0("HS", versions$HS, "-stade--share.csv")
file <- file.path(paths$nomenclatures_p, "conversions", filen)
hs_stade_df <- 
  read_csv(file) |> 
  mutate(k = as.numeric(HS5)) |>
  select(k, stade, share)
  
# BACI aggregated in ISIC_2d, useful to know the number of trade flows 
baci_in_isic_2d <- 
  baci_df |>
  left_join(hs_isic_df, by = c("k")) |>
  mutate(v = v * share) |>
  summarize(.by = c(t, i, j, isic_2d),
            nb_flows = n_distinct(k),
            v = sum(v)) 
filen <- paste0("t-i-j-isic_2d--BACI--V", versions$baci_V, ".fst")
file <- here("data", filen)
write_fst(baci_in_isic_2d, file, compress = 100)

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
  
nb_obs_per_isic_2d <-
  baci_in_isic_2d |>
  summarize(.by = isic_2d, 
            nb_flows = sum(nb_flows),
            v = sum(v)) |>
  mutate(across(
    c(nb_flows, v),
    ~ .x / sum(.x) * 100,
    .names = "sh_{.col}")) |>
  arrange(-nb_flows) |>
  left_join(isic_2d_dict, by = "isic_2d")

filen <- paste0("t-i-j-isic_2d--BACI--V", versions$baci_V, ".fst")
file <- here("data", filen)
baci_in_isic_2d <- read_fst(file)

isic__isic_for_prices <- 
  baci_in_isic_2d |>
  summarize(.by = isic_2d, 
            nb_flows = sum(nb_flows),
            v = sum(v)) |>
  mutate(across(
    c(nb_flows, v),
    ~ .x / sum(.x) * 100,
    .names = "sh_{.col}")) |>
  arrange(- sh_nb_flows) |>
  mutate(isic_2d_aggregated = case_when(  
    sh_nb_flows < 1 ~ "NED",
    .default = isic_2d)) |>
  select(isic_2d, isic_2d_aggregated)
  