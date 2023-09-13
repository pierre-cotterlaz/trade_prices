filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
message(file.info(file)$mtime) 
baci_df <- 
  read_fst(file) 
# |>
  # mutate(uv = v/q)

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

#VU au niveau HS 4d 
`tijk_4d--uv` <- 
  baci_df |>
  mutate(uv = v / q) |>
  select(t, i, j, k, v, uv) |>
  mutate(k = str_pad(k, 6, side = "left", pad = "0")) |>
  mutate(k_4d = substr(k, 1, 4)) |>
  filter(!is.na(v) & !is.na(uv)) |>
  summarize(
    .by = c(t, i, j, k_4d), 
    uv_hs4d = weighted.mean(uv, v, na.rm = TRUE)) 
filen <- paste0("t-i-j-k_4d--uv--V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
write_fst(`tijk_4d--uv`, file, compress = 100)

baci_with_infered_uv <-
  baci_df |>
  mutate(uv = v / q) |>
  mutate(k = str_pad(k, 6, side = "left", pad = "0")) |>
  mutate(k_4d = substr(k, 1, 4)) |>
  left_join(`tijk_4d--uv`, by = c("t", "i", "j", "k_4d")) |>
  arrange(t, i, j, k) |>
  mutate(infered_uv_fl = case_when(
    is.na(uv) & !is.na(uv_hs4d) ~ TRUE,
    .default = FALSE)) |>
  mutate(uv = case_when(
    is.na(uv) & !is.na(uv_hs4d) ~ uv_hs4d,
    .default = uv)) |>
  #select(-uv_hs4d) |>
  filter(!is.na(uv)) |>
  select(t, i, j, k, uv, v, infered_uv_fl)
filen <- paste0("t-i-j-k--uv--infered_from_k_4d--V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
write_fst(baci_with_infered_uv, file, compress = 100)

# This file is used to compute total trade values
# It is "raw" as opposed to the "filtered" df used to compute prices
filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
raw_baci_with_group_variables  <- 
  read_fst(file)  |>
  left_join(hs_isic_df, by = "k") |>
  # NB because HS codes may be associated with several ISIC codes
  # the dataset will have more rows than defined by t-i-j-k but 
  # the total trade flow will remain correct
  mutate(v = v * share) |>
  select(-share) |>
  left_join(isic__isic_for_prices, by = "isic_2d") |> 
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share) |>
  select(-share) |>
  mutate(
    t_k = paste(t, k),
    t_isic = paste(t, isic_2d_aggregated),
    t_stade = paste(t, stade),
    t_isic_stade = paste(t, isic_2d_aggregated, stade)
  ) 
filen <- paste0("t-i-j-k--BACI_with_group_variables--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
write_fst(raw_baci_with_group_variables, file, compress = 100)


# delta_ln_uv computed at much aggregated levels 
filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
baci_df <- 
  read_fst(file) |>
  mutate(uv = v / q)

lagged_baci_df <- 
  baci_df |>
  mutate(t = t + 1) |>
  select(t, i, j, k, v, uv) |>
  rename(l_v = v, l_uv = uv)

delta_ln_uv_df <- 
  baci_df |>
  left_join(lagged_baci_df, by = c("t", "i", "j", "k")) |> 
  mutate(delta_ln_uv = log(uv) - log(l_uv)) |>
  mutate(k = str_pad(k, 6, side = "left", pad = "0")) |>
  mutate(k_4d = substr(k, 1, 4)) |>
  select(t, i, j, k, delta_ln_uv, k_4d)

aggregated_delta_ln_uv_df <- 
  delta_ln_uv_df |>
  # sample_n(1E5) |>
  # mutate(k = str_pad(k, 6, side = "left", pad = "0")) |>
  # mutate(k_4d = substr(k, 1, 4)) |>
  group_by(t, i, j, k_4d) |>
  mutate(d_ln_uv__t_i_j_k_4d = median(delta_ln_uv, na.rm = TRUE)) |>
  ungroup() |>
  # Compute median change in delta_ln_uv at a more aggregated level
  group_by(t, i, j, k_4d) |>
  mutate(d_ln_uv__t_i_j_k_4d = median(delta_ln_uv, na.rm = TRUE)) |>
  ungroup() |>
  group_by(t, i, k_4d) |>
  mutate(d_ln_uv__t_i_k_4d = median(delta_ln_uv, na.rm = TRUE)) |>
  ungroup() |>
  group_by(t, k_4d) |>
  mutate(d_ln_uv__t_k_4d = median(delta_ln_uv, na.rm = TRUE)) |>
  ungroup() 
filen <- paste0("t-i-j-k--aggregated_delta_ln_uv--V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
write_fst(aggregated_delta_ln_uv_df, file, compress = 100)
