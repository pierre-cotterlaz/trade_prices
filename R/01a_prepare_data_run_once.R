# filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
# file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
# message(file.info(file)$mtime) 
# baci_df <- 
#   read_fst(file) 
# 
# filen <- paste0("t-i-j-k--WTFC--HS1-DM201901.Rds")
# file <- file.path(paths$wtfc_p, "Data", versions$wtfc_V, filen)
# message(file.info(file)$mtime) 
# wtfc_df <- 
#   readRDS(file)
# df <- 
#   wtfc_df |>
#   select(t, i, j, k, uv) |>
#   mutate(k = as.numeric(k))
# filen <- paste0("t-i-j-k--v-uv--HS", versions$HS, "-V201901.fst")
# file <- file.path(paths$wtfc_p, "Data", "201901", filen)
# write_fst(df, file, compress = 100)
# 
# filen <- paste0("t-i-j-k--WTFC--HS1-V202005b.Rds")
# file <- file.path(paths$wtfc_p, "Data", "202005b", filen)
# message(file.info(file)$mtime) 
# wtfc_df <- 
#   readRDS(file) |>
#   select(t, i, j, k, v, uv) |>
#   mutate(k = as.numeric(k))
# filen <- paste0("t-i-j-k--v-uv--HS", versions$HS, "-V202005b.fst")
# file <- file.path(paths$wtfc_p, "Data", "202005b", filen)
# write_fst(wtfc_df, file, compress = 100)
# 
# filen <- paste0("t-i-j-k--WTFC--HS", versions$HS, "-V202104.fst")
# file <- file.path(paths$wtfc_p, "Data", "202104", filen)
# wtfc_df <- 
#   read_fst(file) |>
#   select(t, i, j, k, v, uv) |>
#   mutate(k = as.numeric(k))
# filen <- paste0("t-i-j-k--v-uv--HS", versions$HS, "-V202104.fst")
# file <- file.path(paths$wtfc_p, "Data", "202104", filen)
# write_fst(wtfc_df, file, compress = 100)


#  nomenclatures --------------------------------------------------------

baci_df <- 
  file.path(paths$baci_p, "Data", versions$baci_V, 
            paste0("t-i-j-k--BACI--HS", versions$HS, 
                   "-V", versions$baci_V, ".fst")) |>
  read_fst() |>
  mutate(uv = v / q)

list_k_in_baci <- 
  baci_df |>
  distinct(k)

# Concordance tables ------------------------------------------------------

k_varname <- sym(paste0("HS", versions$HS))
k_varname_str <- paste0("HS", versions$HS)

# HS - stade

hs_stade_df <-
  file.path(
    paths$nomenclatures_p, "conversions", 
    paste0("HS", versions$HS, "-stade--share.csv")) |>
  read_csv() |> 
  mutate(k = as.numeric(!!k_varname)) |>
  select(k, stade, share) |>
  full_join(list_k_in_baci, by = "k") |> 
  mutate(stade = case_when(
    .default = stade, 
    is.na(stade) ~ "6_NEC"
  )) |>
  replace_na(list(share = 1)) |>
  arrange(k)
filen <- paste0("HS", versions$HS, "-stade--share.csv")
file <- here("data", "nomenclatures", filen)
write_csv(hs_stade_df, file)
rm(list_k_in_baci)



# HS - ISIC_2d

hs_isic_df <-
  file.path(
    paths$nomenclatures_p, "conversions", 
    paste0("HS", versions$HS, "-CPA2_1--share.csv")) |>
  read_csv() |>
  as_tibble() |>
  mutate(isic_2d = substr(CPA2_1, 0, 2)) |>
  distinct(!!k_varname, isic_2d) |>
  group_by(!!k_varname) |>
  mutate(share = 1 / n()) |>
  ungroup() |>
  mutate(manuf = (between(isic_2d, "10", "33")) |>
           as.numeric()) |>
  mutate(k = as.numeric(!!k_varname)) |>
  select(k, isic_2d, share, manuf)

# HS - ISIC for prices

isic__isic_for_prices <- 
  here("data", "nomenclatures", "ISIC_2d-our_ISIC.csv") |>
  read_csv(show_col_type = FALSE) |>
  as_tibble()
hs_isic_for_prices <- 
  hs_isic_df |>
  left_join(isic__isic_for_prices, by = "isic_2d") |> 
  select(k, isic_2d_aggregated, share, manuf) 
write_csv(
  hs_isic_for_prices, 
  here("data", "nomenclatures", 
       paste0("HS", versions$HS, "-our_ISIC--share.csv"))
  )

# distribution_nb_isic_per_hs <- 
#   hs_isic_df |> 
#   mutate(nb_isic = 1 / share) |>
#   summarize(.by = nb_isic, 
#             nb_hs = n_distinct(k))

# BACI aggregated in ISIC_2d, useful to know the number of trade flows 
baci_in_isic_2d <- 
  baci_df |>
  left_join(hs_isic_df, by = c("k")) |>
  mutate(v = v * share) |>
  summarize(.by = c(t, i, j, isic_2d),
            nb_flows = n_distinct(k),
            v = sum(v)) 
write_fst(
  baci_in_isic_2d, 
  here("data", paste0("t-i-j-isic_2d--BACI--V", versions$baci_V, ".fst")), 
  compress = 100
  )

# ISIC_2d - our ISIC

baci_in_isic_2d <- 
  here("data", paste0("t-i-j-isic_2d--BACI--V", versions$baci_V, ".fst")) |>
  read_fst()

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
    sh_nb_flows < 1 ~ "NEC",
    .default = isic_2d)) |>
  select(isic_2d, isic_2d_aggregated) |>
  arrange(isic_2d)

write_csv(
  isic__isic_for_prices, 
  here("data", "nomenclatures", "ISIC_2d-our_ISIC.csv")
  )

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

write_fst(
  `tijk_4d--uv`, 
  here("data", "intermediary", 
       paste0("t-i-j-k_4d--uv--V", versions$baci_V, ".fst")), 
  compress = 100
  )

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

write_fst(
  baci_with_infered_uv, 
  here("data", "intermediary", 
       paste0("t-i-j-k--uv--infered_from_k_4d--V", versions$baci_V, ".fst")), 
  compress = 100
  )

#  BACI with group variables ----------------------------------------------

# This file is used to compute total trade values
# It is "raw" as opposed to the "filtered" df used to compute prices
raw_baci_with_group_variables  <- 
  file.path(
    paths$baci_p, "Data", versions$baci_V, 
    paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")) |>
  read_fst()  |>
  mutate(k = as.numeric(k)) |>
  left_join(hs_isic_for_prices, by = "k") |>
  # NB because HS codes may be associated with several ISIC codes
  # the dataset will have more rows than defined by t-i-j-k but 
  # the total trade flow will remain correct
  mutate(v = v * share) |>
  select(-share) |>
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share) |>
  select(-share) |>
  mutate(
    t_manuf = as.factor(paste(t, manuf)),
    t_isic = as.factor(paste(t, isic_2d_aggregated)),
    t_stade = as.factor(paste(t, stade)),
    t_isic_stade = as.factor(paste(t, isic_2d_aggregated, stade))
  ) 

write_fst(
  raw_baci_with_group_variables, 
  here(
    "data", "intermediary", 
    paste0("t-i-j-k--BACI_with_group_variables--HS", versions$HS, 
           "-V", versions$baci_V, ".fst")
    ), 
  compress = 100)

# Version for the CEPII nomenclature

hs_branch_for_prices <- 
  here("data", "nomenclatures", 
       paste0("HS", versions$HS, "-branches_for_prices--share.csv")) |>
  read_csv() |>
  as_tibble()
hs_isic_for_prices <- 
  hs_branch_for_prices |>
  rename(isic_2d_aggregated = branch_for_price) 

raw_baci_with_group_variables  <- 
  file.path(
    paths$baci_p, "Data", versions$baci_V, 
    paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")) |>
  read_fst() |>
  mutate(k = as.numeric(k)) |>
  left_join(hs_isic_for_prices, by = "k") |>
  # NB because HS codes may be associated with several ISIC codes
  # the dataset will have more rows than defined by t-i-j-k but 
  # the total trade flow will remain correct
  mutate(v = v * share) |>
  select(-share) |>
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share) |>
  select(-share) |>
  mutate(
    t_manuf = as.factor(paste(t, manuf)),
    t_isic = as.factor(paste(t, isic_2d_aggregated)),
    t_stade = as.factor(paste(t, stade)),
    t_isic_stade = as.factor(paste(t, isic_2d_aggregated, stade))
  ) 
write_fst(
  raw_baci_with_group_variables, 
  here("data", "intermediary", 
       paste0("t-i-j-k--BACI_with_group_variables--HS", versions$HS, 
              "-V", versions$baci_V, "-cepii_nomenclature.fst")), 
  compress = 100
  )

tmp <- 
  raw_baci_with_group_variables |>
  distinct(isic_2d_aggregated)


# delta_ln_uv computed at much aggregated levels 

baci_df <- 
  file.path(paths$baci_p, "Data", versions$baci_V, 
            paste0("t-i-j-k--BACI--HS", versions$HS, 
                   "-V", versions$baci_V, ".fst")) |>
  read_fst() |>
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

write_fst(
  aggregated_delta_ln_uv_df, 
  here("data", "intermediary", 
       paste0("t-i-j-k--aggregated_delta_ln_uv--V", versions$baci_V, ".fst")), 
  compress = 100
  )
