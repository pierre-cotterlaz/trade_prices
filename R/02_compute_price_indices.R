# This file is used to compute total trade values
# It is "raw" as opposed to the "filtered" df used to compute prices
raw_baci_with_group_variables  <- 
  baci_df |>
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
    t_stade = paste(t, stade)
  ) 

lagged_baci_df <- 
  baci_df |>
  mutate(t = t + 1) |>
  select(t, i, j, k, v, uv) |>
  rename(l_v = v, l_uv = uv)

delta_ln_uv_df <- 
  baci_df |>
  left_join(lagged_baci_df, by = c("t", "i", "j", "k")) |> 
  # Drop if uv missing in t or t-1 since we cannot compute a time variation in this case
  filter(!(is.na(l_uv) | is.na(uv))) |> 
  mutate(delta_ln_uv = log(uv) - log(l_uv))

# Remove outliers  --------------------------------------------------------

source(here("R", "02a_functions_compute_price_indices.R"))

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
filtered_df <- remove_outliers(
  data = delta_ln_uv_df,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

rm(delta_ln_uv_df, baci_df, lagged_baci_df)

lb_percentile_filter <- 0
ub_percentile_filter <- 1
filtered_df <- remove_outliers(
  data = delta_ln_uv_df,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

# Join with ISIC_2d and BEC 
# Create variables defining the aggregation level 
df_with_group_variables  <- 
  filtered_df |>
  left_join(hs_isic_df, by = "k") |>
  mutate(v = v * share, l_v = l_v * share) |>
  select(-share) |>
  left_join(isic__isic_for_prices, by = "isic_2d") |> 
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share, l_v = l_v * share) |>
  select(-share) |>
  mutate(
    t_k = paste(t, k),
    t_isic = paste(t, isic_2d_aggregated),
    t_stade = paste(t, stade),
    t_isic_stade = paste(t, isic_2d_aggregated, stade)
  ) 
rm(filtered_df)

# Compute price index -----------------------------------------------------

first_year <- 2017

# By t 
t_price_index_df <- 
  compute_price_index(
    data = df_with_group_variables,
    raw_baci_with_group_variables = raw_baci_with_group_variables,
    aggregation_level = t) %>%
  .[["price_df"]] |>
  mutate(delta_ln_price_index = case_when(
    t == first_year ~ 0, 
    .default = delta_ln_price_index
  )) |>
  arrange(t) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) 
filen <- paste0(
  "t--delta_ln_price_index--", 
  "lb_filter_", lb_percentile_filter,
  "ub_filter_", ub_percentile_filter, 
  ".csv")
file <- here("data", "intermediary", filen)
write_csv(t_price_index_df, file)

# By t-k
t_k_price_index_df <- 
  compute_price_index(
    data = df_with_group_variables, 
    aggregation_level = t_k) %>%
  .[["price_df"]] |>
  separate_wider_delim(t_k, delim = " ", names_sep = "_") |>
  rename(t = t_k_1, k = t_k_2) |>
  arrange(k, t) |>
  group_by(k) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) 
filen <- paste0(
  "t-k--delta_ln_price_index--", 
  "lb_filter_", lb_percentile_filter,
  "ub_filter_", ub_percentile_filter, 
  ".csv")
file <- here("data", "intermediary", filen)
write_csv(t_stade_price_index_df, file)

# By t-stade
t_stade_price_index_df <- 
  compute_price_index(
    data = df_with_group_variables,
    raw_baci_with_group_variables = raw_baci_with_group_variables,
    aggregation_level = t_stade) %>%
  .[["price_df"]] |>
  separate_wider_delim(t_stade, delim = " ", names_sep = "_") |>
  rename(t = t_stade_1, stade = t_stade_2) |>
  mutate(delta_ln_price_index = case_when(
    t == 2017 ~ 0, 
    .default = delta_ln_price_index
  )) |>
  arrange(stade, t) |>
  group_by(stade) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "lb_filter_", lb_percentile_filter,
  "ub_filter_", ub_percentile_filter, 
  ".csv")
file <- here("data", "intermediary", filen)
write_csv(t_stade_price_index_df, file)

# By t-ISIC
t_isic2d_price_index_df <- 
  compute_price_index(
    data = df_with_group_variables,
    raw_baci_with_group_variables = raw_baci_with_group_variables,
    aggregation_level = t_isic) %>%
  .[["price_df"]] |>
  separate_wider_delim(t_isic, delim = " ", names_sep = "_") |>
  rename(t = t_isic_1, isic = t_isic_2) |>
  mutate(delta_ln_price_index = case_when(
    t == 2017 ~ 0, 
    .default = delta_ln_price_index
  )) |>
  arrange(isic, t) |>
  group_by(isic) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "lb_filter_", lb_percentile_filter,
  "ub_filter_", ub_percentile_filter, 
  ".csv")
file <- here("data", filen)
write_csv(t_isic2d_price_index_df, file)
