
library(zip)

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- FALSE
replace_outliers <- TRUE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- FALSE

end_of_filenames <- paste0(
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", weighted,
  "-replace_outliers_", replace_outliers, 
  "-infer_missing_uv_before_", infer_missing_uv_before, 
  "-infer_missing_uv_after_", infer_missing_uv_after, ".csv"
)

# * stade level -----------------------------------------------------------

filen <- paste0("t-stade--delta_ln_price_index--", end_of_filenames)
file <- here("data", "intermediary", filen)
stade_data <- 
  read_csv(file) |> 
  as_tibble() |>
  mutate(price_index = price_index * 100) |>
  group_by(stade) |>
  mutate(
    trade_value_base_100 = (v / v[t == first_year]) * 100,
    trade_volume_base_100 = trade_value_base_100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x Production stage",
         yearly_change_price_index = exp(delta_ln_price_index)) |>
  rename(year = t, 
         production_stage = stade,
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  select(aggregation_level, year, production_stage, yearly_change_price_index,
         price_index_base_100, trade_value_dollars, trade_value_base_100)
filen <- paste0(
  "price_indices_by_production_stage__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(stade_data, file)

# * isic level ------------------------------------------------------------

filen <- paste0("t-isic_2d--delta_ln_price_index--", end_of_filenames)
file <- here("data", "intermediary", filen)
isic_data <- 
  read_csv(file) |> 
  as_tibble() |>
  mutate(price_index = price_index * 100) |>
  group_by(isic) |>
  mutate(
    trade_value_base_100 = (v / v[t == first_year]) * 100,
    trade_volume_base_100 = trade_value_base_100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x ISIC",
         yearly_change_price_index = exp(delta_ln_price_index)) |>
  rename(year = t, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  select(aggregation_level, year, isic, yearly_change_price_index,
         price_index_base_100, trade_value_dollars, trade_value_base_100)
filen <- paste0(
  "price_indices_by_isic__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(isic_data, file)

# * isic x stade level ----------------------------------------------------

filen <- paste0("t-isic_2d-stade--delta_ln_price_index--", end_of_filenames)
file <- here("data", "intermediary", filen)
isic_stade_data <- 
  read_csv(file) |> 
  as_tibble() |>
  mutate(price_index = price_index * 100) |>
  group_by(isic, stade) |>
  mutate(
    trade_value_base_100 = (v / v[t == first_year]) * 100,
    trade_volume_base_100 = trade_value_base_100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x ISIC x Production stage",
         yearly_change_price_index = exp(delta_ln_price_index)) |>
  rename(year = t, 
         production_stage = stade, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  select(aggregation_level, year, isic, production_stage,
         yearly_change_price_index,
         price_index_base_100, trade_value_dollars, trade_value_base_100)
filen <- paste0(
  "price_indices_by_isic_production_stage__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(isic_data, file)

# * Create zip file -------------------------------------------------------

files_to_be_zipped <- 
  list.files(
    here("data", "final", versions$trade_price_V),
    pattern = ".csv",
    all.files = FALSE, full.names = TRUE)
filen <- paste0("trade_prices_v", versions$trade_price_V, ".zip")
zip_destination_file <- 
  here("data", "final", versions$trade_price_V, filen)
zipr(zip_destination_file, files_to_be_zipped,
    recurse = TRUE, compression_level = 9, include_directories = FALSE)
