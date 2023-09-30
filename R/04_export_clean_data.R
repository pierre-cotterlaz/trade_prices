
library(zip)

source_data <- "both_aggregate"
sector_classification <- "isic"
lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- TRUE
replace_outliers <- FALSE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- FALSE

end_of_filenames <- paste0(
  "HS_", versions$HS,
  "-source_data_", source_data, 
  "-sectors_", sector_classification, 
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", as.character(as.numeric(weighted)),
  "-replace_outliers_", as.character(as.numeric(replace_outliers)), 
  "-infer_missing_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
  "-infer_missing_uv_after_", as.character(as.numeric(infer_missing_uv_after)),
  ".csv"
)

# * Year level ------------------------------------------------------------

filen <- paste0("t--delta_ln_price_index--", end_of_filenames)
file <- here("data", "intermediary", filen)
aggregate_data <- 
  read_csv(file) |> 
  as_tibble() |>
  mutate(price_index = price_index * 100) |>
  mutate(
    trade_value_base_100 = (v / v[t == first_year]) * 100,
    trade_volume_base_100 = trade_value_base_100 / price_index * 100) |>
  mutate(aggregation_level = "Year") |>
  rename(year = t, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), 
                ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  select(aggregation_level, year,
         price_index_base_100, trade_value_dollars, trade_value_base_100,
         trade_volume_base_100)
filen <- paste0(
  "price_indices_aggregate__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(aggregate_data, file)

# * Manuf level -----------------------------------------------------------

filen <- paste0("t-manuf--delta_ln_price_index--", end_of_filenames)
file <- here("data", "intermediary", filen)
manuf_data <- 
  read_csv(file) |> 
  as_tibble() |>
  mutate(price_index = price_index * 100) |>
  group_by(manuf) |>
  mutate(
    trade_value_base_100 = (v / v[t == first_year]) * 100,
    trade_volume_base_100 = trade_value_base_100 / price_index * 100) |>
  ungroup() |>
  mutate(aggregation_level = "Year x Manufacturing") |>
  rename(year = t, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), 
                ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  select(aggregation_level, year, manuf,
         price_index_base_100, trade_value_dollars, trade_value_base_100,
         trade_volume_base_100)
filen <- paste0(
  "price_indices_manufacturing__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(manuf_data, file)

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
    trade_volume_base_100 = trade_value_base_100 / price_index * 100) |>
  ungroup() |>
  mutate(aggregation_level = "Year x Production stage") |>
  rename(year = t, 
         production_stage = stade,
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), 
                ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  mutate(across(c(price_index_base_100, trade_volume_base_100), 
                ~ case_when(
                  production_stage == "6_NEC" ~ NA,
                  .default = price_index_base_100
  ))) |>
  select(aggregation_level, year, production_stage,
         price_index_base_100, trade_value_dollars, trade_value_base_100,
         trade_volume_base_100)
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
    trade_volume_base_100 = trade_value_base_100 / price_index * 100) |>
  ungroup() |>
  mutate(aggregation_level = "Year x ISIC") |>
  rename(year = t, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), 
                ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  mutate(across(c(price_index_base_100, trade_volume_base_100), 
                ~ case_when(
                  isic == "NEC" ~ NA,
                  .default = price_index_base_100
                ))) |>
  left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
  rename(isic_name = isic_2d_name) |>
  select(aggregation_level, year, isic, isic_name,
         price_index_base_100, trade_value_dollars, trade_value_base_100,
         trade_volume_base_100)
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
    trade_volume_base_100 = trade_value_base_100 / price_index * 100) |>
  ungroup() |>
  mutate(aggregation_level = "Year x ISIC x Production stage") |>
  rename(year = t, 
         production_stage = stade, 
         trade_value_dollars = v,
         price_index_base_100 = price_index) |>
  mutate(across(c(where(is.numeric), - trade_value_dollars), 
                ~ round(.x, digits = 4))) |>
  mutate(trade_value_dollars = round(trade_value_dollars, digits = 3)) |>
  mutate(across(c(price_index_base_100, trade_volume_base_100), 
                ~ case_when(
                  isic == "NEC" | production_stage == "6_NEC" ~ NA,
                  .default = price_index_base_100
                ))) |>
  left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
  rename(isic_name = isic_2d_name) |>
  select(aggregation_level, year, isic, production_stage,
         price_index_base_100, trade_value_dollars, trade_value_base_100,
         trade_volume_base_100, isic_name)
# filen <- paste0(
#   "price_indices_by_isic_production_stage__v_", versions$trade_price_V, ".csv")
# file <- here("data", "final", versions$trade_price_V, filen)
# write_csv(isic_stade_data, file)

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
