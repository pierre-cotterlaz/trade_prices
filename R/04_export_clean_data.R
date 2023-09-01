
filen <- paste0("t-stade--delta_ln_price_index.csv")
file <- here("data", filen)
stade_data <- 
  read_csv(file) |> 
  group_by(stade) |>
  mutate(
    trade_value_base100 = (v / v[t == 2017]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x Production stage")

filen <- paste0("t-isic_2d--delta_ln_price_index.csv")
file <- here("data", filen)
isic_data <- 
  read_csv(file) |> 
  group_by(isic) |>
  mutate(
    trade_value_base100 = (v / v[t == 2017]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x ISIC")

final_df <- 
  bind_rows(stade_data, isic_data) |>
  mutate(yearly_change_price_index = exp(delta_ln_price_index),
         trade_value_dollars = v / 1E3,
         price_index_base100 = price_index * 100) |>
  rename(year = t,
         production_stage = stade, 
         sector_isic = isic) |>
  select(aggregation_level, year, production_stage, sector_isic, 
         yearly_change_price_index, price_index_base100,
         trade_value_base100, trade_volume_base100, trade_value_dollars)

rm(stade_data, isic_data)