filen <- paste0("t-stade--delta_ln_price_index.csv")
file <- here("data", filen)
graph_df <- 
  read_csv(file) |> 
  group_by(stade) |>
  mutate(
    trade_value_base100 = v / v[t == 2018],
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()