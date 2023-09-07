lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- TRUE

filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "weighted_", weighted_select,
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter, ".csv")
file <- here("data", "intermediary", filen)
stade_data <- 
  read_csv(file) |> 
  group_by(stade) |>
  mutate(
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup() |>
  mutate(aggregation_level = "Year x Production stage")filen <- paste0(
  "price_indices_by_production_stage__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
write_csv(t_isic2d_price_index_df, file)

# to_zip <- 
#   list.files(
#     file.path(paths$data_p, V),
#     pattern = c(paste0("BACI_HS", as.character(rev), "_Y")), 
#     all.files = F, full.names = T
#   )
# 
# filen <- paste0("BACI_HS", rev, "_V", V,  ".zip")
# zipfile <- file.path(paths$data_p, V, filen)
# zipr(zipfile, to_zip, recurse = T, compression_level = 9, include_directories = F)



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