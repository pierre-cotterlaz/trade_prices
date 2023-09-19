filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$baci_p, "Data", versions$baci_V, filen)
message(file.info(file)$mtime) 
baci_df <- 
  read_fst(file) |>
  mutate(uv = v / q)


filen <- paste0(
  "price_indices_aggregate__v_", versions$trade_price_V, ".csv")
file <- here("data", "final", versions$trade_price_V, filen)
treated_df <- 
  read_csv(file) |>
  as_tibble() |>
  rename(t = year) |>
  select(t, trade_value_dollars) |>
  mutate(trade_value_dollars = trade_value_dollars / 1E6)

comparison_df <-
  baci_df |>
  summarize(.by = t, 
            v = sum(v) / 1E9) |>
  left_join(treated_df, by = "t")


# * Ventilation par stade concorde avec mail Guillaume 19/09 --------

filen <- paste0("t-i-j-k--BACI_with_group_variables--HS", 
                versions$HS, "-V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
raw_baci_with_group_variables <-
  read_fst(file) |>
  filter(t >= first_year)

by_stade_df <- 
  raw_baci_with_group_variables |>
  filter(between(t, 2012, 2016)) |>
  summarize(.by = stade,
            v = sum(v, na.rm = TRUE) / 1E9) |>
  mutate(share_v = round(v / sum(v) * 100, digits = 2),
         v = round(v, digits = 3)) |>
  arrange(stade) |>
  as_tibble()
by_stade_df
