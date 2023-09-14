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
