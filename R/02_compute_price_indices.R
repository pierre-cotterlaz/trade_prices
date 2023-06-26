raw_baci_with_group_variables  <- 
  baci_df |>
  left_join(hs_isic_df, by = "k") |>
  # NB the dataset will have more rows than defined by t-i-j-k but the total trade flow will remain correct
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

# Remove outliers 
# We remove the outliers on a dataset that has the correct t-i-j-k structure
# not the dataset with group variables which has more rows than t-i-j-k
remove_outliers <- function(data, lb_percentile_filter, ub_percentile_filter) {
  filtered_df <- 
    data |>
    group_by(t, k) |>
    filter(between(
      delta_ln_uv, 
      quantile(delta_ln_uv, lb_percentile_filter),
      quantile(delta_ln_uv, ub_percentile_filter))) |>
    ungroup() 
  return(filtered_df)
}
lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
filtered_df <- remove_outliers(
  data = delta_ln_uv_df,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter)

# Join with ISIC_2d and BEC 
# Create variables defining the aggregation level 
df_with_group_variables  <- 
  filtered_df |>
  left_join(hs_isic_df, by = "k") |>
  # NB the dataset will have more rows than defined by t-i-j-k but the total trade flow will remain correct
  mutate(v = v * share, l_v = l_v * share) |>
  select(-share) |>
  left_join(isic__isic_for_prices, by = "isic_2d") |> 
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share, l_v = l_v * share) |>
  select(-share) |>
  mutate(
    t_k = paste(t, k),
    t_isic = paste(t, isic_2d_aggregated),
    t_stade = paste(t, stade)
  ) 

# Weight = share of observation in the cell for which we compute the price index

compute_price_index <- function(data, raw_baci_with_group_variables, aggregation_level){
  aggregation_level_str <- deparse(substitute(aggregation_level))
  price_df <- 
    data |>
    group_by({{ aggregation_level }}) |>
    mutate(weight = 1/2 * (v / sum(v) + l_v / sum(l_v))) |>
    summarize(
      delta_ln_price_index = sum(delta_ln_uv * weight)) |>
    ungroup() 
  # Requires a separate df because otherwise initial year is missing
  trade_value_df <- 
    raw_baci_with_group_variables |>
    group_by({{ aggregation_level }}) |>
    summarize(v = sum(v) / 1E3) |>
    ungroup()
  price_df <-
    price_df |>
    full_join(trade_value_df, by = aggregation_level_str)
  return(list(price_df = price_df, aggregation_level_str = aggregation_level_str))
}
result <- 
  compute_price_index(data = df_with_group_variables, 
                      raw_baci_with_group_variables = raw_baci_with_group_variables, 
                      aggregation_level = t_stade)

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
filen <- paste0("t-k--delta_ln_price_index.csv")
file <- here("data", filen)
baci_in_isic_2d <- read_fst(file)

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
filen <- paste0("t-stade--delta_ln_price_index.csv")
file <- here("data", filen)
write_csv(t_stade_price_index_df, file)

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
filen <- paste0("t-isic_2d--delta_ln_price_index.csv")
file <- here("data", filen)
write_csv(t_isic2d_price_index_df, file)

list_aggregation_level <- c("t", "t_isic", "t_stade")
names(list_aggregation_level) <- list_aggregation_level
price_index_df <-
  map(list_aggregation_level, compute_price_index) %>%
  .[["price_df"]] 
|>
  list_rbind(names_to = "aggregation_level") |>

tmp <- result$price_df |>
  separate_wider_delim(t_k, delim = " ", names_sep = "_") 



