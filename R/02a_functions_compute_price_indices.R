# Remove outliers 
# We remove the outliers on a dataset that has the correct t-i-j-k structure
# not the dataset with group variables which has more rows than t-i-j-k
remove_outliers <- 
function(data, 
         lb_percentile_filter, 
         ub_percentile_filter,
         weighted) {
  if (weighted == FALSE){
    filtered_df <- 
      data |>
      group_by(t, k) |>
      filter(between(
        delta_ln_uv, 
        quantile(delta_ln_uv, lb_percentile_filter),
        quantile(delta_ln_uv, ub_percentile_filter))) |>
      ungroup() 
  }
  if (weighted == TRUE){
    filtered_df <- 
      delta_ln_uv_df |>
      filter(!is.na(delta_ln_uv)) |>
      group_by(t, k) |>
      mutate(`t-k--v` = sum(v, na.rm = T),
             `t-k--l_v` = sum(l_v, na.rm = T))|>
      ungroup() |>
      mutate(w_k = 1 / 2 * (v / `t-k--v` + l_v / `t-k--l_v`)) |>
      group_by(k, t) |>
      mutate(sh_w = w_k / sum(w_k, na.rm = TRUE)) |>
      arrange(t, k, delta_ln_uv) |>
      mutate(`t-k--cum_sh_w` = cumsum(sh_w)) |>
      ungroup() |> 
      group_by(t, k) |>
      filter(between(
        `t-k--cum_sh_w`, 
        lb_percentile_filter,
        ub_percentile_filter)) |>
      ungroup() 
  }
  filtered_df <- 
    filtered_df |>
    select(t, i, j, k, delta_ln_uv, v, l_v)
  return(filtered_df)
}

# Weight = share of observation in the cell for which we compute the price index

compute_price_index <- function(
    data, 
    raw_baci_with_group_variables, 
    aggregation_level){
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
