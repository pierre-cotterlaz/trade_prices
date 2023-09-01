# Remove outliers 
# We remove the outliers on a dataset that has the correct t-i-j-k structure
# not the dataset with group variables which has more rows than t-i-j-k
remove_outliers <- function(data, 
                            lb_percentile_filter, 
                            ub_percentile_filter) {
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
