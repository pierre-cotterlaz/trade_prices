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