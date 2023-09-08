
#
create_delta_ln_uv_data <- function(infer_missing_uv){
  if (infer_missing_uv == FALSE){
    filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
    file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
    message(file.info(file)$mtime) 
    baci_df <- 
      read_fst(file) |>
      mutate(uv = v/q)
  }
  if (infer_missing_uv == TRUE){
    filen <- paste0("t-i-j-k--uv--infered_from_k_4d--V", versions$baci_V, ".fst")
    file <- here("data", "intermediary", filen)
    baci_df <- 
      read_fst(file)
  }
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
    mutate(delta_ln_uv = log(uv) - log(l_uv)) |>
    select(t, i, j, k, v, l_v, delta_ln_uv)
  return(delta_ln_uv_df)
}


# Remove outliers 
# We remove the outliers on a dataset that has the correct t-i-j-k structure
# not the dataset with group variables which has more rows than t-i-j-k
remove_outliers <- 
function(lb_percentile_filter, 
         ub_percentile_filter,
         weighted,
         infer_missing_uv,
         save_dataset) {
  delta_ln_uv_df <- create_delta_ln_uv_data(infer_missing_uv) 
  if (weighted == FALSE){
    filtered_df <- 
      delta_ln_uv_df |>
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
  filen <- paste0("filtered_data--",
                  "-lb_perc_", lb_percentile_filter, 
                  "-ub_perc_", ub_percentile_filter,
                  "-weighted_", weighted,
                  "-infer_missing_uv_", infer_missing_uv, ".fst")
  file <- here("data", "intermediary", filen)
  if (save_dataset == TRUE){
    write_fst(filtered_df, file, compress = 100)
  }
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
