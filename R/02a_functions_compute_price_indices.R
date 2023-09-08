
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
    df_with_group_variables, 
    raw_baci_with_group_variables, 
    aggregation_level){
  aggregation_level_str <- deparse(substitute(aggregation_level))
  price_df <- 
    df_with_group_variables |>
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

save_csv_files_price_index <- 
  function(lb_percentile_filter, 
           ub_percentile_filter,
           weighted,
           infer_missing_uv) {
    filen <- paste0(
      "filtered_data--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted,
      "-infer_missing_uv_", infer_missing_uv, ".fst"
    )
    file <- here("data", "intermediary", filen)
    df_with_group_variables  <- 
      read_fst(file) |>
      mutate(k = as.numeric(k)) |>
      left_join(hs_isic_df, by = "k") |>
      mutate(v = v * share, l_v = l_v * share) |>
      select(-share) |>
      left_join(isic__isic_for_prices, by = "isic_2d") |> 
      left_join(hs_stade_df, by = "k") |>
      mutate(v = v * share, l_v = l_v * share) |>
      select(-share) |>
      mutate(
        t_k = paste(t, k),
        t_isic = paste(t, isic_2d_aggregated),
        t_stade = paste(t, stade),
        t_isic_stade = paste(t, isic_2d_aggregated, stade)
      ) 
    
    # t level 
    t_price_index_df <- 
      compute_price_index(
        df_with_group_variables = df_with_group_variables,
        raw_baci_with_group_variables = raw_baci_with_group_variables,
        aggregation_level = t) %>%
      .[["price_df"]] |>
      mutate(delta_ln_price_index = case_when(
        t == first_year ~ 0, 
        .default = delta_ln_price_index
      )) |>
      arrange(t) |>
      mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
      mutate(price_index = exp(cumul_delta_ln_price_index)) 
    filen <- paste0(
      "t--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted_select,
      "-infer_missing_uv_", infer_missing_uv_select, ".csv")
    file <- here("data", "intermediary", filen)
    write_csv(t_price_index_df, file)
    
    # By t-stade
    t_stade_price_index_df <- 
      compute_price_index(
        df_with_group_variables = df_with_group_variables,
        raw_baci_with_group_variables = raw_baci_with_group_variables,
        aggregation_level = t_stade) %>%
      .[["price_df"]] |>
      separate_wider_delim(t_stade, delim = " ", names_sep = "_") |>
      rename(t = t_stade_1, stade = t_stade_2) |>
      mutate(delta_ln_price_index = case_when(
        t == first_year ~ 0, 
        .default = delta_ln_price_index
      )) |>
      arrange(stade, t) |>
      group_by(stade) |>
      mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
      ungroup() |>
      mutate(price_index = exp(cumul_delta_ln_price_index)) |>
      select(-cumul_delta_ln_price_index)
    filen <- paste0(
      "t-stade--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted_select,
      "-infer_missing_uv_", infer_missing_uv_select, ".csv")
    file <- here("data", "intermediary", filen)
    write_csv(t_stade_price_index_df, file)
    
    # By t-ISIC
    t_isic2d_price_index_df <- 
      compute_price_index(
        df_with_group_variables = df_with_group_variables,
        raw_baci_with_group_variables = raw_baci_with_group_variables,
        aggregation_level = t_isic) %>%
      .[["price_df"]] |>
      separate_wider_delim(t_isic, delim = " ", names_sep = "_") |>
      rename(t = t_isic_1, isic = t_isic_2) |>
      mutate(delta_ln_price_index = case_when(
        t == first_year ~ 0, 
        .default = delta_ln_price_index
      )) |>
      arrange(isic, t) |>
      group_by(isic) |>
      mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
      ungroup() |>
      mutate(price_index = exp(cumul_delta_ln_price_index)) |>
      select(-cumul_delta_ln_price_index)
    filen <- paste0(
      "t-isic_2d--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted_select,
      "-infer_missing_uv_", infer_missing_uv_select, ".csv")
    file <- here("data", "intermediary", filen)
    write_csv(t_isic2d_price_index_df, file)
    
    # By t-ISIC-stade
    t_isic2d_stade_price_index_df <- 
      compute_price_index(
        df_with_group_variables = df_with_group_variables,
        raw_baci_with_group_variables = raw_baci_with_group_variables,
        aggregation_level = t_isic_stade) %>%
      .[["price_df"]] |>
      separate_wider_delim(t_isic_stade, delim = " ", names_sep = "_") |>
      rename(t = t_isic_stade_1, isic = t_isic_stade_2, stade = t_isic_stade_3) |>
      mutate(delta_ln_price_index = case_when(
        t == first_year ~ 0, 
        .default = delta_ln_price_index
      )) |>
      arrange(isic, stade, t) |>
      group_by(isic, stade) |>
      mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
      ungroup() |>
      mutate(price_index = exp(cumul_delta_ln_price_index)) |>
      select(-cumul_delta_ln_price_index)
    filen <- paste0(
      "t-isic_2d-stade--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted_select,
      "-infer_missing_uv_", infer_missing_uv_select, ".csv")
    file <- here("data", "intermediary", filen)
    write_csv(t_isic2d_stade_price_index_df, file)
    
  }
  
