
#' Create a tibble with delta_ln_uv
#' @param source_data BACI, WTFC or both
#' @param infer_missing_uv_before TRUE if missing values have to be filled in 
#' @return a tibble with delta_ln_uv
create_delta_ln_uv_data <- function(
    infer_missing_uv_before,
    source_data = "baci"){
  
  if (source_data == "wtfc"){
    filen <- paste0("t-i-j-k--v-uv--HS", versions$HS, "-V", versions$wtfc_V, ".fst")
    file <- file.path(paths$wtfc_p, "Data", versions$wtfc_V, filen)
    source_df <- 
      read_fst(file) 
  }
  
  if (source_data == "both"){
    filen <- paste0("t-i-j-k--v-uv--HS", versions$HS, "-V", versions$wtfc_V, ".fst")
    file <- file.path(paths$wtfc_p, "Data", versions$wtfc_V, filen)
    wtfc_df <- 
      read_fst(file) 
    
    filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
    file <- file.path(paths$baci_p, "Data", versions$baci_V, filen)
    baci_df <- 
      read_fst(file) |>
      mutate(uv = v / q) |>
      filter(t >= 2020)
    
    source_df <-
      bind_rows(wtfc_df, baci_df)
    rm(wtfc_df, baci_df)
  }
  
  if (source_data == "baci"){
    if (infer_missing_uv_before == FALSE){
      filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
      file <- file.path(paths$baci_p, "Data", versions$baci_V, filen)
      source_df <- 
        read_fst(file) |>
        mutate(uv = v / q)
      
    }
    if (infer_missing_uv_before == TRUE){
      filen <- paste0("t-i-j-k--uv--infered_from_k_4d--V", versions$baci_V, ".fst")
      file <- here("data", "intermediary", filen)
      source_df <- 
        read_fst(file)
    }
  }
  
  lagged_source_df <- 
    source_df |>
    mutate(t = t + 1) |>
    select(t, i, j, k, v, uv) |>
    rename(l_v = v, l_uv = uv)
  
  delta_ln_uv_df <- 
    source_df |>
    left_join(lagged_source_df, by = c("t", "i", "j", "k")) |> 
    # Drop if uv missing in t or t-1 since we cannot compute a time variation in this case
    filter(!(is.na(l_uv) | is.na(uv))) |> 
    mutate(delta_ln_uv = log(uv) - log(l_uv)) |>
    filter(t >= first_year + 1) |>
    select(t, i, j, k, v, l_v, delta_ln_uv)
  
  return(delta_ln_uv_df)
}

#' Replace missing delta log uv after the filtering of extreme values
#' @param input_df tibble with filtered d_ln_uv
#' @param infer_missing_uv_after TRUE if missing values have to be filled in 
#' @return a tibble ready to be used for price index computations
replace_missing_delta_ln_uv_after_filtering <-
  function(input_df, infer_missing_uv_after){
  
  if (infer_missing_uv_after == TRUE) {
    filtered_df <- 
      input_df |>
      select(-v, -l_v)
    
    filen <- paste0("t-i-j-k--aggregated_delta_ln_uv--V", versions$baci_V, ".fst")
    file <- here("data", "intermediary", filen)
    aggregated_delta_ln_uv_df <-
      read_fst(file) |>
      select(-k_4d, -delta_ln_uv) |>
      mutate(k = as.numeric(k)) |>
      filter(t != first_year) 
    
    # BACI is used to obtain all t-i-j-k with positive trade flows
    # and the associated value of v (which is missing from filtered_df)
    filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
    file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
    
    lagged_baci_df <- 
      read_fst(file) |>
      mutate(t = t + 1) |>
      select(t, i, j, k, v) |>
      rename(l_v = v)
    
    baci_df <- 
      read_fst(file) |>
      select(t, i, j, k, v) |>
      left_join(lagged_baci_df, by = c("t", "i", "j", "k")) |>
      filter(t != first_year)

    missing_uv_replaced_df <-
      baci_df |>
      left_join(filtered_df, by = c("t", "i", "j", "k")) |>
      left_join(aggregated_delta_ln_uv_df, by = c("t", "i", "j", "k")) |>
      mutate(delta_ln_uv = case_when( 
        is.na(delta_ln_uv) & !is.na(d_ln_uv__t_i_j_k_4d) ~ d_ln_uv__t_i_j_k_4d,
        is.na(delta_ln_uv) & !is.na(d_ln_uv__t_i_k_4d) ~ d_ln_uv__t_i_k_4d,
        is.na(delta_ln_uv) & !is.na(d_ln_uv__t_k_4d) ~ d_ln_uv__t_k_4d,
        .default = delta_ln_uv)) |>
      filter(!is.na(v) & !is.na(l_v)) |>
      filter(!is.na(delta_ln_uv)) |>
      select(t, i, j, k, delta_ln_uv, v, l_v)
    
  }
  if (infer_missing_uv_after == FALSE) {
    missing_uv_replaced_df <-
      input_df 
  }
  return(missing_uv_replaced_df)
  }

remove_outliers <- function(
    input_df, 
    lb_percentile_filter, 
    ub_percentile_filter, 
    weighted,
    replace_by_centiles){
  if (weighted == FALSE){
    filtered_df <- 
      input_df |>
      group_by(t, k) |>
      mutate(outlier = !between(
        delta_ln_uv, 
        quantile(delta_ln_uv, lb_percentile_filter),
        quantile(delta_ln_uv, ub_percentile_filter))) |>
      ungroup()
    if (replace_by_centiles == TRUE) {
      filtered_df <- 
        filtered_df |>
        group_by(t, k) |>
        mutate(delta_ln_uv = case_when(
          (delta_ln_uv < quantile(delta_ln_uv, lb_percentile_filter)) 
          ~ quantile(delta_ln_uv, lb_percentile_filter),
          (delta_ln_uv > quantile(delta_ln_uv, ub_percentile_filter)) 
          ~ quantile(delta_ln_uv, ub_percentile_filter),
          .default = delta_ln_uv
        )) |>
        ungroup()
    }
    if (replace_by_centiles == FALSE){
      filtered_df <- 
        filtered_df |> 
        filter(outlier == FALSE)
    }
  }
  if (weighted == TRUE){
    filtered_df <- 
      input_df |>
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
      ungroup()
    if (replace_by_centiles == FALSE) {
      filtered_df <-
        filtered_df |> 
        group_by(t, k) |>
        filter(between(
          `t-k--cum_sh_w`, 
          lb_percentile_filter,
          ub_percentile_filter)) |>
        ungroup() 
    }
    if (replace_by_centiles == TRUE) {
      filtered_df <- 
        filtered_df |>
        mutate(lb_delta_ln_uv = case_when( 
          `t-k--cum_sh_w` <= 0.05 ~ delta_ln_uv, 
          .default = NA_real_)) |>
        mutate(ub_delta_ln_uv = case_when( 
          `t-k--cum_sh_w` >= 0.95 ~ delta_ln_uv, 
          .default = NA_real_)) |>
        group_by(t, k) |>
        mutate(lb_delta_ln_uv = max(lb_delta_ln_uv, na.rm = TRUE),
               ub_delta_ln_uv = min(ub_delta_ln_uv, na.rm = TRUE)) |>
        mutate(lb_delta_ln_uv = case_when( 
          lb_delta_ln_uv == -Inf ~ min(delta_ln_uv),
          .default = lb_delta_ln_uv)) |>
        mutate(ub_delta_ln_uv = case_when( 
          ub_delta_ln_uv == +Inf ~ min(delta_ln_uv),
          .default = ub_delta_ln_uv)) |>
        ungroup() |>
        mutate(delta_ln_uv = case_when(
          delta_ln_uv < lb_delta_ln_uv ~ lb_delta_ln_uv, 
          delta_ln_uv > ub_delta_ln_uv ~ ub_delta_ln_uv,
          .default = delta_ln_uv
        )) |> 
        select(t, i, j, k, delta_ln_uv, v, l_v)
      
    }
  }
  return(filtered_df)
}

# Remove outliers 
# We remove the outliers on a dataset that has the correct t-i-j-k structure
# not the dataset with group variables which has more rows than t-i-j-k
prepare_data_for_price_indices <- 
  function(lb_percentile_filter, 
         ub_percentile_filter,
         weighted,
         replace_by_centiles,
         infer_missing_uv_before,
         infer_missing_uv_after,
         source_data = "baci") {
  message("source_data: ", source_data)
  message("lb_percentile_filter: ", lb_percentile_filter, 
          ", ub_percentile_filter: ", ub_percentile_filter)
  message("weighted: ", weighted, 
          ", replace_outliers: ", replace_by_centiles)
  message("infer_missing_uv_before: ", infer_missing_uv_before, 
          ", infer_missing_uv_after: ", infer_missing_uv_after)
  message("=================================================")

  delta_ln_uv_df <- create_delta_ln_uv_data(
    source_data = source_data, 
    infer_missing_uv_before = infer_missing_uv_before) 
  
  # remove outliers 
  filtered_df <- 
    remove_outliers(
      input_df = delta_ln_uv_df,
      lb_percentile_filter = lb_percentile_filter, 
      ub_percentile_filter = ub_percentile_filter, 
      weighted = weighted,
      replace_by_centiles = replace_by_centiles)
  
  # replace missing delta_ln_uv
  missing_uv_replaced_df <- 
    replace_missing_delta_ln_uv_after_filtering(
      input_df = filtered_df,
      infer_missing_uv_after = infer_missing_uv_after)
  
  missing_uv_replaced_df <- 
    missing_uv_replaced_df |>
    select(t, i, j, k, delta_ln_uv, v, l_v)
  
  # save file
  filen <- paste0("filtered_data--",
                  "HS_", versions$HS,
                  "-source_data_", source_data, 
                  "-lb_perc_", lb_percentile_filter, 
                  "-ub_perc_", ub_percentile_filter,
                  "-weighted_", weighted,
                  "-replace_outliers_", replace_by_centiles, 
                  "-infer_missing_uv_before_", infer_missing_uv_before, 
                  "-infer_missing_uv_after_", infer_missing_uv_after, ".fst")
  file <- here("data", "intermediary", filen)
  write_fst(missing_uv_replaced_df, file, compress = 100)
  
  return(missing_uv_replaced_df)
}

# Weight = share of observation in the cell for which we compute the price index

compute_price_index <- 
  function(
    df_with_group_variables, 
    raw_baci_with_group_variables, 
    aggregation_level){
  aggregation_level_str <- deparse(substitute(aggregation_level))
  price_df <- 
    df_with_group_variables |>
    group_by({{ aggregation_level }}) |>
    mutate(weight = 1/2 * (v / sum(v) + l_v / sum(l_v))) |>
    summarize(
      nb_obs_used_for_price_index = n_distinct(t, i, j, k),
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
  function(
    sector_classification = "isic",
    source_data = "baci",
    lb_percentile_filter,
    ub_percentile_filter,
    weighted,
    replace_by_centiles,
    infer_missing_uv_before,
    infer_missing_uv_after) {
    message("sector_classification: ", sector_classification,
            ", source_data: ", source_data)
    message("lb_percentile_filter: ", lb_percentile_filter, 
            ", ub_percentile_filter: ", ub_percentile_filter)
    message("weighted: ", weighted, 
            ", replace_outliers: ", replace_by_centiles)
    message("infer_missing_uv_before: ", infer_missing_uv_before, 
            ", infer_missing_uv_after: ", infer_missing_uv_after)
    message("=================================================")
    
    #message("Creating dataset with group variables")
    filen <- paste0("filtered_data--",
                    "HS_", versions$HS,
                    "-source_data_", source_data, 
                    "-lb_perc_", lb_percentile_filter, 
                    "-ub_perc_", ub_percentile_filter,
                    "-weighted_", weighted,
                    "-replace_outliers_", replace_by_centiles, 
                    "-infer_missing_uv_before_", infer_missing_uv_before, 
                    "-infer_missing_uv_after_", infer_missing_uv_after, ".fst")

    file <- here("data", "intermediary", filen)
    df_with_group_variables  <- 
      read_fst(file) |>
      mutate(k = as.numeric(k)) |>
      left_join(hs_isic_for_prices, by = "k") |>
      mutate(v = v * share, l_v = l_v * share) |>
      select(-share) |>
      # left_join(isic__isic_for_prices, by = "isic_2d") |> 
      left_join(hs_stade_df, by = "k") |>
      mutate(v = v * share, l_v = l_v * share) |>
      select(-share) |>
      mutate(
        # t_k = paste(t, k),
        t_isic = paste(t, isic_2d_aggregated),
        t_stade = paste(t, stade),
        t_isic_stade = paste(t, isic_2d_aggregated, stade)
      ) 
    if (sector_classification == "cepii"){
      df_with_group_variables <- 
        df_with_group_variables |>
        filter(manuf == 1)
    }
    
# suffix for csv filenames containing the price indices
    end_of_filenames <- paste0(
      "HS_", versions$HS,
      "-source_data_", source_data, 
      "-sectors_", sector_classification, 
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", as.character(as.numeric(weighted)),
      "-replace_outliers_", as.character(as.numeric(replace_by_centiles)), 
      "-infer_missing_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
      "-infer_missing_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
      ".csv"
    )

    # message("Computing year price index")
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
    filen <- paste0("t--delta_ln_price_index--", end_of_filenames)
    file <- here("data", "intermediary", filen)
    write_csv(t_price_index_df, file)
    
    # message("Computing year x production_stage price index")
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
      "t-stade--delta_ln_price_index--", end_of_filenames)
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
      "t-isic_2d--delta_ln_price_index--", end_of_filenames)
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
    filen <- paste0("t-isic_2d-stade--delta_ln_price_index--", end_of_filenames)
    file <- here("data", "intermediary", filen)
    write_csv(t_isic2d_stade_price_index_df, file)
    
  }
  
