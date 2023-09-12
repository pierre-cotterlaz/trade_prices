

# delta_ln_uv_df <- 
#   create_delta_ln_uv_data(infer_missing_uv = TRUE)

# Remove outliers  --------------------------------------------------------

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- FALSE
infer_missing_uv_select <- FALSE
filtered_df <- 
  remove_outliers(
    lb_percentile_filter = lb_percentile_filter,
    ub_percentile_filter = ub_percentile_filter,
    weighted = weighted_select,
    replace_outliers = TRUE, 
    infer_missing_uv_before = infer_missing_uv_select,
    infer_missing_uv_after = FALSE,
    save_dataset = FALSE
  )

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , TRUE                    , FALSE
  )

pwalk(list_arguments, remove_outliers)

rm(delta_ln_uv_df, baci_df, lagged_baci_df)

# * Create df with group variables ----------------------------------------

# filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
# file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
# baci_df <- 
#   read_fst(file)

# This file is used to compute total trade values
# It is "raw" as opposed to the "filtered" df used to compute prices
raw_baci_with_group_variables  <- 
  baci_df |>
  left_join(hs_isic_df, by = "k") |>
  # NB because HS codes may be associated with several ISIC codes
  # the dataset will have more rows than defined by t-i-j-k but 
  # the total trade flow will remain correct
  mutate(v = v * share) |>
  select(-share) |>
  left_join(isic__isic_for_prices, by = "isic_2d") |> 
  left_join(hs_stade_df, by = "k") |>
  mutate(v = v * share) |>
  select(-share) |>
  mutate(
    t_k = paste(t, k),
    t_isic = paste(t, isic_2d_aggregated),
    t_stade = paste(t, stade),
    t_isic_stade = paste(t, isic_2d_aggregated, stade)
  ) 

# Compute price index -----------------------------------------------------

first_year <- 2017

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- TRUE
infer_missing_uv_select <- FALSE
save_csv_files_price_index(
  weighted = weighted_select,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  infer_missing_uv = infer_missing_uv_select)

list_methods <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , TRUE                    , FALSE
  )

# list_methods <- 
#   tribble(~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~infer_missing_uv,
#           0, 1, FALSE, FALSE,
#           0.05, 0.95, FALSE, FALSE, 
#           0.05, 0.95, TRUE, FALSE,
#           0.05, 0.95, FALSE, TRUE)
pwalk(list_methods, save_csv_files_price_index)



