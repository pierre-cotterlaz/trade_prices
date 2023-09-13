
# Remove outliers  --------------------------------------------------------

# rm(filtered_df)
# HS_revision <- 5
# lb_percentile_filter <- 0.05
# ub_percentile_filter <- 0.95
# weighted_select <- FALSE
# replace_outliers_select <- TRUE
# infer_missing_uv_before_select <- FALSE
# infer_missing_uv_after_select <- TRUE
# filtered_df <- 
#   remove_outliers(
#     lb_percentile_filter = lb_percentile_filter,
#     ub_percentile_filter = ub_percentile_filter,
#     weighted = weighted_select,
#     replace_outliers = replace_outliers_select, 
#     infer_missing_uv_before = infer_missing_uv_before_select,
#     infer_missing_uv_after = infer_missing_uv_after_select
#   )

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

pwalk(list_arguments, remove_outliers)

rm(delta_ln_uv_df, baci_df, lagged_baci_df)

# * Create df with group variables ----------------------------------------

filen <- paste0("t-i-j-k--BACI_with_group_variables--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
raw_baci_with_group_variables <-
  read_fst(file)

# Compute price index -----------------------------------------------------


lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- FALSE
replace_outliers <- TRUE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- TRUE
filen <- paste0("filtered_data--",
                "-lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter,
                "-weighted_", weighted,
                "-replace_outliers_", replace_outliers, 
                "-infer_missing_uv_before_", infer_missing_uv_before, 
                "-infer_missing_uv_after_", infer_missing_uv_after, ".fst")
file <- here("data", "intermediary", filen)
df <- read_fst(file)
tmp <- df |>
  filter(is.na(delta_ln_uv))

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- FALSE
replace_outliers_select <- TRUE
infer_missing_uv_before_select <- FALSE
infer_missing_uv_after_select <- TRUE
save_csv_files_price_index(
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  weighted = weighted_select,
  replace_outliers = replace_outliers_select, 
  infer_missing_uv_before = infer_missing_uv_before_select,
  infer_missing_uv_after = infer_missing_uv_after_select)

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

pwalk(list_arguments, save_csv_files_price_index)



