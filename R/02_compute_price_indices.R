

# delta_ln_uv_df <- 
#   create_delta_ln_uv_data(infer_missing_uv = TRUE)

# Remove outliers  --------------------------------------------------------

list_arguments <- 
  tribble(~lb_percentile_filter, ~ub_percentile_filter, ~weighted, infer_missing_uv,
          0, 1, FALSE, FALSE,
          0.05, 0.95, FALSE, FALSE,
          0.05, 0.95, TRUE, FALSE)

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- FALSE
infer_missing_uv_select <- TRUE
filtered_df <- remove_outliers(
  weighted = weighted_select,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  infer_missing_uv = infer_missing_uv_select,
  save_dataset = FALSE)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter,
                "-weighted_", weighted_select,
                "-infer_missing_uv_", infer_missing_uv_select, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

lb_percentile_filter <- 0
ub_percentile_filter <- 1
weighted_select <- FALSE
infer_missing_uv_select <- FALSE
filtered_df <- remove_outliers(
  weighted = weighted_select,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  infer_missing_uv = infer_missing_uv_select,
  save_dataset = FALSE)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter,
                "-weighted_", weighted_select,
                "-infer_missing_uv_", infer_missing_uv_select, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- FALSE
infer_missing_uv_select <- FALSE
filtered_df <- remove_outliers(
  weighted = weighted_select,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  infer_missing_uv = infer_missing_uv_select,
  save_dataset = FALSE)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter,
                "-weighted_", weighted_select,
                "-infer_missing_uv_", infer_missing_uv_select, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted_select <- TRUE
infer_missing_uv_select <- FALSE
filtered_df <- remove_outliers(
  weighted = weighted_select,
  lb_percentile_filter = lb_percentile_filter,
  ub_percentile_filter = ub_percentile_filter,
  infer_missing_uv = infer_missing_uv_select,
  save_dataset = FALSE)
filen <- paste0("filtered_data--", 
                "lb_perc_", lb_percentile_filter, 
                "-ub_perc_", ub_percentile_filter,
                "-weighted_", weighted_select,
                "-infer_missing_uv_", infer_missing_uv_select, ".fst")
file <- here("data", "intermediary", filen)
write_fst(filtered_df, file, compress = 100)

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

# lb_percentile_filter <- 0.05
# ub_percentile_filter <- 0.95
# weighted_select <- FALSE
# infer_missing_uv_select <- FALSE
# filen <- paste0("filtered_data--", 
#                 "lb_perc_", lb_percentile_filter, 
#                 "-ub_perc_", ub_percentile_filter,
#                 "-weighted_", weighted_select,
#                 "-infer_missing_uv_", infer_missing_uv_select, ".fst")
# file <- here("data", "intermediary", filen)
# # filtered_df <- 
# #   read_fst(file)
# 
# # Join with ISIC_2d and BEC 
# # Create variables defining the aggregation level 
# df_with_group_variables  <- 
#   read_fst(file) |>
#   mutate(k = as.numeric(k)) |>
#   left_join(hs_isic_df, by = "k") |>
#   mutate(v = v * share, l_v = l_v * share) |>
#   select(-share) |>
#   left_join(isic__isic_for_prices, by = "isic_2d") |> 
#   left_join(hs_stade_df, by = "k") |>
#   mutate(v = v * share, l_v = l_v * share) |>
#   select(-share) |>
#   mutate(
#     t_k = paste(t, k),
#     t_isic = paste(t, isic_2d_aggregated),
#     t_stade = paste(t, stade),
#     t_isic_stade = paste(t, isic_2d_aggregated, stade)
#   ) 
# rm(filtered_df)

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

list_arguments <- 
  tribble(~lb_percentile_filter, ~ub_percentile_filter, ~weighted, infer_missing_uv,
          0, 1, FALSE, FALSE,
          0.05, 0.95, FALSE, FALSE,
          0.05, 0.95, TRUE, FALSE)

list_methods <- 
  tribble(~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~infer_missing_uv,
          0, 1, FALSE, FALSE,
          0.05, 0.95, FALSE, FALSE, 
          0.05, 0.95, TRUE, FALSE,
          0.05, 0.95, FALSE, TRUE)
pwalk(list_methods, save_csv_files_price_index)



