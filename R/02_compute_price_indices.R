
# Remove outliers  --------------------------------------------------------

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_by_centiles, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data,
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.00                 , 1.00                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "both",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "both"
  )

list_arguments <- 
  list_arguments |>
  filter(source_data == "baci")

pwalk(list_arguments, prepare_data_for_price_indices)

sector_classification <- "isic"
source_data <- "both"
lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- TRUE 
replace_by_centiles <- FALSE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- FALSE
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
df <- read_fst(file)
tmp <- df |>
  distinct(t)

# * Create df with group variables ----------------------------------------

filen <- paste0("t-i-j-k--BACI_with_group_variables--HS", 
                versions$HS, "-V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
raw_baci_with_group_variables <-
  read_fst(file) |>
  filter(t >= first_year) |>
  mutate(across(c(t_isic, t_stade, t_isic_stade),
                ~ as.character(.x)))

# Compute price index -----------------------------------------------------

pwalk(list_arguments, save_csv_files_price_index)

