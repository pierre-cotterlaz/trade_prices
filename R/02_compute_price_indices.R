
filen <- paste0("t-i-j-k--BACI_with_group_variables--HS",
                versions$HS, "-V", versions$baci_V, ".fst")
file <- here("data", "intermediary", filen)
raw_baci_with_group_variables <-
  read_fst(file) |>
  filter(t >= first_year) |>
  mutate(across(c(t_manuf, t_isic, t_stade, t_isic_stade),
                ~ as.character(.x)))

# Remove outliers  --------------------------------------------------------

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_by_centiles, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data,
    0.00                 , 1.00                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
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
  filter(source_data != "both" & lb_percentile_filter != 0.075 & weighted == TRUE)

pwalk(list_arguments, prepare_data_for_price_indices)

# sector_classification <- "isic"
# source_data <- "both"
# lb_percentile_filter <- 0.05
# ub_percentile_filter <- 0.95
# weighted <- TRUE 
# replace_by_centiles <- FALSE
# infer_missing_uv_before <- FALSE
# infer_missing_uv_after <- FALSE
# filen <- paste0("filtered_data--",
#                 "HS_", versions$HS,
#                 "-source_data_", source_data, 
#                 "-lb_perc_", lb_percentile_filter, 
#                 "-ub_perc_", ub_percentile_filter,
#                 "-weighted_", weighted,
#                 "-replace_outliers_", replace_by_centiles, 
#                 "-infer_missing_uv_before_", infer_missing_uv_before, 
#                 "-infer_missing_uv_after_", infer_missing_uv_after, ".fst")
# file <- here("data", "intermediary", filen)
# df <- read_fst(file)
# tmp <- df |>
#   distinct(t)

# * Create df with group variables ----------------------------------------



# Compute price index -----------------------------------------------------

pwalk(list_arguments, save_csv_files_price_index)

sector_classification <- "isic"
lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- TRUE
replace_by_centiles <- FALSE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- FALSE
end_of_filenames <- paste0(
  "-sectors_", sector_classification, 
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", as.character(as.numeric(weighted)),
  "-replace_outliers_", as.character(as.numeric(replace_by_centiles)), 
  "-infer_missing_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
  "-infer_missing_uv_after_", as.character(as.numeric(infer_missing_uv_after)),
  ".csv"
)

source_data <- "wtfc"
filen <- paste0(
  "t--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
wtfc_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t <= 2019) 

source_data <- "baci"
filen <- paste0(
  "t--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
baci_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t > 2019)

df <-
  bind_rows(wtfc_prices, baci_prices) |>
  select(t, nb_obs_used_for_price_index, delta_ln_price_index, v) |>
  arrange(t) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index) 
source_data <- "both_aggregate"
filen <- paste0(
  "t--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
write_csv(df, file)

# t-manuf
source_data <- "wtfc"
filen <- paste0(
  "t-manuf--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
wtfc_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t <= 2019) 

source_data <- "baci"
filen <- paste0(
  "t-manuf--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
baci_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t > 2019)

df <-
  bind_rows(wtfc_prices, baci_prices) |>
  select(-price_index) |>
  arrange(manuf, t) |>
  group_by(manuf) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
source_data <- "both_aggregate"
filen <- paste0(
  "t-manuf--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
write_csv(df, file)

# t-isic
source_data <- "wtfc"
filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
wtfc_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t <= 2019) 

source_data <- "baci"
filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
baci_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t > 2019)

df <-
  bind_rows(wtfc_prices, baci_prices) |>
  select(-price_index) |>
  arrange(isic, t) |>
  group_by(isic) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
source_data <- "both_aggregate"
filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
write_csv(df, file)

# stade
source_data <- "wtfc"
filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
wtfc_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t <= 2019) 

source_data <- "baci"
filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
baci_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t > 2019)

df <-
  bind_rows(wtfc_prices, baci_prices) |>
  select(-price_index) |>
  arrange(stade, t) |>
  group_by(stade) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
source_data <- "both_aggregate"
filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
write_csv(df, file)

# t-isic_2d-stade

source_data <- "wtfc"
filen <- paste0(
  "t-isic_2d-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
wtfc_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t <= 2019) 

source_data <- "baci"
filen <- paste0(
  "t-isic_2d-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
baci_prices <-  
  read_csv(file) |>
  as_tibble() |>
  filter(t > 2019)

df <-
  bind_rows(wtfc_prices, baci_prices) |>
  select(-price_index) |>
  arrange(isic, stade, t) |>
  group_by(isic, stade) |>
  mutate(cumul_delta_ln_price_index = cumsum(delta_ln_price_index)) |>
  ungroup() |>
  mutate(price_index = exp(cumul_delta_ln_price_index)) |>
  select(-cumul_delta_ln_price_index)
source_data <- "both_aggregate"
filen <- paste0(
  "t-isic_2d-stade--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-source_data_", source_data, 
  end_of_filenames)
file <- here("data", "intermediary", filen)
write_csv(df, file)
