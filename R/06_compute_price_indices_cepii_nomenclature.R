
library(tidyverse)
library(glue)
library(ggrepel)
library(here)
theme_set(theme_bw())

get_branches_cepii_nomenclature <- function(){
  k_varname <- sym(paste0("HS", versions$HS))
  k_varname_str <- paste0("HS", versions$HS)
  
  filen <- paste0("HS", versions$HS, "-branch--share--fil_santé.csv")
  file <- file.path(paths$nomenclatures_p, "Conversions", filen)
  hs_branch <-
    read_csv(file) |>
    mutate(branch = str_pad(branch, 4, side = "left", pad = "0")) |>
    mutate(fil = substr(branch, 1, 2)) |>
    mutate(k = as.numeric(!!k_varname)) |>
    left_join(hs_stade_df |> select(-share), by = "k") |>
    mutate(manuf = as.numeric(stade != "1_P" & fil != "01")) |>
    distinct(k, branch, .keep_all = TRUE) |>
    select(k, branch, share, manuf) |> 
    arrange(k)
  
  filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
  file <- file.path(paths$baci_p, "Data", versions$baci_V, filen)
  message(file.info(file)$mtime) 
  baci_df <- 
    read_fst(file) |>
    left_join(hs_branch, by = "k") |>
    mutate(v = v * share)
  
  share_obs_by_branch <- 
    baci_df |>
    summarize(.by = branch,
              nb_obs = n_distinct(t, i, j, k)) |>
    mutate(sh_obs = nb_obs / sum(nb_obs)) |>
    arrange(-sh_obs)
  
  branch__branch_for_price <- 
    share_obs_by_branch |>
    mutate(branch_for_price = case_when(
      sh_obs < 0 ~ "NEC",
      .default = branch
    )) |>
    select(branch, branch_for_price) |>
    arrange(branch) 
  
  hs_branch_for_prices <- 
    hs_branch |>
    left_join(branch__branch_for_price, by = "branch") |>
    select(k, share, branch_for_price, manuf)
  return(hs_branch_for_prices)
}

# hs_branch_for_prices <- get_branches_cepii_nomenclature()
# 
# write_csv(
#   hs_branch_for_prices, 
#   here(
#     "data", "nomenclatures", 
#     paste0("HS", versions$HS, "-branches_for_prices--share.csv"))
#   )

# * Compute price indices -------------------------------------------------

hs_branch_for_prices <- 
  here(
    "data", "nomenclatures", 
    paste0("HS", versions$HS, "-branches_for_prices--share.csv")
    ) |>
  read_csv(show_col_types = FALSE) |>
  as_tibble()

# On "déguise" hs_branch en hs_isic pour pouvoir l'utiliser comme argument de la fonction
hs_isic_for_prices <- 
  hs_branch_for_prices |>
  rename(isic_2d_aggregated = branch_for_price)

list_arguments <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_by_centiles, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data,
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "both",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "both"
  )

list_arguments <- 
  list_arguments |>
  filter(source_data != "both" & weighted == TRUE & lb_percentile_filter == 0.05) |>
  mutate(sector_classification = "cepii")

# pwalk(list_arguments, remove_outliers)

raw_baci_with_group_variables <-
  here(
    "data", "intermediary", 
    paste0("t-i-j-k--BACI_with_group_variables--HS", versions$HS, 
           "-V", versions$baci_V, "-cepii_nomenclature.fst")
    ) |>
  read_fst() |>
  filter(t >= first_year) |>
  mutate(across(c(t_isic, t_stade, t_isic_stade),
                ~ as.character(.x)))

pwalk(list_arguments, save_csv_files_price_index)

# * Create "both aggregated" series ---------------------------------------

sector_classification <- "cepii"
create_both_aggregated_series(sector_classification)

# * Plot graphs -----------------------------------------------------------

#  All manuf products -----------------------------------------------------

list_methods <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_by_centiles, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data,
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
    0.075                , 0.925                , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "wtfc",
    0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "both_aggregate",
    0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "both_aggregate"
  )
list_methods <-
  list_methods |>
  filter(weighted == TRUE & lb_percentile_filter == 0.05) |>
  mutate(sector_classification = "cepii")
dict_method_names <- 
  list_methods |>
  mutate(method_name = case_when(
    source_data == "baci" ~ "baci 5 % weighted",
    source_data == "wtfc" ~ "wtfc 5 % weighted",
    source_data == "both_aggregated" ~ "both 5 % weighted",
    .default = NA_character_
  ))

open_csv <- function(
    source_data, 
    sector_classification,
    lb_percentile_filter, 
    ub_percentile_filter, 
    weighted, 
    replace_by_centiles, 
    infer_missing_uv_before, 
    infer_missing_uv_after) {
  
  filen <- 
    paste0(
      aggregation_level, "--delta_ln_price_index--", 
      "HS_", versions$HS,
      "-source_data_", source_data, 
      "-sectors_", sector_classification, 
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", as.character(as.numeric(weighted)),
      "-replace_outliers_", as.character(as.numeric(replace_by_centiles)), 
      "-infer_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
      "-infer_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
      ".csv"
      )
  
  df <- 
    here("data", "intermediary", versions$trade_price_V, filen) |>
    read_csv(show_col_types = FALSE) |>
    as_tibble() |>
    mutate(source_data = source_data, 
           sector_classification = sector_classification, 
           lb_percentile_filter = lb_percentile_filter,
           ub_percentile_filter = ub_percentile_filter,
           weighted = weighted,
           replace_by_centiles = replace_by_centiles,
           infer_missing_uv_before = infer_missing_uv_before,
           infer_missing_uv_after = infer_missing_uv_after) 
  return(df)

  }
aggregation_level <- "t"
graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_by_centiles",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup() |>
  rename(trade_value = v) |> 
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())

write_csv(
  graph_df, 
  here(
    "data", "final", versions$trade_price_V, "all_methods", 
    paste0("t--price_indices_all_methods--cepii_nomenclature.csv"))
  )

# By isic -----------------------------------------------------------------

# list_methods <- 
#   list_arguments |>
#   filter(source_data != "both" & weighted == TRUE & lb_percentile_filter == 0.05) |>
#   mutate(sector_classification = "cepii")
# dict_method_names <- 
#   list_methods |>
#   mutate(method_name = case_when( 
#     source_data == "wtfc" ~ "WTFC 5% weighted",
#     source_data == "baci" ~ "BACI 5% weighted"))


aggregation_level <- "t-isic_2d"
graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_by_centiles",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(isic, method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup() |>
  rename(trade_value = v,
         branch = isic) |> 
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())
filen <- paste0("t-branch--price_indices_all_methods--cepii_nomenclature.csv")
file <- here("data", "final", versions$trade_price_V, "all_methods", filen)
write_csv(graph_df, file)

graph <- 
  graph_df |>
  filter(source_data == "wtfc") |>
  filter(substr(branch, 1, 2) == "11") |>
  ggplot(aes(x = t, y = price_index, colour = branch, shape = branch)) +
  geom_line() +
  geom_point()
plot(graph)
  

#  By stade --------------------------------------------------------------

aggregation_level <- "t-stade"
graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_by_centiles",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(stade, method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup() |>
  rename(trade_value = v) |> 
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())
filen <- paste0("t-stade--price_indices_all_methods--cepii_nomenclature.csv")
file <- here("data", "final", versions$trade_price_V, "all_methods", filen)
write_csv(graph_df, file)

# list_method_names <- 
#   dict_method_names |>
#   distinct(method_name) |>
#   pull()
# list_graphs <-
#   map(list_method_names, make_graph_trade_volume_all_stades)
# pdf(
#   file = here(
#     "output", "figures",
#     glue("trade_volume_over_time__by_stade__different_methodologies__cepii_nomenclature.pdf")
#   ),
#   width = 7, height = 5, onefile = TRUE
# )
# walk(
#   1:length(list_method_names),
#   \(i) plot(list_graphs[[i]])
# )
# dev.off()

# By ISIC x stade -------------------------------------------------------

aggregation_level <- "t-isic_2d-stade"
graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_by_centiles",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(isic, stade, method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup() |>
  rename(trade_value = v,
         branch = isic) |> 
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())
filen <- paste0("t-isic-stade--price_indices_all_methods--cepii_nomenclature.csv")
file <- here("data", "final", versions$trade_price_V, "all_methods", filen)
write_csv(graph_df, file)

# * Export csv ------------------------------------------------------------


