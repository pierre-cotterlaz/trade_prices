
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

k__manuf <- 
  hs_isic_for_prices |>
  summarize(.by = k,
            manuf = max(manuf, na.rm = TRUE))

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

list_arguments <-
  expand.grid(
    lb_percentile_filter = 0.05,
    ub_percentile_filter = 0.95,
    weighted = TRUE,
    replace_by_centiles = FALSE,
    infer_missing_uv_before = FALSE, 
    infer_missing_uv_after = FALSE,
    source_data = c("baci", "wtfc"),
    remove_primary_goods = c(TRUE, FALSE),
    sector_classification = "cepii"
  )

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
create_both_aggregated_series(
  sector_classification, 
  remove_primary_goods = TRUE)
create_both_aggregated_series(
  sector_classification,
  remove_primary_goods = FALSE)

# * Plot graphs -----------------------------------------------------------

#  All manuf products -----------------------------------------------------

# list_methods <- 
#   tribble(
#     ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_by_centiles, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data,
#     0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
#     0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
#     0.075                , 0.925                , TRUE     , FALSE               , FALSE                   , FALSE                  , "baci",
#     0.075                , 0.925                , FALSE    , FALSE               , FALSE                   , FALSE                  , "baci",
#     0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "wtfc",
#     0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "wtfc",
#     0.05                 , 0.95                 , TRUE     , FALSE               , FALSE                   , FALSE                  , "both_aggregate",
#     0.05                 , 0.95                 , FALSE    , FALSE               , FALSE                   , FALSE                  , "both_aggregate"
#   )
# list_methods <-
#   list_methods |>
#   filter(weighted == TRUE & lb_percentile_filter == 0.05) |>
#   mutate(sector_classification = "cepii")

list_methods <-
  expand_grid(
    lb_percentile_filter = 0.05,
    ub_percentile_filter = 0.95,
    weighted = TRUE,
    replace_by_centiles = FALSE,
    infer_missing_uv_before = FALSE, 
    infer_missing_uv_after = FALSE,
    source_data = c("baci", "wtfc", "both_aggregate"),
    remove_primary_goods = c(TRUE, FALSE),
    sector_classification = "cepii"
  )

dict_method_names <- 
  list_methods |>
  mutate(method_name = case_when(
    source_data == "baci" & remove_primary_goods == FALSE ~ "baci",
    source_data == "wtfc" & remove_primary_goods == FALSE ~ "wtfc",
    source_data == "both_aggregate" & remove_primary_goods == FALSE ~ "both",
    source_data == "baci" & remove_primary_goods == TRUE ~ "baci, manuf",
    source_data == "wtfc" & remove_primary_goods == TRUE ~ "wtfc, manuf",
    source_data == "both_aggregate" & remove_primary_goods == TRUE ~ "both, manuf",
    .default = NA_character_
  ))

open_csv_new <- function(
    source_data, 
    sector_classification,
    lb_percentile_filter, 
    ub_percentile_filter, 
    weighted, 
    replace_by_centiles, 
    infer_missing_uv_before, 
    infer_missing_uv_after,
    remove_primary_goods) {
  
  filename <- 
    paste0(
      "price_index--", 
      "HS_", versions$HS,
      "-source_data_", source_data, 
      "-sectors_", sector_classification, 
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", as.character(as.numeric(weighted)),
      "-replace_outliers_", as.character(as.numeric(replace_by_centiles)), 
      "-infer_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
      "-infer_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
      "-remove_primary_", as.character(as.numeric(remove_primary_goods)),
      ".csv"
    )
  
  df <- 
    here("data", "intermediary", versions$trade_price_V, filename) |>
    read_csv(show_col_types = FALSE) |>
    as_tibble() |>
    mutate(
      source_data = source_data, 
      sector_classification = sector_classification, 
      lb_percentile_filter = lb_percentile_filter,
      ub_percentile_filter = ub_percentile_filter,
      weighted = weighted,
      replace_by_centiles = replace_by_centiles,
      infer_missing_uv_before = infer_missing_uv_before,
      infer_missing_uv_after = infer_missing_uv_after,
      remove_primary_goods = remove_primary_goods
    ) 
  
  return(df)
  
}

all_results_df <-
  pmap(list_methods, open_csv_new) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_by_centiles",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after",
                   "remove_primary_goods"))  

t__all_methods_df <-
  all_results_df |>
  filter(aggregation_level == "year") |>
  mutate(
    .by = method_name,
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  rename(trade_value = v) |> 
  select(-c(manuf, stade, isic, group_id)) |>
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())

write_csv(
  t__all_methods_df, 
  here(
    "data", "final", versions$trade_price_V, "all_methods", 
    paste0("t--price_indices_all_methods--cepii_nomenclature.csv"))
  )

label_data <-
  t__all_methods_df |>
  filter(source_data != "wtfc") |>
  group_by(method_name) |>
  slice_max(order_by = t, n = 1) 

graph <- 
  t__all_methods_df |> 
  filter(source_data != "wtfc") |>
  ggplot(aes(x = t, y = price_index, 
             colour = method_name, shape = method_name)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = method_name), data = label_data) +
  geom_hline(yintercept = 100) +
  labs(
    x = element_blank(),
    y = paste0("Price index (100 = ", first_year, ")")
  ) +
  theme(
    legend.position = "null"
  ) 
plot(graph)

ggsave(
  filename = here(
    "output",
    "figures",
    paste0("price_index_over_time__different_methodologies.pdf")
  ),
  width = 7, height = 5
) 

graph <- 
  t__all_methods_df |> 
  filter(source_data != "wtfc") |>
  ggplot(aes(x = t, y = delta_ln_price_index, 
             colour = method_name, shape = method_name)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_label_repel(aes(label = method_name), data = label_data) +
  geom_hline(yintercept = 0) +
  labs(
    x = element_blank(),
    y = paste0("Price index (yearly log change)")
  ) +
  theme(
    legend.position = "null"
  ) 
plot(graph)


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


t_isic__all_methods_df <- 
  all_results_df |>
  filter(aggregation_level == "year x sector") |>
  select(method_name, isic, t, everything()) |>
  arrange(method_name, isic, t) |>
  mutate(
    .by = c(isic, method_name),
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  rename(trade_value = v,
         branch = isic) |> 
  select(-c(stade, manuf, cumul_delta_ln_price_index, group_id)) |>
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())

write_csv(
  t_isic__all_methods_df, 
  here("data", "final", versions$trade_price_V, "all_methods", 
       "t-branch--price_indices_all_methods--cepii_nomenclature.csv")
  )

#  By stade --------------------------------------------------------------

t_stade__all_methods_df <- 
  all_results_df |>
  filter(aggregation_level == "year x stade") |>
  mutate(
    .by = c(stade, method_name), 
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  rename(trade_value = v) |> 
  select(-c(isic, manuf, cumul_delta_ln_price_index, group_id)) |>
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())

write_csv(
  t_stade__all_methods_df, 
  here("data", "final", versions$trade_price_V, "all_methods", 
       "t-stade--price_indices_all_methods--cepii_nomenclature.csv")
  )

list_stades <- 
  hs_stade_df |>
  distinct(stade) |>
  arrange(stade) |>
  pull()
names(list_stades) <- list_stades
make_graph_price_index_compare_methodologies <- function(stade_select){
  graph <- 
    graph_df |> 
    filter(source_data != "wtfc") |>
    filter(stade == stade_select)|>
    ggplot(aes(x = t, y = price_index, 
               colour = method_name, shape = method_name)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 100) +
    labs(
      title = stade_select,
      x = element_blank(),
      y = paste0("Price index (100 = ", first_year, ")")
    ) +
    theme(
      legend.title = element_blank()
    ) 
}
graph_df <- t_stade__all_methods_df
list_graphs <-
  map(list_stades, make_graph_price_index_compare_methodologies)
pdf(
  file = here(
    "output", "figures",
    paste0("price_index_by_stade_over_time__different_methodologies.pdf")
  ),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# By ISIC x stade -------------------------------------------------------


t_isic_stade__all_methods_df <- 
  all_results_df |>
  filter(aggregation_level == "year x sector x stade") |>
  mutate(
    .by = c(isic, stade, method_name),
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  rename(trade_value = v,
         branch = isic) |> 
  select(-c(manuf, cumul_delta_ln_price_index, group_id)) |>
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())
write_csv(
  t_isic_stade__all_methods_df, 
  here("data", "final", versions$trade_price_V, "all_methods", 
       "t-isic-stade--price_indices_all_methods--cepii_nomenclature.csv")
  )


graph <- 
  here("data", "final", versions$trade_price_V, "all_methods", 
       "t-stade--price_indices_all_methods--cepii_nomenclature.csv") |>
  read_csv(show_col_types = FALSE) |>
  filter(stade != "6_NEC") |>
  filter(method_name == "both, manuf") |>
  ggplot(aes(x = t, y = trade_volume_base100, colour = stade)) +
  geom_line() +
  geom_point()
plot(graph)
graph <- 
  here("data", "final", versions$trade_price_V, "all_methods", 
       "t-stade--price_indices_all_methods--cepii_nomenclature.csv") |>
  read_csv(show_col_types = FALSE) |>
  filter(stade != "6_NEC") |> 
  filter(method_name == "both, manuf") |>
  mutate(.by = stade, 
         price_growth = exp(delta_ln_price_index),
         volume_growth = 
           trade_volume_base100 / lag(trade_volume_base100, order_by = t)) |>
  ggplot(aes(x = t, y = delta_ln_price_index, colour = stade, shape = stade)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0) +
  labs(
    x = element_blank(),
    y = "Yearly price index growth (log change)"
  ) +
  theme(
    text = element_text(size = 12, family = "serif")
  ) 
  
plot(graph) 
ggsave(
  filename = here(
    "output",
    "figures",
    paste0("growth_rate_volume_over_time__by_stade.pdf")
  ),
  width = 7, height = 5
) 
