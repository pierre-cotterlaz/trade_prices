
library(tidyverse)
library(glue)
library(ggrepel)
library(here)
theme_set(theme_bw())

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
  filter(manuf == 1) |>
  summarize(.by = branch,
            nb_obs = n_distinct(t, i, j, k)) |>
  mutate(sh_obs = nb_obs / sum(nb_obs)) |>
  arrange(-sh_obs)

branch__branch_for_price <- 
  share_obs_by_branch |>
  mutate(branch_for_price = case_when(
    sh_obs < 0.01 ~ "NEC",
    .default = branch
  )) |>
  select(branch, branch_for_price) |>
  arrange(branch) 

hs_branch_for_prices <- 
  hs_branch |>
  left_join(branch__branch_for_price, by = "branch") |>
  select(k, share, branch_for_price, manuf)
filen <- paste0("HS", versions$HS, "-branches_for_prices--share.csv")
file <- here("data", "nomenclatures", filen)
write_csv(hs_branch_for_prices, file)

rm(hs_branch, share_obs_by_branch, branch__branch_price)



# * Compute price indices -------------------------------------------------

filen <- paste0("HS", versions$HS, "-branches_for_prices--share.csv")
file <- here("data", "nomenclatures", filen)
hs_branch_for_prices <- 
  read_csv(file) |>
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
  filter(source_data == "both" & weighted == TRUE)

# pwalk(list_arguments, remove_outliers)

filen <- paste0("t-i-j-k--BACI_with_group_variables--HS", 
                versions$HS, "-V", versions$baci_V, 
                "-cepii_nomenclature.fst")
file <- here("data", "intermediary", filen)
raw_baci_with_group_variables <-
  read_fst(file) |>
  filter(t >= first_year) |>
  filter(manuf == 1) |>
  mutate(across(c(t_isic, t_stade, t_isic_stade),
                ~ as.character(.x)))

sector_classification <- "cepii"
pwalk(list_arguments, save_csv_files_price_index)

# * Plot graphs -----------------------------------------------------------

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- FALSE
replace_outliers <- TRUE
infer_missing_uv_before <- FALSE
infer_missing_uv_after <- FALSE
end_of_filenames <- paste0(
  "HS_", versions$HS,
  "-sector_classification", sector_classification, 
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", weighted,
  "-replace_outliers_", replace_outliers, 
  "-infer_missing_uv_before_", infer_missing_uv_before, 
  "-infer_missing_uv_after_", infer_missing_uv_after, ".csv"
)
filen <- paste0(
  "t--delta_ln_price_index--", 
  end_of_filenames)
file <- here("data", "intermediary", filen)
message(file.info(file)$mtime)
graph_df <- 
  read_csv(file) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()
graph <- 
  graph_df |> 
  ggplot(aes(x = t, y = price_index)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100) +
  labs(
    x = element_blank(),
    y = paste0("Price index (100 = ", first_year, ")")
  ) +
  theme(
    legend.position = "null"
  ) 
plot(graph)

list_stades <- 
  hs_stade_df |>
  distinct(stade) |>
  arrange(stade) |>
  pull()
names(list_stades) <- list_stades
indicator_dict <- 
  tribble(
    ~type, ~indicator_name,
    "trade_value_base100", "Value",
    "trade_volume_base100", "Volume")
filen <- paste0(
  "t-stade--delta_ln_price_index--",
  end_of_filenames)
file <- here("data", "intermediary", filen)
graph_df <- 
  read_csv(file) |> 
  as_tibble() |>
  group_by(stade) |>
  mutate(
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

make_graph_stade <- function(stade_select){
  label_data <-
    graph_df |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(stade == stade_select) |>
    left_join(indicator_dict, by = "type") |>
    group_by(indicator_name) |>
    slice_max(order_by = t, n = 1) 
  
  graph <- 
    graph_df |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(stade == stade_select) |>
    left_join(indicator_dict, by = "type") |>
    ggplot(aes(x = t, y = trade, 
               colour = indicator_name, shape = indicator_name)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = indicator_name), data = label_data) +
    geom_hline(yintercept = 100) +
    labs(
      title = stade_select,
      x = element_blank(),
      y = paste0("Trade (100 = ", first_year, ")")
    ) +
    theme(
      legend.position = "null"
    )
  return(graph)
}

list_graphs <-
  map(list_stades, make_graph_stade)
list_graphs[[1]]
pdf(
  file = here(
    "output",
    "figures",
    glue("trade_by_stade_over_time__hs{versions$HS}__cepii_nomenclature.pdf")),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()
