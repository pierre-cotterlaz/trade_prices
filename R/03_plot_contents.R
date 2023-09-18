library(tidyverse)
library(glue)
library(ggrepel)
library(here)
theme_set(theme_bw())

# * Lists of possible methodologies ---------------------------------------

list_methods <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

list_methods <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE
  )

dict_method_names <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~method_name,
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE                  , "Raw data",
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE                  , "5% unweighted",
    0.05                 , 0.95                 , TRUE     , TRUE             , FALSE                   , FALSE                  , "5% weighted",
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , TRUE                   , "5% unweighted, infer missing uv"
  )

# * Over time -------------------------------------------------------------

open_csv <- 
  function(source_data, 
           sector_classification,
           lb_percentile_filter, 
           ub_percentile_filter, 
           weighted, 
           replace_outliers, 
           infer_missing_uv_before, 
           infer_missing_uv_after) {
    # Intermediary dataset from 02_compute_price_indices
    filen <- paste0(
      "t--delta_ln_price_index--", 
      "HS_", versions$HS,
      "-source_data_", source_data, 
      "-sectors_", sector_classification, 
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", as.character(as.numeric(weighted)),
      "-replace_outliers_", as.character(as.numeric(replace_outliers)), 
      "-infer_missing_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
      "-infer_missing_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
      ".csv")
    file <- here("data", "intermediary", filen)
    df <- 
      read_csv(file) |>
      as_tibble() |>
      mutate(source_data = source_data, 
             sector_classification = sector_classification, 
             lb_percentile_filter = lb_percentile_filter,
             ub_percentile_filter = ub_percentile_filter,
             weighted = weighted,
             replace_outliers = replace_outliers,
             infer_missing_uv_before = infer_missing_uv_before,
             infer_missing_uv_after = infer_missing_uv_after) 
    return(df)
  }

list_methods <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"
  )

dict_method_names <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification, ~method_name,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "5% unweighted"
  )

graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

label_data <-
  graph_df |>
  group_by(method_name) |>
  slice_max(order_by = t, n = 1) 

graph <- 
  graph_df |> 
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
    glue("price_index_over_time__different_methodologies__hs{versions$HS}_source_data_wtfc.pdf")
  ),
  width = 7, height = 5
) 

# * By stade over time ----------------------------------------------------

# List of stades, to loop over
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

open_csv <- 
  function(source_data, 
           sector_classification,
           lb_percentile_filter, 
           ub_percentile_filter, 
           weighted, 
           replace_outliers, 
           infer_missing_uv_before, 
           infer_missing_uv_after) {
    # Intermediary dataset from 02_compute_price_indices
    filen <- paste0(
      "t-stade--delta_ln_price_index--", 
      "HS_", versions$HS,
      "-source_data_", source_data, 
      "-sectors_", sector_classification, 
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", as.character(as.numeric(weighted)),
      "-replace_outliers_", as.character(as.numeric(replace_outliers)), 
      "-infer_missing_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
      "-infer_missing_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
      ".csv")
    file <- here("data", "intermediary", filen)
    df <- 
      read_csv(file) |>
      as_tibble() |>
      mutate(source_data = source_data, 
             sector_classification = sector_classification, 
             lb_percentile_filter = lb_percentile_filter,
             ub_percentile_filter = ub_percentile_filter,
             weighted = weighted,
             replace_outliers = replace_outliers,
             infer_missing_uv_before = infer_missing_uv_before,
             infer_missing_uv_after = infer_missing_uv_after) 
    return(df)
  }

# list_methods <- 
#   tribble(
#     ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification,
#     0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic",
#     0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"
#   )
# 
# dict_method_names <- 
#   tribble(
#     ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification, ~method_name,
#     0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "5% weighted",
#     0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "5% unweighted"
#   )

list_methods <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"      , "isic", 
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"      , "isic", 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"     
  )

dict_method_names <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data, ~sector_classification, ~method_name,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"      , "isic"                , "baci 5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"      , "isic"                , "baci 5% unweighted",
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "wtfc 5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"      , "isic"                , "wtfc 5% unweighted"
  )

graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("source_data", 
                   "sector_classification",
                   "lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(stade, method_name) |>
  filter(!is.na(price_index)) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup()

make_graph_price_index_over_time <- 
  function(
    stade_select,
    method_select){
  label_data <-
    graph_df |>
    filter(method_name == method_select) |>
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
    filter(method_name == method_select) |>
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
  list_stades |>
  map(\(x) make_graph_price_index_over_time(x, method_select = "5% weighted"))
list_graphs[[1]]
pdf(
  file = here("output",
              "figures",
              glue("trade_by_stade_over_time__hs{versions$HS}_source_data_{source_data}.pdf")),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()
  

make_graph_trade_volume_all_stades <- function(method_name_select){
  label_data <- 
    graph_df |>
    filter(method_name == method_name_select) |>
    filter(!is.na(price_index)) |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(type == "trade_volume_base100") |>
    filter(stade != "6_NEC") |>
    group_by(stade) |>
    slice_max(order_by = t, n = 1) 
  graph <- 
    graph_df |>
    filter(method_name == method_name_select) |>
    filter(!is.na(price_index)) |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(type == "trade_volume_base100") |>
    filter(stade != "6_NEC") |>
    ggplot(aes(x = t, y = trade, colour = stade, shape = stade)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = stade), data = label_data) +
    geom_hline(yintercept = 100) +
    labs(
      title = method_name_select,
      x = element_blank(),
      y = paste0("Trade volume (100 = ", first_year, ")")
    ) +
    theme(
      legend.position = "null"
    )
  return(graph)
}
method_name <- "baci 5% weighted"
graph <- 
  make_graph_trade_volume_all_stades(method_name)
list_method_names <- 
  dict_method_names |>
  distinct(method_name) |>
  pull()
list_graphs <-
  map(list_method_names, make_graph_trade_volume_all_stades)
list_graphs[[1]]
pdf(
  file = here(
    "output", "figures",
    glue("trade_volume_over_time__by_stade__different_methodologies__hs{versions$HS}.pdf")
  ),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_method_names),
  \(i) plot(list_graphs[[i]])
)
dev.off()

make_graph_price_index_all_stades <- function(method_name_select){
  label_data <- 
    graph_df |>
    filter(method_name == method_name_select) |>
    filter(!is.na(price_index)) |>
    select(t, stade, starts_with("price_index")) |>
    pivot_longer(cols = starts_with("price_index"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(type == "price_index") |>
    filter(stade != "6_NEC") |>
    group_by(stade) |>
    slice_max(order_by = t, n = 1) 
  graph <- 
    graph_df |>
    filter(!is.na(price_index)) |>
    select(t, stade, starts_with("price_index")) |>
    pivot_longer(cols = starts_with("price_index"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(type == "price_index") |>
    filter(stade != "6_NEC") |>
    ggplot(aes(x = t, y = trade, colour = stade, shape = stade)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = stade), data = label_data) +
    geom_hline(yintercept = 100) +
    labs(
      title = method_name_select,
      x = element_blank(),
      y = paste0("Trade volume (100 = ", first_year, ")")
    ) +
    theme(
      legend.position = "null"
    )
  return(graph)
}
list_graphs <-
  map(list_method_names, make_graph_trade_volume_all_stades)
list_graphs[[1]]
pdf(
  file = here(
    "output", "figures",
    glue("price_index_over_time__by_stade__different_methodologies__hs{versions$HS}.pdf")
  ),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_method_names),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# Compare different methodologies

make_graph_price_index_compare_methodologies <- function(stade_select){
  graph <- 
    graph_df |> 
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
stade_select <- "3_PC"
graph <- make_graph_stade(stade_select)
plot(graph)

list_graphs <-
  map(list_stades, make_graph_stade)
list_graphs[[1]]

pdf(
  file = here(
    "output", "figures",
    glue("price_index_by_stade_over_time__different_methodologies__hs{versions$HS}.pdf")
  ),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()


make_graph_trade_volume_compare_methodologies 

graph <- 
  graph_df |> 
  ggplot(aes(x = t, y = price_index, colour = method_name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100) +
  labs(
    x = element_blank(),
    y = "Price index (100 = )"
  ) +
  theme(
    legend.title = element_blank()
  ) +
  facet_wrap(vars(stade))
plot(graph)

# * By ISIC over time -----------------------------------------------------

filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "HS_", versions$HS,
  "-lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", weighted,
  "-replace_outliers_", replace_outliers, 
  "-infer_missing_uv_before_", infer_missing_uv_before, 
  "-infer_missing_uv_after_", infer_missing_uv_after, ".csv")
file <- here("data", "intermediary", filen)
graph_df <- 
  read_csv(file) |> 
  group_by(isic) |>
  mutate(
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

make_graph_isic <- function(isic_select){
  isic_2d_name_str <- 
    isic_2d_dict |> 
    filter(isic_2d == isic_select) |>
    pull(isic_2d_name)
  label_data <-
    graph_df |>
    select(t, isic, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type", 
                 values_to = "trade") |>
    filter(isic == isic_select) |>
    left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
    left_join(indicator_dict, by = "type") |>
    group_by(indicator_name) |>
    slice_max(order_by = t, n = 1) 

  graph <- 
    graph_df |>
    select(t, isic, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type", 
                 values_to = "trade") |>
    filter(isic == isic_select) |>
    left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
    ggplot(aes(x = t, y = trade, colour = type)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = indicator_name), data = label_data) +
    geom_hline(yintercept = 100) +
    labs(
      title = isic_2d_name_str,
      x = element_blank(),
      y = glue("Trade (100 = {first_year})")
    ) +
    theme(
      legend.position = "null"
    )
  
  return(graph)
}

isic_select <- "01"
list_isic <- 
  graph_df |>
  distinct(isic) |>
  pull()
list_graphs <-
  map(list_isic, make_graph_isic)
list_graphs[[1]]

pdf(
  file = here("output", "figures",
              glue("trade_by_isic_over_time__hs{versions$HS}.pdf")),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# Compare different methodologies

open_csv <- 
  function(lb_percentile_filter, 
           ub_percentile_filter, 
           weighted, 
           replace_outliers, 
           infer_missing_uv_before, 
           infer_missing_uv_after) {
    filen <- paste0(
      "t-isic_2d--delta_ln_price_index--", 
      "HS_", versions$HS,
      "-lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted,
      "-replace_outliers_", replace_outliers, 
      "-infer_missing_uv_before_", infer_missing_uv_before, 
      "-infer_missing_uv_after_", infer_missing_uv_after, ".csv")
    file <- here("data", "intermediary", filen)
    df <- 
      read_csv(file) |>
      as_tibble() |>
      mutate(lb_percentile_filter = lb_percentile_filter,
             ub_percentile_filter = ub_percentile_filter,
             weighted = weighted,
             replace_outliers = replace_outliers,
             infer_missing_uv_before = infer_missing_uv_before,
             infer_missing_uv_after = infer_missing_uv_after) 
    return(df)
  }

graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("lb_percentile_filter", 
                   "ub_percentile_filter", 
                   "weighted",
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(isic, method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

make_graph_isic <- function(isic_select){
  isic_2d_name_str <- 
    isic_2d_dict |> 
    filter(isic_2d == isic_select) |>
    pull(isic_2d_name)
  
  graph <- 
    graph_df |>
    filter(isic == isic_select) |>
    left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
    ggplot(aes(x = t, 
               y = price_index, 
               colour = method_name,
               shape = method_name)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 100) +
    labs(
      title = isic_2d_name_str,
      x = element_blank(),
      y = glue("Price index (100 = {first_year})")
    ) +
    theme(
      legend.title = element_blank()
    )
  return(graph)
}
graph <- make_graph_isic(28)
plot(graph)

list_isic <- 
  graph_df |>
  distinct(isic) |>
  pull()
list_graphs <-
  map(list_isic, make_graph_isic)
list_graphs[[1]]

pdf(
  file = here(
    "output",
    "figures",
    glue("price_index_by_isic_2d_over_time__different_methodologies__hs{versions$HS}.pdf"
  )),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_graphs[[i]])
)
dev.off()
