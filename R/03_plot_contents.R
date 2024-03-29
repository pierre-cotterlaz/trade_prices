library(tidyverse)
library(glue)
library(ggrepel)
library(here)
theme_set(theme_bw())

# * Lists of possible methodologies ---------------------------------------

list_methods <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data    , ~sector_classification,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic", 
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic", 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "both_aggregate", "isic", 
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "both_aggregate", "isic", 
    0.075                , 0.925                , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic", 
    0.075                , 0.925                , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic", 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"          , "isic",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"          , "isic"     
  )

dict_method_names <- 
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~source_data    , ~sector_classification, ~method_name,
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic"                , "baci 5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic"                , "baci 5% unweighted",
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "both_aggregate", "isic"                , "both 5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "both_aggregate", "isic"                , "both 5% unweighted",
    0.075                , 0.925                , TRUE     , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic"                , "baci 7.5% weighted",
    0.075                , 0.925                , FALSE    , FALSE            , FALSE                   , FALSE                  , "baci"          , "isic"                , "baci 7.5% unweighted",
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "wtfc"          , "isic"                , "wtfc 5% weighted",
    0.05                 , 0.95                 , FALSE    , FALSE            , FALSE                   , FALSE                  , "wtfc"          , "isic"                , "wtfc 5% unweighted"
  )

# * Over time -------------------------------------------------------------

open_csv <- function(
    source_data, 
    sector_classification,
    lb_percentile_filter, 
    ub_percentile_filter, 
    weighted, 
    replace_outliers, 
    infer_missing_uv_before, 
    infer_missing_uv_after){
  
  filen <- paste0(
    aggregation_level, "--delta_ln_price_index--", 
    "HS_", versions$HS,
    "-source_data_", source_data, 
    "-sectors_", sector_classification, 
    "-lb_perc_", lb_percentile_filter, 
    "-ub_perc_", ub_percentile_filter,
    "-weighted_", as.character(as.numeric(weighted)),
    "-replace_outliers_", as.character(as.numeric(replace_outliers)), 
    "-infer_uv_before_", as.character(as.numeric(infer_missing_uv_before)), 
    "-infer_uv_after_", as.character(as.numeric(infer_missing_uv_after)), 
    ".csv")
  file <- here("data", "intermediary", versions$trade_price_V, filen)
  
  if (file.exists(file)){
    df <- 
      read_csv(file, show_col_types = FALSE) |>
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
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index * 100) |>
  ungroup() |>
  filter(!is.na(price_index)) |>
  rename(trade_value = v) |> 
  relocate(trade_value, delta_ln_price_index, price_index, .after = last_col()) |>
  relocate(trade_value_base100, trade_volume_base100, .after = last_col())

write_csv(
  graph_df,
  here("data", "final", versions$trade_price_V, "all_methods", 
       paste0("t--price_indices_all_methods.csv"))
)

label_data <-
  graph_df |>
  filter(source_data != "wtfc") |>
  group_by(method_name) |>
  slice_max(order_by = t, n = 1) 

graph <- 
  graph_df |> 
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
    paste0("price_index_over_time__different_methodologies__hs", 
           versions$HS, ".pdf")
  ),
  width = 7, height = 5
) 

label_data <-
  graph_df |>
  filter(source_data != "wtfc") |>
  group_by(method_name) |>
  slice_max(order_by = t, n = 1) 
graph <- 
  graph_df |> 
  filter(source_data != "wtfc") |>
  ggplot(aes(x = t, y = trade_volume_base100, 
             colour = method_name, shape = method_name)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = method_name), data = label_data) +
  geom_hline(yintercept = 100) +
  labs(
    x = element_blank(),
    y = paste0("Trade volume (100 = ", first_year, ")")
  ) +
  theme(
    legend.position = "null"
  ) 
plot(graph)
ggsave(
  filename = here(
    "output",
    "figures",
    paste0("trade_volume_over_time__different_methodologies.pdf")
  ),
  width = 7, height = 5
) 

# * By manuf over time ----------------------------------------------------

aggregation_level <- "t-manuf"
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
  group_by(manuf, method_name) |>
  filter(!is.na(price_index)) |>
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
  here("data", "final", versions$trade_price_V, "all_methods", 
       paste0("t-manuf--price_indices_all_methods.csv"))
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
    "trade_volume_base100", "Volume"
    )

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
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(stade, method_name) |>
  filter(!is.na(price_index)) |>
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
  here("data", "final", versions$trade_price_V, "all_methods", 
       paste0("t-stade--price_indices_all_methods.csv"))
  )

plot_stade_trade_over_time <- function(
    stade_select,
    method_select){
  
  label_data <-
    graph_df |>
    filter(source_data != "wtfc") |>
    filter(method_name == method_select) |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type",
                 values_to = "trade") |>
    filter(type %in% c("trade_value_base100", "trade_volume_base100")) |>
    filter(stade == stade_select) |>
    left_join(indicator_dict, by = "type") |>
    group_by(indicator_name) |>
    slice_max(order_by = t, n = 1)

  graph <-
    graph_df |>
    filter(source_data != "wtfc") |>
    filter(method_name == method_select)  |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type",
                 values_to = "trade") |>
    filter(type %in% c("trade_value_base100", "trade_volume_base100")) |>
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
  map(\(x) plot_stade_trade_over_time(
    x, 
    method_select = "both 5% weighted"
    ))
list_graphs[[1]]
pdf(
  file = here("output",
              "figures",
              paste0("trade_by_stade_over_time__hs",
                     versions$HS, ".pdf")),
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

list_method_names <- 
  dict_method_names |>
  distinct(method_name) |>
  pull()
list_graphs <-
  map(list_method_names, make_graph_trade_volume_all_stades)
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
    filter(method_name == method_name_select) |>
    filter(!is.na(price_index)) |>
    select(t, stade, starts_with("price_index")) |>
    pivot_longer(cols = starts_with("price_index"), 
                 names_to = "type", 
                 values_to = "trade") |>
    filter(type == "price_index") |>
    filter(stade != "6_NEC")  |>
    ggplot(aes(x = t, y = trade, colour = stade, shape = stade)) +
    geom_line() +
    geom_point() +
    geom_label_repel(aes(label = stade), data = label_data) +
    geom_hline(yintercept = 100) +
    labs(
      title = method_name_select,
      x = element_blank(),
      y = paste0("Price index (100 = ", first_year, ")")
    ) +
    theme(
      legend.position = "null"
    )
  return(graph)
}
list_graphs <-
  map(list_method_names, make_graph_price_index_all_stades)
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
list_graphs <-
  map(list_stades, make_graph_price_index_compare_methodologies)
pdf(
  file = here(
    "output", "figures",
    paste0("price_index_by_stade_over_time__different_methodologies__hs", 
           versions$HS, ".pdf")
  ),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()

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
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(isic, method_name) |>
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
  here("data", "final", versions$trade_price_V, "all_methods", 
       paste0("t-isic--price_indices_all_methods.csv"))
  )

plot_isic_trade_over_time <- function(isic_select,
                                      method_select){
  isic_2d_name_str <- 
    isic_2d_dict |> 
    filter(isic_2d == isic_select) |>
    pull(isic_2d_name)
  label_data <-
    graph_df |>
    filter(method_name == method_select) |>
    select(t, isic, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type", 
                 values_to = "trade") |>
    filter(isic == isic_select) |>
    filter(type %in% c("trade_value_base100", "trade_volume_base100")) |>
    left_join(isic_2d_dict, by = c("isic" = "isic_2d")) |>
    left_join(indicator_dict, by = "type") |>
    group_by(indicator_name) |>
    slice_max(order_by = t, n = 1) 

  graph <- 
    graph_df |>
    filter(method_name == method_select) |>
    select(t, isic, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"),
                 names_to = "type", 
                 values_to = "trade") |>
    filter(isic == isic_select) |>
    filter(type %in% c("trade_value_base100", "trade_volume_base100")) |>
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

isic_select <- "21"
method_select <- "both 5% weighted"

list_isic <- 
  graph_df |>
  distinct(isic) |>
  filter(!isic %in% c("NEC", "NED")) |>
  pull()
list_graphs <-
  list_isic |>
  map(\(x) plot_isic_trade_over_time(
    x, 
    method_select = "both 5% weighted"
  ))
list_graphs[[1]]
pdf(
  file = here("output",
              "figures",
              paste0("trade_by_isic_over_time.pdf")),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_isic[[i]])
)
dev.off()

# Compare different methodologies

make_graph_isic <- function(isic_select){
  isic_2d_name_str <- 
    isic_2d_dict |> 
    filter(isic_2d == isic_select) |>
    pull(isic_2d_name)
  
  graph <- 
    graph_df |>
    filter(source_data != "wtfc") |>
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
    paste0("price_index_by_isic_2d_over_time__different_methodologies__hs", 
           versions$HS, ".pdf")),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# * By ISIC x stade -------------------------------------------------------

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
                   "replace_outliers",
                   "infer_missing_uv_before",
                   "infer_missing_uv_after")) |>
  group_by(isic, stade, method_name) |>
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
  here("data", "final", versions$trade_price_V, "all_methods", paste0("t-isic-stade--price_indices_all_methods.csv"))
  )