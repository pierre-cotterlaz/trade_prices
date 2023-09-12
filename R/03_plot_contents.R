library(tidyverse)
library(here)
theme_set(theme_bw())

# * Lists of possible methodologies ---------------------------------------

list_methods <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, 
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE, 
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE, 
    0.05                 , 0.95                 , FALSE    , TRUE             , TRUE                    , FALSE
  )

dict_method_names <- 
  tribble(~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~infer_missing_uv, ~method_name,
          0, 1, FALSE, FALSE, "No filtering",
          0.05, 0.95, FALSE, FALSE, "5%, unweighted",
          0.05, 0.95, TRUE, FALSE, "5%, weighted",
          0.05, 0.95, FALSE, TRUE, "5%, unweighted, infer missing uv")

dict_method_names <-  
  tribble(
    ~lb_percentile_filter, ~ub_percentile_filter, ~weighted, ~replace_outliers, ~infer_missing_uv_before, ~infer_missing_uv_after, ~method_name,
    0                    , 1                    , FALSE    , FALSE            , FALSE                   , FALSE                  , "Raw data",
    0.05                 , 0.95                 , FALSE    , TRUE             , FALSE                   , FALSE                  , "5% unweighted",
    0.05                 , 0.95                 , TRUE     , FALSE            , FALSE                   , FALSE                  , "5% weighted",
    0.05                 , 0.95                 , FALSE    , TRUE             , TRUE                    , FALSE                  , "5% unweighted, infer missing uv"
  )

first_year <- 2017

# * Over time -------------------------------------------------------------

open_csv <- 
  function(lb_percentile_filter, 
           ub_percentile_filter, 
           weighted, 
           replace_outliers, 
           infer_missing_uv_before, 
           infer_missing_uv_after) {
    # Intermediary dataset from 02_compute_price_indices
    filen <- paste0(
      "t--delta_ln_price_index--", 
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
    y = paste0("Trade (100 = ", first_year, ")")
  ) +
  theme(
    legend.position = "null"
  ) 
plot(graph)

ggsave(
  filename = here("output",
              "figures",
              "price_index_over_time__different_methodologies.pdf"),
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

lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
weighted <- FALSE
infer_missing_uv <- FALSE

filen <- paste0(
  "t-stade--delta_ln_price_index--", 
  "lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", weighted,
  "-infer_missing_uv_", infer_missing_uv, ".csv")
file <- here("data", "intermediary", filen)
graph_df <- 
  read_csv(file) |> 
  as_tibble() |>
  group_by(stade) |>
  mutate(
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

indicator_dict <- 
  tribble(
    ~type, ~indicator_name,
    "trade_value_base100", "Value",
    "trade_volume_base100", "Volume")

library(ggrepel)
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
  file = here("output",
              "figures",
              "trade_by_stade_over_time.pdf"),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_stades),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# Compare different methodologies

open_csv <- 
  function(lb_percentile_filter, ub_percentile_filter, weighted, infer_missing_uv){
    filen <- paste0(
      "t-stade--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted,
      "-infer_missing_uv_", infer_missing_uv, ".csv")
    file <- here("data", "intermediary", filen)
    df <- 
      read_csv(file) |>
      as_tibble() |>
      mutate(lb_percentile_filter = lb_percentile_filter,
             ub_percentile_filter = ub_percentile_filter,
             weighted = weighted,
             infer_missing_uv = infer_missing_uv) 
    return(df)
  }

graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c(
              "lb_percentile_filter", 
              "ub_percentile_filter", 
              "weighted",
              "infer_missing_uv")) |>
  group_by(stade, method_name) |>
  mutate(
    price_index = price_index * 100,
    trade_value_base100 = (v / v[t == first_year]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

make_graph_stade <- function(stade_select){
  graph <- 
    graph_df |> 
    filter(stade == stade_select) |>
    ggplot(aes(x = t, y = price_index, colour = method_name)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 100) +
    labs(
      title = stade_select,
      x = element_blank(),
      y = "Price index (100 = )"
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
  file = here("output",
              "figures",
              "price_index_by_stade_over_time__different_methodologies.pdf"),
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

filen <- paste0(
  "t-isic_2d--delta_ln_price_index--", 
  "lb_perc_", lb_percentile_filter, 
  "-ub_perc_", ub_percentile_filter,
  "-weighted_", weighted,
  "-infer_missing_uv_", infer_missing_uv, ".csv"
)
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
    geom_hline(yintercept = 100) +
    labs(
      title = isic_2d_name_str,
      x = element_blank(),
      y = "Trade (100 = 2017)"
    ) +
    theme(
      legend.title = element_blank()
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
              "trade_by_isic_over_time.pdf"),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_graphs[[i]])
)
dev.off()

# Compare different methodologies

open_csv <- 
  function(lb_percentile_filter, ub_percentile_filter, weighted, infer_missing_uv){
    filen <- paste0(
      "t-isic_2d--delta_ln_price_index--", 
      "lb_perc_", lb_percentile_filter, 
      "-ub_perc_", ub_percentile_filter,
      "-weighted_", weighted,
      "-infer_missing_uv_", infer_missing_uv, ".csv")
    file <- here("data", "intermediary", filen)
    df <- 
      read_csv(file) |>
      as_tibble() |>
      mutate(lb_percentile_filter = lb_percentile_filter,
             ub_percentile_filter = ub_percentile_filter,
             weighted = weighted,
             infer_missing_uv = infer_missing_uv) 
    return(df)
  }

graph_df <-
  pmap(list_methods, open_csv) |>
  list_rbind() |>
  left_join(dict_method_names,
            by = c("lb_percentile_filter",
                   "ub_percentile_filter",
                   "weighted",
                   "infer_missing_uv")) |>
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
      y = "Price index (100 = 2017)"
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
  file = here("output",
              "figures",
              "price_index_by_isic_2d_over_time__different_methodologies.pdf"),
  width = 7, height = 5, onefile = TRUE
)
walk(
  1:length(list_isic),
  \(i) plot(list_graphs[[i]])
)
dev.off()
