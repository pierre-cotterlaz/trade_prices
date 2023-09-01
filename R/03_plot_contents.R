theme_set(theme_bw())


# * By stade over time ----------------------------------------------------


filen <- paste0("t-stade--delta_ln_price_index.csv")
file <- here("data", filen)
graph_df <- 
  read_csv(file) |> 
  group_by(stade) |>
  mutate(
    trade_value_base100 = (v / v[t == 2017]) * 100,
    trade_volume_base100 = trade_value_base100 / price_index) |>
  ungroup()

make_graph_stade <- function(stade_select){
  graph <- 
    graph_df |>
    select(t, stade, starts_with("trade")) |>
    pivot_longer(cols = starts_with("trade"), names_to = "type", values_to = "trade") |>
    filter(stade == stade_select) |>
    ggplot(aes(x = t, y = trade, colour = type)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 100) +
    labs(
      title = stade_select,
      x = element_blank(),
      y = "Trade (100 = 2017)"
    ) +
    theme(
      legend.title = element_blank()
    )
  return(graph)
}

list_stades <- 
  graph_df |>
  distinct(stade) |>
  pull()
names(list_stades) <- list_stades
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


# * By ISIC over time -----------------------------------------------------

filen <- paste0("t-isic_2d--delta_ln_price_index.csv")
file <- here("data", filen)
graph_df <- 
  read_csv(file) |> 
  group_by(isic) |>
  mutate(
    trade_value_base100 = (v / v[t == 2017]) * 100,
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
    pivot_longer(cols = starts_with("trade"), names_to = "type", values_to = "trade") |>
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

