

lagged_baci_df <- 
  baci_df |>
  mutate(t = t + 1) |>
  select(t, i, j, k, v, uv) |>
  rename(l_v = v, l_uv = uv)

# Join with ISIC_2d and BEC 
df1 <- 
  baci_df |>
  left_join(lagged_baci_df, by = c("t", "i", "j", "k")) |> 
  # Drop if uv missing in t or t-1 since we cannot compute a time variation in this case
  filter(!(is.na(l_uv) | is.na(uv))) |> 
  mutate(delta_ln_uv = log(uv) - log(l_uv)) 

# Remove outliers 
lb_percentile_filter <- 0.05
ub_percentile_filter <- 0.95
df2 <- 
  df1 |>
  mutate(
    t_k = paste(t, k)
    ) |>
  group_by(t, k) |>
  filter(between(
    delta_ln_uv, 
    quantile(delta_ln_uv, lb_percentile_filter),
    quantile(delta_ln_uv, ub_percentile_filter))) |>
  ungroup() 

# Weight = share of observation in the cell for which we compute the price index

compute_price_index <- function(data, var){
  price_df <- 
    data |>
    group_by({{ var }}) |>
    mutate(weight = 1/2 * (v / sum(v) + l_v / sum(l_v))) |>
    summarize(
      delta_ln_price_index = sum(delta_ln_uv * weight)) |>
    ungroup()
  return(list(price_df = price_df))
}
result <- compute_price_index(data = df2, var = t_k)
# result <- compute_price_index(data = df2, var = sym("t, k"))
tmp <- result$price_df |>
  separate_wider_delim(t_k, delim = " ", names_sep = "_")




# On calcule un indice de prix au niveau t-k
# Pondération de tijk = moyenne de la part de ij dans le flux de k en t et t-1

`tk--delta_ln_T--df` <- 
  `tijk--uv--df` %>%
  select(t, i, j, k, v, uv) %>%
  left_join(`tijk--l_uv--df`, by = c("t", "i", "j", "k")) %>%
  filter(!is.na(uv) & !is.na(l_uv)) %>%
  group_by(t, k) %>%
  mutate(`k--v` = sum(v, na.rm = T),
         `k--l_v` = sum(l_v, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(delta_ln_uv = log(uv) - log(l_uv), 
         w = 1/2*(v/`k--v` + l_v/`k--l_v`)) %>%
  #Filtrage des outliers
  group_by(k, t) %>%
  mutate(`5_p` = quantile(delta_ln_uv, 0.05, na.rm = T),
         `95_p` = quantile(delta_ln_uv, 0.95, na.rm = T)) %>%
  ungroup() %>%
  #Conservation des delta_ln_compris entre le 5e et le 95e centile
  filter(`5_p` < delta_ln_uv & delta_ln_uv < `95_p`) %>%
  group_by(t, k) %>%
  summarize(`k--delta_ln_T` = sum(delta_ln_uv*w, na.rm = T),
            `k--v` = mean(`k--v`)) %>%
  ungroup() %>%
  mutate(`k--ratio_T` = exp(`k--delta_ln_T`))
#Il faut s'arrêter là car on a besoin des delta_ln_T  