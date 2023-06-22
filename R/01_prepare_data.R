filen <- paste0("t-i-j-k--BACI--HS", versions$HS, "-V", versions$baci_V, ".fst")
file <- file.path(paths$pc_baci_p, "Data", versions$baci_V, filen)
message(file.info(file)$mtime) 

baci_df <- 
  read_fst(file) |>
  mutate(uv = v/q)

filen <- paste0("HS", versions$HS, "-CPA2_1--share.csv")
file <- file.path(paths$nomenclatures_p, "conversions", filen)
hs_isic_df <- 
  read_csv(file) |>
  as_tibble() |>
  mutate(isic_2d = substr(CPA2_1, 0, 2)) |>
  distinct(HS5, isic_2d) |>
  group_by(HS5) |>
  mutate(share = 1 / n()) |>
  ungroup() |>
  mutate(k = as.numeric(HS5)) |>
  select(k, isic_2d, share)
  
distribution_nb_isic_per_hs <- 
  hs_isic_df |> 
  mutate(nb_isic = 1 / share) |>
  summarize(.by = nb_isic, 
            nb_hs = n_distinct(k))
 
baci_in_isic_2d <- 
  baci_df |>
  left_join(hs_isic_df, by = c("k")) |>
  mutate(v = v * share) |>
  summarize(.by = c(t, i, j, isic_2d),
            nb_flows = n_distinct(k),
            v = sum(v)) 
filen <- paste0("t-i-j-isic_2d--BACI--V", versions$baci_V, ".fst")
file <- here("data", filen)
write_fst(baci_in_isic_2d, file, compress = 100)

nb_obs_per_isic_2d <-
  baci_in_isic_2d |>
  summarize(.by = isic_2d, 
            nb_flows = sum(nb_flows),
            v = sum(v)) |>
  mutate(across(
    c(nb_flows, v),
    ~ .x / sum(.x) * 100,
    .names = "sh_{.col}")) |>
  arrange(-nb_flows)



