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
  ungroup()
  
distribution_nb_isic_per_hs <- 
  hs_isic_df |> 
  mutate(nb_isic = 1 / share) |>
  summarize(.by = nb_isic, 
            nb_hs = n_distinct(HS5))
 