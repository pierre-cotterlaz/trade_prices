define_paths <- function(location) {
  if (location == "pc") {
    PC_p <- file.path("D:", "cotterlaz")
    nomenclatures_p <- file.path(PC_p, "cepii_datasets", "nomenclatures")
    pc_baci_p <- file.path(PC_p, "cepii_datasets", "baci")
    baci_p <- pc_baci_p
    wtfc_p <- file.path(PC_p, "cepii_datasets", "wtfc")
  }
  if (location == "baci8") {
    PC_p <- file.path("\\\\dell5820108", "cotterlaz$")
    nomenclatures_p <- file.path(PC_p, "cepii_datasets", "nomenclatures")
    pc_baci_p <- file.path(PC_p, "cepii_datasets", "baci")
    baci_p <- file.path("D:", "BACI", "BACI")
    wtfc_p <- file.path("D:", "BACI", "WTFC")
  }
  return(list(
    PC_p = PC_p,
    nomenclatures_p = nomenclatures_p,
    pc_baci_p = pc_baci_p,
    baci_p = baci_p,
    wtfc_p = wtfc_p
  ))
}

define_first_year <- function(revision){
  if (revision == 1){
    first_year <- 2000
  }
  return(first_year)
}
