## code to prepare `ICD9_map` dataset goes here

icd9_map <- readRDS("data-raw//ICD9_map.rds")
usethis::use_data(icd9_map, overwrite = TRUE)
