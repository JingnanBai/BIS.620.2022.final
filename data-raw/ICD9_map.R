## code to prepare `ICD9_map` dataset goes here

ICD9_map = readRDS("data-raw//ICD9_map.rds")
usethis::use_data(ICD9_map, overwrite = TRUE)
