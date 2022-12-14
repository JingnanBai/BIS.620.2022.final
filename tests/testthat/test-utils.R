test_that(
  "data_ids_map",
  {
    data(diabetic_data)
    data(ids_map)
    data_mapped <- data_ids_map(diabetic_data[1:100, ], ids_map)
    expect_true(dim(data_mapped)[1] == 100)
})

test_that(
  "data_ids_map",
  {
    data(diabetic_data)
    expect_error(data_ids_map(diabetic_data[1:100, ], data.frame()))
  })

test_that(
  "format_factor_col",
  {
    data(diabetic_data)
    data_factor <- format_factor_col(diabetic_data[1:100, "readmitted"])
    expect_true(class(data_factor) == "factor")
  }
)

test_that(
  "format_factor_dataset",
  {
    data(diabetic_data)
    diabetic_data <- format_factor_dataset(diabetic_data[1:100, ])
    expect_true(class(diabetic_data$readmitted) == "factor")
  }
)


test_that(
  "encode_icd9", {
    data(diabetic_data)
    data(icd9_map)
    diabetic_data <- encode_icd9(x = diabetic_data[1:100, ],
                                 data_icd = icd9_map, maplist = c("diag_1"))
    expect_true(dim(diabetic_data)[1] == 100)
  }
)


test_that(
  "encode_icd9", {
    data(diabetic_data)
    data(icd9_map)
    expect_error(encode_icd9(x = diabetic_data[1:100, ],
                             data_icd = data.frame(), maplist = c("diag_1")))
  }
)
