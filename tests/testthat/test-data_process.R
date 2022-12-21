test_that(
  "replace_missing_col: should return columns with the same length",
  {
    data("diabetic_data")
    data_mod <- replace_missing_col(diabetic_data$race,
                                    mark_list = c("?", "null"))
    expect_equal(length(data_mod), dim(diabetic_data)[1])
  }
)

test_that(
  "replace_missing_col: when no mark_list for replace_missing_col",
  {
    data("diabetic_data")
    data_mod <- replace_missing_col(diabetic_data$race, mark_list = c())
    expect_equal(length(data_mod), dim(diabetic_data)[1])
  }
)

test_that(
  "replace_missing_col: capture no missing data",
  {
    data("diabetic_data")
    data_mod <- replace_missing_col(diabetic_data$race,
                                    mark_list = c("invalid"))
    expect_equal(length(data_mod), dim(diabetic_data)[1])
  }
)

test_that(
  "replace_missing_dataset: do not drop any row",
  {
    data("diabetic_data")
    data_mod <- replace_missing_dataset(diabetic_data)
    expect_equal(dim(data_mod)[1], dim(diabetic_data)[1])
  }
)

test_that(
  "missing_explore: with no missing data",
  {
    data("diabetic_data")
    res <- missing_explore(diabetic_data[1:100,
                                        c("readmitted", "admission_type_id")],
                                is_fill = TRUE)
    expect_true(class(res$newdata) == "data.frame")
  }
)

test_that(
  "missing_explore: with missing data",
  {
    data("diabetic_data")
    diabetic_data <- replace_missing_dataset(diabetic_data)
    res <- missing_explore(diabetic_data[1:100, ],
                           is_fill = TRUE, is_drop = TRUE)
    expect_true(class(res$newdata) == "data.frame")
  }
)

test_that(
  "missing_explore: fill with numeric data",
  {
    data("diabetic_data")
    diabetic_data <- replace_missing_dataset(diabetic_data)
    diabetic_data[1:10, "num_medications"] <- NA
    res <- missing_explore(diabetic_data[1:100, ], is_fill = TRUE,
                                fill_cre_num = "median")
    res <- missing_explore(diabetic_data[1:100, ], is_fill = TRUE,
                                fill_cre_num = "avg")
    expect_true(class(res$newdata) == "data.frame")
  }
)

test_that(
  "outlier_explore:",
  {
    data("diabetic_data")
    data_fix <- outlier_explore(diabetic_data[1:1000, ], is_drop = TRUE,
                                is_plot = TRUE)
    expect_true(class(data_fix) == "data.frame")
  }
)

test_that(
  "outlier_explore: with no outlier",
  {
    data("diabetic_data")
    data_fix <- outlier_explore(diabetic_data[1:1000, c("readmitted", "race")],
                                is_drop = TRUE, is_plot = TRUE)
    expect_true(class(data_fix) == "data.frame")
  }
)

test_that(
  "extreme_imbalance_col: with extreme cols", {
    data("diabetic_data")
    data_fix <- extreme_imbalance_col(diabetic_data[1:100, ], is_table = TRUE,
                                      is_drop = TRUE)
    expect_true(class(data_fix) == "data.frame")
  }
)

test_that(
  "extreme_imbalance_col: with no extreme cols", {
    data("diabetic_data")
    data_fix <- extreme_imbalance_col(diabetic_data[1:100,
                                                    c("readmitted", "race")])
    expect_true(class(data_fix) == "data.frame")
  }
)
