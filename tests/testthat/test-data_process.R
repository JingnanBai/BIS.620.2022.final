test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data("diabetic_data")
    data.mod <- replace_missing_col(diabetic_data$race, mark_list = "?")
    expect_equal(length(data.mod), dim(diabetic_data)[1])
  }
)
