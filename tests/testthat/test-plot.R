test_that("test_missing_pattern", {
  data("diabetic_data")
  mark_list <- c("?", "unknown", "invalid", "not available", "not mapped",
                 "null", "none")
  diabetic_data <- replace_missing_dataset(diabetic_data,
                                           mark_list = mark_list)
  p <- plot_missing_pattern(diabetic_data[1:100, ],
                    nalist = c("medical_specialty", "payer_code", "A1Cresult"))
  expect_true(class(p) == "aggr")
})

test_that("test_boxplot for outlier", {
  data(diabetic_data)
  p <- plot_boxplot(diabetic_data[1:100,
                                  c("num_procedures", "num_medications")])
  expect_true("gtable" %in% class(p))
})


test_that("test for mosaic", {
  data(diabetic_data)
  p <- plot_mosaic(diabetic_data[1:100, ], "readmitted", c("insulin"),
                   ncol = 3)
  expect_true("gtable" %in% class(p))
})


test_that("test for hist_plot", {
  data(diabetic_data)
  p <- plot_comparehist(diabetic_data[1:100, ], "readmitted",
                             c("num_medications"), ncol = 4)
  expect_true("gtable" %in% class(p))
})


test_that("test for WOE_plot", {
  data(diabetic_data)
  diabetic_data[1:1, "metformin.rosiglitazone"] <- "YES"
  res_woe_temp <- build_woe(diabetic_data[1:100,
              c("metformin.rosiglitazone", "time_in_hospital", "readmitted")],
              ycol = "readmitted",
              positive = "YES", is_dropsame = TRUE)
  p <- get_woe_plot(res_woe_temp$woe_bins,
                    xcol = c("time_in_hospital"), ncol = 2)
  expect_true("gtable" %in% class(p))
})
