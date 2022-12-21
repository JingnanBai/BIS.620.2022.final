test_that("smote with y not at the last position", {
  data <- data.frame(y = rep(as.factor(c("Yes", "No")), times = c(90, 10)),
                   x1 = rnorm(100),
                   x2 = rnorm(100))
  data_smote <- upsample_smote(data, "y")
  expect_true(dim(data_smote)[2] == 3)
})
