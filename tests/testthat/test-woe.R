test_that(
  "build_woe", {
    data(diabetic_data)
    res <-
      build_woe(diabetic_data[1:100, c("num_procedures", "readmitted")],
                                   ycol = "readmitted",
                                   positive = "YES", is_dropsame = TRUE)
    expect_true(dim(res$newdata)[1] == 100)
})


test_that(
  "build_woe", {
    data(diabetic_data)
    diabetic_data[1:1, "metformin.rosiglitazone"] <- "YES"
    res <- build_woe(diabetic_data[1:100,
                c("metformin.rosiglitazone", "time_in_hospital", "readmitted")],
                                   ycol = "readmitted",
                                   positive = "YES", is_dropsame = TRUE)
    expect_true(dim(res$newdata)[1] == 100)
})


test_that(
  "explain woe", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("num_procedures", "insulin", "readmitted")]
    diabetic_data$insulin <- as.factor(diabetic_data$insulin)
    res <- build_woe(diabetic_data,
                                   ycol = "readmitted",
                                   positive = "YES", is_dropsame = TRUE)
    tab <- get_woe_explain(res$newdata, diabetic_data)
    expect_true(dim(tab)[2] == 3)
  }
)


test_that(
  "iv filter", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100, c("num_procedures", "readmitted")]
    res <- build_woe(diabetic_data, ycol = "readmitted",
                                   positive = "YES", is_dropsame = TRUE)
    iv_res <- iv_filter(res$newdata, iv_limit = .02, ycol = "readmitted",
                        positive = "YES")
    expect_true(dim(iv_res$data_iv)[1] == 100)
  }
)
