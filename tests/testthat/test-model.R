test_that(
  "ycol_mapped01:",
  {
    data("diabetic_data")
    data_fix <- ycol_mapped01(diabetic_data[1:100, ],
                              "readmitted", positive = "YES")
    expect_true(dim(data_fix)[2] == dim(diabetic_data)[2])
})


test_that(
  "build logistic regression model", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("time_in_hospital", "readmitted")] |>
                     ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_logistic_regression(diabetic_data,
                             "readmitted", is_kfold = TRUE,
                             cv_num = 2, seed = 103221,
                             is_smote = TRUE, thre = 0.4)
    expect_true(length(res) == 4)
  }
)

test_that(
  "build logistic regression model: calculate thre with no k-fold", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("time_in_hospital", "readmitted")] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_logistic_regression(diabetic_data,
                                    "readmitted", is_kfold = FALSE,
                                    cv_num = 2, seed = 103221, is_smote = TRUE)
    expect_true(length(res) == 4)
  }
)

test_that(
  "build rf model", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("time_in_hospital", "readmitted")] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_random_forest(diabetic_data,
                              "readmitted", is_kfold = TRUE,
                              cv_num = 2, seed = 103221,
                              is_smote = TRUE, thre = 0.2)
    expect_true(length(res) == 4)
  }
)

test_that(
  "build rf model: calculate thre automatically with no kfold", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("time_in_hospital", "readmitted")] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_random_forest(diabetic_data,
                              "readmitted", is_kfold = FALSE,
                              cv_num = 2, seed = 103221, is_smote = TRUE)
    expect_true(length(res) == 4)
  }
)

test_that(
  "predictor with logistic regression model", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                                   c("time_in_hospital", "num_lab_procedures",
                                            "readmitted")] |>
                     ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_logistic_regression(diabetic_data,
                             "readmitted", is_kfold = TRUE,
                             cv_num = 2, seed = 103221,
                             is_smote = FALSE, thre = 0.4)
    pred_lr <- model_predict(diabetic_data[,
                                  c("time_in_hospital", "num_lab_procedures")],
                             res$model, thre = 0.4, classname = c("YES", "NO"))
    expect_true(length(pred_lr) == dim(diabetic_data)[1])
  }
)

test_that(
  "predictor with random forest model", {
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100,
                  c("time_in_hospital", "num_lab_procedures", "readmitted")] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_random_forest(diabetic_data,
                             "readmitted", is_kfold = TRUE,
                             cv_num = 2, seed = 103221,
                             is_smote = FALSE, thre = 0.2)
    pred_rf <- model_predict(diabetic_data[,
                                  c("time_in_hospital", "num_lab_procedures")],
                             res$model, thre = 0.4, classname = c("YES", "NO"))
    expect_true(length(pred_rf) == dim(diabetic_data)[1])
  }
)
