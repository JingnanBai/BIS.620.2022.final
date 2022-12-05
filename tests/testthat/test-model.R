test_that(
  "ycol_mapped01:",
  {
    data("diabetic_data")
    data.fix <- ycol_mapped01(diabetic_data[1:100, ],
                              "readmitted", positive = "YES")
    expect_true(dim(data.fix)[2] == dim(diabetic_data)[2])
})


test_that(
  "build logistic regression model",{
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
                     ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_LogisticRegression(diabetic_data,
                             "readmitted", is.kfold = TRUE,
                             cv_num = 2, seed = 103221, is.smote = TRUE, thre = 0.4)
    expect_true(length(res) == 2)
  }
)

test_that(
  "build logistic regression model: calculate thre with no k-fold",{
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_LogisticRegression(diabetic_data,
                                    "readmitted", is.kfold = FALSE,
                                    cv_num = 2, seed = 103221, is.smote = TRUE)
    expect_true(length(res) == 2)
  }
)

test_that(
  "build rf model",{
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_RandomForest(diabetic_data,
                              "readmitted", is.kfold = TRUE,
                              cv_num = 2, seed = 103221, is.smote = TRUE, thre = 0.2)
    expect_true(length(res) == 2)
  }
)

test_that(
  "build rf model: calculate thre automatically with no kfold",{
    data(diabetic_data)
    diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
      ycol_mapped01(ycol = "readmitted", positive = "YES")
    res <- build_RandomForest(diabetic_data,
                              "readmitted", is.kfold = FALSE,
                              cv_num = 2, seed = 103221, is.smote = TRUE)
    expect_true(length(res) == 2)
  }
)
