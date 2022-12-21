

#' @title mapped target column to 0/1
#' @description This function could be use for mapping the target column for
#' given dataset to 0/1, as 1 refer to the positive class
#' @param data the dataset needed to do the transformation
#' @param ycol String, the colname for the target column
#' @param positive String, denotes the positive class
#' @return the processed dataset
#' @examples
#' data(diabetic_data)
#' diabetic_data <- ycol_mapped01(diabetic_data[1:100, ], "readmitted", "YES")
#' @export
ycol_mapped01 <- function(data, ycol, positive) {
  data[, ycol] <- data[[ycol]] |> as.character()
  tempidx <- data[[ycol]] == positive
  data[, ycol] <- 0
  data[tempidx, ycol] <- 1
  data[, ycol] <- data[[ycol]] |> as.factor()
  return(data)
}



#' @title evaluate the classification model
#' @description This function could be use for do evaluation for a
#' classification model
#' @param pre the prediction of the model
#' @param gt label for the true value
#' @return `result` a data.frame with `acc`, `precision`, `recall`, `f1`
#'
#' @examples
#' pred <- c(0,1,1,1,1,0)
#' gt <- c(1,1,1,1,0,1)
#' result <- eval_model(pred, gt)
#' @export
eval_model <- function(pre, gt) {
  pre <- pre |> as.factor()
  gt <- gt |> as.factor()
  levels(pre) <- c(0, 1)
  levels(gt) <- c(0, 1)
  temp <- table(pre, gt)
  tp <- temp[2, 2]
  fp <- temp[2, 1]
  fn <- temp[1, 2]
  tn <- temp[1, 1]
  result <- data.frame(matrix(NA, 1, 0))
  result$acc <- (tp + tn) / (tp + fp + tn + fn)
  result$precision <- tp / (tp + fp)
  result$recall <- tp / (tp + fn)
  result$f1 <- 2 * result$pre * result$recall / (result$pre + result$recall)
  return(result)
}


#' @title build idx matrix for K-fold validation
#' @description This function could be use for groupping idx for K-fold
#' validation
#' @param k for k fold validation
#' @param n the total number of rows for dataset
#' @param seed random seed for sampling
#' @return `kfold` a matrix with each index of obs in k groups
#'
#' @examples
#' data(diabetic_data)
#' kfold <- build_kfold(5, dim(diabetic_data)[1], seed = 103221)
#' @export
build_kfold <- function(k, n, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  kfold <- sample(1:n, floor(n / k) * k)
  kfold <- matrix(kfold, ncol = k)
  return(kfold)
}




#' @title build the logistic regression model for given dataset
#' @description This function could be use for training a logistic regression
#' model for given dataset, with k-fold validation (if applicable) and evaluate
#' the result
#' @param data dataset for modeling
#' @param ycol the colname for target column
#' @param is_kfold if TRUE, evaluate the model with k-fold evaluation
#' @param cv_num for k = cv_num fold validation
#' @param seed use for spliting train/test data
#' @param is_smote whether upsampling with smote method for solving imbalance
#' @param auto_thre whether learn the best thre automatically based on ROC
#' @param thre classification threshold, when the model gives probability
#' higher than this value, assign it to the positive, otherwise to negative. If
#' it's NA, then use the information from ROC (specificities & sensitivities)
#' to calculated automatically.
#'
#' @return `result` a list with `result$mod` trained model; `result$eval_tab`
#' the model evaluation result; `result$roc_res` the ROC and AUC information;
#' `result$best_thre` the best threshold used in modeling
#'
#' @importFrom pROC roc
#' @importFrom dplyr group_by summarise
#' @importFrom stats as.formula glm binomial predict
#'
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
#'                  ycol_mapped01(ycol = "readmitted", positive = "YES")
#' res <- build_logistic_regression(diabetic_data,
#'                          "readmitted", is_kfold = TRUE,
#'                          cv_num = 2, seed = 103221, is_smote = FALSE,
#'                          thre = 0.4)
#' @export
#'
build_logistic_regression <- function(data, ycol,
                                     is_kfold = FALSE, cv_num = 10, seed = NA,
                                     is_smote = FALSE,
                                     auto_thre = TRUE, thre = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  if (is_kfold) {
    epoch_num <- cv_num
  } else {
      epoch_num <- 1
      }
  form <- as.formula(paste(ycol, " ~ ."))
  auc <- 0
  pre <- 0
  acc <- 0
  recall <- 0
  fscore <- 0
  if (is_kfold) {
    kfold <- build_kfold(cv_num, dim(data)[1], seed = seed)
    epoch_num <- cv_num
    }
  for (epoch in 1:epoch_num) {
    if (is_kfold) {
      train <- data[-kfold[, epoch], ]
      test <- data[kfold[, epoch], ]
    } else {
      train <- data
      test <- data
    }
    if (is_smote) {
      train <- upsample_smote(train, ycol)
    }
    mod <- glm(form, data = train, family = binomial(link = "logit"))
    pred_prob <- predict(mod, newdata = test, "response")
    roc0 <- roc(test$readmitted, pred_prob, quiet = TRUE)
    if (is.na(thre) || auto_thre) {
      max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
      idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
      thre <- roc0$thresholds[idx]
    }
    test$pred <- 0
    test$pred[pred_prob > thre] <- 1
    test$pred <- test$pred |> as.factor()
    result <- eval_model(test$pred, test$readmitted)
    auc <- auc + roc0$auc
    acc <- acc + result$acc
    pre <- pre + result$pre
    recall <- recall + result$recall
    fscore <- result$f1 + fscore
  }
  if (is_kfold) {
    data <- upsample_smote(data, ycol)
    mod <- glm(form, data = data, family = binomial(link = "logit"))
    pred_prob <- predict(mod, newdata = data, "response")
    roc0 <- roc(data$readmitted, pred_prob, quiet = TRUE)
    if (is.na(thre) || auto_thre) {
      max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
      idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
      thre <- roc0$thresholds[idx]
    }
  }
  eval_tab <- cbind(c("accuracy", "precision", "F1-score", "recall", "AUC"),
                    round(c(acc, pre, fscore, recall, auc) / epoch_num, 3)) |>
    data.frame()
  colnames(eval_tab) <- c("index", "value")
  result <- list(eval_tab = eval_tab, model = mod,
                 roc_res = roc0, best_thre = thre)
  return(result)
}







#' @title build the random forest model for given dataset
#' @description This function could be use for training a random forest model
#' for given dataset, with k-fold validation (if applicable) and evaluate the
#' result
#' @param data dataset for modeling
#' @param ycol the colname for target column
#' @param is_kfold if TRUE, evaluate the model with k-fold evaluation
#' @param cv_num for k = cv_num fold validation
#' @param seed use for spliting train/test data
#' @param is_smote whether upsampling with smote method for solving imbalance
#' @param thre classification threshold, when the model gives probability
#' higher than this value, assign it to the positive, otherwise to negative. If
#' it's NA, then use the information from ROC (specificities & sensitivities)
#' to calculated automatically. If it's NA, the threshold would be calculated
#' automatically based on ROC
#' @param ntree the hyp for random forest
#' @param maxnodes the hyp for random forest
#' @param mtry the hyp for random forest
#'
#' @return `result` a list with `result$mod` trained model; `result$eval_tab`
#' the model evaluation result; `result$roc_res` the ROC and AUC information;
#' `result$best_thre` the best threshold used in modeling
#'
#' @importFrom pROC roc
#' @importFrom dplyr group_by summarise
#' @importFrom randomForest randomForest importance
#' @importFrom stats as.formula predict
#'
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
#'                  ycol_mapped01(ycol = "readmitted", positive = "YES")
#' res <- build_random_forest(diabetic_data,
#'                          "readmitted", is_kfold = TRUE,
#'                          cv_num = 2, seed = 103221, is_smote = FALSE,
#'                          thre = 0.2)
#' @export
build_random_forest <- function(data, ycol,
                                is_kfold = FALSE, cv_num = 10, seed = NA,
                                is_smote = FALSE, thre = NA,
                                ntree = 100, maxnodes = 50, mtry = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  if (is_kfold) {
    epoch_num <- cv_num
  } else {
    epoch_num <- 1
  }
  form <- as.formula(paste(ycol, " ~ ."))
  auc <- 0
  pre <- 0
  acc <- 0
  recall <- 0
  fscore <- 0
  data <- upsample_smote(data, ycol, perc_over = 400)
  if (is_kfold) {
    kfold <- build_kfold(cv_num, dim(data)[1], seed = seed)
  }
  for (epoch in 1:epoch_num) {
    if (is_kfold) {
      train <- data[-kfold[, epoch], ]
      test <- data[kfold[, epoch], ]
    } else {
      train <- data
      test <- data
    }
    if (is.na(mtry)) {
      mtry <- log2(dim(train)[2]) |> floor()
    }
    mod <- randomForest(form, data = train,
                        ntree = ntree, maxnodes = maxnodes, mtry = mtry)
    mod$importance <- randomForest::importance(mod)
    pred_prob <- predict(mod, newdata = test, "prob")[, 2]
    roc0 <- roc(as.ordered(test$readmitted), as.ordered(pred_prob),
                quiet = TRUE)
    if (is.na(thre)) {
      max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
      idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
      thre_temp <- roc0$thresholds[idx]
    } else {
      thre_temp <- thre
    }
    test$pred <- 0
    test$pred[pred_prob > thre_temp] <- 1
    test$pred <- test$pred |> as.factor()
    result <- eval_model(test$pred, test$readmitted)

    auc <- auc + roc0$auc
    acc <- acc + result$acc
    pre <- pre + result$pre
    recall <- recall + result$recall
    fscore <- result$f1 + fscore
  }
  if (is_kfold) {
    mod <- randomForest(form, data = data)
    mod$importance <- randomForest::importance(mod)
    pred_prob <- predict(mod, newdata = data, "prob")[, 2]
    roc0 <- roc(as.ordered(data$readmitted), as.ordered(pred_prob),
                quiet = TRUE)
    max_temp <- abs(1 - roc0$specificities - roc0$sensitivities)
    idx <- which(max_temp == max(max_temp), arr.ind = TRUE)
    thre <- roc0$thresholds[idx]
  }
  eval_tab <- cbind(c("accuracy", "precision", "F1-score", "recall", "AUC"),
                    round(c(acc, pre, fscore, recall, auc) / epoch_num, 3)) |>
    data.frame()
  colnames(eval_tab) <- c("index", "value")
  result <- list(eval_tab = eval_tab, model = mod,
                 roc_res = roc0, best_thre = thre)
  return(result)
}



#' @title get prediction for newdata based on given dataset
#' @description This function could be use for conduction classification based
#' on given new dataset and models with specific threshold
#' @param data new dataset for classification, should be data.frame
#' @param mod the trained model, should be LogisticRegression(glm/lm) or
#' RandomForest model(randomForest)
#' @param thre classification threshold, if the predicted probability is higher
#' than given threshold, it would be assigned as positive class. default = 0.5
#' @param classname the name for the classes, should be assigned as c("positive
#' class", "negative class"). if is empty, the return value would be 0/1
#'
#' @return predicted value, return as a single factor column
#'
#' @importFrom stats predict
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c("time_in_hospital",
#'                                      "num_lab_procedures", "readmitted")] |>
#'                  ycol_mapped01(ycol = "readmitted", positive = "YES")
#' res <- build_logistic_regression(diabetic_data,
#'                          "readmitted", is_kfold = TRUE,
#'                          cv_num = 2, seed = 103221, is_smote = FALSE,
#'                          thre = 0.4)
#' pred_lr <- model_predict(diabetic_data[, c("time_in_hospital",
#'                                            "num_lab_procedures")],
#'                          res$model, thre = 0.4, classname = c("YES", "NO"))
#' @export
model_predict <- function(data, mod, thre = 0.5, classname = c()) {
  if (any(class(mod) == "glm")) {
    pred_prob <- predict(mod, newdata = data, "response")
  } else if (any(class(mod) == "randomForest")) {
    pred_prob <- predict(mod, newdata = data, "prob")[, 2]
  }
  pos <- 1
  neg <- 0
  if (length(classname) > 0) {
    pos <- classname[1]
    neg <- classname[2]
  }
  data$pred <- neg
  data$pred[pred_prob > thre] <- pos
  data$pred <- data$pred |> as.factor()
  return(data$pred)
}
