

#' @title mapped target column to 0/1
#' @description This function could be use for mapping the target column for given
#' dataset to 0/1, as 1 refer to the positive class
#' @param data the dataset needed to do the transformation
#' @param ycol String, the colname for the target column
#' @param positive String, denotes the positive class
#' @return the processed dataset
#' @examples
#' data(diabetic_data)
#' diabetic_data <- ycol_mapped01(diabetic_data[1:100, ], "readmitted", "YES")
#' @export
ycol_mapped01 <- function(data, ycol, positive){
  data[, ycol] <- data[[ycol]] |> as.character()
  if((class(ycol) != "character")|(class(positive) != "character")){
    stop("the input `ycol` and `positive` should be String/Character")
  }
  tempidx <- data[[ycol]] == positive
  data[, ycol] <- 0
  data[tempidx, ycol] <- 1
  data[, ycol] <- data[[ycol]] |> as.factor()
  return(data)
}



#' @title evaluate the classification model
#' @description This function could be use for do evaluation for a classification model
#' @param pre the prediction of the model
#' @param gt label for the true value
#' @return `result` a data.frame with `acc`, `precision`, `recall`, `f1`
#'
#' @examples
#' pred <- c(0,1,1,1,1,0)
#' gt <- c(1,1,1,1,0,1)
#' result <- eval_model(pred, gt)
#' @export
eval_model <- function(pre,gt){
  pre <- pre |> as.factor()
  gt <- gt |> as.factor()
  levels(pre) <- c(0,1)
  levels(gt) <- c(0,1)
  temp <- table(pre,gt)
  TP <- temp[2,2]
  FP <- temp[2,1]
  FN <- temp[1,2]
  TN <- temp[1,1]
  result <- data.frame(matrix(NA,1,0))
  result$acc <- (TP+TN)/(TP+FP+TN+FN)
  result$precision <- TP/(TP+FP)
  result$recall <- TP/(TP+FN)
  result$f1 <- 2*result$pre*result$recall/(result$pre+result$recall)
  return(result)
}


#' @title build idx matrix for K-fold validation
#' @description This function could be use for groupping idx for K-fold validation
#' @param k for k fold validation
#' @param n the total number of rows for dataset
#' @param seed random seed for sampling
#' @return `kfold` a matrix with each index of obs in k groups
#'
#' @examples
#' data(diabetic_data)
#' kfold <- build_kfold(5, dim(diabetic_data)[1], seed = 103221)
#' @export
build_kfold <- function(k, n, seed = NA){
  if(!is.na(seed)){set.seed(seed)}
  kfold <- sample(1:n,floor(n/k) * k)
  kfold <- matrix(kfold, ncol = k)
  return(kfold)
}




#' @title build the logistic regression model for given dataset
#' @description This function could be use for training a logistic regression model
#' for given dataset, with k-fold validation (if applicable) and evaluate the result
#' @param data dataset for modeling
#' @param ycol the colname for target column
#' @param is.kfold if TRUE, evaluate the model with k-fold evaluation
#' @param cv_num for k = cv_num fold validation
#' @param seed use for spliting train/test data
#' @param is.smote whether upsampling with SMOTE method for solving imbalance
#' @param thre classification threshold, when the model gives probability higher
#' than this value, assign it to the positive, otherwise to negative
#'
#' @return `result` a list with `result$mod` trained model; `result$eval_tab` the
#' model evaluation result
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
#' res <- build_LogisticRegression(diabetic_data,
#'                          "readmitted", is.kfold = TRUE,
#'                          cv_num = 2, seed = 103221, is.smote = FALSE, thre = 0.4)
#' @export
build_LogisticRegression <- function(data, ycol,
                                     is.kfold = FALSE, cv_num = 10, seed = NA,
                                     is.smote = FALSE,
                                     thre = NA){
  epoch_num <- cv_num
  form <- as.formula(paste(ycol, " ~ ."))
  auc <- 0
  pre <- 0
  acc <- 0
  recall <- 0
  Fscore <- 0
  if(is.kfold){
    kfold <- build_kfold(cv_num, dim(data)[1], seed = seed)
    if(cv_num < 1 | cv_num %% 1 != 0){stop("cv_num should be integer >= 1")}
    epoch_num <- cv_num}
  for(epoch in 1:epoch_num){
    if(is.kfold){
      train <- data[-kfold[,epoch],]
      test <- data[kfold[,epoch],]
    } else{
      train <- data
      test <- data
    }
    if(is.smote){
      train <- upsample_smote(train, ycol)
    }
    mod <- glm(form,
               data = train, family = binomial(link = 'logit'))
    pred_prob <- predict(mod, newdata = test, 'response')
    roc0 <- roc(test$readmitted, pred_prob, quiet = TRUE)
    if(is.na(thre)){
      temp <- data |> group_by(.data[[ycol]]) |>
        dplyr::summarise(n = n())
      thre <- temp[temp[[ycol]] == 1, ]$n / dim(data)[1]
    }
    test$pred <- 0
    test$pred[pred_prob > thre] <- 1
    test$pred <- test$pred |> as.factor()
    result <- eval_model(test$pred ,test$readmitted)
    auc <- auc + roc0$auc
    acc <- acc + result$acc
    pre <- pre + result$pre
    recall <- recall + result$recall
    Fscore <- result$f1 + Fscore
  }
  if(is.kfold){mod <- glm(form,
                  data = data, family = binomial(link = 'logit'))}
  eval_tab <- cbind(c("accuracy", "precision", "F1-score", "recall", "AUC"),
                    round(c(acc, pre, Fscore, recall, auc)/epoch_num, 3)) |>
    data.frame()
  colnames(eval_tab) <- c("index", "value")
  result <- list(eval_tab = eval_tab, model = mod)
  return(result)
}






#' @title build the random forest model for given dataset
#' @description This function could be use for training a random forest model
#' for given dataset, with k-fold validation (if applicable) and evaluate the result
#' @param data dataset for modeling
#' @param ycol the colname for target column
#' @param is.kfold if TRUE, evaluate the model with k-fold evaluation
#' @param cv_num for k = cv_num fold validation
#' @param seed use for spliting train/test data
#' @param is.smote whether upsampling with SMOTE method for solving imbalance
#' @param thre classification threshold, when the model gives probability higher
#' than this value, assign it to the positive, otherwise to negative
#'
#' @return `result` a list with `result$mod` trained model; `result$eval_tab` the
#' model evaluation result
#'
#' @importFrom pROC roc
#' @importFrom dplyr group_by summarise
#' @importFrom randomForest randomForest
#' @importFrom stats as.formula predict
#'
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c('time_in_hospital', 'readmitted')] |>
#'                  ycol_mapped01(ycol = "readmitted", positive = "YES")
#' res <- build_RandomForest(diabetic_data,
#'                          "readmitted", is.kfold = TRUE,
#'                          cv_num = 2, seed = 103221, is.smote = FALSE, thre = 0.2)
#' @export
build_RandomForest <- function(data, ycol,
                                     is.kfold = FALSE, cv_num = 10, seed = NA,
                                     is.smote = FALSE,
                                     thre = NA){
  epoch_num <- 1
  form <- as.formula(paste(ycol, " ~ ."))
  auc <- 0
  pre <- 0
  acc <- 0
  recall <- 0
  Fscore <- 0
  if(is.kfold){
    kfold <- build_kfold(cv_num, dim(data)[1], seed = seed)
    if(cv_num < 1 | cv_num %% 1 != 0){stop("cv_num should be integer >= 1")}
    epoch_num <- cv_num}
  for(epoch in 1:epoch_num){
    if(is.kfold){
      train <- data[-kfold[,epoch],]
      test <- data[kfold[,epoch],]
    } else{
      train <- data
      test <- data
    }
    if(is.smote){
      train <- upsample_smote(train, ycol)
    }
    mod <- randomForest(form, data = train)
    pred_prob <- predict(mod, newdata = test, 'prob')[, 2]
    roc0 <- roc(as.ordered(test$readmitted), as.ordered(pred_prob),
                direction = '<', quite = TRUE)
    if(is.na(thre)){
      temp <- data |> group_by(.data[[ycol]]) |>
        dplyr::summarise(n = n())
      thre <- temp[temp[[ycol]] == 1, ]$n / dim(data)[1]
    }
    test$pred <- 0
    test$pred[pred_prob > thre] <- 1
    test$pred <- test$pred |> as.factor()
    result <- eval_model(test$pred ,test$readmitted)
    auc <- auc + roc0$auc
    acc <- acc + result$acc
    pre <- pre + result$pre
    recall <- recall + result$recall
    Fscore <- result$f1 + Fscore
  }
  if(is.kfold){mod <- randomForest(form, data = data)}
  eval_tab <- cbind(c("accuracy", "precision", "F1-score", "recall", "AUC"),
                    round(c(acc, pre, Fscore, recall, auc)/epoch_num, 3)) |>
    data.frame()
  colnames(eval_tab) <- c("index", "value")
  result <- list(eval_tab = eval_tab, model = mod)
  return(result)
}
