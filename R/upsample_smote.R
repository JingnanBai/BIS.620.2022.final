


#' @title upsampling with smote for given dataset
#' @description this function would do smote upsampling for given dataset while
#' calculating appropriate over percentage automatically
#' @param data_forsample dataset need to do upsampling, need to be data.frame
#' @param ycol the colname for the target column
#'
#' @return processed dataset (data.frame())
#' @importFrom dplyr group_by summarise n
#' @importFrom stats as.formula
#'
#' @examples
#' data <- data.frame(y=rep(as.factor(c('Yes', 'No')), times=c(90, 10)),
#'                  x1=rnorm(100),
#'                  x2=rnorm(100))
#' data_smote <- upsample_smote(data, "y")
#' @export
upsample_smote <- function(data_forsample, ycol) {
  perc_over <- data_forsample |>
    group_by(.data[[ycol]]) |>
    dplyr::summarise(n = n())
  perc_over <- 100 * ((max(perc_over$n) / min(perc_over$n))
                      |> ceiling())
  data_smote <- smote(data_forsample, ycol, perc_over = perc_over)
  return(data_smote)
}



#' @title Creating a smote training sample for classification problems
#' @description this function would do smote upsampling for given dataset with
#' given sample percent to solve the imbalance problem
#' @source modified based on L. torgo, Feb 2010, from `DMwR` package
#'
#' @param data the original training set (with the unbalanced distribution)
#' @param ycol the colname for the target column
#' @param perc_over is the 100 * number of new cases (smoted cases) generated
#' for each rare case. If perc_over < 100 a single case is generated uniquely
#' for a randomly selected perc_over of the rare cases
#' @param k is the number of neighbours to consider as the pool from where
#' the new examples are generated
#' @param perc_under is the number of "normal" cases that are randomly selected
#' for each smoted case
#'
#' @return processed dataset (data.frame())
#'
#' @examples
#' data <- data.frame(y=rep(as.factor(c('Yes', 'No')), times=c(90, 10)),
#'                  x1=rnorm(100),
#'                  x2=rnorm(100))
#' newdata <- smote(data, "y", perc_over = 500, k = 5, perc_under = 200)
#' @export
smote <- function(data, ycol, perc_over = 500, k = 5, perc_under = 200) {
  # get the minority class
  mincl <- levels(data[, ycol])[which.min(table(data[, ycol]))]
  minexs <- which(data[, ycol] == mincl)
  col_idx <- which(names(data) == ycol)
  if (col_idx < ncol(data)) {
    n <- ncol(data)
    cols <- 1:n
    cols[c(col_idx, ncol(data))] <- cols[c(ncol(data), col_idx)]
    data <-  data[, cols]
  }

  # generate new cases
  newexs <- smote_exs(data[minexs, ], ycol, perc_over, k)
  if (col_idx < ncol(data)) {
    newexs <- newexs[, cols]
    data <- data[, cols]
  }

  # get the undersample of the "majority class" examples
  n <- nrow(data)
  selmaj <- sample((1:n)[-minexs],
                   as.integer((perc_under / 100) * nrow(newexs)),
                   replace = TRUE)

  # the final data set (the undersample+the rare cases + the smoted exs)
  newdataset <- rbind(data[selmaj, ], data[minexs, ], newexs)
  return(newdataset)
}




#' @title Obtain a set of smoted examples for a set of rare cases.
#' @description this function would do smote upsampling for a set of rare cases.
#' @source modified based on L. torgo, Feb 2010, from `DMwR` package
#'
#' @param data the original training set (rare cases only)
#' @param ycol the colname for the target column
#' @param perc_over is the 100 * number of new cases (smoted cases) generated
#' for each rare case. If perc_over < 100 a single case is generated uniquely
#' for a randomly selected perc_over of the rare cases
#' @param k is the number of neighbours to consider as the pool from where
#' the new examples are generated
#'
#' @return processed dataset (data.frame())
#'
#' @importFrom stats runif
#' @examples
#' data <- data.frame(x1=rnorm(100),
#'                    x2=rnorm(100),
#'                    y=rep(as.factor(c('Yes', 'No')), times=c(90, 10)))
#' newcase <- smote_exs(data[91:100, ], "y", perc_over = 500, k = 5)
#' @export
smote_exs <- function(data, ycol, perc_over, k) {
  t0 <- data.matrix(data[, 1:(ncol(data) - 1)])
  if (perc_over < 100) { # only a percentage of the t cases will be smote
    nt <- nrow(t)
    idx <- sample(1:nt, as.integer((perc_over / 100) * nt))
    t0 <- t0[idx, ]
    perc_over <- 100
  }

  p <- dim(t0)[2]
  nt <- dim(t0)[1]

  ranges <- apply(t0, 2, max) - apply(t0, 2, min)

  nexs <-  as.integer(perc_over / 100)
  # for each member of t
  new <- matrix(nrow = nexs * nt, ncol = p)    # the new cases

  for (i in 1:nt) {

    # the kNNs of case t[i,]
    xd <- scale(t0, t0[i, ], ranges)
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    knns <- order(dd)[2:(k + 1)]

    for (n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k, 1)
      # the attribute values of the generated case
      difs <- t0[knns[neig], ] - t0[i, ]
      new[(i - 1) * nexs + n, ] <- t0[i, ] + runif(1) * difs
    }
  }
  newcases <- data.frame(new)
  newcases[, ycol] <- factor(rep(data[1, ycol], nrow(newcases)),
                            levels = levels(data[, ycol]))
  colnames(newcases) <- colnames(data)
  newcases
}
