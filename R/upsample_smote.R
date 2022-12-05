


#' @title upsampling with SMOTE for given dataset
#' @description This function would do SMOTE upsampling for given dataset while
#' calculating appropriate over percentage automatically
#' @param data.forsample dataset need to do upsampling, need to be data.frame
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
#' data.smote <- upsample_smote(data, "y")
#' @export
upsample_smote <- function(data.forsample, ycol){
  perc.over <- data.forsample |>
    group_by(.data[[ycol]]) |>
    dplyr::summarise(n = n())
  perc.over <- 100 * ((max(perc.over$n)/min(perc.over$n))
                      |> ceiling())
  data.smote <- SMOTE(data.forsample, ycol, perc.over = perc.over)
  return(data.smote)
}



#' @title Creating a SMOTE training sample for classification problems
#' @description This function would do SMOTE upsampling for given dataset with given
#' sample percent to solve the imbalance problem
#' @source modified based on L. Torgo, Feb 2010, from `DMwR` package
#'
#' @param data the original training set (with the unbalanced distribution)
#' @param ycol the colname for the target column
#' @param perc.over is the 100 * number of new cases (smoted cases) generated for each
#' rare case. If perc.over < 100 a single case is generated uniquely for a randomly
#' selected perc.over of the rare cases
#' @param k is the number of neighbours to consider as the pool from where
#' the new examples are generated
#' @param perc.under is the number of "normal" cases that are randomly selected for
#' each smoted case
#'
#' @return processed dataset (data.frame())
#'
#' @examples
#' data <- data.frame(y=rep(as.factor(c('Yes', 'No')), times=c(90, 10)),
#'                  x1=rnorm(100),
#'                  x2=rnorm(100))
#' newdata <- SMOTE(data, "y", perc.over = 500, k = 5, perc.under = 200)
#' @export
SMOTE <- function(data, ycol, perc.over = 500, k = 5, perc.under = 200)
{
  # get the minority class
  minCl <- levels(data[, ycol])[which.min(table(data[, ycol]))]
  minExs <- which(data[, ycol] == minCl)
  col.idx <- which(names(data) == ycol)
  if (col.idx < ncol(data)) {
    cols <- 1:ncol(data)
    cols[c(col.idx, ncol(data))] <- cols[c(ncol(data), col.idx)]
    data <-  data[, cols]
  }

  # generate new cases
  newExs <- smote.exs(data[minExs, ], ycol, perc.over, k)
  if (col.idx < ncol(data)) {
    newExs <- newExs[, cols]
    data <- data[, cols]
  }

  # get the undersample of the "majority class" examples
  selMaj <- sample((1:NROW(data))[-minExs],
                   as.integer((perc.under / 100) * nrow(newExs)),
                   replace = TRUE)

  # the final data set (the undersample+the rare cases + the smoted exs)
  newdataset <- rbind(data[selMaj, ], data[minExs, ], newExs)
  return(newdataset)
}




#' @title Obtain a set of smoted examples for a set of rare cases.
#' @description This function would do SMOTE upsampling for a set of rare cases.
#' @source modified based on L. Torgo, Feb 2010, from `DMwR` package
#'
#' @param data the original training set (rare cases only)
#' @param ycol the colname for the target column
#' @param perc.over is the 100 * number of new cases (smoted cases) generated for each
#' rare case. If perc.over < 100 a single case is generated uniquely for a randomly
#' selected perc.over of the rare cases
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
#' newcase <- smote.exs(data[91:100, ], "y", perc.over = 500, k = 5)
#' @export
smote.exs <- function(data, ycol, perc.over, k)
{
  nomatr <- c()
  T <- matrix(nrow = dim(data)[1], ncol = dim(data)[2]-1)
  for(col in 1:dim(T)[2]){
    if (class(data[, col]) %in% c('factor','character')) {
      T[, col] <- as.integer(data[, col])
      nomatr <- c(nomatr, col)
    } else {T[,col] <- data[, col]}
  }

  if (perc.over < 100) { # only a percentage of the T cases will be SMOTE
    nT <- nrow(T)
    idx <- sample(1: nT, as.integer((perc.over / 100) * nT))
    T <- T[idx, ]
    perc.over <- 100
  }

  p <- dim(T)[2]
  nT <- dim(T)[1]

  ranges <- apply(T, 2, max)-apply(T, 2, min)

  nexs <-  as.integer(perc.over / 100) # this is the number of artificial exs generated
  # for each member of T
  new <- matrix(nrow = nexs * nT, ncol = p)    # the new cases

  for(i in 1:nT) {

    # the k NNs of case T[i,]
    xd <- scale(T, T[i, ], ranges)
    for(a in nomatr) xd[, a] <- xd[, a]==0
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    kNNs <- order(dd)[2:(k+1)]

    for(n in 1:nexs) {
      # select randomly one of the k NNs
      neig <- sample(1:k, 1)

      ex <- vector(length=ncol(T))

      # the attribute values of the generated case
      difs <- T[kNNs[neig], ]-T[i, ]
      new[(i-1) * nexs+n, ] <- T[i, ] + runif(1)*difs
      for(a in nomatr)
        new[(i-1) * nexs + n, a] <- c(T[kNNs[neig], a],T[i, a])[1 + round(runif(1), 0)]
    }
  }
  newCases <- data.frame(new)
  for(a in nomatr){
    newCases[, a] <- factor(newCases[, a],
                           levels = 1:nlevels(data[, a]),
                           labels = levels(data[, a]))}

  newCases[,ycol] <- factor(rep(data[1, ycol], nrow(newCases)),
                            levels = levels(data[, ycol]))
  colnames(newCases) <- colnames(data)
  newCases
}


