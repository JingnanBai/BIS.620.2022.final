


#' @title upsampling with SMOTE for given dataset
#' @description This function would do SMOTE upsampling for given dataset while
#' calculating appropriate over percentage automatically
#' @param data.forsample dataset need to do upsampling, need to be data.frame
#' @param ycol the colname for the target column
#'
#' @return processed dataset (data.frame())
#' @importFrom dplyr group_by summarise n
#' @importFrom DMwR SMOTE
#' @importFrom stats as.formula
#'
#' @examples
#' df <- data.frame(y=rep(as.factor(c('Yes', 'No')), times=c(90, 10)),
#'                  x1=rnorm(100),
#'                  x2=rnorm(100))
#' new_df <- upsample_smote(df, "y")
#' @export
upsample_smote <- function(data.forsample, ycol){
  prec.over <- data.forsample |>
    group_by(.data[[ycol]]) |>
    dplyr::summarise(n = n())
  prec.over <- 100 * ((max(prec.over$n)/min(prec.over$n))
                      |> ceiling())
  form <- as.formula(paste(ycol, " ~ ."))
  data.smote <- SMOTE(form, data.forsample, prec.over = prec.over)
  return(data.smote)
}
