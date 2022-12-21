

#' @title do WOE transformation for given dataset
#' @description This function would do WOE transformation for given data
#' @param data dataset, need to be data.frame()
#' @param ycol the name for the target column
#' @param positive the level in target y which denotes the positive class
#' @param is_dropsame if TRUE, drop the constant columns after transformation
#'
#' @return `res`, a list with woe_bins = bins, newdata = processed
#' dataset (data.frame())
#' @importFrom dplyr select
#' @importFrom scorecard woebin woebin_ply
#' @importFrom tidyr all_of
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data_woe <- build_woe(diabetic_data[1:100, c("num_procedures",
#'                                                      "readmitted")],
#'                                ycol = "readmitted",
#'                                positive = "YES", is_dropsame = TRUE)
#' @export
build_woe <- function(data, ycol = "readmitted", positive = "YES",
                      is_dropsame = TRUE) {
  bins <- woebin(data, y = ycol, positive = positive, check_cate_num = FALSE)
  data_woe <- woebin_ply(data, bins = bins) |> data.frame()
  if (is_dropsame) {
    same_list <- c()
    for (idx in colnames(data_woe)) {
      if (idx == ycol) {
        next
        }
      if (max(data_woe[, idx]) == min(data_woe[, idx])) {
        same_list <- c(same_list, idx)
      }
    }
    if (!length(same_list)) {
      print("there is no constant columns found")
    } else {
      data_woe <- data_woe |> select(-all_of(same_list))
      print(paste("delete", length(same_list), "constant columns"))
      }
  }
  res <- list(woe_bins = bins, newdata = data_woe)
  return(res)
}



#' @title Get the correspondence between the woe code and the original data
#' @description This function would give a table to show the correspondence
#' between the value after woe transformation and the value in original data
#' @param data_woe dataset after woe transformation. which column names should
#' be the original column names in `data` with `_woe` as suffix
#' @param data original dataset
#'
#' @return data.frame to show the correspondence information
#' @importFrom dplyr select group_by arrange summarise
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c("num_procedures", "readmitted")]
#' diabetic_data_woe <- build_woe(diabetic_data,
#'                                ycol = "readmitted",
#'                                positive = "YES", is_dropsame = TRUE)
#' tab <- get_woe_explain(diabetic_data_woe, diabetic_data)
#' @export
get_woe_explain <- function(data_woe, data) {
  tab <- data.frame()
  for (col in colnames(data_woe)) {
    if (col == "readmitted") {
      next
      }
    col_ori <- substr(col, 1, nchar(col) - 4)
    temp <- cbind(data_woe[col], data[col_ori])
    temp <- temp[!duplicated(temp), ]
    colnames(temp) <- c("woe_value", "original_value")
    if (is.factor(data[, col_ori])) {
      temp <- temp |> group_by(woe_value) |>
        arrange(original_value) |>
        dplyr::summarise(original_value = original_value |> unique() |>
                           paste(collapse = ","),
                         variable_name = col_ori)
      temp <- temp |> select(variable_name, original_value, woe_value)
    } else {
      temp <- temp |> group_by(woe_value) |>
        dplyr::summarize(original_value = paste("[", min(original_value), ", ",
                                                max(original_value), "]",
                                                sep = ""),
                         variable_name = col_ori)
    }
    temp <- temp |> select(variable_name, original_value, woe_value)
    tab <- rbind(tab, temp)
  }
  return(tab)
}



#' @title model selection with iv (Information Value)
#' @description This function would do model selection based on iv
#' (Information Value) for dataset after WOE transformation
#' @param data_woe dataset, need to be data.frame()
#' @param iv_limit delete columns with iv value < iv_limit
#' @param ycol the name for the target column
#' @param positive the level in target y which denotes the positive class
#'
#' @return `out`, a list with data_iv = processed dataset, iv_info =
#' a data.frame to show the iv value for each column
#' @importFrom dplyr mutate
#' @importFrom scorecard iv var_filter
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c("num_procedures", "num_medications",
#'                                         "readmitted")]
#' diabetic_data_woe <- build_woe(diabetic_data, ycol = "readmitted",
#'                                positive = "YES", is_dropsame = TRUE)
#' iv_res <- iv_filter(diabetic_data_woe$newdata,
#'                     iv_limit = .01, ycol = "readmitted",
#'                     positive = "YES")
#' diabetic_data_iv <- iv_res$data_iv
#' iv_info <- iv_res$iv_info
#' @export
iv_filter <- function(data_woe, iv_limit = 0.02, ycol, positive = 1) {
  iv_chart <- iv(data_woe, y = ycol, positive = positive)
  iv_chart <- iv_chart |>
    data.frame() |>
    dplyr::mutate(info_value = info_value |> round(4))
  data_iv <- var_filter(data_woe, y = ycol, iv_limit = iv_limit,
                        positive = positive)
  data_iv[, ycol] <- data_woe[[ycol]] |> as.factor()
  out <- list(data_iv = data_iv, iv_info = iv_chart)
  return(out)
}
