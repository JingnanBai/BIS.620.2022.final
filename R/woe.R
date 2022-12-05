

#' @title do WOE transformation for given dataset
#' @description This function would do WOE transformation for given data
#' @param data dataset, need to be data.frame()
#' @param ycol the name for the target column
#' @param positive the level in target y which denotes the positive class
#' @param is.dropsame if TRUE, drop the constant columns after transformation
#'
#' @return processed dataset (data.frame())
#' @importFrom dplyr select
#' @importFrom scorecard woebin woebin_ply
#' @importFrom tidyr all_of
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data.woe <- build_woe(diabetic_data[1:100, c("num_procedures", "readmitted")],
#'                                ycol = "readmitted",
#'                                positive = "YES", is.dropsame = TRUE)
#' @export
build_woe <- function(data, ycol = "readmitted", positive = "YES",
                      is.dropsame = TRUE){
  bins <- woebin(data, y=ycol, positive = positive, check_cate_num = FALSE)
  data.woe <- woebin_ply(data, bins = bins) |> data.frame()
  if(is.dropsame){
    same.list <- c()
    for(idx in colnames(data.woe)){
      if (idx == ycol){next}
      if(max(data.woe[,idx])==min(data.woe[,idx])){
        same.list <- c(same.list, idx)
      }
    }
    if(!length(same.list)){
      print("there is no constant columns found")
    } else{
      data.woe <- data.woe |> select(-all_of(same.list))
      print(paste("delete", length(same.list), "constant columns"))}
  }
  return(data.woe)
}



#' @title Get the correspondence between the woe code and the original data
#' @description This function would give a table to show the correspondence between
#' the value after woe transformation and the value in original data
#' @param data.woe dataset after woe transformation. which column names should be
#' the original column names in `data` with `_woe` as suffix
#' @param data original dataset
#'
#' @return data.frame to show the correspondence information
#' @importFrom dplyr select group_by arrange summarise
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c("num_procedures", "readmitted")]
#' diabetic_data.woe <- build_woe(diabetic_data,
#'                                ycol = "readmitted",
#'                                positive = "YES", is.dropsame = TRUE)
#' tab <- get_woe_explain(diabetic_data.woe, diabetic_data)
#' @export
get_woe_explain <- function(data.woe, data){
  tab <- data.frame()
  for(col in colnames(data.woe)){
    if(col == 'readmitted'){next}
    col.ori <- substr(col, 1, nchar(col)-4)
    temp <- cbind(data.woe[col], data[col.ori])
    temp <- temp[!duplicated(temp), ]
    colnames(temp) <- c("woe_value", "original_value")
    if(is.factor(data[,col.ori])){
      temp <- temp |> group_by(woe_value) |>
        arrange(original_value) |>
        dplyr::summarise(original_value = original_value |> unique() |>
                           paste(collapse = ","),
                         variable_name = col.ori)
      temp <- temp |> select(variable_name, original_value, woe_value)
    } else{
      temp <- temp |> group_by(woe_value) |>
        dplyr::summarize(original_value = paste('[', min(original_value), ', ',
                                                max(original_value),']',
                                                sep = ""),
                         variable_name = col.ori)
    }
    temp <- temp |> select(variable_name, original_value, woe_value)
    tab <- rbind(tab, temp)
  }
  return(tab)
}



#' @title model selection with iv (Information Value)
#' @description This function would do model selection based on iv (Information Value)
#' for dataset after WOE transformation
#' @param data.woe dataset, need to be data.frame()
#' @param iv_limit delete columns with iv value < iv_limit
#' @param ycol the name for the target column
#' @param positive the level in target y which denotes the positive class
#'
#' @return `out`, a list with data.iv = processed dataset, iv.info = a data.frame
#' to show the iv value for each column
#' @importFrom dplyr mutate
#' @importFrom scorecard iv var_filter
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- diabetic_data[1:100, c("num_procedures", "readmitted")]
#' diabetic_data.woe <- build_woe(diabetic_data, ycol = "readmitted",
#'                                positive = "YES", is.dropsame = TRUE)
#' iv_res <- iv_filter(diabetic_data.woe, iv_limit = .02, ycol = "readmitted",
#'                     positive = "YES")
#' diabetic_data.iv <- iv_res$data.iv
#' iv.info <- iv_res$iv.info
#' @export
iv_filter <- function(data.woe, iv_limit = 0.02, ycol = "readmitted",
                      positive = "YES"){
  dim.ori <- dim(data.woe)
  iv_chart <- iv(data.woe, y='readmitted', positive = 'YES')
  iv_chart <- iv_chart |>
    data.frame() |>
    dplyr::mutate(info_value = info_value |> round(4))
  data.iv <- var_filter(data.woe, y='readmitted', iv_limit = iv_limit,
                        positive = 'YES')
  data.iv[,ycol] <- data.woe[[ycol]] |> as.factor()
  out <- list(data.iv = data.iv, iv.info = iv_chart)
  return(out)
}
