# utils.R
# useful tools during data process and analysis



#' @title mapping ids with its meaning for dataset
#' @description This function could be use for replacing the id value in original
#' dataset with its meaning
#' @param data the original dataset need to do the replacement. need to be data.frame
#' @param data_map the map information, need to be data.frame,
#' with `column` denotes the variable name, `value` denotes the ids valu,
#' and `description` shows its meaning
#' @return the mapped dataset
#' @importFrom dplyr filter arrange
#' @examples
#' data(diabetic_data)
#' data(IDs_map)
#' data.mapped <- data_ids_map(diabetic_data, IDs_map)
#' @export
data_ids_map <- function(data, data_map){
  if(!("column" %in% colnames(data_map)) | !("value" %in% colnames(data_map)) |
     !("description" %in% colnames(data_map))){
    stop("check the colnames for the mapping information dataset, it should have
         columns named `column`, `value`, `description`")
  }
  for(col in (data_map$column |> unique())){
    temp <- data_map |>
      filter(column == col) |>
      arrange(value)
    data[col] <- temp[data[[col]], "description"]
  }
  return(data)
}


#' @title format given column to factor
#' @description This function could be use for transforming given character/category
#' variables to factor. If the given one is numeric, process would be skipped
#' @param x the original column need to do the transformation
#' @return the processed column
#' @examples
#' data(diabetic_data)
#' diabetic_data$readmitted <- format_factor_col(diabetic_data$readmitted)
#' @export
format_factor_col <- function(x){
  if (!is.numeric(x)){
    x <- x |> as.factor()
  }
  return(x)
}


#' @title format character/category columns to factor for given dataset
#' @description This function could be use for transforming the character/category
#' variables in given dataset to factor. If the column is numeric,
#' process would be skipped
#' @param x the original dataset need to do the transformation
#' @return the processed datasaet
#' @examples
#' data(diabetic_data)
#' diabetic_data <- format_factor_dataset(diabetic_data[1:100, ])
#' @export
format_factor_dataset <- function(x){
  return(lapply(x, format_factor_col) |> data.frame())
}



#' @title encoding ICD-9 to primary category
#' @description This function could be use for transforming ICD-9 codes in dataset
#' to its primary category
#' @param x the original dataset need to do the transformation, need to be data.frame
#' @param data_icd the mapping dataset with ICD-9 codes and its primary category,
#' should have columns (`description`, `code_start`, `code_end`). refer to the sample
#' data `ICD9_map` for more information about data format
#' @param maplist the vector contains column names which need to be mapped
#'
#' @return the processed dataset
#'
#' @importFrom sqldf sqldf
#' @examples
#' data(diabetic_data)
#' data(ICD9_map)
#' diabetic_data <- encode_ICD9(x = diabetic_data[1:100, ], data_icd = ICD9_map,
#'                              maplist = c('diag_1', 'diag_2', 'diag_3'))
#' @export
encode_ICD9 <- function(x, data_icd, maplist = c('diag_1', 'diag_2', 'diag_3')){
  if(!("description" %in% colnames(data_icd)) | !("code_start" %in% colnames(data_icd))
     |!("code_end" %in% colnames(data_icd))){
    stop("cannot find needed columns in given ICD coding data, need columns with
         `description`, `code_start`, `code_end`")
  }
  for(col in maplist){
    print(paste("mapping", col, "with ICD-9 ..."))
    tempcol <- sqldf(paste("select description from x left join data_icd on x.",
                           col, " >= code_start and x.", col, "<= code_end"))[,1]
    tempcol[grepl("E", x)] <- 'External Causes of Injury and Poisoning'
    tempcol[grepl("V", x)] <- 'Factors influencing Health Status and Contact with Health Services'
    tempcol[is.na(tempcol)] <- 'others'
    x[col] <- tempcol |> as.factor()
  }
  return(x)
}
