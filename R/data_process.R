#' @title replacing missing mark in one column with NA
#' @description This function could be use for replacing the missing mark (e.g.
#' Unknown / invalid / null) with NA for a single column
#' @param x the column needed to do the replacement, if is.numeric(), the replacement
#' process would be skipped.
#' @param mark_list the vector including marks which denotes missing
#' @return the processed column
#' @examples
#' data(diabetic_data)
#' diabetic_data$race <- replace_missing_col(diabetic_data$race, mark_list = c('?'))
#' @export
replace_missing_col <-
  function(x, mark_list = c("?", "unknown", "invalid", "not available",
                            "not mapped", "null", "none")){
  # replace ?/unknown/invalid mark with NA to clarify the missing data
  if (!is.numeric(x)){
    if(!length(mark_list)){return(x)}
    pred_list <- c()
    for(i in 1:length(mark_list)){
      mark <- mark_list[i]
      if(mark == "?"){mark = paste("\\", mark, sep = "")}
      if(!exists("sent")){sent <- mark}
      else{sent <- paste(sent, mark, sep = "|")}
    }
    # "\\?|unknown|invalid|null|not available|not mapped"
    idx <- grepl(sent, x, ignore.case = TRUE)
    if(!any(idx)){return(x)}
    x[grepl(sent, x, ignore.case = TRUE)] <- NA
  }
  return(x)
}





#' @title replacing missing mark in dataset with NA
#' @description This function could be use for replacing the missing mark (e.g.
#' Unknown / invalid / null) with NA for a dataset
#' @param data the dataset needed to do the replacement
#' @param mark_list the vector including marks which denotes missing
#' @return the processed column
#' @examples
#' data(diabetic_data)
#' diabetic_data <- replace_missing_dataset(diabetic_data[1:100,],
#'                                          mark_list = c("?", "null"))
#' @export
replace_missing_dataset <-
function(data, mark_list = c("?", "unknown", "invalid", "not available",
                               "not mapped", "null", "none")){
  return(lapply(data, replace_missing_col, mark_list = mark_list) |>
           data.frame())
}





#' @title explore the missing variable in dataset and fix it with drop and fill
#' @description This function would do exploration to show the missing pattern
#' in the dataset and try to fix it by dropping and filling
#' @param x dataset, need to be data.frame()
#' @param is.drop if TRUE, do the drop process to fix missing
#' @param upper.pro if the missing proportion is higher than upper.pro for a
#' specific variable, drop this column (only available when is.drop = TRUE)
#' @param lower.pro if the missing proportion is lower than lower.pro for a
#' specific variable, drop the observations that are missing in this column
#' (only available when is.drop = TRUE)
#' @param is.fill if TRUE, do the fill process to fix missing
#' @param fill_mark_fac if a specific column is factor/character, fill the missing
#' value with fill_mark_fac (only available when is.fill = TRUE)
#' @param fill_cre_num if a specific column is numeric, fill the missing value
#' with fill_cre_num (only available when is.fill = TRUE)
#' @param is.plot if TRUE, give the plot to show the missing pattern
#' @param is.table if TRUE, give a table to show the missing proportion
#'
#' @return fixed dataset (data.frame())
#' @importFrom dplyr filter select arrange
#' @importFrom tidyr replace_na drop_na all_of
#' @importFrom stats median
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- missing_explore(diabetic_data[1:100, ],
#'              is.drop = TRUE, upper.pro = 0.95, lower.pro = 0.04,
#'              is.fill = TRUE, fill_mark_fac = "others", fill_cre_num = "median",
#'              is.plot = TRUE, is.table = TRUE)
#' @export
missing_explore <-
  function(x, is.drop = TRUE, upper.pro = 0.95, lower.pro = 0.04,
              is.fill = TRUE, fill_mark_fac = "others", fill_cre_num = "medium",
           is.plot = TRUE, is.table = TRUE){
  na.name <- colnames(x)[apply(x, 2, function(x){return(any(is.na(x)))})]
  tab <- cbind(colSums(is.na(x)), (colSums(is.na(x))/dim(x)[1]) |> round(3)) |>
    data.frame()
  names(tab) <- c('missing_number', 'missing_proportion')
  tab <- tab |> filter(missing_proportion > 0)
  if(nrow(tab)==0){
    print("no missing data found")
    return(x)
  }
  if(is.plot){
    plot_missing_pattern(x, na.name)
  }
  if(is.table){
    print(tab |> arrange(desc(missing_proportion)))
  }
  dropcol <- rownames(tab[tab$missing_proportion >= upper.pro,])
  droprow <- rownames(tab[tab$missing_proportion <= lower.pro,])
  if(is.drop){
    oridim <- dim(x)
    if(length(dropcol)){
      x <- x |>
        select(-all_of(dropcol))}
    if(length(droprow)){
      x <- x |>
      drop_na(all_of(droprow))}
    print(paste("auto_process done, with ", round((1-dim(x)[2]/oridim[2])*100, 3),
          "% columns deleted and ", round((length(dropcol)/oridim[2])*100, 3),
          "% rows dropped", sep = ""))
  }
  if(is.fill){
    for(coln in rownames(tab[tab$missing_proportion < upper.pro &
                             tab$missing_proportion > lower.pro,])){
      if(is.numeric(x[[coln]])){
        if(fill_cre_num == 'median'){x[coln] <- replace_na(x[[coln]], median(x[[coln]]))}
        else if(fill_cre_num == "avg"){x[coln] <- replace_na(x[[coln]], mean(x[[coln]]))}
      } else {
        x[coln] <- replace_na(x[[coln]], fill_mark_fac)}
    }
  }
  return(x)
}




#' @title explore the outlier obs in dataset and fix it with drop
#' @description This function would do exploration to show the possible outlier
#' in the dataset and try to fix it by dropping
#' @param x dataset, need to be data.frame()
#' @param outlier.prop define the maximum proportion of outliers. identify outliers
#' with obs less than Q1-(1.5)IQR or more than Q3+(1.5)IQR and check the rough
#' result with outlier.prop. if more than outlier.prop obs are found, assume they are
#' dispersed due to the feature of the distribution, instead of being outliers.
#' otherwise, assume outliers are found, save the index for processing (if applicable)
#' and plot (if applicable)
#' @param is.drop if TRUE, drop the outliers
#' @param is.plot if TRUE, give the boxplot to show the outliers
#'
#' @return fixed dataset (data.frame())
#' @importFrom dplyr select
#' @importFrom tidyr replace_na drop_na all_of
#' @importFrom grDevices boxplot.stats
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- outlier_explore(diabetic_data[1:100,], outlier.prop = .01,
#'                                  is.drop = FALSE, is.plot = TRUE)
#' @export
outlier_explore <- function(x, outlier.prop = .01,
                            is.drop = FALSE, is.plot = TRUE){
  outlier_idx <- c()
  dimn <- dim(x)[1]
  col_list <- c()
  for(col in colnames(x)){
    temp <- x[[col]]
    if(is.numeric(temp)){
      tempout <- boxplot.stats(temp)$out
      outlier_idx <- c(outlier_idx, which(temp %in% tempout))
      if(length(tempout)>0 & length(tempout)<=(dimn * outlier.prop)){
        col_list <- c(col_list, col)
      }
    }
  }
  if(!length(col_list)){
    print("there is no outlier found based on the given outlier.prop")
    return(x)
  }
  if(is.plot){plot_boxplot(x |> select(all_of(col_list)), 3)}
  if(is.drop){x <- x[!(x %in% outlier_idx)]}
  return(x)
}




#' @title explore the extreme imbalance columns
#' @description This function would do exploration to find extreme imbalance columns
#' in given dataset
#' @param x dataset, need to be data.frame()
#' @param extreme.prop if a column with more than extreme.prop obs concentrated in
#' one single level, it could say that this column is extreme imbalance
#' @param is.drop if TRUE, drop the extreme imbalance column
#' @param is.table if TRUE, print a table to show the information about the
#' extreme imbalance column found by the rules above
#'
#' @return processed dataset (data.frame())
#' @importFrom dplyr select arrange desc
#' @importFrom tidyr all_of
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- extreme_imbalance_col(diabetic_data[1:100, ], extreme.prop = .95,
#'                                  is.drop = FALSE, is.table = TRUE)
#' @export
extreme_imbalance_col <- function(x, extreme.prop = 0.95,
                                  is.table = TRUE, is.drop = TRUE){
  dimn = dim(x)[1]
  x <- x |> format_factor_dataset()
  factor_cols <- colnames(x)[vapply(x, is.factor, logical(1))]
  numlist <- c()
  collist <- c()
  for(col in factor_cols){
    tempnum <- (table(x[col]) |> max())/dimn
    if(tempnum > extreme.prop){
      numlist <- c(numlist, tempnum |> round(3))
      collist <- c(collist, col)
    }
  }
  if(!length(collist)){
    print("there is no extreme imbalance columns based on given extreme.prop")
    return(x)}
  tab <- cbind(collist, numlist) |>
    data.frame()
  if(is.table){
    colnames(tab) <- c("column", "proportion")
    print(tab|>
            arrange(desc(proportion)))
  }
  if(is.drop){
    x <- x |> select(-all_of(collist))
    print(paste("processed, left", dim(x)[1], "rows and", dim(x)[2], "columns"))}
  return(x)
}
