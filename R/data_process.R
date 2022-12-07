#' @title replacing missing mark in one column with NA
#' @description This function could be use for replacing the missing mark (e.g.
#' Unknown / invalid / null) with NA for a single column
#' @param x the column needed to do the replacement, if is.numeric(), the
#' replacement process would be skipped.
#' @param mark_list the vector including marks which denotes missing
#' @return the processed column
#' @examples
#' data(diabetic_data)
#' diabetic_data$race <- replace_missing_col(diabetic_data$race,
#'                                           mark_list = c('?'))
#' @export
replace_missing_col <-
  function(x, mark_list = c("?", "unknown", "invalid", "not available",
                            "not mapped", "null", "none")) {
  # replace ?/unknown/invalid mark with NA to clarify the missing data
  if (!is.numeric(x)) {
    if (!length(mark_list)) {
      return(x)
    }
    for (i in seq_along(mark_list)){
      mark <- mark_list[i]
      if (mark == "?") {
        mark <- paste("\\", mark, sep = "")
      }
      if (!exists("sent")) {
        sent <- mark
      } else {
        sent <- paste(sent, mark, sep = "|")
        }
    }
    idx <- grepl(sent, x, ignore.case = TRUE)
    if (!any(idx)) {
      return(x)
    }
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
                               "not mapped", "null", "none")) {
  return(lapply(data, replace_missing_col, mark_list = mark_list) |>
           data.frame())
}





#' @title explore the missing variable in dataset and fix it with drop and fill
#' @description This function would do exploration to show the missing pattern
#' in the dataset and try to fix it by dropping and filling
#' @param x dataset, need to be data.frame()
#' @param is_drop if TRUE, do the drop process to fix missing
#' @param upper_pro if the missing proportion is higher than upper.pro for a
#' specific variable, drop this column (only available when is.drop = TRUE)
#' @param lower_pro if the missing proportion is lower than lower.pro for a
#' specific variable, drop the observations that are missing in this column
#' (only available when is.drop = TRUE)
#' @param is_fill if TRUE, do the fill process to fix missing
#' @param fill_mark_fac if a specific column is factor/character,
#' fill the missing value with fill_mark_fac
#' (only available when is.fill = TRUE)
#' @param fill_cre_num if a specific column is numeric, fill the missing value
#' with fill_cre_num (only available when is.fill = TRUE)
#'
#' @return res(list) with pattern_plot = aggr plot; missing table = a
#' data.frame that show the missing situation; newdata = fixed dataset
#' (data.frame())
#' @importFrom dplyr filter select arrange
#' @importFrom tidyr replace_na drop_na all_of
#' @importFrom stats median
#'
#' @examples
#' data(diabetic_data)
#' res <- missing_explore(diabetic_data[1:100, ],
#'              is_drop = TRUE, upper_pro = 0.95, lower_pro = 0.04,
#'              is_fill = TRUE, fill_mark_fac = "others",
#'              fill_cre_num = "median")
#' @export
missing_explore <-
  function(x, is_drop = TRUE, upper_pro = 0.95, lower_pro = 0.04,
              is_fill = TRUE, fill_mark_fac = "others",
           fill_cre_num = "medium") {
  na_name <- colnames(x)[apply(x, 2,
                               function(x) {
                                 return(any(is.na(x)))
                                 })]
  tab <- cbind(colSums(is.na(x)), (colSums(is.na(x)) / dim(x)[1]) |>
                 round(3)) |> data.frame()
  names(tab) <- c("missing_number", "missing_proportion")
  tab <- tab |> filter(missing_proportion > 0)
  if (nrow(tab) == 0) {
    print("no missing data found")
    res <- list(pattern_plot = NULL, missing_table = NULL, newdata = x)
    return(res)
  }
  aggr_plot <- plot_missing_pattern(x, na_name)
  tab <- tab |> arrange(desc(missing_proportion))
  dropcol <- rownames(tab[tab$missing_proportion >= upper_pro, ])
  droprow <- rownames(tab[tab$missing_proportion <= lower_pro, ])
  if (is_drop) {
    oridim <- dim(x)
    if (length(dropcol)) {
      x <- x |>
        select(-all_of(dropcol))
      }
    if (length(droprow)) {
      x <- x |>
      drop_na(all_of(droprow))
      }
    print(paste("auto_process done, with ",
                round((1 - dim(x)[2] / oridim[2]) * 100, 3),
          "% columns deleted and ",
          round((length(dropcol) / oridim[2]) * 100, 3),
          "% rows dropped", sep = ""))
  }
  if (is_fill) {
    for (coln in rownames(tab[tab$missing_proportion < upper_pro &
                             tab$missing_proportion > lower_pro, ])) {
      if (is.numeric(x[[coln]])) {
        if (fill_cre_num == "median") {
          x[coln] <- replace_na(x[[coln]], median(x[[coln]]))
          } else if (fill_cre_num == "avg") {
          x[coln] <- replace_na(x[[coln]], mean(x[[coln]]))
          }
      } else {
        x[coln] <- replace_na(x[[coln]], fill_mark_fac)
        }
    }
  }
  res <- list(pattern_plot = aggr_plot, missing_table = tab, newdata = x)
  return(res)
}




#' @title explore the outlier obs in dataset and fix it with drop
#' @description This function would do exploration to show the possible outlier
#' in the dataset and try to fix it by dropping
#' @param x dataset, need to be data.frame()
#' @param outlier_prop define the maximum proportion of outliers. identify
#' outliers with obs less than Q1-(1.5)IQR or more than Q3+(1.5)IQR and check
#' the rough result with outlier_prop. if more than outlier_prop obs are found,
#' assume they are dispersed due to the feature of the distribution, instead of
#' being outliers. otherwise, assume outliers are found, save the index for
#' processing (if applicable) and plot (if applicable)
#' @param is_drop if TRUE, drop the outliers
#' @param is_plot if TRUE, give the boxplot to show the outliers
#'
#' @return fixed dataset (data.frame())
#' @importFrom dplyr select
#' @importFrom tidyr replace_na drop_na all_of
#' @importFrom grDevices boxplot.stats
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- outlier_explore(diabetic_data[1:100,], outlier_prop = .01,
#'                                  is_drop = FALSE, is_plot = TRUE)
#' @export
outlier_explore <- function(x, outlier_prop = .01,
                            is_drop = FALSE, is_plot = TRUE) {
  outlier_idx <- c()
  dimn <- dim(x)[1]
  col_list <- c()
  for (col in colnames(x)) {
    temp <- x[[col]]
    if (is.numeric(temp)) {
      tempout <- boxplot.stats(temp)$out
      outlier_idx <- c(outlier_idx, which(temp %in% tempout))
      if (length(tempout) > 0 && length(tempout) <= (dimn * outlier_prop)) {
        col_list <- c(col_list, col)
      }
    }
  }
  if (!length(col_list)) {
    print("there is no outlier found based on the given outlier_prop")
    return(x)
  }
  if (is_plot) {
    plot_boxplot(x |> select(all_of(col_list)), 3)
    }
  if (is_drop) {
    x <- x[!(x %in% outlier_idx)]
    }
  return(x)
}




#' @title explore the extreme imbalance columns
#' @description This function would do exploration to find extreme imbalance
#' columns in given dataset
#' @param x dataset, need to be data.frame()
#' @param extreme_prop if a column with more than extreme_prop obs concentrated
#' in one single level, it could say that this column is extreme imbalance
#' @param is_drop if TRUE, drop the extreme imbalance column
#' @param is_table if TRUE, print a table to show the information about the
#' extreme imbalance column found by the rules above
#'
#' @return processed dataset (data.frame())
#' @importFrom dplyr select arrange desc
#' @importFrom tidyr all_of
#'
#' @examples
#' data(diabetic_data)
#' diabetic_data <- extreme_imbalance_col(diabetic_data[1:100, ],
#'                    extreme_prop = .95, is_drop = FALSE, is_table = TRUE)
#' @export
extreme_imbalance_col <- function(x, extreme_prop = 0.95,
                                  is_table = TRUE, is_drop = TRUE) {
  dimn <- dim(x)[1]
  x <- x |> format_factor_dataset()
  factor_cols <- colnames(x)[vapply(x, is.factor, logical(1))]
  numlist <- c()
  collist <- c()
  for (col in factor_cols) {
    tempnum <- (table(x[col]) |> max()) / dimn
    if (tempnum > extreme_prop) {
      numlist <- c(numlist, tempnum |> round(3))
      collist <- c(collist, col)
    }
  }
  if (!length(collist)) {
    print("there is no extreme imbalance columns based on given extreme_prop")
    return(x)
    }
  tab <- cbind(collist, numlist) |>
    data.frame()
  if (is_table) {
    colnames(tab) <- c("column", "proportion")
    print(tab |> arrange(desc(proportion)))
  }
  if (is_drop) {
    x <- x |> select(-all_of(collist))
    print(paste("processed, left", dim(x)[1],
                "rows and", dim(x)[2], "columns"))
    }
  return(x)
}
