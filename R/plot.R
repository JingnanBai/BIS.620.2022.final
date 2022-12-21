
#' @title plot missing pattern
#' @description This function could be use for plotting the missing proportion
#' and possible patterns for specific dataset
#' @param data the dataset for plot
#' @param nalist the vector of column names which contains missing data
#' @return the aggr obj
#'
#' @importFrom VIM aggr
#' @importFrom dplyr select all_of
#' @examples
#' data(diabetic_data)
#' plot_missing_pattern(diabetic_data[1:100, ],
#'           nalist = c("medical_specialty", "payer_code", "A1Cresult"))
#' @export
plot_missing_pattern <- function(data, nalist) {
  data <- data|>
    select(all_of(nalist))
  p <- aggr(data, plot = FALSE)
  return(p)
}




#' @title plot outliers
#' @description This function could be used for giving a series of boxplot to
#' show the possible outliers
#' @param x the dataset for plot
#' @param ncol the number of plots to show per row
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip labs theme
#' element_text
#' @importFrom gridExtra grid.arrange
#' @examples
#' data(diabetic_data)
#' plot_boxplot(diabetic_data[1:100, c("num_procedures", "num_medications")])
#' @export
plot_boxplot <- function(x, ncol = 2) {
  glist <- list(NULL)
  for (idx in seq_along(colnames(x))) {
    glist[[idx]] <- ggplot(data = x, aes(x = .data[[colnames(x)[idx]]])) +
      geom_boxplot() +
      coord_flip() +
      labs(title = colnames(x)[idx])
  }
  grid.arrange(grobs = glist, ncol = min(length(glist), ncol))
}



#' @title give mosaic plot for given columns
#' @description This function could be used for giving a series of mosaic plots
#' for given columns, to explore the relationship between the given variables
#' and the target column (denote as ycol)
#' @param data the dataset for plot
#' @param ycol the column name for the target variable
#' @param xcol a vector of column names, the function would draw mosaic plot
#' for each variable in xcol with ycol (the target one)
#' @param ncol the number of plots to show per row
#'
#' @importFrom dplyr select all_of
#' @importFrom ggplot2 ggplot aes theme element_text
#' @importFrom ggmosaic geom_mosaic scale_x_productlist
#' @importFrom gridExtra grid.arrange
#' @examples
#' data(diabetic_data)
#' plot_mosaic(diabetic_data[1:100, ], "readmitted", c("insulin"), ncol = 3)
#' @export
plot_mosaic <- function(data, ycol, xcol, ncol = 3) {
  data <- data |>
    select(all_of(c(xcol, ycol))) |>
    format_factor_dataset()
  n_plot <- length(xcol)
  glist <- list(NULL)
  for (idx in 1:n_plot) {
    xcol_temp <- xcol[idx]
    data_temp <- data[!is.na(data[, xcol_temp]), ]
    wrap_list <- list(NULL)
    wrap_list[[1]] <- ycol |> as.name()
    wrap_list[[2]] <- xcol_temp |> as.name()
    glist[[idx]] <- ggplot(data = data_temp) +
      geom_mosaic(aes(x = wrap_list), offset = .02) +
      ggmosaic::scale_x_productlist() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
  }
  gridExtra::grid.arrange(grobs = glist, ncol = min(length(glist), ncol))
}


#' @title give histogram plot for given columns
#' @description This function could be used for giving a series of histograms
#' for given columns, to explore the relationship between the given variables
#' and the target column (denote as ycol)
#' @param data the dataset for plot
#' @param ycol the column name for the target variable
#' @param xcol a vector of column names, the function would draw histograms
#' for each variable in xcol with ycol (the target one)
#' @param ncol the number of plots to show per row
#' @param bins used for histograms
#'
#' @importFrom dplyr select all_of
#' @importFrom ggplot2 ggplot aes geom_freqpoly after_stat
#' @importFrom gridExtra grid.arrange
#' @importFrom stats density
#' @examples
#' data(diabetic_data)
#' plot_comparehist(diabetic_data[1:100, ], "readmitted",
#'                            c("num_medications"), ncol = 4)
#' @export
plot_comparehist <- function(data, ycol, xcol, ncol = 4, bins = 10) {
  data <- data |>
    select(all_of(c(xcol, ycol)))
  n_plot <- length(xcol)
  glist <- list(NULL)
  for (idx in 1:n_plot) {
    xcol_temp <- xcol[idx]
    glist[[idx]] <- ggplot(data = data, aes(.data[[xcol_temp]],
                                            after_stat(density),
                                            colour = .data[[ycol]])) +
      geom_freqpoly(bins = bins)
  }
  grid.arrange(grobs = glist, ncol = min(length(glist), ncol))
}


#' @title give histogram plot for given columns
#' @description This function could be used for giving the stack barplot with
#' line to explore more about the WOE encoding for specific variable
#' @param bins_woe the bins created by WOE function from `scorecard`
#' @param xcol a vector of target column names
#' @param ncol the number of plots to show per row
#'
#' @importFrom scorecard woebin_plot
#' @importFrom gridExtra grid.arrange
#' @examples
#' data(diabetic_data)
#' diabetic_data[1:1, "metformin.rosiglitazone"] <- "YES"
#' res_woe_temp <- build_woe(diabetic_data[1:100,
#'             c("metformin.rosiglitazone", "time_in_hospital", "readmitted")],
#'             ycol = "readmitted",
#'             positive = "YES", is_dropsame = TRUE)
#' get_woe_plot(res_woe_temp$woe_bins, xcol = c("time_in_hospital"), ncol = 2)
#' @export
get_woe_plot <- function(bins_woe, xcol, ncol = 2) {
  woe_plot <- woebin_plot(bins_woe)
  glist <- woe_plot[xcol]
  grid.arrange(grobs = glist, ncol = min(length(glist), ncol))
}
