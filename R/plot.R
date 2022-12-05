
#' @title plot missing pattern
#' @description This function could be use for plotting the missing proportion and
#' possible patterns for specific dataset
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
plot_missing_pattern <- function(data, nalist){
  data <- data|>
    select(all_of(nalist))
  p <- aggr(data, plot = FALSE)
  plot(p)
}




#' @title plot outliers
#' @description This function could be used for giving a series of boxplot to
#' show the possible outliers
#' @param x the dataset for plot
#' @param ncol the number of plots to show per row
#'
#' @importFrom dplyr select
#' @importFrom ggplot2 ggplot aes geom_boxplot coord_flip labs theme element_text
#' @importFrom gridExtra grid.arrange
#' @examples
#' data(diabetic_data)
#' plot_boxplot(diabetic_data[1:100, c("num_procedures", "num_medications")])
#' @export
plot_boxplot <- function(x, ncol = 2){
  glist <- list(NULL)
  for(idx in 1:length(colnames(x))){
    glist[[idx]] <- ggplot(data = x, aes(x = .data[[colnames(x)[idx]]])) +
      geom_boxplot() +
      coord_flip() +
      labs(title = colnames(x)[idx])
  }
  grid.arrange(grobs = glist, ncol = min(length(glist), ncol))
}
