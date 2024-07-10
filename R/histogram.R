#' Plot Histogram
#'
#' This function creates a histogram for a specified column in a data frame.
#'
#' @param df A data frame.
#' @param column The name of the column to plot.
#' @param bins Number of bins for the histogram.
#' @return A ggplot object.
#' @export
#'
#' @examples
#' data(iris)
#' plot_histogram(iris, 'Sepal.Length')
plot_histogram <- function(df, column, bins = 30) {
  library(ggplot2)
  p <- ggplot(df, aes_string(column)) +
    geom_histogram(bins = bins, fill = 'blue', color = 'black') +
    theme_minimal() +
    labs(title = paste('Histogram of', column), x = column, y = 'Frequency')
  return(p)
}
