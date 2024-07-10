#' Summary Statistics
#'
#' This function calculates summary statistics for a given data frame.
#'
#' @param df A data frame.
#' @return A data frame with summary statistics.
#' @export
#'
#' @examples
#' data(iris)
#' summary_stats(iris)
summary_stats <- function(df) {
  summary <- data.frame(
    Variable = names(df),
    Mean = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
    Median = sapply(df, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
    SD = sapply(df, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA),
    Min = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
    Max = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA)
  )
  return(summary)
}
