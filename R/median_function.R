#' A Self-made Median Function
#'
#' This function allows you to calculate the median from a numeric vector.
#' @param x A numeric vector.
#' @keywords median
#' @export
#' @examples
#' median_function(seq(1:10))

median_function <- function(x) {
  x_sorted <- sort(x)
  if (length(x) %% 2 == 1) {
    return(x_sorted[(length(x) + 1) / 2])
  } else {
    return((x_sorted[length(x) / 2] + x_sorted[(length(x) / 2) + 1]) / 2)
  }
}
