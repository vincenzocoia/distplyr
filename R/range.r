#' @title Range of Distribution
#' @description Range returns a 2 index vector with the 0th index
#' containing the minimum value, and the 1st index containing the maximum value
#' for a given distribution; currently, the ellipsis argument has no
#' functionality.
#' @param x Object to compute range
#' @param ... Other arguments to pass to specific methods. Currently not used.
#' @details If there are no methods for the x's class, the range is calculated
#' using eval_quantile at 0 and at 1
#' @examples
#' a <- dst_gpd(0, 1, 0.5)
#' b <- dst_unif(0, 1)
#' c <- dst_norm(3, 4)
#' range(a)
#' range(b)
#' range(c)
#' @export
range <- function(x, ...) {
  UseMethod("range")
}

#' @rdname range
#' @export
range.dst <- function(x, ...) {
  eval_quantile(x, at = 0:1)
}
