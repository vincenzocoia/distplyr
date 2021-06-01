#' Extreme Value Index
#'
#' @param x Object to obtain EVI from.
#' @param ... Other arguments to pass to specific methods.
#'
#' @return A single numeric.
#' @export
evi <- function(x, ...) {
  UseMethod("evi")
}

#' Extreme Value Index
#'
#' @param x Distribution object to obtain EVI from.
#' @param ... Not used.
#'
#' @return A single numeric.
#'
#' @details Doesn't calculate EVI if not present in the object.
#'
#' @examples
#' evi(dst_gpd(0, 1, 2))
#' evi(dst_gpd(0, 1, -1))
#' evi(dst_unif(0, 10))
#' evi(dst_norm(5, 5))
#' evi(dst_empirical(1:10))
#' @export
evi.dst <- function(x, ...) {
  x$prop$evi
}
