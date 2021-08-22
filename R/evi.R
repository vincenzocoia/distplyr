#' Extreme Value Index
#'
#' @param x Object to obtain EVI from.
#' @param ... Other arguments to pass to specific methods.
#'
#' @return A single numeric.
#' @examples
#' evi(dst_gpd(0, 1, 2))
#' evi(dst_gpd(0, 1, -1))
#' evi(dst_unif(0, 10))
#' evi(dst_norm(5, 5))
#' evi(dst_empirical(1:10))
#' @export
evi <- function(x, ...) {
  UseMethod("evi")
}

#' @export
evi.dst <- function(x, ...) {
  warning("Currently not implemented")
  NA
}
