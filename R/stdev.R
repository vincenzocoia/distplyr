#' @rdname moments
#' @export
stdev <- function(x, ...) {
  UseMethod("stdev")
}


#' @rdname moments
#' @export
stdev.dst <- function(x, ...) {
  ss <- variance(x, ...)
  sqrt(ss)
}


#' Dispatch sd function from stats
#'
#' @param x Object for which to calculate standard deviation
#' @param ... Other arguments to pass to \code{sd} function
#' from the stats package.
#' @export
stdev.default <- function(x, ...) {
  stats::sd(x, ...)
}
