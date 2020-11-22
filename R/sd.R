#' @rdname moments
#' @export
sd <- function(x, ...) {
	UseMethod("sd")
}


#' @rdname moments
#' @export
sd.dst <- function(x, ...) {
	ss <- variance(x, ...)
	sqrt(ss)
}


#' Dispatch sd function from stats
#'
#' @param x Object for which to calculate standard deviation
#' @param ... Other arguments to pass to \code{sd} function
#' from the stats package.
#'
#' @export
sd.default <- function(x, ...) {
	stats::sd(x, ...)
}
