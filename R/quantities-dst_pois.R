#' @export
mean.pois <- function(x, ...) {
	with(parameters(x), lambda)
}

#' @export
variance.pois <- function(x, ...) {
	with(parameters(x), lambda)
}

#' @export
evi.pois <- function(x, ...) {
	NaN
}

#' @export
skewness.pois <- function(x, ...) {
	with(parameters(x), lambda^(-0.5))
}

#' @export
kurtosis_exc.pois <- function(x, ...) {
	with(parameters(x), lambda^(-1))
}

#' @rdname range
#' @export
range.pois <- function(x, ...) {
	c(0, Inf)
}
