#' @export
mean.norm <- function(x, ...) {
	with(parameters(x), mean)
}

#' @export
median.norm <- function(x, ...) {
	with(parameters(x), mean)
}

#' @export
variance.norm <- function(x, ...) {
	with(parameters(x), variance)
}

#' @export
evi.norm <- function(x, ...) {
	0
}

#' @export
skewness.norm <- function(x, ...) {
	0
}

#' @export
kurtosis_exc.norm <- function(x, ...) {
	0
}

#' @rdname range
#' @export
range.norm <- function(x, ...) {
	c(-Inf, Inf)
}
