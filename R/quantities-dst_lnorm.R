#' @export
mean.lnorm <- function(x, ...) {
	with(parameters(x), {
		exp(meanlog + variancelog / 2)
	})
}

#' @export
median.lnorm <- function(x, ...) {
	with(parameters(x), {
		exp(meanlog)
	})
}

#' @export
variance.lnorm <- function(x, ...) {
	with(parameters(x), {
		ev <- exp(variancelog)
		(ev - 1) * ev * exp(2 * meanlog)
	})
}


#' @export
skewness.lnorm <- function(x, ...) {
	with(parameters(x), {
		ev <- exp(variancelog)
		(ev + 2) * sqrt(ev - 1)
	})
}

#' @export
kurtosis_exc.lnorm <- function(x, ...) {
	with(parameters(x), {
		e4 <- exp(4 * variancelog)
		e3 <- exp(3 * variancelog)
		e2 <- exp(2 * variancelog)
		e4 + 2 * e3 + 3 * e2 - 6
	})
}

#' @rdname range
#' @export
range.lnorm <- function(x, ...) {
	c(0, Inf)
}
