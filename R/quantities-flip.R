#' @export
mean.negative <- function(distribution, ...) {
	with(distribution, {
		-mean(distribution)
	})
}

#' @export
median.negative <- function(distribution, ...) {
	with(distribution, {
		-median(distribution)
	})
}

#' @export
variance.negative <- function(distribution, ...) {
	with(distribution, {
		variance(distribution)
	})
}

#' @export
stdev.negative <- function(distribution, ...) {
	with(distribution, {
		stdev(distribution)
	})
}

#' @export
skewness.negative <- function(distribution, ...) {
	with(distribution, {
		-skewness(distribution)
	})
}

#' @export
kurtosis_exc.negative <- function(distribution, ...) {
	with(distribution, {
		kurtosis_exc(distribution)
	})
}

#' @export
range.negative <- function(distribution, ...) {
	with(distribution, {
		d <- range(distribution)
		c(-d[2], -d[1])
	})
}
