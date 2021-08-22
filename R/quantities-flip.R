#' @export
mean.negative <- function(object, ...) {
	with(object, {
		-mean(distribution)
	})
}

#' @export
median.negative <- function(object, ...) {
	with(object, {
		-median(distribution)
	})
}

#' @export
variance.negative <- function(object, ...) {
	with(object, {
		variance(distribution)
	})
}

#' @export
stdev.negative <- function(object, ...) {
	with(object, {
		stdev(distribution)
	})
}

#' @export
skewness.negative <- function(object, ...) {
	with(object, {
		-skewness(distribution)
	})
}

#' @export
kurtosis_exc.negative <- function(object, ...) {
	with(object, {
		kurtosis_exc(distribution)
	})
}

#' @export
range.negative <- function(object, ...) {
	with(object, {
		d <- range(distribution)
		c(-d[2], -d[1])
	})
}
