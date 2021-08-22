#' @export
mean.scale <- function(object) {
	with(object$components, {
		mean(distribution) * scale
	})
}

#' @export
median.scale <- function(object) {
	with(object$components, {
		median(distribution) * scale
	})
}

#' @export
stdev.scale <- function(object) {
	with(object$components, {
		stdev(distribution) * scale
	})
}

#' @export
variance.scale <- function(object) {
	with(object$components, {
		variance(distribution) * scale^2
	})
}

#' @export
evi.scale <- function(object) {
	with(object$components, {
		evi(distribution)
	})
}

#' @export
skewness.scale <- function(object) {
	with(object$components, {
		skewness(distribution)
	})
}

#' @export
kurtosis_exc.scale <- function(object) {
	with(object$components, {
		kurtosis_exc(distribution)
	})
}

#' @export
range.scale <- function(object) {
	with(object$components, {
		range(distribution) * scale
	})
}
