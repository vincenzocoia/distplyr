
#' @export
mean.shift <- function(object) {
	with(object$components, {
		mean(distribution) + shift
	})
}

#' @export
median.shift <- function(object) {
	with(object$components, {
		median(distribution) + shift
	})
}

#' @export
stdev.shift <- function(object) {
	with(object$components, {
		stdev(distribution)
	})
}

#' @export
range.shift <- function(object) {
	with(object$components, {
		range(distribution) + shift
	})
}

#' @export
variance.shift <- function(object) {
	with(object$components, {
		variance(distribution)
	})
}

#' @export
evi.shift <- function(object) {
	with(object$components, {
		evi(distribution)
	})
}

#' @export
skewness.shift <- function(object) {
	with(object$components, {
		skewness(distribution)
	})
}

#' @export
kurtosis_exc.shift <- function(object) {
	with(object$components, {
		kurtosis_exc(distribution)
	})
}
