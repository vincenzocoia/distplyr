#' @export
mean.scale <- function(distribution) {
	with(distribution$components, {
		mean(distribution) * scale
	})
}

#' @export
median.scale <- function(distribution) {
	with(distribution$components, {
		median(distribution) * scale
	})
}

#' @export
stdev.scale <- function(distribution) {
	with(distribution$components, {
		stdev(distribution) * scale
	})
}

#' @export
variance.scale <- function(distribution) {
	with(distribution$components, {
		variance(distribution) * scale^2
	})
}

#' @export
evi.scale <- function(distribution) {
	with(distribution$components, {
		evi(distribution)
	})
}

#' @export
skewness.scale <- function(distribution) {
	with(distribution$components, {
		skewness(distribution)
	})
}

#' @export
kurtosis_exc.scale <- function(distribution) {
	with(distribution$components, {
		kurtosis_exc(distribution)
	})
}

#' @export
range.scale <- function(distribution) {
	with(distribution$components, {
		range(distribution) * scale
	})
}
