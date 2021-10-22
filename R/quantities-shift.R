
#' @export
mean.shift <- function(distribution) {
	with(distribution$components, {
		mean(distribution) + shift
	})
}

#' @export
median.shift <- function(distribution) {
	with(distribution$components, {
		median(distribution) + shift
	})
}

#' @export
stdev.shift <- function(distribution) {
	with(distribution$components, {
		stdev(distribution)
	})
}

#' @export
range.shift <- function(distribution) {
	with(distribution$components, {
		range(distribution) + shift
	})
}

#' @export
variance.shift <- function(distribution) {
	with(distribution$components, {
		variance(distribution)
	})
}

#' @export
evi.shift <- function(distribution) {
	with(distribution$components, {
		evi(distribution)
	})
}

#' @export
skewness.shift <- function(distribution) {
	with(distribution$components, {
		skewness(distribution)
	})
}

#' @export
kurtosis_exc.shift <- function(distribution) {
	with(distribution$components, {
		kurtosis_exc(distribution)
	})
}
