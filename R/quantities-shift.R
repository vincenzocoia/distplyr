
#' @export
mean.shift <- function(distribution) {
	with(distribution$components, {
		distionary::mean(distribution) + shift
	})
}

#' @export
median.shift <- function(distribution) {
	with(distribution$components, {
		distionary::median(distribution) + shift
	})
}

#' @export
stdev.shift <- function(distribution) {
	with(distribution$components, {
		distionary::stdev(distribution)
	})
}

#' @export
range.shift <- function(distribution) {
	with(distribution$components, {
		distionary::range(distribution) + shift
	})
}

#' @export
variance.shift <- function(distribution) {
	with(distribution$components, {
		distionary::variance(distribution)
	})
}

#' @export
evi.shift <- function(distribution) {
	with(distribution$components, {
		distionary::evi(distribution)
	})
}

#' @export
skewness.shift <- function(distribution) {
	with(distribution$components, {
		distionary::skewness(distribution)
	})
}

#' @export
kurtosis_exc.shift <- function(distribution) {
	with(distribution$components, {
		distionary::kurtosis_exc(distribution)
	})
}
