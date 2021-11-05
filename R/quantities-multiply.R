#' @export
mean.scale <- function(distribution) {
	with(distribution$components, {
		distionary::mean(distribution) * scale
	})
}

#' @export
median.scale <- function(distribution) {
	with(distribution$components, {
		distionary::median(distribution) * scale
	})
}

#' @export
stdev.scale <- function(distribution) {
	with(distribution$components, {
		distionary::stdev(distribution) * scale
	})
}

#' @export
variance.scale <- function(distribution) {
	with(distribution$components, {
		distionary::variance(distribution) * scale^2
	})
}

#' @export
evi.scale <- function(distribution) {
	with(distribution$components, {
		distionary::evi(distribution)
	})
}

#' @export
skewness.scale <- function(distribution) {
	with(distribution$components, {
		distionary::skewness(distribution)
	})
}

#' @export
kurtosis_exc.scale <- function(distribution) {
	with(distribution$components, {
		distionary::kurtosis_exc(distribution)
	})
}

#' @export
range.scale <- function(distribution) {
	with(distribution$components, {
		distionary::range(distribution) * scale
	})
}
