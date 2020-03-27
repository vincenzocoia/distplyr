#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param location Parameter of the distribution family.
#' @return Object of class "dst".
#' @rdname degenerate
#' @export
dst_degenerate <- function(location) {
	stepdst(location, variable = "discrete")
}

#' @rdname degenerate
#' @export
is_degenerate <- function(object) {
	inherits(object, "degenerate")
}

#' @rdname degenerate
#' @export
is.degenerate <- function(object) {
	inherits(object, "degenerate")
}

#' @export
get_mean.degenerate <- function(object) {
	object[["steps"]][["y"]]
}

#' @export
get_median.degenerate <- function(object) {
	get_mean(object)
}

#' @export
get_variance.degenerate <- function(object) {
	0
}

#' @export
get_sd.degenerate <- function(object) {
	0
}

#' @export
get_skewness.degenerate <- function(object) {
	NaN
}

#' @export
get_kurtosis_exc.degenerate <- function(object) {
	NaN
}

#' @export
get_randfn.degenerate <- function(object) {
	location <- get_mean(object)
	function(n) rep(location, n)
}




# Using .stepdst method for:
# - all functional representations (cdf, hazard, etc.), except random number
#    generator.
# - get_evi
