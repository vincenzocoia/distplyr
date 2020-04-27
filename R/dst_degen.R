#' Make a Degenerate Distribution
#'
#' Makes a distribution belonging to the degenerate family of
#' distributions. That is, distributions of fixed values.
#' @param location Parameter of the distribution family.
#' @return Object of class "dst".
#' @examples
#' require(graphics)
#' d <- dst_degenerate(5)
#' plot(d, "quantile")
#' @rdname degenerate
#' @export
dst_degenerate <- function(location) {
	if (!is.numeric(location)) {
		stop("'location' parameter must be numeric.")
	}
	df <- make_discontinuities_df(location, size = 1)
	res <- list(name = "Degenerate",
				discontinuities = df,
				parameters = list(location = location))
	new_stepdst(res, variable = "discrete", class = "degenerate")
}

#' @param object Object to test
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
get_mean.degenerate <- function(object, ...) {
	discontinuities(object)[["location"]]
}

#' @export
get_median.degenerate <- function(object) {
	get_mean(object)
}

#' @export
get_variance.degenerate <- function(object, ...) {
	0
}

#' @export
get_sd.degenerate <- function(object, ...) {
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
realise.degenerate <- function(object, n = 1, ...) {
	location <- get_mean(object)
	rep(location, n)
}




# Using .stepdst method for:
# - all functional representations (cdf, hazard, etc.), except random number
#    generator.
# - get_evi
