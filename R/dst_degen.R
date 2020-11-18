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
mean.degenerate <- function(object, ...) {
	discontinuities(object)[["location"]]
}

#' @export
median.degenerate <- function(object, ...) {
	mean(object)
}

#' @export
variance.degenerate <- function(object, ...) {
	0
}

#' @export
sd.degenerate <- function(object, ...) {
	0
}

#' @export
skewness.degenerate <- function(object) {
	NaN
}

#' @export
kurtosis_exc.degenerate <- function(object) {
	NaN
}

#' @export
realise.degenerate <- function(object, n = 1, ...) {
	location <- mean(object)
	rep(location, n)
}




# Using .stepdst method for:
# - all functional representations (cdf, hazard, etc.), except random number
#    generator.
# - evi
