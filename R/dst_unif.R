#' Make a Uniform distribution
#'
#' Makes a distribution belonging to the continuous uniform family of
#' distributions.
#' @param min,max Parameters of the distribution family.
#' @return Object of class "dst".
#' @export
dst_unif <- function(min = 0, max = 1) {
	if (max < min) stop("Parameter 'min' must be less than 'max'.")
	if (max == min) return(dst_degenerate(min))
	res <- list(discontinuities = make_empty_discontinuities_df(),
				parameters = list(min = min, max = max))
	new_dst(
		res,
		variable = "continuous",
		class    = c("unif", "parametric")
	)
}



#' @export
get_mean.unif <- function(object, ...) {
	with(parameters(object), (min + max) / 2)
}

#' @export
get_median.unif <- function(object) {
	with(parameters(object), (min + max) / 2)
}

#' @export
get_variance.unif <- function(object, ...) {
	with(parameters(object), (min - max)^2 / 12)
}


#' @export
get_evi.unif <- function(object) {
	-1
}

#' @export
get_skewness.unif <- function(object) {
	0
}

#' @export
get_kurtosis_exc.unif <- function(object) {
	-6/5
}

#' @export
get_cdf.unif <- function(object) {
	with(
		parameters(object),
		function(x) stats::punif(x, min = min, max = max)
	)
}

#' @export
get_survival.unif <- function(object) {
	with(
		parameters(object),
		function(x) stats::punif(x, min = min, max = max, lower.tail = FALSE)
	)
}

#' @export
get_probfn.unif <- function(object) {
	with(
		parameters(object),
		function(x) stats::dunif(x, min = min, max = max)
	)
}

#' @export
get_randfn.unif <- function(object) {
	with(
		parameters(object),
		function(n) stats::runif(n, min = min, max = max)
	)
}

#' @export
get_quantile.unif <- function(object, ...) {
	with(
		parameters(object),
		function(x) stats::qunif(x, min = min, max = max)
	)
}


# Using .dst method for:
# - get_hazard
# - get_chf
# - get_sd
