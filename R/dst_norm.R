#' Normal (Gaussian) Distribution
#'
#' Makes a distribution belonging to the family of
#' Normal (Gaussian) distributions.
#' @param mean,variance Mean and variance of the distribution.
#' @return Object of class "dst".
#' @export
dst_norm <- function(mean, variance) {
	if (variance == 0) return(dst_degenerate(mean))
	if (variance < 0) stop("'variance' parameter must be non-negative.")
	sd <- sqrt(variance)
	res <- list(params = list(mean = mean, variance = variance, sd = sd))
	new_parametric_dst(
		res,
		variable = "continuous",
		class    = "norm"
	)
}


#' @export
get_mean.norm <- function(object, ...) {
	with(parameters(object), mean)
}

#' @export
get_median.norm <- function(object) {
	with(parameters(object), mean)
}

#' @export
get_variance.norm <- function(object, ...) {
	with(parameters(object), variance)
}

#' @export
get_sd.norm <- function(object, ...) {
	with(parameters(object), sd)
}

#' @export
get_evi.norm <- function(object) {
	0
}

#' @export
get_skewness.norm <- function(object) {
	0
}

#' @export
get_kurtosis_exc.norm <- function(object) {
	0
}

#' @export
get_cdf.norm <- function(object) {
	with(
		parameters(object),
		function(x) stats::pnorm(x, mean = mean, sd = sd)
	)
}

#' @export
get_survival.norm <- function(object) {
	with(
		parameters(object),
		function(x) stats::pnorm(x, mean = mean, sd = sd, lower.tail = FALSE)
	)
}

#' @export
get_probfn.norm <- function(object) {
	with(
		parameters(object),
		function(x) stats::dnorm(x, mean = mean, sd = sd)
	)
}

#' @export
get_randfn.norm <- function(object) {
	with(
		parameters(object),
		function(n) stats::rnorm(n, mean = mean, sd = sd)
	)
}

#' @export
get_quantile.norm <- function(object) {
	with(
		parameters(object),
		function(x) stats::qnorm(x, mean = mean, sd = sd)
	)
}


# Using .dst method for:
# - get_hazard
# - get_chf






