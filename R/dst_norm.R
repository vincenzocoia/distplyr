#' Normal (Gaussian) Distribution
#'
#' Makes a distribution belonging to the family of
#' Normal (Gaussian) distributions.
#' @param mean,variance Mean and variance of the distribution.
#' @return Object of class "dst".
#' dst_norm(0, 1)
#' @export
dst_norm <- function(mean, variance) {
	# qq <- rlang::enquos(mean, variance)
	# qmean <- rlang::enquo(mean)
	# qvariance <- rlang::enquo(variance)
	# try_variance <- try(rlang::eval_tidy(qvariance), silent = TRUE)
	try_variance <- variance
	if (!inherits(try_variance, "try-error")) {
		if (try_variance == 0) return(dst_degenerate(mean))
		if (try_variance < 0) stop("'variance' parameter must be non-negative.")
	}
	res <- list(name = "Gaussian",
				discontinuities = make_empty_discontinuities_df(),
				parameters = list(mean = mean, variance = variance))
	new_dst(
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
realise.norm <- function(object, n = 1) {
	with(
		parameters(object),
		stats::rnorm(n, mean = mean, sd = sd)
	)
}

#' @export
get_quantile.norm <- function(object, ...) {
	with(
		parameters(object),
		function(x) stats::qnorm(x, mean = mean, sd = sd)
	)
}


# Using .dst method for:
# - get_hazard
# - get_chf






