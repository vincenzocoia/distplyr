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
				parameters = list(mean = mean, variance = variance, sd = sqrt(variance)))
	new_distribution(
		res,
		variable = "continuous",
		class    = "norm"
	)
}


#' @export
mean.norm <- function(object, ...) {
	with(parameters(object), mean)
}

#' @export
median.norm <- function(object) {
	with(parameters(object), mean)
}

#' @export
variance.norm <- function(object, ...) {
	with(parameters(object), variance)
}

#' @export
sd.norm <- function(object, ...) {
	with(parameters(object), sd)
}

#' @export
evi.norm <- function(object) {
	0
}

#' @export
skewness.norm <- function(object) {
	0
}

#' @export
kurtosis_exc.norm <- function(object) {
	0
}

#' @export
eval_cdf.norm <- function(object, at) {
	with(parameters(object), {
		stats::pnorm(at, mean = mean, sd = sd)
	})
}

#' @export
eval_survival.norm <- function(object, at) {
	with(parameters(object), {
		stats::pnorm(at, mean = mean, sd = sd, lower.tail = FALSE)
	})
}

#' @export
eval_density.norm <- function(object, at) {
	with(parameters(object), {
		stats::dnorm(at, mean = mean, sd = sd)
	})
}

#' @export
realise.norm <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::rnorm(n, mean = mean, sd = sd)
	})
}

#' @export
eval_quantile.norm <- function(object, at, ...) {
	with(parameters(object), {
		stats::qnorm(at, mean = mean, sd = sd)
	})
}


# Using .dst method for:
# - get_hazard
# - get_chf






