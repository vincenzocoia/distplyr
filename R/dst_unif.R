#' Make a Uniform distribution
#'
#' Makes a distribution belonging to the continuous uniform family of
#' distributions.
#' @param min,max Parameters of the distribution family.
#' @return Object of class "dst".
#' dst_unif(0, 1)
#' @export
dst_unif <- function(min = 0, max = 1) {
	if (max < min) stop("Parameter 'min' must be less than 'max'.")
	if (max == min) return(dst_degenerate(min))
	res <- list(name = "Uniform",
				discontinuities = make_empty_discontinuities_df(),
				parameters = list(min = min, max = max))
	new_distribution(
		res,
		variable = "continuous",
		class    = "unif"
	)
}



#' @export
mean.unif <- function(object, ...) {
	with(parameters(object), (min + max) / 2)
}

#' @export
median.unif <- function(object) {
	with(parameters(object), (min + max) / 2)
}

#' @export
variance.unif <- function(object, ...) {
	with(parameters(object), (min - max)^2 / 12)
}


#' @export
evi.unif <- function(object) {
	-1
}

#' @export
skewness.unif <- function(object) {
	0
}

#' @export
kurtosis_exc.unif <- function(object) {
	-6/5
}

#' @export
eval_cdf.unif <- function(object, at) {
	with(parameters(object), {
		stats::punif(at, min = min, max = max)
	})
}

#' @export
eval_survival.unif <- function(object, at) {
	with(parameters(object), {
		stats::punif(at, min = min, max = max, lower.tail = FALSE)
	})
}

#' @export
eval_density.unif <- function(object, at) {
	with(parameters(object), {
		stats::dunif(at, min = min, max = max)
	})
}

#' @export
realise.unif <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::runif(n, min = min, max = max)
	})
}

#' @export
eval_quantile.unif <- function(object, at, ...) {
	with(parameters(object), {
		stats::qunif(at, min = min, max = max)
	})
}


# Using .dst method for:
# - get_hazard
# - get_chf
# - sd
