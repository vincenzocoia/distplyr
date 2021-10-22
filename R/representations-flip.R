#' @export
eval_cdf.negative <- function(distribution, at, ...) {
	eval_pmf(distribution$distribution, at = -at, strict = FALSE) +
		eval_survival(distribution$distribution, at = -at)
}

#' @export
eval_survival.negative <- function(distribution, at, ...) {
	eval_cdf(distribution$distribution, at = -at) -
		eval_pmf(distribution$distribution, at = -at, strict = FALSE)
}

#' @export
eval_pmf.negative <- function(distribution, at, ...) {
	eval_pmf(distribution$distribution, at = -at, ...)
}

#' @export
eval_density.negative <- function(distribution, at, ...) {
	eval_density(distribution$distribution, at = -at, ...)
}

#' @export
eval_quantile.negative <- function(distribution, at, ...) {
	-eval_quantile(distribution$distribution, at = 1 - at)
}

#' @export
realise.negative <- function(distribution, ...) {
	-realise(distribution$distribution)
}
