#' @export
eval_cdf.pois <- function(distribution, at) {
	with(parameters(distribution), {
		stats::ppois(at, lambda = lambda)
	})
}

#' @export
eval_survival.pois <- function(distribution, at) {
	with(parameters(distribution), {
		stats::ppois(at, lambda = lambda, lower.tail = FALSE)
	})
}

#' @export
eval_pmf.pois <- function(distribution, at, strict = TRUE) {
	with(parameters(distribution), {
		suppressWarnings(stats::dpois(at, lambda = lambda))
	})
}

#' @export
realise.pois <- function(distribution, n = 1, ...) {
	with(parameters(distribution), {
		stats::rpois(n, lambda = lambda)
	})
}

#' @export
eval_quantile.pois <- function(distribution, at, ...) {
	with(parameters(distribution), {
		stats::qpois(at, lambda = lambda)
	})
}
