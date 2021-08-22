#' @export
eval_cdf.pois <- function(object, at) {
	with(parameters(object), {
		stats::ppois(at, lambda = lambda)
	})
}

#' @export
eval_survival.pois <- function(object, at) {
	with(parameters(object), {
		stats::ppois(at, lambda = lambda, lower.tail = FALSE)
	})
}

#' @export
eval_pmf.pois <- function(object, at, strict = TRUE) {
	with(parameters(object), {
		suppressWarnings(stats::dpois(at, lambda = lambda))
	})
}

#' @export
realise.pois <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::rpois(n, lambda = lambda)
	})
}

#' @export
eval_quantile.pois <- function(object, at, ...) {
	with(parameters(object), {
		stats::qpois(at, lambda = lambda)
	})
}
