#' @export
eval_cdf.unif <- function(distribution, at) {
	with(parameters(distribution), {
		stats::punif(at, min = min, max = max)
	})
}

#' @export
eval_density.unif <- function(distribution, at, strict = TRUE) {
	with(parameters(distribution), {
		res <-
			resolve_if_possible(stats::dunif(!!at, min = !!min, max = !!max))
		if (res$resolved) {
			res$outcome
		} else {
			rlang::expr_print(res$outcome)
		}
	})
}

#' @export
eval_quantile.unif <- function(distribution, at, ...) {
	with(parameters(distribution), {
		stats::qunif(at, min = min, max = max)
	})
}

#' @export
realise.unif <- function(distribution, n = 1, ...) {
	with(parameters(distribution), {
		stats::runif(n, min = min, max = max)
	})
}
