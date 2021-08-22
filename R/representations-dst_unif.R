#' @export
eval_cdf.unif <- function(object, at) {
	with(parameters(object), {
		stats::punif(at, min = min, max = max)
	})
}

#' @export
eval_density.unif <- function(object, at, strict = TRUE) {
	with(parameters(object), {
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
eval_quantile.unif <- function(object, at, ...) {
	with(parameters(object), {
		stats::qunif(at, min = min, max = max)
	})
}

#' @export
realise.unif <- function(object, n = 1, ...) {
	with(parameters(object), {
		stats::runif(n, min = min, max = max)
	})
}
