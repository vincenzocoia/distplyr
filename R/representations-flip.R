#' @export
eval_cdf.negative <- function(object, at, ...) {
	eval_pmf(object$distribution, at = -at, strict = FALSE) +
		eval_survival(object$distribution, at = -at)
}

#' @export
eval_survival.negative <- function(object, at, ...) {
	with(object$distribution, {
		eval_cdf(distribution, at = -at) -
			eval_pmf(distribution, at = -at, strict = FALSE)
	})
}

#' @export
eval_pmf.negative <- function(object, at, ...) {
	eval_pmf(object$distribution, at = -at, ...)
}

#' @export
eval_density.negative <- function(object, at, ...) {
	eval_density(object$distribution, at = -at, ...)
}

#' @export
eval_quantile.negative <- function(object, at, ...) {
	with(object$distribution, {
		-eval_quantile(object, at = 1 - at)
	})
}

#' @export
realise.negative <- function(object, ...) {
	with(object, {
		-realise(distribution)
	})
}
