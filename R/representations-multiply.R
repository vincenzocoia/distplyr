#' @export
eval_cdf.scale <- function(object, at) {
	with(object$components, {
		eval_cdf(distribution, at = at / scale)
	})
}

#' @export
eval_quantile.scale <- function(object, at) {
	with(object$components, {
		eval_quantile(distribution, at) * scale
	})
}

#' @export
eval_pmf.scale <- function(object, at, strict = TRUE, ...) {
	with(object$components, {
		eval_pmf(distribution, at / scale, strict = strict, ...)
	})
}

#' @export
eval_density.scale <- function(object, at) {
	with(object$components, {
		eval_density(distribution, at / scale) / scale
	})
}

#' @export
eval_survival.scale <- function(object, at) {
	with(object$components, {
		eval_survival(distribution, at / scale)
	})
}

#' @export
realise.scale <- function(object, ...) {
	with(object$components, {
		realise(distribution, ...) * scale
	})
}
