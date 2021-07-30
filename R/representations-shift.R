#' @export
eval_cdf.shift <- function(object, at) {
	with(object$components, {
		eval_cdf(distribution, at - shift)
	})
}

#' @export
eval_quantile.shift <- function(object, at, ...) {
	with(object$components, {
		eval_quantile(distribution, at - shift, ...)
	})
}

#' @export
eval_pmf.shift <- function(object, at, ...) {
	with(object$components, {
		eval_pmf(distribution, at - shift, ...)
	})
}

#' @export
eval_density.shift <- function(object, at) {
	with(object$components, {
		eval_density(distribution, at - shift)
	})
}

#' @export
eval_survival.shift <- function(object, at) {
	with(object$components, {
		eval_survival(distribution, at - shift)
	})
}

#' @export
realise.shift <- function(object, ...) {
	with(object$components, {
		realise(distribution, ...) + shift
	})
}

#' @export
eval_quantile.shift <- function(object, at, ...) {
	with(object$components, {
		eval_quantile(distribution, at, ...) + shift
	})
}
