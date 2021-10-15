#' @export
eval_cdf.shift <- function(distribution, at) {
	with(distribution$components, {
		eval_cdf(distribution, at - shift)
	})
}

#' @export
eval_quantile.shift <- function(distribution, at, ...) {
	with(distribution$components, {
		eval_quantile(distribution, at - shift, ...)
	})
}

#' @export
eval_pmf.shift <- function(distribution, at, ...) {
	with(distribution$components, {
		eval_pmf(distribution, at - shift, ...)
	})
}

#' @export
eval_density.shift <- function(distribution, at) {
	with(distribution$components, {
		eval_density(distribution, at - shift)
	})
}

#' @export
eval_survival.shift <- function(distribution, at) {
	with(distribution$components, {
		eval_survival(distribution, at - shift)
	})
}

#' @export
realise.shift <- function(distribution, ...) {
	with(distribution$components, {
		realise(distribution, ...) + shift
	})
}

#' @export
eval_quantile.shift <- function(distribution, at, ...) {
	with(distribution$components, {
		eval_quantile(distribution, at, ...) + shift
	})
}
