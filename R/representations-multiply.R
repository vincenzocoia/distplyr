#' @export
eval_cdf.scale <- function(distribution, at) {
	with(distribution$components, {
		distionary::eval_cdf(distribution, at = at / scale)
	})
}

#' @export
eval_quantile.scale <- function(distribution, at) {
	with(distribution$components, {
		distionary::eval_quantile(distribution, at) * scale
	})
}

#' @export
eval_pmf.scale <- function(distribution, at, strict = TRUE, ...) {
	with(distribution$components, {
		distionary::eval_pmf(distribution, at / scale, strict = strict, ...)
	})
}

#' @export
eval_density.scale <- function(distribution, at) {
	with(distribution$components, {
		distionary::eval_density(distribution, at / scale) / scale
	})
}

#' @export
eval_survival.scale <- function(distribution, at) {
	with(distribution$components, {
		distionary::eval_survival(distribution, at / scale)
	})
}

#' @export
realise.scale <- function(distribution, ...) {
	with(distribution$components, {
		distionary::realise(distribution, ...) * scale
	})
}
