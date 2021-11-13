#' @export
eval_cdf.negative <- function(distribution, at) {
	distionary::eval_pmf(distribution$distribution, at = -at, strict = FALSE) +
		distionary::eval_survival(distribution$distribution, at = -at)
}

#' @export
eval_survival.negative <- function(distribution, at) {
	distionary::eval_cdf(distribution$distribution, at = -at) -
		distionary::eval_pmf(
			distribution$distribution, at = -at, strict = FALSE
		)
}

#' @export
eval_pmf.negative <- function(distribution, at, ...) {
	distionary::eval_pmf(distribution$distribution, at = -at, ...)
}

#' @export
eval_density.negative <- function(distribution, at, ...) {
	distionary::eval_density(distribution$distribution, at = -at, ...)
}

#' @export
eval_quantile.negative <- function(distribution, at, ...) {
	-distionary::eval_quantile(distribution$distribution, at = 1 - at)
}

#' @export
realise.negative <- function(distribution, ...) {
	-distionary::realise(distribution$distribution)
}
